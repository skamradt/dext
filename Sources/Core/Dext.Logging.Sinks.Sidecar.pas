{***************************************************************************}
{           Dext Framework                                                  }
{           Copyright (C) 2025 Cesar Romero & Dext Contributors             }
{***************************************************************************}
unit Dext.Logging.Sinks.Sidecar;

interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.TypInfo,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.DateUtils,
  Dext.Logging,
  Dext.Logging.Async,
  Dext.Logging.RingBuffer,
  Dext.Types.UUID;

type
  /// <summary>
  ///   Sink that sends logs to Dext Sidecar via HTTP.
  /// </summary>
  TSidecarSink = class(TInterfacedObject, ILogSink)
  private
    FUrl: string;
    FClient: THTTPClient;
    FBuffer: TStringBuilder;
    FLock: TObject;
    FFlushTimer: TThread; // Background flusher
    FCount: Integer;
    
    procedure FlushInternal;
  public
    constructor Create(const ABaseUrl: string = 'http://localhost:5000');
    destructor Destroy; override;
    
    procedure Emit(const Entry: TLogEntry);
    procedure Flush;
  end;
  
  // Simple Flush Timer to ensure logs are sent even if low volume
  TSidecarFlushTimer = class(TThread)
  private
    FSink: TSidecarSink;
  public
    constructor Create(ASink: TSidecarSink);
    procedure Execute; override;
  end;

implementation

{ TSidecarSink }

constructor TSidecarSink.Create(const ABaseUrl: string);
begin
  inherited Create;
  FUrl := ABaseUrl + '/api/telemetry/logs';
  FClient := THTTPClient.Create;
  FClient.ContentType := 'application/json';
  
  FBuffer := TStringBuilder.Create;
  FLock := TObject.Create;
  FCount := 0;
  
  FFlushTimer := TSidecarFlushTimer.Create(Self);
end;

destructor TSidecarSink.Destroy;
begin
  if FFlushTimer <> nil then
  begin
    FFlushTimer.Terminate;
    FFlushTimer.WaitFor;
    FFlushTimer.Free;
  end;
  
  FlushInternal; // Send remaining
  
  FClient.Free;
  FBuffer.Free;
  FLock.Free;
  inherited;
end;

procedure TSidecarSink.Emit(const Entry: TLogEntry);
begin
  TMonitor.Enter(FLock);
  try
    // Format as JSON Object in a list
    // If buffer is empty, start list
    if FCount = 0 then
      FBuffer.Append('[');
    
    if FCount > 0 then
      FBuffer.Append(',');
      
    FBuffer.Append('{');
    FBuffer.Append('"ts":"').Append(DateToISO8601(Entry.TimeStamp)).Append('",');
    FBuffer.Append('"lvl":"').Append(GetEnumName(TypeInfo(TLogLevel), Integer(Entry.Level))).Append('",');
    
    if not Entry.TraceId.IsEmpty then
      FBuffer.Append('"traceId":"').Append(Entry.TraceId.ToString).Append('",');
      
    if not Entry.SpanId.IsEmpty then
      FBuffer.Append('"spanId":"').Append(Entry.SpanId.ToString).Append('",');
      
    // ThreadID
    FBuffer.Append('"tid":').Append(Entry.ThreadID).Append(',');
    
    FBuffer.Append('"msg":"').Append(Entry.FormattedMessage.Replace('\', '\\').Replace('"', '\"').Replace(#13, '\r').Replace(#10, '\n')).Append('"');
    FBuffer.Append('}');
    
    Inc(FCount);
    
    // Auto-flush on count or size
    if (FCount >= 50) or (FBuffer.Length > 64 * 1024) then
      FlushInternal;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TSidecarSink.Flush;
begin
  TMonitor.Enter(FLock);
  try
    FlushInternal;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TSidecarSink.FlushInternal;
var
  Payload: TStringStream;
  Resp: IHTTPResponse;
begin
  if FCount = 0 then Exit;
  
  FBuffer.Append(']'); // Close list
  
  try
    Payload := TStringStream.Create(FBuffer.ToString, TEncoding.UTF8);
    try
      // Fire and Forget? Or wait?
      // Since we are in Consumer Thread (or Timer), blocking is bad but necessary for HTTP.
      // 50 logs won't take long.
      try
        Resp := FClient.Post(FUrl, Payload);
      except
        // Network error - ignore to prevent crash
        // Ideally retry or log to fallback
      end;
    finally
      Payload.Free;
    end;
  except
    // ignore
  end;
  
  // Reset
  FBuffer.Clear;
  FCount := 0;
end;

{ TSidecarFlushTimer }

constructor TSidecarFlushTimer.Create(ASink: TSidecarSink);
begin
  inherited Create(False);
  FSink := ASink;
end;

procedure TSidecarFlushTimer.Execute;
begin
  while not Terminated do
  begin
    Sleep(1000); // 1 second
    if not Terminated then
      FSink.Flush;
  end;
end;

end.
