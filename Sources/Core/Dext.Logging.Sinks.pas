{***************************************************************************}
{           Dext Framework                                                  }
{           Copyright (C) 2025 Cesar Romero & Dext Contributors             }
{***************************************************************************}
unit Dext.Logging.Sinks;

interface

uses
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  System.IOUtils,
  System.SyncObjs,
  Dext.Types.UUID,
  Dext.Logging,
  Dext.Logging.Async,
  Dext.Logging.RingBuffer;

type
  /// <summary>
  ///   Sink that writes to Standard Output (Console).
  /// </summary>
  TConsoleSink = class(TInterfacedObject, ILogSink)
  public
    procedure Emit(const Entry: TLogEntry);
    procedure Flush;
  end;

  /// <summary>
  ///   Sink that writes to a file.
  /// </summary>
  TFileSink = class(TInterfacedObject, ILogSink)
  private
    FFileName: string;
    FBuffer: TStringBuilder;
    FLock: TObject;
    procedure FlushInternal;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    procedure Emit(const Entry: TLogEntry);
    procedure Flush;
  end;

implementation

{ TConsoleSink }

procedure TConsoleSink.Emit(const Entry: TLogEntry);
var
  Lvl: string;

begin
  // Simple color coding (ANSI)
  case Entry.Level of
    TLogLevel.Trace: Lvl := 'TRC';
    TLogLevel.Debug: Lvl := 'DBG';
    TLogLevel.Information: Lvl := 'INF';
    TLogLevel.Warning: Lvl := 'WRN';
    TLogLevel.Error: Lvl := 'ERR';
    TLogLevel.Critical: Lvl := 'CRT';
    else Lvl := 'UNK';
  end;
  
  // Format: [HH:MM:SS INF] Message
  // Format with TraceId: [HH:MM:SS INF] [TraceId] Message
  
  if not Entry.TraceId.IsEmpty then
    WriteLn(Format('[%s %s] [%s] %s', [FormatDateTime('HH:nn:ss.zzz', Entry.TimeStamp), Lvl, Entry.TraceId.ToString, Entry.FormattedMessage]))
  else
    WriteLn(Format('[%s %s] %s', [FormatDateTime('HH:nn:ss.zzz', Entry.TimeStamp), Lvl, Entry.FormattedMessage]));
end;

procedure TConsoleSink.Flush;
begin
  // Flush stdout?
end;

{ TFileSink }

constructor TFileSink.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FBuffer := TStringBuilder.Create;
  FLock := TObject.Create;
  
  // Ensure dir exists
  TDirectory.CreateDirectory(TPath.GetDirectoryName(FFileName));
end;

destructor TFileSink.Destroy;
begin
  FlushInternal;
  FBuffer.Free;
  FLock.Free;
  inherited;
end;

procedure TFileSink.Emit(const Entry: TLogEntry);
begin
  TMonitor.Enter(FLock);
  try
    // Switching to JSON Lines for better machine readability
    // {"ts":"...", "lvl":"INF", "traceId":"...", "msg":"..."}
    FBuffer.Append('{');
    FBuffer.Append('"ts":"').Append(FormatDateTime('yyyy-mm-dd HH:nn:ss.zzz', Entry.TimeStamp)).Append('",');
    FBuffer.Append('"lvl":"').Append(GetEnumName(TypeInfo(TLogLevel), Integer(Entry.Level))).Append('",');
    
    if not Entry.TraceId.IsEmpty then
      FBuffer.Append('"traceId":"').Append(Entry.TraceId.ToString).Append('",');
      
    if not Entry.SpanId.IsEmpty then
      FBuffer.Append('"spanId":"').Append(Entry.SpanId.ToString).Append('",');
      
    FBuffer.Append('"msg":"').Append(Entry.FormattedMessage.Replace('"', '\"').Replace(#13, '\r').Replace(#10, '\n')).Append('"');
    FBuffer.Append('}').AppendLine;
           
    if FBuffer.Length > 4096 then
      FlushInternal;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TFileSink.Flush;
begin
  TMonitor.Enter(FLock);
  try
    FlushInternal;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TFileSink.FlushInternal;
begin
  if FBuffer.Length = 0 then Exit;
  
  try
    TFile.AppendAllText(FFileName, FBuffer.ToString, TEncoding.UTF8);
    FBuffer.Clear;
  except
    // ignore file errors
  end;
end;

end.
