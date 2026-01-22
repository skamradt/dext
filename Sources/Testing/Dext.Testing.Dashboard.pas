unit Dext.Testing.Dashboard;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  IdHTTPServer,
  IdContext,
  IdCustomHTTPServer,
  IdGlobal, // Added for ToBytes/TIdBytes
  System.Types, // Added for RT_RCDATA
  Dext.Testing.Runner,
  Dext.Testing.History;

type
  { TDashboardListener }
  TDashboardListener = class(TInterfacedObject, ITestListener)
  private
    FServer: TIdHTTPServer;
    FClients: TList<TIdContext>;
    FLock: TObject;
    FPort: Integer;
    FEventBuffer: TList<string>; 
    
    // Server events
    procedure OnConnect(AContext: TIdContext);
    procedure OnDisconnect(AContext: TIdContext);
    procedure OnCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    
    // ITestListener implementation
    procedure OnRunStart(TotalTests: Integer);
    procedure OnRunComplete(const Summary: TTestSummary);
    procedure OnFixtureStart(const FixtureName: string; TestCount: Integer);
    procedure OnFixtureComplete(const FixtureName: string);
    procedure OnTestStart(const Fixture, Test: string);
    procedure OnTestComplete(const Info: TTestInfo);
    
    // Helper
    procedure BroadcastEvent(const EventType: string; const DataJson: string);
  public
    constructor Create(Port: Integer = 9000);
    destructor Destroy; override;
    
    procedure Start;
    procedure Stop;
  end;

implementation

uses
  Dext.Utils;


{$R 'Dext.Dashboard.res'}


{ TDashboardListener }

constructor TDashboardListener.Create(Port: Integer);
begin
  inherited Create;
  FPort := Port;
  FLock := TObject.Create;
  FClients := TList<TIdContext>.Create;
  FEventBuffer := TList<string>.Create;
  
  FServer := TIdHTTPServer.Create(nil);
  FServer.OnConnect := OnConnect;
  FServer.OnDisconnect := OnDisconnect;
  FServer.OnCommandGet := OnCommandGet;
  FServer.DefaultPort := FPort;
end;

destructor TDashboardListener.Destroy;
begin
  Stop;
  FServer.Free;
  FClients.Free;
  FEventBuffer.Free;
  FLock.Free;
  inherited;
end;

procedure TDashboardListener.Start;
begin
  if not FServer.Active then
  begin
    FServer.Active := True;
    SafeWriteLn('Dext Dashboard running at http://localhost:' + FPort.ToString);
    TTestRunner.RegisterListener(Self);
  end;
end;

procedure TDashboardListener.Stop;
begin
  if FServer.Active then
    FServer.Active := False;
end;

procedure TDashboardListener.OnConnect(AContext: TIdContext);
begin
end;

procedure TDashboardListener.OnDisconnect(AContext: TIdContext);
begin
  TMonitor.Enter(FLock);
  try
    FClients.Remove(AContext);
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TDashboardListener.OnCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Msg: string;
begin
  if SameText(ARequestInfo.URI, '/') then
  begin
    AResponseInfo.ContentStream := TResourceStream.Create(HInstance, 'DASHBOARD_HTML', RT_RCDATA); 
    AResponseInfo.ContentType := 'text/html; charset=utf-8';
    AResponseInfo.ResponseNo := 200;
  end
  else if SameText(ARequestInfo.URI, '/styles/dashboard.css') then
  begin
    AResponseInfo.ContentStream := TResourceStream.Create(HInstance, 'DASHBOARD_CSS', RT_RCDATA); 
    AResponseInfo.ContentType := 'text/css';
    AResponseInfo.ResponseNo := 200;
  end
  else if SameText(ARequestInfo.URI, '/scripts/dashboard.js') then
  begin
    AResponseInfo.ContentStream := TResourceStream.Create(HInstance, 'DASHBOARD_JS', RT_RCDATA); 
    AResponseInfo.ContentType := 'text/javascript';
    AResponseInfo.ResponseNo := 200;
  end
  else if SameText(ARequestInfo.URI, '/favicon.ico') then
  begin
    AResponseInfo.ResponseNo := 204; // No Content
  end
  else if SameText(ARequestInfo.URI, '/api/history') then
  begin
    AResponseInfo.ContentText := TTestHistoryManager.LoadHistoryJson;
    AResponseInfo.ContentType := 'application/json';
    AResponseInfo.ResponseNo := 200;
  end
  else if SameText(ARequestInfo.URI, '/events') then
  begin
    // Write Raw Headers to force Content-Type and avoid defaults    
    AContext.Connection.IOHandler.WriteLn('HTTP/1.1 200 OK');
    AContext.Connection.IOHandler.WriteLn('Content-Type: text/event-stream; charset=utf-8');
    AContext.Connection.IOHandler.WriteLn('Cache-Control: no-cache');
    AContext.Connection.IOHandler.WriteLn('Connection: keep-alive');
    AContext.Connection.IOHandler.WriteLn('');

    
    TMonitor.Enter(FLock);
    try
      // if FEventBuffer.Count > 0 then
      //   SafeWriteLn('Replaying ' + FEventBuffer.Count.ToString + ' events to new client');

      // Replay existing events to the new client
      for Msg in FEventBuffer do
      begin
        try
           // Write raw bytes to avoid Indy string length prefix
           AContext.Connection.IOHandler.Write(ToBytes(Msg, IndyTextEncoding_UTF8));
        except
        end;
      end;
      
      if not FClients.Contains(AContext) then
        FClients.Add(AContext);
    finally
      TMonitor.Exit(FLock);
    end;
    
    // Hold for SSE
    try
       while AContext.Connection.Connected and FServer.Active do
         Sleep(100); 
    except
    end;
    
    TMonitor.Enter(FLock);
    try
      FClients.Remove(AContext);
    finally
      TMonitor.Exit(FLock);
    end;
  end
  else
  begin
    AResponseInfo.ResponseNo := 404;
  end;
end;

procedure TDashboardListener.BroadcastEvent(const EventType: string; const DataJson: string);
var
  Ctx: TIdContext;
  I: Integer;
  FullMsg: string;
begin
  // Format message once
  FullMsg := 'event: ' + EventType + sLineBreak + 
             'data: ' + DataJson + sLineBreak + 
             sLineBreak;

  TMonitor.Enter(FLock);
  try
    // Store in buffer
    FEventBuffer.Add(FullMsg);
    
    // Send to active clients
    for I := FClients.Count - 1 downto 0 do
    begin
      Ctx := FClients[I];
      try
        // Write raw bytes to avoid Indy string length prefix
        Ctx.Connection.IOHandler.Write(ToBytes(FullMsg, IndyTextEncoding_UTF8));
      except
        FClients.Remove(Ctx);
      end;
    end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TDashboardListener.OnRunStart(TotalTests: Integer);
begin
  // Clear buffer on new run
  TMonitor.Enter(FLock);
  try
    FEventBuffer.Clear;
  finally
    TMonitor.Exit(FLock);
  end;
  BroadcastEvent('run_start', Format('{"total": %d}', [TotalTests]));
end;

procedure TDashboardListener.OnRunComplete(const Summary: TTestSummary);
begin
  // Save history
  TTestHistoryManager.AppendRun(Summary);
  
  BroadcastEvent('run_complete', '{}');
  
  SafeWriteLn;
  SafeWriteLn('📊 Dext Dashboard: http://localhost:' + FPort.ToString);
  SafeWriteLn;
end;

procedure TDashboardListener.OnFixtureStart(const FixtureName: string; TestCount: Integer);
begin
  // Not used in frontend yet
end;

procedure TDashboardListener.OnFixtureComplete(const FixtureName: string);
begin
  // Not used
end;

procedure TDashboardListener.OnTestStart(const Fixture, Test: string);
begin
  BroadcastEvent('test_start', Format('{"fixture": "%s", "test": "%s"}', [Fixture, Test]));
end;

procedure TDashboardListener.OnTestComplete(const Info: TTestInfo);
var
  Status: string;
  ErrMsg: string;
begin
  if Info.Result = trPassed then Status := 'passed'
  else if Info.Result = trSkipped then Status := 'skipped'
  else Status := 'failed';

  ErrMsg := Info.ErrorMessage.Replace('"', '\"').Replace(#13#10, '\n').Replace(#10, '\n').Replace(#13, '');
  
  BroadcastEvent('test_complete', Format('{"fixture": "%s", "test": "%s", "status": "%s", "passed": %s, "duration": %d, "error": "%s"}', 
    [Info.FixtureName, Info.DisplayName, Status, BoolToStr(Info.Result = trPassed, True).ToLower, Round(Info.Duration.TotalMilliseconds), ErrMsg]));
end;

end.
