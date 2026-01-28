unit Dext.Testing.Listeners.Telemetry;

interface

uses
  System.SysUtils,
  System.Classes,
  System.TimeSpan,
  System.TypInfo,
  System.IOUtils,
  System.Net.HttpClient,
  System.Net.URLClient,
  Dext.Testing.Runner,
  Dext.Logging,
  Dext.Logging.Global,
  System.Generics.Collections;

type
  TTelemetryTestListener = class(TInterfacedObject, ITestListener)
  private
    FLogger: ILogger;
    FLogCache: TList<string>;
    procedure CacheLog(const Msg: string);
    procedure FlushLogs;
  public
    constructor Create(const ALogger: ILogger);
    destructor Destroy; override;
    
    procedure OnRunStart(TotalTests: Integer);
    procedure OnRunComplete(const Summary: TTestSummary);
    procedure OnFixtureStart(const FixtureName: string; TestCount: Integer);
    procedure OnFixtureComplete(const FixtureName: string);
    procedure OnTestStart(const Fixture, Test: string);
    procedure OnTestComplete(const Info: TTestInfo);
  end;

implementation

{ TTelemetryTestListener }

constructor TTelemetryTestListener.Create(const ALogger: ILogger);
begin
  inherited Create;
  FLogger := ALogger;
  FLogCache := TList<string>.Create;
end;

destructor TTelemetryTestListener.Destroy;
begin
  FLogCache.Free;
  inherited;
end;

procedure TTelemetryTestListener.CacheLog(const Msg: string);
begin
  FLogCache.Add(Msg);
  // Optional: Auto-flush if cache gets too big to avoid memory spikes
  if FLogCache.Count >= 100 then
    FlushLogs;
end;

procedure TTelemetryTestListener.FlushLogs;
var
  Http: THTTPClient;
  LogItem: string;
  AllLogs: TStringStream;
  Resp: IHTTPResponse;
begin
  if FLogCache.Count = 0 then Exit;
  
  Http := THTTPClient.Create;
  try
    for LogItem in FLogCache do
    begin
        try
           AllLogs := TStringStream.Create(LogItem, TEncoding.UTF8);
           try
             Resp := Http.Post('http://localhost:3030/api/telemetry/logs', AllLogs, nil, 
               [TNetHeader.Create('Content-Type', 'application/json')]);
           finally
             AllLogs.Free;
           end;
        except
          on E: Exception do
          begin
             // Failing to send telemetry shouldn't crash the test run
          end;
        end;
    end;
    FLogCache.Clear;
  finally
    Http.Free;
  end;
end;

procedure TTelemetryTestListener.OnRunStart(TotalTests: Integer);
begin
  CacheLog(Format('{"event": "RunStart", "totalTests": %d}', [TotalTests]));
end;

procedure TTelemetryTestListener.OnRunComplete(const Summary: TTestSummary);
begin
  CacheLog(Format('{"event": "RunComplete", "passed": %d, "failed": %d, "duration": %d}', 
    [Summary.Passed, Summary.Failed, Round(Summary.TotalDuration.TotalMilliseconds)]));
  FlushLogs; // Force send at end
end;

procedure TTelemetryTestListener.OnFixtureStart(const FixtureName: string; TestCount: Integer);
begin
  CacheLog(Format('{"event": "FixtureStart", "name": "%s", "tests": %d}', [FixtureName, TestCount]));
end;

procedure TTelemetryTestListener.OnFixtureComplete(const FixtureName: string);
begin
   CacheLog(Format('{"event": "FixtureComplete", "name": "%s"}', [FixtureName]));
end;

procedure TTelemetryTestListener.OnTestStart(const Fixture, Test: string);
begin
  CacheLog(Format('{"event": "TestStart", "fixture": "%s", "test": "%s"}', [Fixture, Test]));
end;

procedure TTelemetryTestListener.OnTestComplete(const Info: TTestInfo);
var
  Status: string;
begin
   case Info.Result of
     trPassed: Status := 'Passed';
     trFailed: Status := 'Failed';
     trSkipped: Status := 'Skipped';
     trTimeout: Status := 'Timeout';
     trError: Status := 'Error';
   end;

   if (Info.Result = trFailed) or (Info.Result = trError) then
     CacheLog(Format('{"event": "TestComplete", "fixture": "%s", "test": "%s", "status": "%s", "duration": %d, "error": "%s"}', 
       [Info.FixtureName, Info.TestName, Status, Round(Info.Duration.TotalMilliseconds), Info.ErrorMessage.Replace('"', '\"')]))
   else
     CacheLog(Format('{"event": "TestComplete", "fixture": "%s", "test": "%s", "status": "%s", "duration": %d}', 
       [Info.FixtureName, Info.TestName, Status, Round(Info.Duration.TotalMilliseconds)]));
end;

end.
