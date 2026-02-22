unit WebFrameworkTests.Tests.Base;

interface

uses
  System.SysUtils,
  System.Classes,
  Dext.WebHost,
  Dext.DI.Interfaces,
  Dext.Web.Interfaces,
  System.Net.HttpClient;

type
  TBaseTest = class
  protected
    FPort: Integer;
    FClient: THttpClient;
    FHost: IWebHost;
    FServerThread: TThread; // Explicit thread management
    FServerError: string;
    
    procedure Log(const Msg: string);
    procedure LogSuccess(const Msg: string);
    procedure LogError(const Msg: string);
    procedure AssertTrue(Condition: Boolean; const SuccessMsg, FailMsg: string);
    procedure AssertEqual(const Expected, Actual: string; const Context: string);
    
    procedure Setup; virtual;
    procedure TearDown; virtual;
    
    /// <summary>
    ///  Configures the web host builder. Override to add services/middleware.
    /// </summary>
    procedure ConfigureHost(const Builder: IWebHostBuilder); virtual;

    function GetBaseUrl: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run; virtual; abstract;
  end;

  TBaseTestClass = class of TBaseTest;

implementation

{ TBaseTest }

constructor TBaseTest.Create;
begin
  inherited;
  FPort := 8081; // Default test port
  FClient := THttpClient.Create;
  Setup;
end;

destructor TBaseTest.Destroy;
begin
  TearDown;
  FClient.Free;
  inherited;
end;

procedure TBaseTest.Setup;
var
  Builder: IWebHostBuilder;
  HostRef: IWebHost; // Explicitly capture FHost for thread safety
begin
  WriteLn('🔧 Setting up test...');
  Builder := TDextWebHost.CreateDefaultBuilder
    .UseUrls('http://localhost:' + FPort.ToString);
    
  ConfigureHost(Builder);
  
  FHost := Builder.Build;
  
  Builder := nil; // Force release
  
  FServerError := '';
  FServerThread := TThread.CreateAnonymousThread(procedure
    begin
      try
        // Keep a reference to prevent premature destruction
        HostRef := FHost; 
        if HostRef <> nil then
        begin
          // Use Start instead of Run because Run might exit prematurely 
          // if the 'no-wait' command line switch is present.
          HostRef.Start;
        end;
      except
        on E: Exception do
          FServerError := E.Message;
      end;
    end);
    
  FServerThread.FreeOnTerminate := False;
  FServerThread.Start;
  
  // Robust wait for server to start
  var Retries := 0;
  var Success := False;
  while (Retries < 50) and (not Success) and (FServerError = '') do
  begin
    try
      // Try to connect to the base URL
      var Resp := FClient.Get(GetBaseUrl + '/');
      if Resp <> nil then
      begin
        Success := True;
        Break;
      end;
    except
      // Server not yet active
    end;
    
    Sleep(100);
    Inc(Retries);
  end;

  if FServerError <> '' then
    raise Exception.Create('Server failed to start: ' + FServerError);

  if not Success then
    raise Exception.Create('Server start timeout after 5 seconds at ' + GetBaseUrl);
end;

procedure TBaseTest.ConfigureHost(const Builder: IWebHostBuilder);
begin
  // Default implementation does nothing
end;

procedure TBaseTest.TearDown;
begin
  if Assigned(FHost) then
  begin
    FHost.Stop;
  end;
  
  if Assigned(FServerThread) then
  begin
    FServerThread.WaitFor;
    FreeAndNil(FServerThread);
  end;
  
  FHost := nil;
end;

function TBaseTest.GetBaseUrl: string;
begin
  Result := Format('http://localhost:%d', [FPort]);
end;

procedure TBaseTest.Log(const Msg: string);
begin
  WriteLn(Msg);
end;

procedure TBaseTest.LogSuccess(const Msg: string);
begin
  WriteLn('   ✅ ' + Msg);
end;

procedure TBaseTest.LogError(const Msg: string);
begin
  WriteLn('   ❌ ' + Msg);
end;

procedure TBaseTest.AssertTrue(Condition: Boolean; const SuccessMsg, FailMsg: string);
begin
  if Condition then
    LogSuccess(SuccessMsg)
  else
    LogError(FailMsg);
end;

procedure TBaseTest.AssertEqual(const Expected, Actual: string; const Context: string);
begin
  if Expected = Actual then
    LogSuccess(Format('%s: Expected "%s" and got "%s"', [Context, Expected, Actual]))
  else
    LogError(Format('%s: Expected "%s" BUT got "%s"', [Context, Expected, Actual]));
end;

end.
