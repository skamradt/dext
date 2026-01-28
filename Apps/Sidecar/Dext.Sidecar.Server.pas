unit Dext.Sidecar.Server;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.DateUtils,
  System.JSON,
  System.Types,
  System.SyncObjs,
  Winapi.Windows,
  Dext.DI.Core,
  Dext.DI.Interfaces,
  Dext.Hosting.CLI.Logger,
  Dext.Hosting.CLI.Registry,
  Dext.Logging,
  Dext.Hosting.ApplicationLifetime,
  Dext.Web.Interfaces,
  Dext.Web.Hubs.Extensions,
  Dext.WebHost,
  Dext.Dashboard.Routes;

type
  TSidecarServer = class
  private
    FHost: IWebHost;
    FPort: Integer;
    FRunning: Boolean;
  public
    constructor Create(APort: Integer = 3030);
    destructor Destroy; override;
    
    procedure Start;
    procedure Stop;
    
    function GetUrl: string;
    property Port: Integer read FPort;
    property Running: Boolean read FRunning;
  end;

implementation



{ TSidecarServer }

constructor TSidecarServer.Create(APort: Integer);
begin
  inherited Create;
  FPort := APort;
  FRunning := False;
end;

destructor TSidecarServer.Destroy;
begin
  Stop;
  inherited;
end;

function TSidecarServer.GetUrl: string;
begin
  Result := Format('http://localhost:%d', [FPort]);
end;

procedure TSidecarServer.Start;
begin
  if FRunning then Exit;
  FRunning := True;

  // Build the host in the main thread
  // Using Start (non-blocking) allows us to run on Main Thread without freezing UI
  FHost := TWebHostBuilder.CreateDefault(nil)
    .UseUrls(Format('http://localhost:%d', [FPort]))
    .ConfigureServices(procedure(Services: IServiceCollection)
      var
        RegistryType: TServiceType;
        LoggerType: TServiceType;
        FactoryFunc: TFunc<IServiceProvider, TObject>;
      begin
        RegistryType := TServiceType.FromClass(TProjectRegistry);
        Services.AddSingleton(RegistryType, TProjectRegistry, nil);
        
        LoggerType := TServiceType.FromInterface(TypeInfo(ILoggerFactory));
        FactoryFunc := function(Provider: IServiceProvider): TObject
           begin
              Result := TLoggerFactory.Create; // Silent logger for Sidecar
           end;
        Services.AddSingleton(LoggerType, TClass(nil), FactoryFunc);
      end)
    .Configure(procedure(App: IApplicationBuilder)
      begin
        TDashboardRoutes.Configure(App);
      end)
    .Build;

  try
    FHost.Start;
  except
    on E: Exception do
      OutputDebugString(PChar('DextSidecar: Run Exception: ' + E.Message));
  end;
end;

procedure TSidecarServer.Stop;
var
  WebHost: IWebHost;
begin
  if not FRunning then Exit;
  
  // 1. Capture local reference and clear field
  WebHost := FHost;
  FHost := nil;
  
  // 2. First, shutdown Hub connections (SSE loops need to exit before Indy stops)
  THubExtensions.ShutdownHubs;
  
  // 3. Signal stop on the local reference
  // This now triggers StopApplication inside TDextApplication
  if WebHost <> nil then
  begin
    try
      WebHost.Stop;
    except
      on E: Exception do
        OutputDebugString(PChar('DextSidecar: Stop Signal Error: ' + E.Message));
    end;
  end;
  
  // 4. Finally release the interface
  WebHost := nil;
  FRunning := False;
end;

end.
