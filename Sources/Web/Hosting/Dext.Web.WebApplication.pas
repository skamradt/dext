{***************************************************************************}
{                                                                           }
{           Dext Framework                                                  }
{                                                                           }
{           Copyright (C) 2025 Cesar Romero & Dext Contributors             }
{                                                                           }
{           Licensed under the Apache License, Version 2.0 (the "License"); }
{           you may not use this file except in compliance with the License.}
{           You may obtain a copy of the License at                         }
{                                                                           }
{               http://www.apache.org/licenses/LICENSE-2.0                  }
{                                                                           }
{           Unless required by applicable law or agreed to in writing,      }
{           software distributed under the License is distributed on an     }
{           "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    }
{           either express or implied. See the License for the specific     }
{           language governing permissions and limitations under the        }
{           License.                                                        }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Author:  Cesar Romero                                                    }
{  Created: 2025-12-08                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.Web.WebApplication;
{$I ..\..\Dext.inc}

interface

uses
  Dext.Web.ControllerScanner,
  Dext.DI.Interfaces,
  Dext.Web.Interfaces,
  Dext.Configuration.Interfaces;

type
  TDextApplication = class(TInterfacedObject, IWebApplication, IWebHost)
  private
    FServices: IServiceCollection;
    FServiceProvider: IServiceProvider;
    FAppBuilder: IApplicationBuilder;
    FScanner: IControllerScanner;
    FConfiguration: IConfiguration;
    FDefaultPort: Integer;
    FActiveHost: IWebHost; // ✅ Track active host
  public
    constructor Create;
    destructor Destroy; override;

    // IWebApplication
    function GetApplicationBuilder: IApplicationBuilder;
    function GetConfiguration: IConfiguration;
    function GetServices: TDextServices;
    function GetBuilder: TDextAppBuilder;
    function BuildServices: IServiceProvider; // ?
    function UseMiddleware(Middleware: TClass): IWebApplication;
    function UseStartup(Startup: IStartup): IWebApplication; // ? Non-generic
    function MapControllers: IWebApplication;
    procedure Run; overload;
    procedure Run(Port: Integer); overload;
    procedure Stop;
    procedure SetDefaultPort(Port: Integer);

    property DefaultPort: Integer read FDefaultPort write FDefaultPort;
  end;

implementation

uses
  System.SysUtils,
  {$IF Defined(MSWINDOWS)}
  Dext.Utils,
  {$ENDIF}
  Dext.DI.Core,
  Dext.Hosting.BackgroundService,
  Dext.Web.Core,
  Dext.Web.Indy.Server,
  Dext.Web.Indy.SSL.Interfaces,
  Dext.Web.Indy.SSL.OpenSSL,
  Dext.Web.Indy.SSL.Taurus,
  Dext.Configuration.Core,
  Dext.Configuration.Json,
  Dext.Configuration.Yaml,
  Dext.Configuration.EnvironmentVariables,
  Dext.HealthChecks,
  Dext.Hosting.ApplicationLifetime,
  Dext.Hosting.AppState,
  Dext.Entity.Core,
  Dext.Entity.Migrations.Runner;

{ TDextApplication }

constructor TDextApplication.Create;
var
  ConfigBuilder: IConfigurationBuilder;
begin
  inherited Create;
  FDefaultPort := 8080;
  {$IF Defined(MSWINDOWS)}
  SetConsoleCharSet(CP_UTF8);
  SetTextCodePage(Output, CP_UTF8);
  {$ENDIF}

  // Initialize Configuration
  ConfigBuilder := TConfigurationBuilder.Create;
  
  // 1. Base appsettings
  ConfigBuilder.Add(TJsonConfigurationSource.Create('appsettings.json', True));
  ConfigBuilder.Add(TYamlConfigurationSource.Create('appsettings.yaml', True));
  ConfigBuilder.Add(TYamlConfigurationSource.Create('appsettings.yml', True));

  // 2. Environment specific appsettings.{Env}
  var Env := GetEnvironmentVariable('DEXT_ENVIRONMENT');
  if Env = '' then Env := 'Production'; // Default to Production
  
  if Env <> '' then
  begin
    ConfigBuilder.Add(TJsonConfigurationSource.Create('appsettings.' + Env + '.json', True));
    ConfigBuilder.Add(TYamlConfigurationSource.Create('appsettings.' + Env + '.yaml', True));
    ConfigBuilder.Add(TYamlConfigurationSource.Create('appsettings.' + Env + '.yml', True));
  end;

  // 3. Environment Variables
  ConfigBuilder.Add(TEnvironmentVariablesConfigurationSource.Create);
    
  FConfiguration := ConfigBuilder.Build;
  
  FServices := TDextServiceCollection.Create;
  
  // Register Configuration
  var LConfig := FConfiguration;
  FServices.AddSingleton(
    TServiceType.FromInterface(IConfiguration),
    TConfigurationRoot,
    function(Provider: IServiceProvider): TObject
    begin
      Result := LConfig as TConfigurationRoot;
    end
  );

  // Register Application Lifetime
  FServices.AddSingleton(
    TServiceType.FromInterface(IHostApplicationLifetime),
    THostApplicationLifetime
  );

  // Register Application State
  FServices.AddSingleton(
    TServiceType.FromInterface(IAppStateObserver),
    TApplicationStateManager
  );
  FServices.AddSingleton(
    TServiceType.FromInterface(IAppStateControl),
    TApplicationStateManager,
    function(Provider: IServiceProvider): TObject
    begin
      // Return the same instance as IAppStateObserver (Singleton)
      Result := Provider.GetService(TServiceType.FromInterface(IAppStateObserver));
    end
  );
  
  // Don't build ServiceProvider yet - will be built lazily after all services registered
  // This ensures services added via WebHostBuilder.AddRange are included
  FServiceProvider := nil;
  FAppBuilder := nil; // Will be created lazily when GetApplicationBuilder is called
  ConfigBuilder := nil;
end;

destructor TDextApplication.Destroy;
begin
  FAppBuilder := nil;
  FServiceProvider := nil;
  inherited Destroy;
end;

function TDextApplication.GetApplicationBuilder: IApplicationBuilder;
begin
  // Lazy initialization: create ApplicationBuilder with nil ServiceProvider initially.
  // The ServiceProvider will be set/updated in Run() AFTER all services are registered.
  if FAppBuilder = nil then
    FAppBuilder := TApplicationBuilder.Create(nil); // Will be updated in Run()
  Result := FAppBuilder;
end;

function TDextApplication.GetConfiguration: IConfiguration;
begin
  Result := FConfiguration;
end;

function TDextApplication.GetServices: TDextServices;
begin
  Result := TDextServices.Create(FServices);
end;

function TDextApplication.GetBuilder: TDextAppBuilder;
begin
  Result := TDextAppBuilder.Create(GetApplicationBuilder);
end;

function TDextApplication.BuildServices: IServiceProvider;
begin
  // ? REBUILD ServiceProvider to include all services registered after Create()
  FServiceProvider := nil; // Release old provider
  FServiceProvider := FServices.BuildServiceProvider;
  // Ensure AppBuilder is updated or created with the new provider
  GetApplicationBuilder.SetServiceProvider(FServiceProvider);
  Result := FServiceProvider;
end;

function TDextApplication.MapControllers: IWebApplication;
var
  RouteCount: Integer;
begin
  // No need to rebuild usage provider here, scanning uses RTTI.
  // FServiceProvider will be rebuilt in Run() to include all services.
  
  FScanner := TControllerScanner.Create;
  RouteCount := FScanner.RegisterRoutes(GetApplicationBuilder);

  if RouteCount = 0 then
  begin
    SafeWriteLn('No routes found!')
  end;

  Result := Self;
end;

procedure TDextApplication.Run;
begin
  Run(FDefaultPort);
end;

procedure TDextApplication.Run(Port: Integer);
var
  WebHost: IWebHost;
  RequestHandler: TRequestDelegate;
  HostedManager: IHostedServiceManager;
  SSLHandler: IIndySSLHandler;
  Lifetime: THostApplicationLifetime;
  StateControl: IAppStateControl;
begin
  FDefaultPort := Port;
  
  // Build ServiceProvider now - this is the correct place to do it,
  // AFTER all services have been registered (including HealthChecks, etc.)
  FServiceProvider := FServices.BuildServiceProvider;
  
  // Update ApplicationBuilder with the final ServiceProvider
  GetApplicationBuilder.SetServiceProvider(FServiceProvider);
  
  // Get Lifetime & State Service
  var LifetimeIntf := FServiceProvider.GetServiceAsInterface(TServiceType.FromInterface(IHostApplicationLifetime));
  if LifetimeIntf <> nil then
    Lifetime := LifetimeIntf as THostApplicationLifetime
  else
    Lifetime := nil;

  var StateIntf := FServiceProvider.GetServiceAsInterface(TServiceType.FromInterface(IAppStateControl));
  if StateIntf <> nil then
    StateControl := StateIntf as IAppStateControl
  else
    StateControl := nil;

  // Update State: Starting -> Migrating
  if StateControl <> nil then
    StateControl.SetState(asMigrating);

  // 🔄 Run Migrations automatically if configured
  var DbConfig := FConfiguration.GetSection('Database');
  if (DbConfig <> nil) and (SameText(DbConfig['AutoMigrate'], 'true')) then
  begin
    SafeWriteLn('⚙️ AutoMigrate enabled. Checking database schema...');
    
    // Resolve DbContext
    // Note: We assume the user has registered their DbContext in ConfigureServices
    // If not, this might fail or return nil depending on DI implementation.
    // For now, we try to get IDbContext.
    var DbContextIntf := FServiceProvider.GetServiceAsInterface(TServiceType.FromInterface(IDbContext));
    if DbContextIntf <> nil then
    begin
      var Migrator := TMigrator.Create(DbContextIntf as IDbContext);
      try
        Migrator.Migrate;
      finally
        Migrator.Free;
      end;
    end
    else
      ;
  end;
  
  // Update State: Migrating -> Seeding
  if StateControl <> nil then
    StateControl.SetState(asSeeding);
    
  // TODO: Run Seeding automatically if configured

  // Update State: Seeding -> Running
  if StateControl <> nil then
    StateControl.SetState(asRunning);


  // Start Hosted Services
  HostedManager := nil;
  try
    // ? Resolve as INTERFACE (enables ARC management)
    var ManagerIntf := FServiceProvider.GetServiceAsInterface(TServiceType.FromInterface(IHostedServiceManager));
    if ManagerIntf <> nil then
    begin
      HostedManager := ManagerIntf as THostedServiceManager;
      HostedManager.StartAsync;
    end;
  except
    on E: Exception do
      SafeWriteLn('Error starting hosted services: ' + E.Message);
  end;

  // Notify Started
  if Lifetime <> nil then
    Lifetime.NotifyStarted;

  // Build pipeline
  RequestHandler := GetApplicationBuilder.Build;

  // Create WebHost
  SSLHandler := nil;
  var ServerSection := FConfiguration.GetSection('Server');
  if (ServerSection <> nil) and (SameText(ServerSection['UseHttps'], 'true')) then
  begin
    var CertFile := ServerSection['SslCert'];
    var KeyFile := ServerSection['SslKey'];
    var RootFile := ServerSection['SslRootCert'];
    
    // Only enable SSL if certificate files exist
    if (CertFile <> '') and (KeyFile <> '') and 
       FileExists(CertFile) and FileExists(KeyFile) then
    begin
      var ProviderName := ServerSection['SslProvider'];
      if SameText(ProviderName, 'Taurus') then
        SSLHandler := TIndyTaurusSSLHandler.Create(CertFile, KeyFile, RootFile)
      else
        SSLHandler := TIndyOpenSSLHandler.Create(CertFile, KeyFile, RootFile);
    end
    else if (CertFile <> '') or (KeyFile <> '') then
      SafeWriteLn('[WARN] HTTPS configured but certificate files not found. Using HTTP.');
  end;

  WebHost := TIndyWebServer.Create(Port, RequestHandler, FServiceProvider, SSLHandler);
  
  // ✅ Store active host so Stop() can access it
  FActiveHost := WebHost;

  try
    WebHost.Run;
  finally
    // ✅ Release active host reference
    FActiveHost := nil;

    // Update State: Running -> Stopping
    if StateControl <> nil then
      StateControl.SetState(asStopping);

    // Notify Stopping
    if Lifetime <> nil then
      Lifetime.NotifyStopping;

    // Stop Hosted Services
    if HostedManager <> nil then
    begin
      HostedManager.StopAsync;
      // Do NOT Free HostedManager here if it is a Singleton managed by FServiceProvider
      // It will be freed when FServiceProvider is destroyed.
      // If it's transient, we might need to free it, but it's registered as singleton usually.
    end;
    
    // Update State: Stopping -> Stopped
    if StateControl <> nil then
      StateControl.SetState(asStopped);

    // Notify Stopped
    if Lifetime <> nil then
      Lifetime.NotifyStopped;
      
    // Explicitly release provider reference to ensure cleanup
    FServiceProvider := nil;
  end;
end;

function TDextApplication.UseMiddleware(Middleware: TClass): IWebApplication;
begin
  GetApplicationBuilder.UseMiddleware(Middleware);
  Result := Self;
end;

function TDextApplication.UseStartup(Startup: IStartup): IWebApplication;
begin
  // 1. Configure Services
  Startup.ConfigureServices(TDextServices.Create(FServices), FConfiguration);
  
  // 2. Configure Pipeline
  Startup.Configure(Self);
  
  Result := Self;
end;

procedure TDextApplication.Stop;
begin
  if FActiveHost <> nil then
  begin
    SafeWriteLn('Stopping active host...');
    FActiveHost.Stop;
  end;
end;

procedure TDextApplication.SetDefaultPort(Port: Integer);
begin
  FDefaultPort := Port;
end;

end.
