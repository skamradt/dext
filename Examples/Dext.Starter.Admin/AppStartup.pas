unit AppStartup;

interface

uses
  System.SysUtils,
  Dext,
  Dext.Web,
  Dext.Persistence,
  // Features
  Auth.Service,
  Auth.Endpoints,
  Dashboard.Endpoints,
  Customer.Endpoints,
  Settings.Endpoints,
  // Domain
  User,
  Customer,
  Order,
  DbContext,
  DbSeeder;

type
  TAppStartup = class
  public
    class procedure ConfigureServices(const Services: IServiceCollection; const Configuration: IConfiguration);
    class procedure Configure(const App: IWebApplication);
    class procedure RunSeeder(const App: IWebApplication);
  end;

implementation

{ TAppStartup }

class procedure TAppStartup.ConfigureServices(const Services: IServiceCollection; const Configuration: IConfiguration);
begin
  // 1. Auth Service
  TServiceCollectionExtensions.AddScoped<IAuthService, TAuthService>(Services);

  // 2. Database (SQLite) - Using new AddDbContext helper with Pooling support
  TPersistence.AddDbContext<TAppDbContext>(Services,
    procedure(Options: TDbContextOptions)
    begin
      // Use Helper
      Options.UseSQLite('dext_admin.db');
      
      // Enable Pooling (Safe for real servers, overkill for SQLite file but good practice)
      // TODO : Configure ConnectionDefs
      // AUTH: Exception in middleware: EFDException: [FireDAC][Comp][Clnt]-507.
      // Connection [$04AC7FC0: TFDConnection] cannot be pooled.
      // Possible reason: connection definition is not in the FDManager.ConnectionDefs list or
      // TFDConnection.Params has additional parameters
      Options.Pooling := False;
      Options.PoolMax := 20;
    end);

  // 3. Register DbSeeder (Manual)
  Services.AddTransient(
    TServiceType.FromClass(TDbSeeder),
    TDbSeeder,
    function(Provider: Dext.DI.Interfaces.IServiceProvider): TObject
    begin
       Result := TDbSeeder.Create(Provider);
    end);

  // 4. Register JWT Token Handler
  Services.AddSingleton(
    TServiceType.FromInterface(TypeInfo(IJwtTokenHandler)),
    TJwtTokenHandler,
    function(Provider: Dext.DI.Interfaces.IServiceProvider): TObject
    begin
      // TODO: Move secret key to configuration
      Result := TJwtTokenHandler.Create(
        'dext-admin-secret-key-change-in-production-2024',  // Secret
        'DextAdmin',                                          // Issuer
        'DextAdminUI',                                        // Audience
        60                                                    // Expiration (minutes)
      );
    end);
end;

class procedure TAppStartup.RunSeeder(const App: IWebApplication);
begin
  Writeln('[*] Preparing to seed database...');
  // Get the ServiceProvider from the ApplicationBuilder
  var ServiceProvider := App.GetApplicationBuilder.GetServiceProvider;
  if ServiceProvider = nil then
  begin
    Writeln('[ERROR] ServiceProvider is nil');
    Exit;
  end;

  var SeederObj := ServiceProvider.GetService(TServiceType.FromClass(TDbSeeder));
  if SeederObj <> nil then
  begin
    var Seeder := SeederObj as TDbSeeder;
    try
      Seeder.Seed;
    finally
      Seeder.Free;
    end;
  end
  else
    Writeln('[WARN] TDbSeeder service not found.');
end;

class procedure TAppStartup.Configure(const App: IWebApplication);
begin
  var WebApp: IApplicationBuilder := App.GetApplicationBuilder;

  // 1. Serve Static Files (from wwwroot)
  TApplicationBuilderStaticFilesExtensions.UseStaticFiles(WebApp);

  // 2. JWT Authentication Middleware
  TApplicationBuilderJwtExtensions.UseJwtAuthentication(
    WebApp,
    TJwtOptions.Create('dext-admin-secret-key-change-in-production-2024')
  );

  // 3. Generate Swagger Documentation
  TSwaggerExtensions.UseSwagger(WebApp);

  // 4. Map Features
  TAuthEndpoints.Map(WebApp);
  TDashboardEndpoints.Map(WebApp);
  TCustomerEndpoints.Map(WebApp);
  TSettingsEndpoints.Map(WebApp);
end;

end.
