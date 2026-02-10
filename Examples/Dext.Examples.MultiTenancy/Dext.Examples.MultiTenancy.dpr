program Dext.Examples.MultiTenancy;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  System.Classes,
  Dext.DI.Interfaces,
  Dext.Web.Interfaces,
  Dext,
  Dext.Web,
  Dext.Entity,
  Dext.Json,
  MultiTenancy.Entities in 'Domain\MultiTenancy.Entities.pas',
  MultiTenancy.DbContext in 'Domain\MultiTenancy.DbContext.pas',
  MultiTenancy.Middleware in 'Middleware\MultiTenancy.Middleware.pas',
  MultiTenancy.Endpoints in 'Features\MultiTenancy.Endpoints.pas',
  MultiTenancy.Service in 'Features\MultiTenancy.Service.pas';

type
  TMultiTenancyStartup = class(TInterfacedObject, IStartup)
  public
    procedure ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
    procedure Configure(const App: IWebApplication);
    class procedure InitializeDatabase(const Services: IServiceProvider);
  end;

{ TMultiTenancyStartup }

procedure TMultiTenancyStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  // Configure Multi-Tenant Database Context
  Services.AddDbContext<TTenantDbContext>(
    procedure(Options: TDbContextOptions)
    begin
      // Using SQLite with schema simulation via separate databases
      Options.UseSQLite('tenants_master.db');
      Options.WithPooling(True, 10);
      Options.Params.AddOrSetValue('JournalMode', 'WAL');
    end);

  // Tenant Service
  Services.AddScoped<ITenantService, TTenantService>;
  
  // Product Service (tenant-aware)
  Services.AddScoped<IProductService, TProductService>;
end;

procedure TMultiTenancyStartup.Configure(const App: IWebApplication);
begin
  var WebApp := App.GetBuilder;

  // Global JSON settings
  JsonDefaultSettings(JsonSettings.Default.CamelCase.CaseInsensitive);

  // Exception Handler
  WebApp.UseExceptionHandler;
  WebApp.UseMiddleware(TRequestLoggingMiddleware);
  // Tenant Resolution Middleware (extracts tenant from header/subdomain)
  WebApp.UseMiddleware(TTenantResolutionMiddleware);

  // Map Endpoints
  TMultiTenancyEndpoints.Map(WebApp);

  WriteLn('[*] Multi-Tenancy Demo running at http://localhost:8080');
  WriteLn('[*] Endpoints:');
  WriteLn('    POST /api/tenants          - Create tenant');
  WriteLn('    GET  /api/tenants          - List tenants');
  WriteLn('    GET  /api/products         - List products (requires X-Tenant-Id header)');
  WriteLn('    POST /api/products         - Create product (requires X-Tenant-Id header)');
  WriteLn('');
  WriteLn('[*] Use header: X-Tenant-Id: <tenant-id>');
end;

class procedure TMultiTenancyStartup.InitializeDatabase(const Services: IServiceProvider);
begin
  WriteLn('[*] Initializing Database...');

  var Scope := Services.CreateScope; // Returns IServiceScope
  try
    // 3. Resolve the DbContext safely
    var ServiceObj := Scope.ServiceProvider.GetService(TServiceType.FromClass(TTenantDbContext));
    
    if ServiceObj <> nil then
    begin
      var DbCtx := ServiceObj as TTenantDbContext;
      try
        DbCtx.EnsureCreated;
        WriteLn('[OK] Database schema created/verified.');
      except
        on E: Exception do
          WriteLn('[ERROR] Failed to initialize database: ' + E.Message);
      end;
    end
    else
    begin
      WriteLn('[ERROR] TTenantDbContext service could not be resolved.');
    end;
  finally
    // Scope (interface) automagically disposes when out of scope
  end;
end;

var
  App: IWebApplication;
begin
  try
    SetConsoleCharSet;
    
    App := TDextApplication.Create;
    App.UseStartup(TMultiTenancyStartup.Create);
    
    // CRITICAL: Build services before trying to access them
    var Services := App.BuildServices;
    
    // Initialize DB
    TMultiTenancyStartup.InitializeDatabase(Services);
    
    App.Run(8080);
    
    ConsolePause;
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      ConsolePause;
    end;
  end;
end.
