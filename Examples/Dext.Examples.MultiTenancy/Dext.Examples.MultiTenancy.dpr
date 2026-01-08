program Dext.Examples.MultiTenancy;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  System.Classes,
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
  TDextJson.SetDefaultSettings(TDextSettings.Default.WithCamelCase.WithCaseInsensitive);

  // Exception Handler
  WebApp.UseExceptionHandler;

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

var
  App: IWebApplication;
begin
  try
    SetConsoleCharSet;
    
    App := TDextApplication.Create;
    App.UseStartup(TMultiTenancyStartup.Create);
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
