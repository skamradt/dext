unit HelpDesk.Startup;

{***************************************************************************}
{                                                                           }
{           Web.HelpDesk - Application Startup                              }
{                                                                           }
{           Configure Services, Middleware Pipeline, and Endpoints           }
{                                                                           }
{***************************************************************************}

interface

uses
  System.SysUtils,
  Dext.Entity.Core,     // Naming Strategy
  Dext.Logging,         // HttpLogging
  Dext.RateLimiting,
  Dext.RateLimiting.Policy,
  Dext,        // Facade
  Dext.Web,    // Facade
  Dext.Entity; // Facade

type
  TStartup = class(TInterfacedObject, IStartup)
  private
    procedure ConfigureDatabase(Options: TDbContextOptions);
  public
    procedure ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
    procedure Configure(const App: IWebApplication);
  end;

implementation

uses
  HelpDesk.Data.Context,
  HelpDesk.Services,
  HelpDesk.Endpoints;

{ TStartup }

procedure TStartup.ConfigureDatabase(Options: TDbContextOptions);
begin
  Options.UseSQLite('HelpDesk.db');
  Options.UseSnakeCaseNamingConvention;
end;

procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  // 1. Data Access
  Services.AddDbContext<THelpDeskContext>(ConfigureDatabase);

  // 2. Domain Services (Scoped - one per request)
  Services.AddScoped<IUserService, TUserService>;
  Services.AddScoped<ITicketService, TTicketService>;
end;

procedure TStartup.Configure(const App: IWebApplication);
begin
  var Builder := App.Builder;

  // Global Settings
  JsonDefaultSettings(JsonSettings.CamelCase.CaseInsensitive.ISODateFormat);

  // Middleware Pipeline
  Builder.UseExceptionHandler;
  Builder.UseHttpLogging;
  Builder.UseCors(CorsOptions.AllowAnyOrigin.AllowAnyMethod.AllowAnyHeader);
  Builder.UseRateLimiting(TRateLimitPolicy.FixedWindow(100, 60));

  // Map Endpoints (Minimal API)
  TEndpoints.MapEndpoints(Builder);

  // Swagger (AFTER endpoints)
  Builder.UseSwagger(
    Swagger
      .Title('HelpDesk API')
      .Version('v1')
      .Description('Issue Tracking System - Minimal API')
  );
end;

end.
