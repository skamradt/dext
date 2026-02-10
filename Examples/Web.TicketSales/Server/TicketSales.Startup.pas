unit TicketSales.Startup;

{***************************************************************************}
{                                                                           }
{           Web.TicketSales - Application Startup                           }
{                                                                           }
{           Configuration of services, middleware, and routing              }
{                                                                           }
{***************************************************************************}

interface

uses
  // 1. Delphi Units
  System.SysUtils,
  // 3. Dext Specialized Units
  Dext.Auth.Middleware,
  Dext.Caching,
  Dext.Entity.Core,
  Dext.Logging,
  Dext.RateLimiting,
  Dext.RateLimiting.Policy,
  Dext.Web.DataApi,
  Dext.Core.SmartTypes,
  Dext.Collections,
  // 4. Dext Facades Last to ensure precedence and valid helpers
  Dext,
  Dext.Entity,
  Dext.Web;

type
  TStartup = class(TInterfacedObject, IStartup)
  private
    const JWT_SECRET = 'ticket-sales-super-secret-key-minimum-32-characters';
    const JWT_ISSUER = 'ticket-sales-api';
    const JWT_AUDIENCE = 'ticket-sales-clients';
    const JWT_EXPIRATION_MINUTES = 120;
    
    procedure ConfigureDatabase(Options: TDbContextOptions);
  public
    procedure ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
    procedure Configure(const App: IWebApplication);
  end;

implementation

uses
  TicketSales.Data.Context,
  TicketSales.Services,
  TicketSales.Controllers;

{ TStartup }

procedure TStartup.ConfigureDatabase(Options: TDbContextOptions);
begin
  Options.UseSQLite('TicketSales.db');
  Options.UseSnakeCaseNamingConvention;
end;

procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  // Database Context
  Services.AddDbContext<TTicketSalesDbContext>(ConfigureDatabase);

  // JWT Token Handler
  Services.AddSingleton<IJwtTokenHandler, TJwtTokenHandler>(
    function(Provider: IServiceProvider): TObject
    begin
      Result := TJwtTokenHandler.Create(JWT_SECRET, JWT_ISSUER, JWT_AUDIENCE, JWT_EXPIRATION_MINUTES);
    end);
  Services.AddTransient<IClaimsBuilder, TClaimsBuilder>;

  // Business Services (Scoped - one per request)
  Services.AddScoped<IEventService, TEventService>;
  Services.AddScoped<ITicketTypeService, TTicketTypeService>;
  Services.AddScoped<ICustomerService, TCustomerService>;
  Services.AddScoped<IOrderService, TOrderService>;
  Services.AddScoped<ITicketService, TTicketService>;

  // Register Controllers
  Services.AddControllers;
end;

procedure TStartup.Configure(const App: IWebApplication);
begin
  var Builder := App.Builder;

  // Global JSON settings: camelCase, case-insensitive
  JsonDefaultSettings(JsonSettings.CamelCase.CaseInsensitive);

  // 1. Exception Handler (first middleware)
  Builder.UseExceptionHandler;

  // 2. HTTP Logging
  Builder.UseHttpLogging;

  // 3. CORS
  Builder.UseCors(CorsOptions.AllowAnyOrigin.AllowAnyMethod.AllowAnyHeader);

  // 4. JWT Authentication
  Builder.UseJwtAuthentication(JWT_SECRET,
    procedure(Auth: TJwtOptionsBuilder)
    begin
      Auth.WithIssuer(JWT_ISSUER)
          .WithAudience(JWT_AUDIENCE)
          .WithExpirationMinutes(JWT_EXPIRATION_MINUTES);
    end);

  // 5. Rate Limiting (100 requests per minute)
  Builder.UseRateLimiting(
    TRateLimitPolicy.FixedWindow(100, 60)
      .WithRejectionMessage('Too many requests. Please try again later.')
      .WithRejectionStatusCode(429)
  );

  // 6. Response Cache
  Builder.UseResponseCache(
    procedure(Cache: TResponseCacheBuilder)
    begin
      Cache.DefaultDuration(30).VaryByQueryString;
    end);

  // 7. Map Controllers
  App.MapControllers;

  // 8. Swagger (last, after routes are mapped)
  Builder.UseSwagger(
    Swagger
      .Title('Ticket Sales API')
      .Description('API for managing event ticket sales')
      .Version('v1')
      .BearerAuth
  );
end;

end.
