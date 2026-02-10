unit Web.DextStore.Startup;

interface

uses
  System.SysUtils,
  Dext,
  Dext.Web,
  Dext.Auth.JWT,
  Dext.Web.Cors;

type
  TStartup = class(TInterfacedObject, IStartup)
  private
    const JwtSecret = 'dext-store-secret-key-must-be-very-long-and-secure';
    const JwtIssuer = 'dext-store';
    const JwtAudience = 'dext-users';
    const JwtExpiration = 120;
  public
    procedure ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
    procedure Configure(const App: IWebApplication);
  end;

implementation

uses
  DextStore.Models,
  DextStore.Services,
  DextStore.Controllers;

{ TStartup }

procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  Services
    // Register JWT Token Handler
    .AddSingleton<IJwtTokenHandler, TJwtTokenHandler>(
      function(Provider: IServiceProvider): TObject
      begin
        Result := TJwtTokenHandler.Create(JwtSecret, JwtIssuer, JwtAudience, JwtExpiration);
      end)
    .AddTransient<IClaimsBuilder, TClaimsBuilder>
    // Register Services (Singleton for In-Memory Persistence)
    .AddSingleton<IProductService, TProductService>
    .AddSingleton<ICartService, TCartService>
    .AddSingleton<IOrderService, TOrderService>
    // Register Controllers
    .AddControllers;
end;

procedure TStartup.Configure(const App: IWebApplication);
begin
  // ✨ CORS with Fluent API
  App.Builder.UseCors(CorsOptions.AllowAnyOrigin.AllowAnyMethod.AllowAnyHeader);

  // ✨ JWT Authentication
  App.Builder.UseJwtAuthentication(JwtSecret,
    procedure(Auth: TJwtOptionsBuilder)
    begin
      Auth.WithIssuer(JwtIssuer)
          .WithAudience(JwtAudience)
          .WithExpirationMinutes(JwtExpiration);
    end);

  // Minimal API Health Check
  App.Builder.MapGet('/health',
    procedure(Ctx: IHttpContext)
    begin
      Ctx.Response.Json('{"status": "healthy", "timestamp": "' + DateTimeToStr(Now) + '"}');
    end);

  // Routing & Controllers
  App.MapControllers;
end;

end.
