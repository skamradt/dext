unit Sales.Startup;

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
  Dext.Web,
  // 5. Current Project Units
  Sales.Auth,
  Sales.Data.Context,
  Sales.Domain.Entities,
  Sales.Domain.Models,
  Sales.Domain.Enums,
  Sales.Endpoints;

type
  TStartup = class(TInterfacedObject, IStartup)
  public
    procedure ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
    procedure Configure(const App: IWebApplication);
  private
    procedure ConfigureDatabase(Options: TDbContextOptions);
  end;

implementation

uses
  System.JSON,
  System.DateUtils,
  Dext.Web.Results,
  Dext.Auth.JWT, 
  Dext.Web.Cors; 

{ TStartup }

procedure TStartup.ConfigureDatabase(Options: TDbContextOptions);
begin
  Options.UseSQLite('SalesSystem.db');
  Options.UseSnakeCaseNamingConvention;
end;

procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  Services.AddDbContext<TSalesDbContext>(ConfigureDatabase);
  {$IFNDEF MODEL_BINDING_ARRAY}
  Services.AddTransient<IList<TOrderItemDto>, TSmartList<TOrderItemDto>>;
  {$ENDIF}

  var Secret := 'my-super-secret-key-for-sales-system-minimum-32-chars';
  Services.AddSingleton<IAuthService, TAuthService>(
    function(P: IServiceProvider): TObject
    begin
      Result := TAuthService.Create(Secret);
    end);
end;

procedure TStartup.Configure(const App: IWebApplication);
begin
  var Builder := App.Builder;

  // ✨ Configurações globais de JSON
  JsonDefaultSettings(JsonSettings.CamelCase.CaseInsensitive);

  Builder.UseExceptionHandler;
  Builder.UseHttpLogging;

  Builder.UseCors(
    procedure(C: TCorsBuilder)
    begin
        C.AllowAnyOrigin.AllowAnyMethod.AllowAnyHeader;
    end);

  var Secret := 'my-super-secret-key-for-sales-system-minimum-32-chars';
  Builder.UseJwtAuthentication(Secret, procedure(Options: TJwtOptionsBuilder) begin end);

  Builder.UseRateLimiting(TRateLimitPolicy.FixedWindow(100, 60));

  Builder.UseResponseCache(
    procedure(Cache: TResponseCacheBuilder)
    begin
      Cache.DefaultDuration(30).VaryByQueryString;
    end);

  TSalesEndpoints.MapEndpoints(Builder);

  Builder.UseSwagger(Swagger
    .Title('Sales System CQRS API')
    .Version('v1'));
end;

end.
