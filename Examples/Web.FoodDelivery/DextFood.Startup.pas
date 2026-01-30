unit DextFood.Startup;

interface

uses
  System.SysUtils,
  Dext,
  Dext.Entity,           // Facade para ORM (TDbContext, TSnakeCaseNamingStrategy)
  Dext.Entity.Prototype, // For Prototype.Entity<T>
  Dext.Web;

type
  /// <summary>
  /// Classe de inicialização (Bootstrap) do backend DextFood.
  /// </summary>
  TStartup = class(TInterfacedObject, IStartup)
  public
    procedure ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
    procedure Configure(const App: IWebApplication);
  private
    procedure ConfigureDatabase(Options: TDbContextOptions);
  end;

implementation

uses
  DextFood.Domain,
  DextFood.Services,
  DextFood.Hubs,
  DextFood.DbSeeder;

{ TStartup }

procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  // 1. Motor de Persistência (SQLite In-Memory + Naming Strategy)
  Services.AddDbContext<TDbContext>(ConfigureDatabase);
  
  // 2. Registro de Serviços de Negócio
  Services.AddSingleton<IOrderService, TOrderService>;

  // 3. Suporte a Controllers
  Services.AddControllers;
end;

procedure TStartup.Configure(const App: IWebApplication);
var
  SwaggerOpts: TOpenAPIOptions;
  CorsOpts: TCorsOptions;
begin
  // ✨ Configurações globais de JSON (CamelCase para APIs modernas)
  TDextJson.SetDefaultSettings(TDextSettings.Default.WithCamelCase.WithCaseInsensitive);

  // Pipeline de middlewares configurado via Facade Dext.Web
  App.Builder
    .UseExceptionHandler
    .UseHttpLogging;

  // 🛡️ Configuração granular de CORS
  CorsOpts := TCorsOptions.Create;
  CorsOpts.AllowedOrigins := ['*'];
  CorsOpts.AllowedMethods := ['*'];
  CorsOpts.AllowedHeaders := ['*'];
  App.Builder.UseCors(CorsOpts);

  // 1. Inicializa e popula o Banco de Dados
  TDbSeeder.Seed(App);

  // 🚀 Mapeia todas as rotas (Minimal APIs e Controllers) ANTES do Swagger
  
  // Health Check
  App.Builder.MapGet('/health',
    procedure(Ctx: IHttpContext)
    begin
      Ctx.Response.Json('{"status": "healthy"}');
    end);

  // Real-time Hub
  // App.Builder.MapHub<TOrderHub>('/hubs/orders');

  // Minimal API Tipada
  App.Builder.MapPost<IOrderService, IHttpContext, IResult>('/api/orders',
    function(Service: IOrderService; Ctx: IHttpContext): IResult
    var
      Total: Currency;
    begin
      Total := StrToCurrDef(Ctx.Request.Query.Values['total'], 0);
      Service.CreateOrder(Total);
      Result := Results.Ok('{"message": "Pedido criado"}');
    end);

  // Exemplo de consulta com Smart Properties
  App.Builder.MapGet('/api/orders/high-value',
    procedure(Ctx: IHttpContext)
    var
      Db: TDbContext;
      o: TOrder;
    begin
      //Db := Ctx.Services.GetService(TDbContext);
      o := Prototype.Entity<TOrder>;
      var List := Db.Entities<TOrder>.Where(o.Total > 50).ToList;
      //Ctx.Response.Json(List);
    end);

  // Controllers
  App.MapControllers;

  // ✨ Swagger UI em /swagger (Inspeção automática de rotas)
  SwaggerOpts := TOpenAPIOptions.Default;
  SwaggerOpts.Title := 'DextFood API';
  SwaggerOpts.Version := 'v1';
  App.Builder.UseSwagger(SwaggerOpts);
end;

procedure TStartup.ConfigureDatabase(Options: TDbContextOptions);
begin
  Options
    .UseSQLite(':memory:')
    .UseNamingStrategy(TSnakeCaseNamingStrategy.Create);
end;

end.
