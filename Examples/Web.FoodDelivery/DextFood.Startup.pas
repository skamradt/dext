unit DextFood.Startup;

interface

uses
  System.TypInfo,
  System.Rtti,
  System.SysUtils,
  Dext,
  Dext.Entity,           // Facade para ORM (TDbContext, TSnakeCaseNamingStrategy)
  Dext.Entity.Core,      // Explicitly needed for IDbSet<T>
  Dext.Web,
  DextFood.Domain;

type
  /// <summary>
  /// Contexto de banco de dados específico para o DextFood.
  /// </summary>
  TAppDbContext = class(TDbContext)
  private
    function GetOrders: IDbSet<TOrder>;
  public
    property Orders: IDbSet<TOrder> read GetOrders;
  end;

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
  Dext.Json,
  DextFood.Services,
  DextFood.Hubs,
  DextFood.DbSeeder;

{ TAppDbContext }

function TAppDbContext.GetOrders: IDbSet<TOrder>;
begin
  Result := Entities<TOrder>;
end;

{ TStartup }

procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  // 1. Motor de Persistência via Contexto Tipado
  Services.AddDbContext<TAppDbContext>(ConfigureDatabase);
  
  // 2. Registro de Serviços de Negócio
  Services.AddSingleton<IOrderService, TOrderService>;

  // 3. Suporte a Controllers
  Services.AddControllers;
end;

procedure TStartup.Configure(const App: IWebApplication);
begin
  var Builder := App.Builder;
  // ✨ Configurações globais de JSON (CamelCase para APIs modernas)
  JsonDefaultSettings(JsonSettings.CamelCase.CaseInsensitive);

  // Pipeline de middlewares configurado via Facade Dext.Web
  Builder
    .UseExceptionHandler
    .UseHttpLogging;

  // 🛡️ Configuração granular de CORS
  Builder.UseCors(Cors
    .AllowAnyOrigin
    .AllowAnyMethod
    .AllowAnyHeader);

  // 🚀 Mapeia todas as rotas (Minimal APIs e Controllers) ANTES do Swagger
  
  // Health Check
  Builder.MapGet('/health',
    procedure(Ctx: IHttpContext)
    begin
      Ctx.Response.Json('{"status": "healthy"}');
    end);

  // Real-time Hub
  //Builder.MapHub<TOrderHub>('/hubs/orders');

  // Minimal API Tipada
  Builder.MapPost<IOrderService, IHttpContext, IResult>('/api/orders',
    function(Service: IOrderService; Ctx: IHttpContext): IResult
    var
      Total: Currency;
    begin
      Total := StrToCurrDef(Ctx.Request.Query.Values['total'], 0);
      Service.CreateOrder(Total);
      Result := Results.Ok('{"message": "Pedido criado"}');
    end);

  // Exemplo de consulta com Smart Properties e Dependency Injection
  Builder.MapGet<TAppDbContext, IResult>('/api/orders/high-value',
    function(Db: TAppDbContext): IResult
    begin
      var Order := Prototype.Entity<TOrder>;
      var List := Db.Orders.Where(Order.Total > 50).ToList;
      Result := Results.Ok(List);
    end);

  // Controllers
  App.MapControllers;

  // ✨ Swagger UI em /swagger (Inspeção automática de rotas)
  Builder.UseSwagger(Swagger
    .Title('DextFood API')
    .Version('v1'));
end;

procedure TStartup.ConfigureDatabase(Options: TDbContextOptions);
begin
  Options
    .UseSQLite('DextFood.db')
    .UseNamingStrategy(TSnakeCaseNamingStrategy.Create);
end;

end.

