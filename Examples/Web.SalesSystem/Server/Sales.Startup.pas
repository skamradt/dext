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
  Sales.Data.Context,
  Sales.Domain.Entities,
  Sales.Domain.Models,
  Sales.Domain.Enums;


type
  { Login DTOs }
  TLoginRequest = record
    username: string;
    password: string;
  end;

  TLoginResponse = record
    token: string;
  end;

  THealthStatus = record
    status: string;
    timestamp: string;
  end;

  { Order DTOs }
  TOrderItemDto = record
    productId: Integer;
    quantity: Integer;
  end;

  { TCreateOrderDto como CLASSE para forçar o uso do BindBody do Dext.
    Agora usando IList<T> novamente pois o framework suporta o fallback para TSmartList<T>! }
  {$M+}
  TCreateOrderDto = class
  private
    FItems: IList<TOrderItemDto>;
    //FItems: TArray<TOrderItemDto>;
  public
    property Items: IList<TOrderItemDto> read FItems write FItems;
    //property Items: TArray<TOrderItemDto> read FItems write FItems;
  end;

  { Service to Issue Tokens }
  IAuthService = interface
    ['{6FAF8D72-1234-4567-8901-ABCDEF123456}']
    function Login(const User, Pass: string): string;
  end;

  { Startup Class }
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

{ TAuthService Implementation }

type
  TAuthService = class(TInterfacedObject, IAuthService)
  private
    FJwtHandler: TJwtTokenHandler;
  public
    constructor Create(const Secret: string);
    destructor Destroy; override;
    function Login(const User, Pass: string): string;
  end;

{ TAuthService }

constructor TAuthService.Create(const Secret: string);
begin
  FJwtHandler := TJwtTokenHandler.Create(Secret, 'SalesApi', 'SalesClient', 60);
end;

destructor TAuthService.Destroy;
begin
  FJwtHandler.Free;
  inherited;
end;

function TAuthService.Login(const User, Pass: string): string;
begin
  if (User = 'admin') and (Pass = 'admin') then
  begin
    var Claims := TClaimsBuilder.Create
      .WithName(User)
      .WithRole('Admin')
      .Build;
    Result := FJwtHandler.GenerateToken(Claims);
  end
  else
    Result := '';
end;

{ TStartup }

procedure TStartup.ConfigureDatabase(Options: TDbContextOptions);
begin
  Options.UseSQLite('SalesSystem.db');
  Options.UseSnakeCaseNamingConvention;
end;

procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  Services.AddDbContext<TSalesDbContext>(ConfigureDatabase);
  Services.AddTransient<IList<TOrderItemDto>, TSmartList<TOrderItemDto>>;

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

  // ROUTES
  
  Builder.MapGet<IResult>('/health', 
    function: IResult
    begin
      var Status: THealthStatus;
      Status.status := 'healthy';
      Status.timestamp := DateTimeToStr(Now);
      Result := Results.Ok(Status);
    end); 

  Builder.MapPost<TLoginRequest, IAuthService, IResult>('/auth/login',
    function(Req: TLoginRequest; Auth: IAuthService): IResult
    begin
      var Token := Auth.Login(Req.username, Req.password);
      if Token = '' then
        Exit(Results.StatusCode(401)); 
        
      var Resp: TLoginResponse;
      Resp.token := Token;
      Result := Results.Ok(Resp);
    end);

  TDataApiHandler<TCustomer>.Map(Builder, '/api/customers',
    TDataApiOptions<TCustomer>.Create
      .DbContext<TSalesDbContext>
      .UseSnakeCase
      .Tag('Customers (Query)'));

  TDataApiHandler<TProduct>.Map(Builder, '/api/products',
    TDataApiOptions<TProduct>.Create
      .DbContext<TSalesDbContext>
      .UseSnakeCase
      .Tag('Products (Query)'));

  // 🏗️ CQRS Command - Order Creation
  Builder.MapPost<IHttpContext, TSalesDbContext, TCreateOrderDto, IResult>('/api/orders',
    function(Context: IHttpContext; Db: TSalesDbContext; Dto: TCreateOrderDto): IResult
    begin
      if (Dto = nil) then
        Exit(Results.BadRequest('Erro na leitura do corpo da requisicao.'));

      if (Dto.items.Count = 0) then
        Exit(Results.BadRequest('O pedido deve conter pelo menos um item.'));

      var OrderEntity := TOrder.Create;
      try
        OrderEntity.Status := TOrderStatus.Draft;
        OrderEntity.CustomerId := 1; 
        
        var Handler := TOrderModel.Create(OrderEntity);
        try
          try
            Writeln(Format('[DEBUG] Processing Order with %d items', [Dto.items.Count]));
            for var ItemDto in Dto.items do
            begin
              Writeln(Format('[DEBUG] Item: ProductId=%d, Quantity=%d', [ItemDto.productId, ItemDto.quantity]));
              var ValidProd := Db.Products.Find(ItemDto.productId);
              if ValidProd = nil then
              begin
                Writeln(Format('[DEBUG] Product %d NOT FOUND in database', [ItemDto.productId]));
                Exit(Results.BadRequest('Produto nao encontrado: ' + IntToStr(ItemDto.productId)));
              end;
              
              Handler.AddItem(ValidProd, ItemDto.quantity);
            end;

            Handler.Submit;

            Db.Orders.Add(Handler.Entity);
            Db.SaveChanges;

            Result := Results.Created('/api/orders/' + Handler.Entity.Id.Value.ToString, Handler.Entity);
          except
            on E: EDomainError do Result := Results.BadRequest(E.Message);
            on E: Exception do Result := Results.InternalServerError(E.Message);
          end;
        finally
          Handler.Free;
        end;
      except
        OrderEntity.Free; 
        raise;
      end;
    end);

  Builder.MapGet<TSalesDbContext, IResult>('/api/orders',
    function(Db: TSalesDbContext): IResult
    begin
      var Orders := Db.Orders.QueryAll.OrderBy(TOrder.Props.CreatedAt.Desc).ToList;
      Result := Results.Ok(Orders);
    end); 

  Builder.UseSwagger(Swagger.Title('Sales System CQRS API').Version('v1'));
end;

end.
