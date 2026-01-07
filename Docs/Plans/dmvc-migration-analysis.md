# Análise de Migração DMVC para Dext

## Objetivo
Analisar as funcionalidades necessárias para facilitar a migração de projetos DelphiMVC Framework para o Dext Framework, identificando o que já está implementado e o que precisa ser desenvolvido.

---

## 1. Registrar Services Somente Classes no DI

### Status: ⚠️ Parcialmente Implementado

O Dext já suporta registro de classes puras (não interfaces) através de `TServiceType.FromClass()`:

**Existente:**
```pascal
// Via TServiceType.FromClass (forma verbosa)
FServices.AddSingleton(TServiceType.FromClass(TMyService), TMyService);

// Via TServiceCollectionExtensions.AddSingleton<T: class>
// Arquivo: Dext.DI.Extensions.pas, linha 40
class function AddSingleton<T: class>(...);
```

### O que precisa ser feito:
Adicionar sobrecarga genérica simples em `TDextServices` para facilitar o uso:

```pascal
// Uso desejado (simplificado):
App.Services.AddSingleton<TMyService>;
App.Services.AddTransient<TMyRepository>;
App.Services.AddScoped<TDbContext>;
```

**Arquivos a modificar:**
- `Sources/Core/Dext.DI.Interfaces.pas` - Adicionar overloads em `TDextServices`:
  ```pascal
  function AddSingleton<T: class>: TDextServices; overload;
  function AddTransient<T: class>: TDextServices; overload;
  function AddScoped<T: class>: TDextServices; overload;
  ```

---

## 2. Basic Authentication

### Status: ❌ Não Implementado

O Dext tem apenas JWT Authentication (`Dext.Auth.Middleware.pas`). Não há middleware para Basic Auth.

### O que precisa ser feito:

**Criar novo arquivo:** `Sources/Web/Dext.Auth.BasicAuth.pas`

```pascal
unit Dext.Auth.BasicAuth;

interface

type
  TBasicAuthOptions = record
    Realm: string;
    class function Default: TBasicAuthOptions; static;
  end;
  
  TBasicAuthValidateFunc = reference to function(const Username, Password: string): Boolean;
  
  TBasicAuthMiddleware = class(TInterfacedObject, IMiddleware)
  private
    FOptions: TBasicAuthOptions;
    FValidateCredentials: TBasicAuthValidateFunc;
  public
    constructor Create(const AOptions: TBasicAuthOptions; 
      AValidateFunc: TBasicAuthValidateFunc);
    procedure Invoke(AContext: IHttpContext; ANext: TRequestDelegate);
  end;
  
  TApplicationBuilderBasicAuthExtensions = class
  public
    class function UseBasicAuthentication(
      const ABuilder: IApplicationBuilder;
      const ARealm: string;
      AValidateFunc: TBasicAuthValidateFunc): IApplicationBuilder; static;
  end;
```

**Uso esperado:**
```pascal
App.Builder.UseBasicAuthentication('Dext API',
  function(Username, Password: string): Boolean
  begin
    Result := (Username = 'admin') and (Password = 'secret');
  end);
```

---

## 3. Unit com Constantes HTTP_STATUS

### Status: ❌ Não Implementado

O DMVC tem `HTTP_STATUS` record em `MVCFramework.Commons.pas` com todas as constantes (OK, Created, NotFound, etc).

### O que precida ser feito:

**Criar novo arquivo:** `Sources/Web/Dext.Web.HttpStatus.pas`

```pascal
unit Dext.Web.HttpStatus;

interface

type
  HTTP_STATUS = record
  public const
    // 1xx Informational
    Continue = 100;
    SwitchingProtocols = 101;
    Processing = 102;
    
    // 2xx Success
    OK = 200;
    Created = 201;
    Accepted = 202;
    NoContent = 204;
    
    // 3xx Redirection
    MovedPermanently = 301;
    Found = 302;
    SeeOther = 303;
    NotModified = 304;
    TemporaryRedirect = 307;
    PermanentRedirect = 308;
    
    // 4xx Client Error
    BadRequest = 400;
    Unauthorized = 401;
    Forbidden = 403;
    NotFound = 404;
    MethodNotAllowed = 405;
    Conflict = 409;
    Gone = 410;
    UnprocessableEntity = 422;
    TooManyRequests = 429;
    
    // 5xx Server Error
    InternalServerError = 500;
    NotImplemented = 501;
    BadGateway = 502;
    ServiceUnavailable = 503;
    GatewayTimeout = 504;
    
  public
    class function ReasonStringFor(ACode: Integer): string; static;
    class function IsSuccess(ACode: Integer): Boolean; static;
    class function IsRedirect(ACode: Integer): Boolean; static;
    class function IsClientError(ACode: Integer): Boolean; static;
    class function IsServerError(ACode: Integer): Boolean; static;
  end;

implementation
// ...
end.
```

---

## 4. Mudar Endpoint Default do Swagger

### Status: ✅ Já Implementado

O `TSwaggerMiddleware` já aceita paths customizados no construtor:

```pascal
// Dext.Swagger.Middleware.pas, linha 81
constructor TSwaggerMiddleware.Create(
  AAppBuilder: IApplicationBuilder; 
  const AOptions: TOpenAPIOptions; 
  const ASwaggerPath: string = '/swagger';  // ✅ Customizável
  const AJsonPath: string = '/swagger.json' // ✅ Customizável
);
```

### O que precisa ser feito:

Apenas adicionar sobrecarga em `TSwaggerExtensions.UseSwagger`:

```pascal
class function UseSwagger(App: IApplicationBuilder; 
  const AOptions: TOpenAPIOptions;
  const ASwaggerPath: string;
  const AJsonPath: string): IApplicationBuilder; overload;
```

**Uso:**
```pascal
TSwaggerExtensions.UseSwagger(App, Options, '/api-docs', '/api-docs/openapi.json');
```

---

## 5. SwaggerParam para Documentar Parâmetros de Endpoint

### Status: ❌ Não Implementado

O DMVC tem `[MVCSwagParam]` para documentar parâmetros (path, query, header, body).

**Exemplo DMVC:**
```pascal
[MVCSwagParam(plPath, 'id', 'Customer ID', ptInteger, True)]
procedure GetCustomer(id: Integer);
```

### O que precisa ser feito:

**Modificar:** `Sources/Web/Dext.OpenAPI.Attributes.pas`

```pascal
type
  TSwaggerParamLocation = (splPath, splQuery, splHeader, splCookie, splBody);
  TSwaggerParamType = (sptString, sptInteger, sptNumber, sptBoolean, sptArray, sptFile);

  SwaggerParamAttribute = class(TCustomAttribute)
  private
    FLocation: TSwaggerParamLocation;
    FName: string;
    FDescription: string;
    FParamType: TSwaggerParamType;
    FRequired: Boolean;
  public
    constructor Create(
      ALocation: TSwaggerParamLocation;
      const AName: string;
      const ADescription: string;
      AParamType: TSwaggerParamType = sptString;
      ARequired: Boolean = False);
    
    property Location: TSwaggerParamLocation read FLocation;
    property Name: string read FName;
    property Description: string read FDescription;
    property ParamType: TSwaggerParamType read FParamType;
    property Required: Boolean read FRequired;
  end;
```

**Modificar:** `Sources/Web/Dext.OpenAPI.Generator.pas`
- Processar `SwaggerParamAttribute` ao gerar a documentação

**Uso:**
```pascal
[DextGet('/customers/{id}')]
[SwaggerParam(splPath, 'id', 'Customer ID', sptInteger, True)]
[SwaggerResponse(200, 'Customer found')]
[SwaggerResponse(404, 'Customer not found')]
procedure GetCustomer(Ctx: IHttpContext; [FromRoute] Id: Integer);
```

---

## 6. Exemplo: API de Comanda para Bares e Restaurantes

### Estrutura Proposta

```
Examples/
└── Web.OrderAPI/
    ├── README.md
    ├── README.pt-br.md
    ├── Web.OrderAPI.dpr
    ├── OrderAPI.Entities.pas
    ├── OrderAPI.Controllers.pas
    ├── OrderAPI.Services.pas
    ├── OrderAPI.Database.pas
    ├── Test.Web.OrderAPI.ps1
    └── appsettings.json
```

### Modelos (Entities)

```pascal
// OrderAPI.Entities.pas

type
  [Table('categories')]
  TCategory = class
  public
    [PK, AutoInc]
    Id: Integer;
    
    [Required]
    [StringLength(100)]
    Name: string;
    
    Description: string;
  end;

  [Table('products')]
  TProduct = class
  public
    [PK, AutoInc]
    Id: Integer;
    
    [Required]
    [StringLength(200)]
    Name: string;
    
    Description: string;
    
    [Required]
    Price: Currency;
    
    [FK('categories')]
    CategoryId: Integer;
    
    Available: Boolean;
  end;

  [Table('tables')]
  TRestaurantTable = class
  public
    [PK, AutoInc]
    Id: Integer;
    
    [Required]
    Number: Integer;
    
    Seats: Integer;
    Status: string; // 'available', 'occupied', 'reserved'
  end;

  [Table('orders')]
  TOrder = class
  public
    [PK, AutoInc]
    Id: Integer;
    
    [FK('tables')]
    TableId: Integer;
    
    Status: string; // 'open', 'closed', 'cancelled'
    
    OpenedAt: TDateTime;
    ClosedAt: TDateTime;
    
    TotalAmount: Currency;
  end;

  [Table('order_items')]
  TOrderItem = class
  public
    [PK, AutoInc]
    Id: Integer;
    
    [FK('orders')]
    OrderId: Integer;
    
    [FK('products')]
    ProductId: Integer;
    
    Quantity: Integer;
    UnitPrice: Currency;
    Notes: string;
    
    CreatedAt: TDateTime;
  end;
```

### Controllers

```pascal
// OrderAPI.Controllers.pas

[DextController('/api/tables')]
[SwaggerTag('Tables')]
TTablesController = class
  [DextGet('')]
  [SwaggerOperation('List all tables')]
  [SwaggerResponse(HTTP_STATUS.OK, 'List of tables')]
  procedure GetAll(Ctx: IHttpContext);
  
  [DextGet('/{id}')]
  [SwaggerOperation('Get table by ID')]
  [SwaggerParam(splPath, 'id', 'Table ID', sptInteger, True)]
  [SwaggerResponse(HTTP_STATUS.OK, 'Table found')]
  [SwaggerResponse(HTTP_STATUS.NotFound, 'Table not found')]
  procedure GetById(Ctx: IHttpContext; [FromRoute] Id: Integer);
  
  [DextPatch('/{id}/status')]
  [SwaggerOperation('Update table status')]
  procedure UpdateStatus(Ctx: IHttpContext; [FromRoute] Id: Integer);
end;

[DextController('/api/orders')]
[SwaggerTag('Orders')]
TOrdersController = class
  [DextPost('')]
  [SwaggerOperation('Open a new order')]
  [SwaggerResponse(HTTP_STATUS.Created, 'Order created')]
  procedure Create(Ctx: IHttpContext);
  
  [DextGet('/{id}')]
  [SwaggerOperation('Get order details')]
  [SwaggerParam(splPath, 'id', 'Order ID', sptInteger, True)]
  procedure GetById(Ctx: IHttpContext; [FromRoute] Id: Integer);
  
  [DextPost('/{id}/items')]
  [SwaggerOperation('Add item to order')]
  procedure AddItem(Ctx: IHttpContext; [FromRoute] Id: Integer);
  
  [DextPost('/{id}/close')]
  [SwaggerOperation('Close order')]
  procedure CloseOrder(Ctx: IHttpContext; [FromRoute] Id: Integer);
end;

[DextController('/api/products')]
[SwaggerTag('Products')]
TProductsController = class
  // CRUD completo com Swagger
end;

[DextController('/api/categories')]
[SwaggerTag('Categories')]
TCategoriesController = class
  // CRUD completo
end;
```

### Programa Principal

```pascal
program Web.OrderAPI;

uses
  Dext.MM,
  System.SysUtils,
  Dext,
  Dext.Web,
  Dext.Web.StatusCodes,
  Dext.Swagger.Middleware,
  Dext.OpenAPI.Types,
  Dext.Entity,
  OrderAPI.Entities,
  OrderAPI.Controllers,
  OrderAPI.Services,
  OrderAPI.Database;

begin
  var App: IWebApplication := TDextApplication.Create;
  
  // Database Configuration (SQLite with pooling para multi-thread)
  App.Services
    // DbContext com Pooling por Thread
    .AddScoped<TOrderDbContext>(
      function(Provider: IServiceProvider): TObject
      begin
        Result := TOrderDbContext.Create(
          TDbContextOptions.Create
            .UseSQLite('orderapi.db')
            .WithPooling(True, 20)  // Pool de 20 conexões
        );
      end)
    
    // Services TRANSIENTES - criados/destruídos por request
    // Usando interfaces para que ARC gerencie o ciclo de vida
    .AddTransient<ITableService, TTableService>
    .AddTransient<IOrderService, TOrderService>
    .AddTransient<IProductService, TProductService>
    .AddTransient<ICategoryService, TCategoryService>
    
    // Controllers
    .AddControllers;
  
  // Middleware
  App.Builder
    .UseCors(procedure(Cors: TCorsBuilder)
    begin
      Cors.AllowAnyOrigin.AllowAnyMethod.AllowAnyHeader;
    end);
  
  // Swagger com paths customizados via Options
  var Options := TOpenAPIOptions.Default;
  Options.Title := 'Order API - Sistema de Comandas';
  Options.Description := 'API para gerenciamento de comandas em bares e restaurantes';
  Options.Version := '1.0.0';
  Options.SwaggerPath := '/docs';           // Path customizado
  Options.SwaggerJsonPath := '/docs/openapi.json';
  
  TSwaggerExtensions.UseSwagger(App.Builder, Options);
  
  // Routes
  App.MapControllers;
  
  // Auto-create database
  using Scope := App.Services.BuildServiceProvider.CreateScope do
  begin
    var Ctx := Scope.ServiceProvider.GetRequiredService(
      TServiceType.FromClass(TOrderDbContext)) as TOrderDbContext;
    Ctx.Database.EnsureCreated;
  end;
  
  WriteLn('Order API running on http://localhost:8080');
  WriteLn('Swagger UI: http://localhost:8080/docs');
  
  App.Run(8080);
end.
```

### Considerações sobre Ciclo de Vida

#### Services Transientes com Interface (Recomendado)
```pascal
// ✅ RECOMENDADO: Interface permite que ARC gerencie destruição
App.Services.AddTransient<IOrderService, TOrderService>;

// A interface é liberada automaticamente quando sai de escopo
```

#### Services Transientes de Classe Pura (Cuidado!)
```pascal
// ⚠️ CUIDADO: Classe pura requer gerenciamento manual
App.Services.AddTransient<TMyService>;

// O objeto NÃO é destruído automaticamente!
// Use apenas para Singletons ou Scoped
```

#### DbContext com Pooling
```pascal
// O DbContext usa conexão do pool, que é thread-safe
// Registrar como Scoped para ter uma instância por request
App.Services.AddScoped<TOrderDbContext>(
  function(Provider: IServiceProvider): TObject
  begin
    Result := TOrderDbContext.Create(
      TDbContextOptions.Create
        .UseSQLite('app.db')
        .WithPooling(True, 20)  // Pool thread-safe
    );
  end);
```

---

## Resumo das Tarefas

| Item | Status | Complexidade | Arquivos |
|------|--------|--------------|----------|
| 1. DI para Classes | ✅ Implementado | Baixa | Dext.DI.Interfaces.pas |
| 2. Basic Auth | ✅ Implementado | Média | Dext.Auth.BasicAuth.pas |
| 3. HttpStatus | ✅ Implementado | Baixa | Dext.Web.StatusCodes.pas |
| 4. Swagger Path Options | ✅ Implementado | Baixa | Dext.OpenAPI.Generator.pas, Dext.Swagger.Middleware.pas |
| 5. SwaggerParam | ✅ Implementado | Média | Dext.OpenAPI.Attributes.pas |
| 6. Response Helper | ✅ Implementado | Baixa | Dext.Web.ResponseHelper.pas |
| 7. Exemplo OrderAPI | ✅ Implementado | Alta | Examples/Web.OrderAPI/* |
| 8. Doc Migração | ✅ Implementado | Média | Docs/Migration/dmvc-to-dext.md |

---

## Implementações Realizadas (2026-01-07)

### 1. HttpStatus (Dext.Web.StatusCodes.pas)
- Constantes com nomenclatura limpa: `HttpStatus.OK`, `HttpStatus.NotFound`, etc.
- Helper methods: `IsSuccess()`, `IsRedirect()`, `IsClientError()`, `IsServerError()`
- `GetReasonPhrase()` para obter descrição textual

### 2. DI para Classes Puras (Dext.DI.Interfaces.pas)
- `AddSingleton<T: class>`
- `AddTransient<T: class>` 
- `AddScoped<T: class>`
- ⚠️ Documentado: Transient de classe pura requer `Free` manual

### 3. Basic Auth Middleware (Dext.Auth.BasicAuth.pas)
- `TBasicAuthMiddleware` com suporte a roles
- `UseBasicAuthentication()` extension
- Popula `IClaimsPrincipal` com claims

### 4. Swagger Path Options (TOpenAPIOptions)
- `SwaggerPath` e `SwaggerJsonPath` customizáveis
- Default: `/swagger` e `/swagger.json`

### 5. SwaggerParam Attribute (Dext.OpenAPI.Attributes.pas)
- `[SwaggerParam('id', 'Description')]` - inferência automática
- Suporte a location, type, required

### 6. Response Helper (Dext.Web.ResponseHelper.pas)
- `RespondJson(Ctx, HttpStatus.BadRequest, '{"error": "%s"}', [E.Message])`
- `RespondError(Ctx, HttpStatus.NotFound, 'Message')`
- `RespondOk(Ctx, Json)`, `RespondCreated(Ctx, Json)`, `RespondNoContent(Ctx)`

### 7. Exemplo Web.OrderAPI
- Controllers com Swagger completo
- Services Transient com interface (ARC)
- TReportService como classe pura (teste de lifecycle)
- DbContext com pooling via AddDbContext<T>

---

## Próximos Passos

1. **Integrar SwaggerParam no Generator** - Processar atributo na geração OpenAPI
2. **Compilar e testar Web.OrderAPI** - Validar todos os componentes
3. **Adicionar testes automatizados** - Script PowerShell para testar endpoints

---

*Documento atualizado em: 2026-01-07*
