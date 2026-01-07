# Guia de Migração: DelphiMVC Framework para Dext

Este guia ajuda desenvolvedores a migrar projetos do DelphiMVC Framework (DMVC) para o Dext Framework, mostrando equivalências de código e padrões.

## Índice

1. [Estrutura Básica do Projeto](#estrutura-básica-do-projeto)
2. [Controllers](#controllers)
3. [Roteamento](#roteamento)
4. [Injeção de Dependência](#injeção-de-dependência)
5. [Autenticação](#autenticação)
6. [Swagger/OpenAPI](#swaggeropenapi)
7. [Códigos de Status HTTP](#códigos-de-status-http)
8. [Model Binding](#model-binding)
9. [Respostas JSON](#respostas-json)
10. [Middleware](#middleware)

---

## Estrutura Básica do Projeto

### DMVC
```pascal
program MyAPI;

uses
  MVCFramework,
  MVCFramework.Commons,
  MVCFramework.RESTAdapter,
  Web.WebReq,
  Web.WebBroker;

begin
  WebRequestHandler...
end.
```

### Dext
```pascal
program MyAPI;

uses
  Dext,     // Unidade curinga (IStartup, Configuration)
  Dext.Web, // Unidade curinga (Web features e middlewares)
  OrderAPI.Startup;

begin
  var App := TDextApplication.Create;
  
  // Padrão recomendado: Usar classe Startup
  App.UseStartup(TStartup.Create);
  
  App.Run(5000);
end.
```

### Classe Startup (Recomendado)
O Dext promove o uso de uma classe `Startup` para organizar a inicialização, similar ao ASP.NET Core.

```pascal
type
  TStartup = class(TInterfacedObject, IStartup)
  public
    // Configuração de Serviços (DI)
    procedure ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
    
    // Configuração do Pipeline (Middleware)
    procedure Configure(const App: IWebApplication);
  end;

procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  Services.AddTransient<ICustomerService, TCustomerService>;
  Services.AddControllers;
end;

procedure TStartup.Configure(const App: IWebApplication);
begin
  App.Builder.UseCors(TCorsOptions.Default);
  App.MapControllers;
  App.Builder.UseSwagger; // Sintaxe fluente e automática
end;
```

---

## Controllers

### DMVC
```pascal
type
  [MVCPath('/api/customers')]
  TCustomersController = class(TMVCController)
  public
    [MVCPath('')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomers;
    
    [MVCPath('/($id)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetCustomer(id: Integer);
  end;

procedure TCustomersController.GetCustomers;
begin
  Render(ObjectList);
end;
```

### Dext
```pascal
type
  [DextController('/api/customers')]
  TCustomersController = class
  public
    [DextGet('')]
    procedure GetCustomers(Ctx: IHttpContext); virtual;
    
    [DextGet('/{id}')]
    procedure GetCustomer(Ctx: IHttpContext; [FromRoute] Id: Integer); virtual;
  end;

procedure TCustomersController.GetCustomers(Ctx: IHttpContext);
begin
  Ctx.Response.Json(TDextJson.Serialize(ObjectList));
end;
```

### Principais Diferenças:
- Dext não requer herança de classe base
- Métodos HTTP são attributes separados (`[DextGet]`, `[DextPost]`, etc.)
- Contexto HTTP é injetado como parâmetro
- Parâmetros de rota usam `{id}` em vez de `($id)`

---

## Roteamento

### DMVC
```pascal
[MVCPath('/users/($id)/orders/($orderId)')]
[MVCHTTPMethod([httpGET])]
procedure GetUserOrder(id: Integer; orderId: Integer);
```

### Dext
```pascal
[DextGet('/users/{id}/orders/{orderId}')]
procedure GetUserOrder(Ctx: IHttpContext; 
  [FromRoute] Id: Integer; 
  [FromRoute] OrderId: Integer); virtual;
```

---

## Injeção de Dependência

### DMVC
```pascal
// Registro no WebModule
procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FMVC.AddController(TCustomersController);
end;

// Injeção via construtor ou campo
constructor TCustomersController.Create;
begin
  FService := GlobalContainer.Resolve<ICustomerService>;
end;
```

### Dext
```pascal
// No método Startup.ConfigureServices
Services
  .AddSingleton<ICustomerService, TCustomerService>
  .AddTransient<IOrderService, TOrderService>
  .AddScoped<TDbContext>
  .AddControllers;

// Injeção via construtor (automática)
type
  TCustomersController = class
  private
    FService: ICustomerService;
  public
    constructor Create(Service: ICustomerService);
  end;

// Ou via parâmetro do método
procedure DoSomething(Ctx: IHttpContext; 
  [FromServices] const Service: IOrderService);
```

### Registro de Classes Puras (Novo!)
```pascal
// DMVC - requer interface
Services.AddSingleton<IMyService, TMyService>;

// Dext - também suporta classes diretamente
App.Services.AddSingleton<TMyService>;  // Sem interface!
App.Services.AddScoped<TDbContext>;
```

---

## Autenticação

### DMVC - Basic Auth
```pascal
uses
  MVCFramework.Middleware.Authentication;

FMVC.AddMiddleware(TMVCBasicAuthenticationMiddleware.Create(
  TAuthenticationSample.Create));
```

### Dext - Basic Auth
```pascal
uses
  Dext.Auth.BasicAuth;

App.Builder.UseBasicAuthentication('My API',
  function(const Username, Password: string): Boolean
  begin
    Result := (Username = 'admin') and (Password = 'secret');
  end);
```

### DMVC - JWT
```pascal
uses
  MVCFramework.Middleware.JWT;

FMVC.AddMiddleware(TMVCJWTAuthenticationMiddleware.Create(...));
```

### Dext - JWT
```pascal
uses
  Dext.Auth.Middleware;

App.Builder.UseJwtAuthentication('my-secret-key',
  procedure(Auth: TJwtOptionsBuilder)
  begin
    Auth.WithIssuer('my-issuer')
        .WithAudience('my-audience')
        .WithExpirationMinutes(120);
  end);
```

### Atributos de Autorização

| DMVC | Dext |
|------|------|
| `[MVCRequiresAuthentication]` | `[Authorize]` |
| `[MVCRequiresRole('admin')]` | `[Authorize('admin')]` |
| (n/a) | `[AllowAnonymous]` |

---

## Swagger/OpenAPI

### DMVC
```pascal
[MVCSwagSummary('Customers', 'Get all customers', 'getCustomers')]
[MVCSwagResponses(200, 'Customers', TCustomer, True)]
[MVCSwagResponses(HTTP_STATUS.BadRequest, '', TMVCErrorResponse)]
[MVCSwagParam(plPath, 'id', 'Customer ID', ptInteger, True)]
[MVCPath('/customers/($id)')]
[MVCHTTPMethod([httpGET])]
procedure GetCustomer(id: Integer);
```

### Dext
```pascal
[DextGet('/customers/{id}')]
[SwaggerOperation('Get customer by ID', 'Returns detailed customer information')]
[SwaggerTag('Customers')]
[SwaggerParam('id', 'Customer unique identifier')]
[SwaggerResponse(HttpStatus.OK, 'Customer found')]
[SwaggerResponse(HttpStatus.NotFound, 'Customer not found')]
procedure GetCustomer(Ctx: IHttpContext; [FromRoute] Id: Integer); virtual;
```

### Configuração do Swagger

#### DMVC
```pascal
// No WebModule
FMVC.AddMiddleware(TMVCSwaggerMiddleware.Create(FMVC, ...));
```

#### Dext
```pascal
uses
  Dext.Swagger.Middleware,
  Dext.OpenAPI.Generator;

var Options := TOpenAPIOptions.Default;
Options.Title := 'My API';
Options.Description := 'API Documentation';
Options.Version := '1.0.0';
Options.SwaggerPath := '/docs';        // Customizável
Options.SwaggerJsonPath := '/docs/openapi.json';

// Registro Fluente:
App.Builder.UseSwagger(Options);
```

---

## Códigos de Status HTTP

### DMVC
```pascal
uses MVCFramework.Commons;

raise EMVCException.Create(HTTP_STATUS.NotFound, 'Customer not found');
Render(HTTP_STATUS.Created, 'Created');
```

### Dext
```pascal
uses Dext.Web.StatusCodes;

Ctx.Response.StatusCode := HttpStatus.NotFound;
Ctx.Response.Json('{"error": "Not found"}');

// Já incluído em Dext.Web (Wildcard)
// uses Dext.Web;

RespondJson(Ctx, HttpStatus.BadRequest, '{"error": "%s"}', [E.Message]);
RespondError(Ctx, HttpStatus.NotFound, 'Resource not found');
RespondOk(Ctx, TDextJson.Serialize(Data));
RespondCreated(Ctx, TDextJson.Serialize(Entity));
```

### Tabela de Equivalência

| DMVC | Dext |
|------|------|
| `HTTP_STATUS.OK` | `HttpStatus.OK` |
| `HTTP_STATUS.Created` | `HttpStatus.Created` |
| `HTTP_STATUS.NoContent` | `HttpStatus.NoContent` |
| `HTTP_STATUS.BadRequest` | `HttpStatus.BadRequest` |
| `HTTP_STATUS.Unauthorized` | `HttpStatus.Unauthorized` |
| `HTTP_STATUS.Forbidden` | `HttpStatus.Forbidden` |
| `HTTP_STATUS.NotFound` | `HttpStatus.NotFound` |
| `HTTP_STATUS.InternalServerError` | `HttpStatus.InternalServerError` |

---

## Model Binding

### DMVC
```pascal
procedure CreateCustomer;
var
  lCustomer: TCustomer;
begin
  lCustomer := Context.Request.BodyAs<TCustomer>;
  try
    // use lCustomer
  finally
    lCustomer.Free;
  end;
end;
```

### Dext
```pascal
// Automático via parâmetro - sem necessidade de Free!
procedure CreateCustomer(Ctx: IHttpContext; const Request: TCreateCustomerRequest); virtual;
begin
  // Request já está populado, tipo gerenciado pelo framework
  FService.Create(Request);
  Ctx.Response.Status(HttpStatus.Created);
end;
```

### Atributos de Binding

| DMVC | Dext |
|------|------|
| `Context.Request.BodyAs<T>` | Parâmetro sem atributo (body) |
| Path param `($id)` | `[FromRoute] Id` |
| Query param | `[FromQuery] Page` |
| Header | `[FromHeader] ApiKey` |
| (n/a) | `[FromServices] Service` |

---

## Respostas JSON

### DMVC
```pascal
Render(Customer);           // Objeto
Render<TCustomer>(List);    // Lista genérica
Render201Created();
Render(HTTP_STATUS.OK);
```

### Dext
```pascal
Ctx.Response.Json(TDextJson.Serialize(Customer));
Ctx.Response.Json(TDextJson.Serialize<TArray<TCustomer>>(Customers));
Ctx.Response.Status(HttpStatus.Created).Json(Data);
Ctx.Response.StatusCode := HttpStatus.OK;
```

---

## Middleware

### DMVC
```pascal
FMVC.AddMiddleware(TMVCCORSMiddleware.Create);
FMVC.AddMiddleware(TMVCCompressionMiddleware.Create);
```

### Dext
// No método Startup.Configure
App.GetBuilder
  .UseCors(TCorsOptions.Default) // Ou configurado manualmente
  .UseCompression;

---

## Checklist de Migração

- [ ] Substituir `TMVCController` por classes simples com `[DextController]`
- [ ] Mudar `[MVCPath]` + `[MVCHTTPMethod]` para `[DextGet]`, `[DextPost]`, etc.
- [ ] Atualizar sintaxe de path params: `($id)` → `{id}`
- [ ] Adicionar `Ctx: IHttpContext` como primeiro parâmetro dos métodos
- [ ] Configurar DI via `App.Services` em vez de container global
- [ ] Substituir `Render()` por `Ctx.Response.Json()`
- [ ] Atualizar atributos Swagger para sintaxe Dext
- [ ] Substituir `HTTP_STATUS` por `HttpStatus`
- [ ] Configurar middleware via `App.Builder`

---

## Exemplo Completo: API de Produtos

### DMVC
```pascal
[MVCPath('/api/products')]
TProductsController = class(TMVCController)
public
  [MVCSwagSummary('Products', 'List all products')]
  [MVCSwagResponses(HTTP_STATUS.OK, '', TProduct, True)]
  [MVCPath('')]
  [MVCHTTPMethod([httpGET])]
  procedure GetProducts;
  
  [MVCSwagSummary('Products', 'Get product by ID')]
  [MVCSwagParam(plPath, 'id', 'Product ID', ptInteger, True)]
  [MVCSwagResponses(HTTP_STATUS.OK, '', TProduct)]
  [MVCSwagResponses(HTTP_STATUS.NotFound, '')]
  [MVCPath('/($id)')]
  [MVCHTTPMethod([httpGET])]
  procedure GetProduct(id: Integer);
end;

procedure TProductsController.GetProducts;
begin
  Render<TProduct>(FService.GetAll);
end;

procedure TProductsController.GetProduct(id: Integer);
var
  Product: TProduct;
begin
  Product := FService.GetById(id);
  if Product = nil then
    raise EMVCException.Create(HTTP_STATUS.NotFound, 'Product not found');
  Render(Product);
end;
```

### Dext
```pascal
[DextController('/api/products')]
[SwaggerTag('Products')]
TProductsController = class
private
  FService: IProductService;
public
  constructor Create(Service: IProductService);
  
  [DextGet('')]
  [SwaggerOperation('List all products')]
  [SwaggerResponse(HttpStatus.OK, 'Products list')]
  procedure GetProducts(Ctx: IHttpContext); virtual;
  
  [DextGet('/{id}')]
  [SwaggerOperation('Get product by ID')]
  [SwaggerParam('id', 'Product unique identifier')]
  [SwaggerResponse(HttpStatus.OK, 'Product found')]
  [SwaggerResponse(HttpStatus.NotFound, 'Product not found')]
  procedure GetProduct(Ctx: IHttpContext; [FromRoute] Id: Integer); virtual;
end;

constructor TProductsController.Create(Service: IProductService);
begin
  FService := Service;
end;

procedure TProductsController.GetProducts(Ctx: IHttpContext);
begin
  Ctx.Response.Json(TDextJson.Serialize(FService.GetAll));
end;

procedure TProductsController.GetProduct(Ctx: IHttpContext; Id: Integer);
begin
  var Product := FService.GetById(Id);
  if Product = nil then
  begin
    Ctx.Response.StatusCode := HttpStatus.NotFound;
    Ctx.Response.Json('{"error": "Product not found"}');
    Exit;
  end;
  Ctx.Response.Json(TDextJson.Serialize(Product));
end;
```

---

## Links Úteis

- [Dext Framework - Documentação](../README.md)
- [Exemplos do Dext](../../Examples/)
- [DelphiMVC Framework](https://github.com/danieleteti/delphimvcframework)

---

*Última atualização: 2026-01-07*
