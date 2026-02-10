# Controllers

Controllers fornecem uma abordagem baseada em classes, estilo MVC, para organizar endpoints.

> 📦 **Exemplo**: [Web.ControllerExample](../../../Examples/Web.ControllerExample/)

## Controller Básico

```pascal
type
  [ApiController]
  [Route('/api/users')]
  TUsersController = class // Não precisa mais herdar de TController
  public
    [HttpGet]
    function GetAll: IActionResult;
    
    [HttpGet('{id}')]
    function Get(Id: Int64): IActionResult;
    
    [HttpPost]
    function Create([Body] User: TUser): IActionResult;
    
    [HttpPut('{id}')]
    function Update(Id: Int64; [Body] User: TUser): IActionResult;
    
    [HttpDelete('{id}')]
    function Delete(Id: Int64): IActionResult;
  end;
```

## Implementação

```pascal
function TUsersController.GetAll: IActionResult;
begin
  var Users := FUserService.GetAll;
  Result := Ok(Users);
end;

function TUsersController.GetById(Id: Integer): IActionResult;
begin
  var User := FUserService.FindById(Id);
  if User = nil then
    Result := NotFound
  else
    Result := Ok(User);
end;

function TUsersController.Create(User: TUser): IActionResult;
begin
  FUserService.Add(User);
  Result := Created('/api/users/' + User.Id.ToString, User);
end;
```

## Registrar Controller

```pascal
App.Configure(procedure(App: IApplicationBuilder)
  begin
    App.MapController<TUsersController>;
  end);
```

## Injeção via Construtor

```pascal
type
  [ApiController]
  [Route('/api/users')]
  TUsersController = class
  private
    FUserService: IUserService;
    FLogger: ILogger;
  public
    constructor Create(UserService: IUserService; Logger: ILogger);
  end;

constructor TUsersController.Create(UserService: IUserService; Logger: ILogger);
begin
  FUserService := UserService;
  FLogger := Logger;
end;
```

Serviços são injetados automaticamente quando registrados:

```pascal
Services.AddScoped<IUserService, TUserService>;
Services.AddSingleton<ILogger, TConsoleLogger>;
```

## Atributos de Rota (Routing)

O Dext suporta dois estilos para definir rotas. **Importante**: Rotas com parâmetros **DEVEM iniciar com barra** (`/`).

### Opção 1: Consolidado (Recomendado)
```pascal
[ApiController('/api/v1/products')] // Rota base no ApiController
type TProductsController = class
  // ...
  [HttpGet]                         // GET /api/v1/products
  function GetAll: IActionResult;

  [HttpGet('/{id}')]                // GET /api/v1/products/123 (Barra inicial é OBRIGATÓRIA)
  function Get(Id: Integer): IActionResult;
end;
```

### Opção 2: Separado (Estilo .NET)
```pascal
[ApiController]
[Route('/api/v1/products')]         // Rota base no atributo Route
type TProductsController = class
  // ...
  [HttpGet]
  function GetAll: IActionResult;

  [HttpGet, Route('/{id}')]         // GET /api/v1/products/123
  function Get(Id: Integer): IActionResult;
end;
```

## Action Results

```pascal
Result := Ok(Data);                        // 200 + JSON
Result := Created('/path', Data);          // 201 + Header Location
Result := NoContent;                        // 204
Result := BadRequest('Dados inválidos');    // 400
Result := Unauthorized;                     // 401
Result := Forbidden;                        // 403
Result := NotFound;                         // 404
Result := StatusCode(418, 'Eu sou um bule'); // Customizado
```

---

[← Minimal APIs](minimal-apis.md) | [Próximo: Model Binding →](model-binding.md)
