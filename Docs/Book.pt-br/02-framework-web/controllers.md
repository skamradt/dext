# Controllers

Controllers fornecem uma abordagem baseada em classes, estilo MVC, para organizar endpoints.

> 📦 **Exemplo**: [Web.ControllerExample](../../../Examples/Web.ControllerExample/)

## Controller Básico

```pascal
type
  [Route('/api/users')]
  TUsersController = class(TController)
  public
    [HttpGet]
    function GetAll: IActionResult;
    
    [HttpGet('/:id')]
    function GetById(Id: Integer): IActionResult;
    
    [HttpPost]
    function Create([FromBody] User: TUser): IActionResult;
    
    [HttpPut('/:id')]
    function Update(Id: Integer; [FromBody] User: TUser): IActionResult;
    
    [HttpDelete('/:id')]
    function Delete(Id: Integer): IActionResult;
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
  TUsersController = class(TController)
  private
    FUserService: IUserService;
    FLogger: ILogger;
  public
    constructor Create(UserService: IUserService; Logger: ILogger);
  end;

constructor TUsersController.Create(UserService: IUserService; Logger: ILogger);
begin
  inherited Create;
  FUserService := UserService;
  FLogger := Logger;
end;
```

Serviços são injetados automaticamente quando registrados:

```pascal
Services.AddScoped<IUserService, TUserService>;
Services.AddSingleton<ILogger, TConsoleLogger>;
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
