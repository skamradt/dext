# Controllers

Controllers provide a class-based, MVC-style approach to organizing endpoints.

> 📦 **Example**: [Web.ControllerExample](../../../Examples/Web.ControllerExample/)

## Basic Controller

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

## Implementation

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

## Register Controller

```pascal
App.Configure(procedure(App: IApplicationBuilder)
  begin
    App.MapController<TUsersController>;
  end);
```

## Constructor Injection

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

Services are automatically injected when registered:

```pascal
Services.AddScoped<IUserService, TUserService>;
Services.AddSingleton<ILogger, TConsoleLogger>;
```

## Route Attributes

```pascal
[Route('/api/v1/products')]     // Base route
[HttpGet]                        // GET /api/v1/products
[HttpGet('/:id')]               // GET /api/v1/products/123
[HttpGet('/search')]            // GET /api/v1/products/search
[HttpPost]                       // POST /api/v1/products
[HttpPut('/:id')]               // PUT /api/v1/products/123
[HttpDelete('/:id')]            // DELETE /api/v1/products/123
```

## Parameter Binding

```pascal
// Route parameter
function GetById(Id: Integer): IActionResult;  // :id -> Id

// Query parameter
function Search([FromQuery] Q: string; [FromQuery] Page: Integer): IActionResult;

// Body
function Create([FromBody] Product: TProduct): IActionResult;

// Header
function Auth([FromHeader('Authorization')] Token: string): IActionResult;
```

## Action Results

```pascal
Result := Ok(Data);                        // 200 + JSON
Result := Created('/path', Data);          // 201 + Location header
Result := NoContent;                        // 204
Result := BadRequest('Invalid data');       // 400
Result := Unauthorized;                     // 401
Result := Forbidden;                        // 403
Result := NotFound;                         // 404
Result := StatusCode(418, 'I am a teapot'); // Custom
```

---

[← Minimal APIs](minimal-apis.md) | [Next: Model Binding →](model-binding.md)
