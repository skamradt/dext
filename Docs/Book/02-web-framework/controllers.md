# Controllers

Controllers provide a class-based, MVC-style approach to organizing endpoints.

> 📦 **Example**: [Web.ControllerExample](../../../Examples/Web.ControllerExample/)

## Basic Controller

```pascal
type
  [ApiController]
  [Route('/api/users')]
  TUsersController = class // No longer needs to inherit from TController
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

Services are automatically injected when registered:

```pascal
Services.AddScoped<IUserService, TUserService>;
Services.AddSingleton<ILogger, TConsoleLogger>;
```

## Route Attributes

Dext supports two styles for defining routes. **Important**: Parameter routes **MUST start with a slash** (`/`).

### Option 1: Consolidated (Recommended)
```pascal
[ApiController('/api/v1/products')] // Base route defined in ApiController
type TProductsController = class
  // ...
  [HttpGet]                         // GET /api/v1/products
  function GetAll: IActionResult;

  [HttpGet('/{id}')]                // GET /api/v1/products/123 (Leading slash is MANDATORY)
  function Get(Id: Integer): IActionResult;
end;
```

### Option 2: Separated (.NET Style)
```pascal
[ApiController]
[Route('/api/v1/products')]         // Base route defined in Route attribute
type TProductsController = class
  // ...
  [HttpGet]
  function GetAll: IActionResult;

  [HttpGet, Route('/{id}')]         // GET /api/v1/products/123
  function Get(Id: Integer): IActionResult;
end;
```

### Route Parameters
```pascal
[HttpGet('/search')]                // GET /api/v1/products/search
[HttpPost]                          // POST /api/v1/products
[HttpPut('/{id}')]                  // PUT /api/v1/products/123
[HttpDelete('/{id}')]               // DELETE /api/v1/products/123
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
