# OpenAPI / Swagger

Auto-generate interactive API documentation.

> 📦 **Example**: [Web.SwaggerExample](../../../Examples/Web.SwaggerExample/)

## Quick Setup

```pascal
App.Configure(procedure(App: IApplicationBuilder)
  begin
    App.UseSwagger;
    App.UseSwaggerUI;
    
    // Your endpoints...
  end);
```

Visit `http://localhost:5000/swagger` to see the UI!

## Documenting Endpoints

### Minimal APIs

```pascal
App.MapGet('/users', procedure(Ctx: IHttpContext)
  begin
    Ctx.Response.Json(UserService.GetAll);
  end)
  .SwaggerEndpoint
    .Summary('List all users')
    .Description('Returns a list of all registered users')
    .Tag('Users')
    .Response(200, 'List of users')
    .Response(401, 'Unauthorized');
```

### Controllers

```pascal
type
  [Route('/api/users')]
  [SwaggerTag('Users', 'User management endpoints')]
  TUsersController = class(TController)
  public
    [HttpGet]
    [SwaggerSummary('List all users')]
    [SwaggerResponse(200, 'Success', TArray<TUser>)]
    function GetAll: IActionResult;
    
    [HttpGet('/:id')]
    [SwaggerSummary('Get user by ID')]
    [SwaggerParam('id', 'User ID', True)]
    [SwaggerResponse(200, 'User found', TUser)]
    [SwaggerResponse(404, 'User not found')]
    function GetById(Id: Integer): IActionResult;
    
    [HttpPost]
    [SwaggerSummary('Create new user')]
    [SwaggerBody(TCreateUserRequest)]
    [SwaggerResponse(201, 'User created', TUser)]
    function Create([FromBody] Request: TCreateUserRequest): IActionResult;
  end;
```

## Swagger Attributes

| Attribute | Description |
|-----------|-------------|
| `[SwaggerSummary('')]` | Short description |
| `[SwaggerDescription('')]` | Detailed description |
| `[SwaggerTag('Name')]` | Group endpoints |
| `[SwaggerParam('name', 'desc')]` | Document parameter |
| `[SwaggerBody(TType)]` | Request body type |
| `[SwaggerResponse(code, 'desc')]` | Response documentation |
| `[SwaggerResponse(code, 'desc', TType)]` | With response type |

## Fluent API

For Minimal APIs, use the fluent syntax:

```pascal
App.MapPost('/orders', OrderHandler)
  .SwaggerEndpoint
    .Summary('Create order')
    .Description('Creates a new order from the shopping cart')
    .Tag('Orders')
    .Body<TCreateOrderRequest>('Order details')
    .Response<TOrder>(201, 'Order created')
    .Response(400, 'Invalid request')
    .Response(401, 'Not authenticated');
```

## Security Definitions

```pascal
App.UseSwagger(
  TSwaggerOptions.Create
    .Title('My API')
    .Version('v1')
    .AddBearerAuth  // Adds JWT authentication button
);
```

---

[← API Features](README.md) | [Next: Rate Limiting →](rate-limiting.md)
