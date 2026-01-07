# Routing

Define URL patterns and extract parameters.

## Route Patterns

### Static Routes

```pascal
App.MapGet('/users', Handler);           // GET /users
App.MapPost('/users', Handler);          // POST /users
App.MapGet('/api/health', Handler);      // GET /api/health
```

### Route Parameters

```pascal
App.MapGet('/users/:id', procedure(Ctx: IHttpContext)
  begin
    var Id := Ctx.Request.RouteParam('id');
    // Id = "123" for /users/123
  end);

App.MapGet('/orders/:orderId/items/:itemId', procedure(Ctx: IHttpContext)
  begin
    var OrderId := Ctx.Request.RouteParam('orderId');
    var ItemId := Ctx.Request.RouteParam('itemId');
  end);
```

### Optional Parameters

```pascal
App.MapGet('/files/:path*', procedure(Ctx: IHttpContext)
  begin
    var Path := Ctx.Request.RouteParam('path');
    // Captures rest of path: /files/docs/readme.md → "docs/readme.md"
  end);
```

## Controller Routes

### Class-Level Route

```pascal
[Route('/api/v1/users')]
TUsersController = class(TController)
public
  [HttpGet]             // GET /api/v1/users
  function GetAll: IActionResult;
  
  [HttpGet('/:id')]     // GET /api/v1/users/123
  function GetById(Id: Integer): IActionResult;
  
  [HttpPost]            // POST /api/v1/users
  function Create([FromBody] User: TUser): IActionResult;
end;
```

### Versioned APIs

```pascal
[Route('/api/v1/orders')]
TOrdersV1Controller = class(TController)
end;

[Route('/api/v2/orders')]
TOrdersV2Controller = class(TController)
end;
```

## Query Parameters

```pascal
// URL: /search?q=delphi&page=1&limit=20
App.MapGet('/search', procedure(Ctx: IHttpContext)
  begin
    var Query := Ctx.Request.QueryParam('q');
    var Page := Ctx.Request.QueryParam('page');
    var Limit := Ctx.Request.QueryParam('limit');
  end);
```

### In Controllers

```pascal
[HttpGet('/search')]
function Search(
  [FromQuery] Q: string;
  [FromQuery('page')] PageNum: Integer;  // Custom name
  [FromQuery] Limit: Integer = 20        // Default value
): IActionResult;
```

## Route Constraints

Validate parameters at route level:

```pascal
// Only match if id is numeric
App.MapGet('/users/:id<int>', Handler);

// Only match specific values
App.MapGet('/status/:status<active|inactive>', Handler);
```

## HTTP Methods

```pascal
App.MapGet('/resource', Handler);     // GET
App.MapPost('/resource', Handler);    // POST
App.MapPut('/resource/:id', Handler); // PUT
App.MapPatch('/resource/:id', Handler); // PATCH
App.MapDelete('/resource/:id', Handler); // DELETE
App.MapOptions('/resource', Handler); // OPTIONS
App.MapHead('/resource', Handler);    // HEAD
```

## Grouping Routes

```pascal
App.MapGroup('/api/v1', procedure(Group: IRouteGroup)
  begin
    Group.MapGet('/users', UsersHandler);
    Group.MapGet('/orders', OrdersHandler);
    // Results in: /api/v1/users, /api/v1/orders
  end);
```

---

[← Model Binding](model-binding.md) | [Next: Middleware →](middleware.md)
