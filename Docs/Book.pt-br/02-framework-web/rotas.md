# Rotas

Defina padrões de URL e extraia parâmetros.

## Padrões de Rota

### Rotas Estáticas

```pascal
App.MapGet('/users', Handler);           // GET /users
App.MapPost('/users', Handler);          // POST /users
App.MapGet('/api/health', Handler);      // GET /api/health
```

### Parâmetros de Rota

```pascal
App.MapGet('/users/:id', procedure(Ctx: IHttpContext)
  begin
    var Id := Ctx.Request.RouteParam('id');
    // Id = "123" para /users/123
  end);

App.MapGet('/orders/:orderId/items/:itemId', procedure(Ctx: IHttpContext)
  begin
    var OrderId := Ctx.Request.RouteParam('orderId');
    var ItemId := Ctx.Request.RouteParam('itemId');
  end);
```

## Rotas em Controllers

### Rota a Nível de Classe

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

### APIs Versionadas

```pascal
[Route('/api/v1/orders')]
TOrdersV1Controller = class(TController)
end;

[Route('/api/v2/orders')]
TOrdersV2Controller = class(TController)
end;
```

## Parâmetros de Query

```pascal
// URL: /search?q=delphi&page=1&limit=20
App.MapGet('/search', procedure(Ctx: IHttpContext)
  begin
    var Query := Ctx.Request.QueryParam('q');
    var Page := Ctx.Request.QueryParam('page');
    var Limit := Ctx.Request.QueryParam('limit');
  end);
```

## Métodos HTTP

```pascal
App.MapGet('/resource', Handler);     // GET
App.MapPost('/resource', Handler);    // POST
App.MapPut('/resource/:id', Handler); // PUT
App.MapPatch('/resource/:id', Handler); // PATCH
App.MapDelete('/resource/:id', Handler); // DELETE
```

## Agrupando Rotas

```pascal
App.MapGroup('/api/v1', procedure(Group: IRouteGroup)
  begin
    Group.MapGet('/users', UsersHandler);
    Group.MapGet('/orders', OrdersHandler);
    // Resulta em: /api/v1/users, /api/v1/orders
  end);
```

---

[← Model Binding](model-binding.md) | [Próximo: Middleware →](middleware.md)
