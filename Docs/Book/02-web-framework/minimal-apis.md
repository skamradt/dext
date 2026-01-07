# Minimal APIs

Minimal APIs provide a lightweight, lambda-based approach to building HTTP endpoints.

> 📦 **Example**: [Web.MinimalAPI](../../../Examples/Web.MinimalAPI/)

## Basic Endpoints

```pascal
App.MapGet('/hello', procedure(Ctx: IHttpContext)
  begin
    Ctx.Response.Write('Hello, World!');
  end);

App.MapPost('/data', procedure(Ctx: IHttpContext)
  begin
    var Body := Ctx.Request.BodyAsString;
    Ctx.Response.Json(Body);
  end);

App.MapPut('/items/:id', procedure(Ctx: IHttpContext)
  begin
    // Update logic
  end);

App.MapDelete('/items/:id', procedure(Ctx: IHttpContext)
  begin
    // Delete logic
  end);
```

## Route Parameters

```pascal
// Single parameter
App.MapGet('/users/:id', procedure(Ctx: IHttpContext)
  begin
    var Id := Ctx.Request.RouteParam('id');
    Ctx.Response.Write('User ID: ' + Id);
  end);

// Multiple parameters
App.MapGet('/orders/:orderId/items/:itemId', procedure(Ctx: IHttpContext)
  begin
    var OrderId := Ctx.Request.RouteParam('orderId');
    var ItemId := Ctx.Request.RouteParam('itemId');
  end);
```

## Query Parameters

```pascal
// GET /search?q=delphi&page=1
App.MapGet('/search', procedure(Ctx: IHttpContext)
  begin
    var Query := Ctx.Request.QueryParam('q');
    var Page := StrToIntDef(Ctx.Request.QueryParam('page'), 1);
  end);
```

## JSON Responses

```pascal
App.MapGet('/api/user', procedure(Ctx: IHttpContext)
  var
    User: TUser;
  begin
    User := TUser.Create;
    User.Id := 1;
    User.Name := 'John';
    
    Ctx.Response.Json(User); // Auto-serializes
  end);
```

## Request Body Binding

```pascal
type
  TCreateUserRequest = record
    Name: string;
    Email: string;
  end;

App.MapPost('/users', procedure(Ctx: IHttpContext)
  var
    Request: TCreateUserRequest;
  begin
    Request := Ctx.Request.BindBody<TCreateUserRequest>;
    // Use Request.Name, Request.Email
  end);
```

## Using IResult

```pascal
App.MapGet('/users/:id', procedure(Ctx: IHttpContext)
  var
    Id: Integer;
    User: TUser;
    Res: IResult;
  begin
    Id := StrToInt(Ctx.Request.RouteParam('id'));
    User := UserService.FindById(Id);
    
    if User = nil then
      Res := Results.NotFound
    else
      Res := Results.Ok(User);
      
    Res.Execute(Ctx);
  end);
```

## Dependency Injection

```pascal
// Register service
Services.AddScoped<IUserService, TUserService>;

// Use in handler
App.MapGet('/users', procedure(Ctx: IHttpContext)
  var
    UserService: IUserService;
  begin
    UserService := Ctx.Services.GetRequiredService<IUserService>;
    Ctx.Response.Json(UserService.GetAll);
  end);
```

---

[← Web Framework](README.md) | [Next: Controllers →](controllers.md)
