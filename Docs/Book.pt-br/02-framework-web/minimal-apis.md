# Minimal APIs

Minimal APIs fornecem uma abordagem leve, baseada em lambdas, para construir endpoints HTTP.

> 📦 **Exemplo**: [Web.MinimalAPI](../../../Examples/Web.MinimalAPI/)

## Endpoints Básicos

```pascal
App.MapGet('/hello', procedure(Ctx: IHttpContext)
  begin
    Ctx.Response.Write('Olá, Mundo!');
  end);

App.MapPost('/data', procedure(Ctx: IHttpContext)
  begin
    var Body := Ctx.Request.BodyAsString;
    Ctx.Response.Json(Body);
  end);

App.MapPut('/items/:id', procedure(Ctx: IHttpContext)
  begin
    // Lógica de atualização
  end);

App.MapDelete('/items/:id', procedure(Ctx: IHttpContext)
  begin
    // Lógica de exclusão
  end);
```

## Parâmetros de Rota

```pascal
// Parâmetro único
App.MapGet('/users/:id', procedure(Ctx: IHttpContext)
  begin
    var Id := Ctx.Request.RouteParam('id');
    Ctx.Response.Write('ID do Usuário: ' + Id);
  end);

// Múltiplos parâmetros
App.MapGet('/orders/:orderId/items/:itemId', procedure(Ctx: IHttpContext)
  begin
    var OrderId := Ctx.Request.RouteParam('orderId');
    var ItemId := Ctx.Request.RouteParam('itemId');
  end);
```

## Parâmetros de Query

```pascal
// GET /search?q=delphi&page=1
App.MapGet('/search', procedure(Ctx: IHttpContext)
  begin
    var Query := Ctx.Request.QueryParam('q');
    var Page := StrToIntDef(Ctx.Request.QueryParam('page'), 1);
  end);
```

## Respostas JSON

```pascal
App.MapGet('/api/user', procedure(Ctx: IHttpContext)
  var
    User: TUser;
  begin
    User := TUser.Create;
    User.Id := 1;
    User.Name := 'João';
    
    Ctx.Response.Json(User); // Serializa automaticamente
  end);
```

## Binding do Request Body

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

## Injeção de Dependência

```pascal
// Registrar serviço
Services.AddScoped<IUserService, TUserService>;

// Usar no handler
App.MapGet('/users', procedure(Ctx: IHttpContext)
  var
    UserService: IUserService;
  begin
    UserService := Ctx.Services.GetRequiredService<IUserService>;
    Ctx.Response.Json(UserService.GetAll);
  end);
```

---

[← Framework Web](README.md) | [Próximo: Controllers →](controllers.md)
