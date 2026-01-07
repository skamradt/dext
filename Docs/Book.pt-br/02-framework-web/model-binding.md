# Model Binding

Converta requisições HTTP em objetos Delphi automaticamente.

## Binding de JSON Body

### Records

```pascal
type
  TCreateUserRequest = record
    Name: string;
    Email: string;
    Age: Integer;
  end;

App.MapPost('/users', procedure(Ctx: IHttpContext)
  var
    Request: TCreateUserRequest;
  begin
    Request := Ctx.Request.BindBody<TCreateUserRequest>;
    // Use Request.Name, Request.Email, Request.Age
  end);
```

### Classes

```pascal
type
  TUser = class
  public
    Name: string;
    Email: string;
  end;

App.MapPost('/users', procedure(Ctx: IHttpContext)
  var
    User: TUser;
  begin
    User := Ctx.Request.BindBody<TUser>;
    try
      // Use User...
    finally
      User.Free;
    end;
  end);
```

## Binding de Parâmetros em Controllers

Em controllers, use `[FromBody]`:

```pascal
[HttpPost]
function Create([FromBody] Request: TCreateUserRequest): IActionResult;
begin
  // Request é automaticamente populado
end;
```

## Parâmetros de Rota

```pascal
// URL: /users/123
App.MapGet('/users/:id', procedure(Ctx: IHttpContext)
  var
    Id: Integer;
  begin
    Id := StrToInt(Ctx.Request.RouteParam('id'));
  end);

// No controller
[HttpGet('/:id')]
function GetById(Id: Integer): IActionResult;  // Auto-binding da rota
```

## Parâmetros de Query

```pascal
// URL: /search?q=delphi&page=1&limit=20
App.MapGet('/search', procedure(Ctx: IHttpContext)
  begin
    var Query := Ctx.Request.QueryParam('q');
    var Page := StrToIntDef(Ctx.Request.QueryParam('page'), 1);
    var Limit := StrToIntDef(Ctx.Request.QueryParam('limit'), 20);
  end);

// No controller com [FromQuery]
[HttpGet('/search')]
function Search(
  [FromQuery] Q: string;
  [FromQuery] Page: Integer;
  [FromQuery] Limit: Integer
): IActionResult;
```

## Objetos Aninhados

```pascal
type
  TAddress = record
    Street: string;
    City: string;
    ZipCode: string;
  end;
  
  TCreateUserRequest = record
    Name: string;
    Address: TAddress;  // Aninhado!
  end;
```

JSON:
```json
{
  "name": "João",
  "address": {
    "street": "Rua Principal, 123",
    "city": "São Paulo",
    "zipCode": "01234-567"
  }
}
```

---

[← Controllers](controllers.md) | [Próximo: Rotas →](rotas.md)
