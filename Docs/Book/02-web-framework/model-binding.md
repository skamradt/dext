# Model Binding

Automatically convert HTTP requests to Delphi objects.

## JSON Body Binding

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

## Controller Parameter Binding

In controllers, use `[FromBody]`:

```pascal
[HttpPost]
function Create([FromBody] Request: TCreateUserRequest): IActionResult;
begin
  // Request is automatically bound
end;
```

## Route Parameters

```pascal
// URL: /users/123
App.MapGet('/users/:id', procedure(Ctx: IHttpContext)
  var
    Id: Integer;
  begin
    Id := StrToInt(Ctx.Request.RouteParam('id'));
  end);

// In controller
[HttpGet('/:id')]
function GetById(Id: Integer): IActionResult;  // Auto-bound from route
```

## Query Parameters

```pascal
// URL: /search?q=delphi&page=1&limit=20
App.MapGet('/search', procedure(Ctx: IHttpContext)
  var
    Query: string;
    Page, Limit: Integer;
  begin
    Query := Ctx.Request.QueryParam('q');
    Page := StrToIntDef(Ctx.Request.QueryParam('page'), 1);
    Limit := StrToIntDef(Ctx.Request.QueryParam('limit'), 20);
  end);

// In controller with [FromQuery]
[HttpGet('/search')]
function Search(
  [FromQuery] Q: string;
  [FromQuery] Page: Integer;
  [FromQuery] Limit: Integer
): IActionResult;
```

## Header Binding

```pascal
// In handler
var Token := Ctx.Request.Header('Authorization');
var ContentType := Ctx.Request.Header('Content-Type');

// In controller
[HttpGet('/protected')]
function GetData([FromHeader('Authorization')] Token: string): IActionResult;
```

## Nested Objects

```pascal
type
  TAddress = record
    Street: string;
    City: string;
    ZipCode: string;
  end;
  
  TCreateUserRequest = record
    Name: string;
    Address: TAddress;  // Nested!
  end;
```

JSON:
```json
{
  "name": "John",
  "address": {
    "street": "123 Main St",
    "city": "New York",
    "zipCode": "10001"
  }
}
```

## Arrays

```pascal
type
  TBulkCreateRequest = record
    Users: TArray<TCreateUserRequest>;
  end;
```

JSON:
```json
{
  "users": [
    {"name": "John", "email": "john@example.com"},
    {"name": "Jane", "email": "jane@example.com"}
  ]
}
```

## TUUID Binding

UUIDs are automatically bound:

```pascal
// Route: /items/550e8400-e29b-41d4-a716-446655440000
[HttpGet('/:id')]
function GetById(Id: TUUID): IActionResult;  // Works!
```

---

[← Controllers](controllers.md) | [Next: Routing →](routing.md)
