# 2. Web Framework

Dext provides two styles for building web APIs: **Minimal APIs** and **Controllers**.

## Chapters

1. [Minimal APIs](minimal-apis.md) - Lambda-based route handlers
2. [Controllers](controllers.md) - Class-based MVC style
3. [Model Binding](model-binding.md) - Request to object mapping
4. [Routing](routing.md) - URL patterns & parameters
5. [Middleware](middleware.md) - Request pipeline

## Quick Comparison

### Minimal API Style

```pascal
App.MapGet('/users/:id', procedure(Ctx: IHttpContext)
  var
    Id: Integer;
  begin
    Id := StrToInt(Ctx.Request.RouteParam('id'));
    Ctx.Response.Json(UserService.GetById(Id));
  end);
```

### Controller Style

```pascal
type
  [Route('/users')]
  TUsersController = class(TController)
  public
    [HttpGet('/:id')]
    function GetById(Id: Integer): IActionResult;
  end;

function TUsersController.GetById(Id: Integer): IActionResult;
begin
  Result := Ok(UserService.GetById(Id));
end;
```

## When to Use Each

| Minimal APIs | Controllers |
|-------------|-------------|
| Quick prototypes | Large applications |
| Microservices | Complex routing |
| Simple CRUD | Shared middleware |
| Lambda style | OOP style |

---

[← Getting Started](../01-getting-started/README.md) | [Next: Minimal APIs →](minimal-apis.md)
