# Swagger + Controllers Example

This example demonstrates how to integrate **Swagger/OpenAPI** documentation with **MVC Controllers** in the Dext Framework.

## 🚀 Features

*   **Controller-based Swagger**: Use `[SwaggerOperation]`, `[SwaggerResponse]`, `[SwaggerTag]` attributes on controller actions
*   **Schema Documentation**: DTOs annotated with `[SwaggerSchema]`, `[SwaggerProperty]`, `[SwaggerExample]`
*   **Security Integration**: `[Authorize]` attributes appear as lock icons in Swagger UI
*   **Automatic Discovery**: Controllers are auto-discovered and documented

## 🛠️ Getting Started

1.  **Compile** `Web.SwaggerControllerExample.dproj`
2.  **Run** `Web.SwaggerControllerExample.exe`
    *   Server starts on **http://localhost:8080**
3.  **Explore**:
    *   **Swagger UI**: http://localhost:8080/swagger
    *   **OpenAPI JSON**: http://localhost:8080/swagger.json
4.  **Test**:
    ```powershell
    .\Test.Web.SwaggerControllerExample.ps1
    ```

## 📝 Attribute Reference

### Controller Attributes

| Attribute | Description |
|-----------|-------------|
| `[DextController('/path')]` | Defines controller route prefix |
| `[SwaggerTag('Name')]` | Groups endpoints under a tag in Swagger UI |
| `[Authorize('Scheme')]` | Marks all actions as requiring authentication |

### Action Attributes

| Attribute | Description |
|-----------|-------------|
| `[DextGet('/path')]` | HTTP GET endpoint |
| `[DextPost('/path')]` | HTTP POST endpoint |
| `[DextPut('/path')]` | HTTP PUT endpoint |
| `[DextPatch('/path')]` | HTTP PATCH endpoint |
| `[DextDelete('/path')]` | HTTP DELETE endpoint |
| `[SwaggerOperation('summary', 'description')]` | Endpoint documentation |
| `[SwaggerResponse(code, 'description')]` | Response documentation |
| `[AllowAnonymous]` | Allows unauthenticated access |

### Schema Attributes

| Attribute | Description |
|-----------|-------------|
| `[SwaggerSchema('name', 'description')]` | Type documentation |
| `[SwaggerProperty('description')]` | Property documentation |
| `[SwaggerExample('value')]` | Example value |
| `[SwaggerRequired]` | Marks field as required |
| `[SwaggerFormat('format')]` | Format hint (email, date, etc.) |

## 📖 Example Controller

```pascal
[DextController('/api/books')]
[SwaggerTag('Books')]
TBooksController = class
public
  [DextGet('')]
  [AllowAnonymous]
  [SwaggerOperation('List all books', 'Returns all books in the catalog')]
  procedure GetAll(Ctx: IHttpContext); virtual;

  [DextPost('')]
  [SwaggerOperation('Create a new book', 'Creates a new book entry')]
  [SwaggerResponse(201, 'Book created')]
  [SwaggerResponse(400, 'Invalid request')]
  procedure Create(Ctx: IHttpContext; const Request: TCreateBookRequest); virtual;
end;
```

## 📚 See Also

- [Web.SwaggerExample](../Web.SwaggerExample) - Minimal API with Swagger
- [Web.ControllerExample](../Web.ControllerExample) - Controllers without Swagger
- [Swagger Documentation](../../Docs/swagger.md)
