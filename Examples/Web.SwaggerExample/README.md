# Swagger/OpenAPI Example

This example demonstrates how to integrate Swagger UI and OpenAPI specification generation into a Dext web application.

## 🚀 Features

*   **Automatic OpenAPI Generation**: Use attributes like `[SwaggerSchema]`, `[SwaggerProperty]` to define your API documentation directly in code.
*   **Swagger UI Middleware**: Serve the interactive Swagger UI at `/swagger`.
*   **Minimal API Integration**: `MapGet`, `MapPost` endpoints are automatically discovered and documented.
*   **Fluent DSL**: Configure endpoints with the elegant `SwaggerEndpoint` fluent API.

## 🛠️ Getting Started

1.  **Compile** `Web.SwaggerExample.dproj`.
2.  **Run** `Web.SwaggerExample.exe`.
    *   Server starts on **http://localhost:5000**.
3.  **Explore**:
    *   **UI**: Open `http://localhost:5000/swagger` in your browser.
    *   **JSON Spec**: `http://localhost:5000/swagger.json`.
4.  **Test**:
    ```powershell
    .\Test.Web.SwaggerExample.ps1
    ```

## 📝 Fluent API (Recommended)

The new fluent DSL provides clean, chainable endpoint configuration:

```pascal
uses
  Dext.OpenAPI.Fluent;

SwaggerEndpoint.From(App.MapGet('/api/users/{id}', Handler))
  .Summary('Get user by ID')
  .Description('Retrieves user details by unique identifier')
  .Tag('Users')
  .Response(200, TypeInfo(TUser), 'User found')
  .Response(404, TypeInfo(TErrorResponse), 'User not found');
```

### Available Methods

| Method | Description |
|--------|-------------|
| `.Summary(...)` | Short description shown in Swagger UI header |
| `.Description(...)` | Detailed endpoint documentation |
| `.Tag(...)` | Group endpoints in Swagger UI |
| `.Tags([...])` | Multiple tags at once |
| `.Response(code, type, desc)` | Document response schemas |
| `.RequestType(type)` | Document request body schema |
| `.RequireAuthorization(scheme)` | Mark as protected endpoint |

## 📝 Traditional API (Alternative)

For explicit control, use `TEndpointMetadataExtensions`:

```pascal
uses
  Dext.OpenAPI.Extensions;

TEndpointMetadataExtensions.WithMetadata(
  App.MapGet('/api/users', Handler),
  'Get all users',
  'Retrieves all registered users',
  ['Users']);
TEndpointMetadataExtensions.WithResponse(App, 200, 'OK', TypeInfo(TUser));
```

## 📚 See Also

- [Swagger Documentation](../../Docs/swagger.md)
- [Swagger Attributes](../../Docs/swagger-attributes.md)
- [OpenAPI Security](../../Docs/openapi-security.md)
