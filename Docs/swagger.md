# Dext Swagger/OpenAPI Integration

## Overview

Dext Framework now includes full support for **OpenAPI 3.0** specification and **Swagger UI**, allowing you to automatically generate interactive API documentation for your endpoints.

## Features

- ✅ **Automatic OpenAPI 3.0 Generation**: Converts your registered endpoints into OpenAPI specification
- ✅ **Swagger UI Integration**: Beautiful, interactive API documentation served at `/swagger`
- ✅ **Fluent Metadata API**: Add summaries, descriptions, and tags to your endpoints
- ✅ **Method-Aware Routing**: Full support for GET, POST, PUT, DELETE, PATCH
- ✅ **Path Parameters**: Automatic detection of route parameters (e.g., `/users/{id}`)
- ✅ **Customizable**: Configure API info, servers, contact, and license information

## Quick Start

### 1. Basic Setup

```pascal
program SwaggerExample;

uses
  Dext.Core.WebApplication,
  Dext.Core.ApplicationBuilder.Extensions,
  Dext.Swagger.Middleware,
  Dext.OpenAPI.Extensions,
  Dext.OpenAPI.Generator;

var
  App: IWebApplication;
  Options: TOpenAPIOptions;
begin
  App := TWebApplication.Create;
  
  // Configure OpenAPI options
  Options := TOpenAPIOptions.Default;
  Options.Title := 'My Awesome API';
  Options.Description := 'A comprehensive API built with Dext Framework';
  Options.Version := '1.0.0';
  Options.ContactName := 'Your Name';
  Options.ContactEmail := 'your.email@example.com';
  
  // Add Swagger middleware
  TSwaggerExtensions.UseSwagger(App.GetApplicationBuilder, Options);
  
  // Register your endpoints
  App.GetApplicationBuilder
    .MapGet('/api/users', 
      procedure(Ctx: IHttpContext)
      begin
        Ctx.Response.Json('{"users": []}');
      end);
  
  App.Run(8080);
end.
```

### 2. Adding Metadata to Endpoints (Fluent API - Recommended)

Use the new fluent DSL for clean, chainable endpoint configuration:

```pascal
uses
  Dext.OpenAPI.Fluent;

// Simple endpoint with summary and tag
SwaggerEndpoint.From(App.MapGet('/api/users', GetUsersHandler))
  .Summary('Get all users')
  .Tag('Users');

// Full documentation with responses
SwaggerEndpoint.From(
  TApplicationBuilderExtensions.MapGet<Integer, IHttpContext>(App, '/api/users/{id}', GetUserHandler))
  .Summary('Get user by ID')
  .Description('Retrieves detailed information about a specific user')
  .Tag('Users')
  .Response(200, TypeInfo(TUser), 'User found')
  .Response(404, TypeInfo(TErrorResponse), 'User not found');

// POST with request type and multiple responses
SwaggerEndpoint.From(
  TApplicationBuilderExtensions.MapPost<TCreateUserRequest, IHttpContext>(App, '/api/users', CreateUserHandler))
  .Summary('Create a new user')
  .Description('Creates a new user account with the provided information')
  .Tag('Users')
  .RequestType(TypeInfo(TCreateUserRequest))
  .Response(201, TypeInfo(TUser), 'User created')
  .Response(400, TypeInfo(TErrorResponse), 'Invalid input');

// Protected endpoint
SwaggerEndpoint.From(App.MapGet('/api/admin/data', AdminHandler))
  .Summary('Get admin data')
  .Tag('Admin')
  .RequireAuthorization('bearerAuth');
```

### 3. Traditional API (Alternative)

For explicit control or backward compatibility, use `TEndpointMetadataExtensions`:

```pascal
uses
  Dext.OpenAPI.Extensions;

// Simple summary
TEndpointMetadataExtensions.WithSummary(
  App.GetApplicationBuilder.MapGet('/api/users', GetUsersHandler),
  'Get all users'
);

// Full metadata
TEndpointMetadataExtensions.WithMetadata(
  App.GetApplicationBuilder.MapPost('/api/users', CreateUserHandler),
  'Create a new user',
  'Creates a new user account with the provided information',
  ['Users', 'Authentication']
);

// Add response documentation
TEndpointMetadataExtensions.WithResponse(App, 200, 'OK', TypeInfo(TUser));
TEndpointMetadataExtensions.WithResponse(App, 400, 'Bad Request', TypeInfo(TErrorResponse));
```

### 4. Using Generic Handlers with Metadata

```pascal
type
  TCreateUserRequest = record
    Name: string;
    Email: string;
    Password: string;
  end;

  TUserResponse = record
    Id: Integer;
    Name: string;
    Email: string;
  end;

// Register endpoint with generic handler
TEndpointMetadataExtensions.WithMetadata(
  TApplicationBuilderExtensions.MapPost<TCreateUserRequest>(
    App.GetApplicationBuilder,
    '/api/users',
    procedure(Req: TCreateUserRequest; Ctx: IHttpContext)
    var
      Response: TUserResponse;
    begin
      // Create user logic
      Response.Id := 1;
      Response.Name := Req.Name;
      Response.Email := Req.Email;
      
      Ctx.Response.Json(TJson.Serialize<TUserResponse>(Response));
    end
  ),
  'Create User',
  'Creates a new user account with the provided credentials',
  ['Users', 'Authentication']
);
```

## Accessing Swagger UI

Once your application is running, you can access:

- **Swagger UI**: `http://localhost:8080/swagger`
- **OpenAPI JSON**: `http://localhost:8080/swagger.json`

## Advanced Configuration

### Custom Swagger Paths

```pascal
var
  Middleware: TSwaggerMiddleware;
begin
  Middleware := TSwaggerMiddleware.Create(
    Options,
    '/api-docs',      // Swagger UI path
    '/api-docs.json'  // OpenAPI JSON path
  );
  
  App.GetApplicationBuilder.UseMiddleware(TSwaggerMiddleware, TValue.From(Options));
end;
```

### Multiple Servers

```pascal
Options := TOpenAPIOptions.Default;
Options.Title := 'Multi-Environment API';
Options.ServerUrl := 'http://localhost:8080';
Options.ServerDescription := 'Development server';

// Note: Currently only one server is supported
// Multiple servers will be added in a future version
```

### License Information

```pascal
Options.LicenseName := 'Apache 2.0';
Options.LicenseUrl := 'https://www.apache.org/licenses/LICENSE-2.0';
```

## Architecture

### Components

1. **`Dext.OpenAPI.Types.pas`**: Type definitions for OpenAPI document structure
   - `TOpenAPIDocument`, `TOpenAPIOperation`, `TOpenAPISchema`, etc.

2. **`Dext.OpenAPI.Generator.pas`**: Converts endpoint metadata to OpenAPI JSON
   - `TOpenAPIGenerator`: Main generator class
   - `TOpenAPIOptions`: Configuration options

3. **`Dext.Swagger.Middleware.pas`**: Serves Swagger UI and OpenAPI spec
   - `TSwaggerMiddleware`: Handles `/swagger` and `/swagger.json` requests
   - `TSwaggerExtensions`: Fluent API for adding Swagger to your app

4. **`Dext.OpenAPI.Extensions.pas`**: Traditional API for endpoint metadata
   - `TEndpointMetadataExtensions`: Methods for adding metadata to routes

5. **`Dext.OpenAPI.Fluent.pas`**: Modern fluent DSL (Recommended)
   - `TEndpointBuilder`: Fluent record-based builder
   - `SwaggerEndpoint`: Factory for creating endpoint builders

### How It Works

1. **Route Registration**: When you call `MapGet`, `MapPost`, etc., a `TRouteDefinition` is created with basic metadata (method, path)

2. **Metadata Enhancement**: Use `TEndpointMetadataExtensions` to add summary, description, and tags

3. **OpenAPI Generation**: `TOpenAPIGenerator` reads all registered routes via `IApplicationBuilder.GetRoutes()` and generates an OpenAPI 3.0 document

4. **Serving Documentation**: `TSwaggerMiddleware` intercepts requests to `/swagger` and `/swagger.json`, serving the UI and spec respectively

## Best Practices

### 1. Organize with Tags

```pascal
// Group related endpoints with tags using fluent API
SwaggerEndpoint.From(App.MapGet('/api/users', GetUsersHandler))
  .Summary('List users')
  .Tag('Users');

SwaggerEndpoint.From(App.MapPost('/api/users', CreateUserHandler))
  .Summary('Create user')
  .Tag('Users');

SwaggerEndpoint.From(App.MapGet('/api/products', GetProductsHandler))
  .Summary('List products')
  .Tag('Products');
```

### 2. Provide Meaningful Descriptions

```pascal
SwaggerEndpoint.From(
  TApplicationBuilderExtensions.MapGet<Integer, IHttpContext>(App, '/api/users/{id}', GetUserByIdHandler))
  .Summary('Get User by ID')
  .Description('Retrieves detailed information about a specific user by their unique identifier. ' +
    'Returns 404 if the user is not found.')
  .Tag('Users')
  .Response(200, TypeInfo(TUser))
  .Response(404, TypeInfo(TErrorResponse));
```

### 3. Use Consistent Naming

```pascal
// Good: RESTful naming
.MapGet('/api/users')        // Get all users
.MapGet('/api/users/{id}')   // Get user by ID
.MapPost('/api/users')       // Create user
.MapPut('/api/users/{id}')   // Update user
.MapDelete('/api/users/{id}') // Delete user

// Avoid: Inconsistent naming
.MapGet('/getUsers')
.MapPost('/createNewUser')
```

## Limitations & Future Enhancements

### Current Limitations

- Schema introspection for request/response bodies is basic
- Only one server configuration supported
- No support for authentication schemes in OpenAPI spec (yet)
- No support for examples in schemas

### Planned Features

- [ ] Full RTTI-based schema generation for records and classes
- [ ] Support for multiple servers
- [ ] Authentication/Authorization scheme documentation
- [ ] Request/Response examples
- [ ] Custom schema annotations via attributes
- [ ] Support for file uploads
- [ ] Webhook documentation

## Troubleshooting

### Swagger UI shows "Failed to load API definition"

**Cause**: The OpenAPI JSON endpoint is not accessible or returning invalid JSON.

**Solution**: 
1. Check that `/swagger.json` returns valid JSON
2. Verify CORS is configured if accessing from a different origin
3. Check browser console for detailed error messages

### Endpoints not appearing in Swagger

**Cause**: Routes registered after `UseSwagger` middleware.

**Solution**: Ensure `UseSwagger` is called **before** registering your endpoints:

```pascal
// ✅ Correct order
App.GetApplicationBuilder.UseSwagger(Options);
App.GetApplicationBuilder.MapGet('/api/users', Handler);

// ❌ Wrong order
App.GetApplicationBuilder.MapGet('/api/users', Handler);
App.GetApplicationBuilder.UseSwagger(Options); // Too late!
```

### Metadata not updating

**Cause**: Metadata extensions called on wrong builder instance.

**Solution**: Use the same builder instance:

```pascal
var Builder := App.GetApplicationBuilder;
TEndpointMetadataExtensions.WithSummary(
  Builder.MapGet('/api/users', Handler),
  'Get all users'
);
```

## Examples

See the `/Examples` directory for complete working examples:

- `SwaggerBasic.dpr`: Basic Swagger setup
- `SwaggerAdvanced.dpr`: Advanced usage with metadata and tags
- `SwaggerGeneric.dpr`: Using generic handlers with Swagger

## Contributing

Contributions are welcome! If you'd like to improve the Swagger/OpenAPI integration:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Submit a pull request

## License

This feature is part of the Dext Framework and is licensed under the MIT License.
