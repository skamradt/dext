# Basic Authentication

A simple authentication scheme built into the HTTP protocol.

## Overview

Basic Authentication is the simplest way to protect your API. The client sends a header `Authorization: Basic <credentials>`, where credentials are a Base64-encoded string of `username:password`.

Dext provides a built-in middleware to handle Basic Auth with a simple validator function.

## Configuration

In your `Startup` or application configuration:

```pascal
App.Builder.UseBasicAuthentication(
  'My API Realm',
  function(const Username, Password: string): Boolean
  begin
    // Simple hardcoded validation
    Result := (Username = 'admin') and (Password = 'secret');
    
    // Or validate against a service/database
    // Result := UserServiceProvider.Validate(Username, Password);
  end);
```

## How it Works

1. When a client requests a protected resource without credentials, Dext returns a `401 Unauthorized` response with a `WWW-Authenticate` header.
2. The browser (or client) prompted for credentials.
3. Subsequent requests include the `Authorization` header.
4. If the validator returns `True`, the request proceeds and the `Ctx.User` is populated with a "Basic" identity.

## Protecting Endpoints

Just like JWT, you can use `.RequireAuthorization` to protect individual endpoints or controllers.

```pascal
App.MapGet('/api/private', procedure(Ctx: IHttpContext)
  begin
    Ctx.Response.Write('You accessed a private area!');
  end)
  .RequireAuthorization;
```

## When to Use Basic Auth?

- **Development/Internal Tools**: Quick to set up for internal dashboards.
- **Service-to-Service**: Simple to implement in scripts or between microservices.
- **Legacy Systems**: Broadly supported by almost every HTTP client.

> ⚠️ **Security Note**: Always use Basic Authentication over **HTTPS**. Since credentials are Base64 encoded (not encrypted), they can be easily intercepted over plain HTTP.

---

[← Authentication](README.md) | [Next: JWT Authentication →](jwt-auth.md)
