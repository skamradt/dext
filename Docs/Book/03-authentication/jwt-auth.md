# JWT Authentication

Secure your API with JSON Web Tokens.

> 📦 **Example**: [Web.JwtAuthDemo](../../../Examples/Web.JwtAuthDemo/)

## Overview

JWT authentication flow:
1. User sends credentials to `/login`
2. Server validates and returns a JWT token
3. Client includes token in `Authorization: Bearer <token>` header
4. Server validates token on protected endpoints

## Setup

### 1. Configure Authentication

```pascal
TWebHostBuilder.CreateDefault(nil)
  .ConfigureServices(procedure(Services: IServiceCollection)
    begin
      Services.AddAuthentication(procedure(Options: TAuthenticationOptions)
        begin
          Options.SecretKey := 'your-secret-key-must-be-at-least-32-characters';
          Options.Issuer := 'your-app';
          Options.Audience := 'your-api';
          Options.ExpirationMinutes := 60;
        end);
    end)
  .Configure(procedure(App: IApplicationBuilder)
    begin
      App.UseAuthentication;
      // ... endpoints
    end)
  .Build
  .Run;
```

### 2. Create Login Endpoint

```pascal
type
  TLoginRequest = record
    Username: string;
    Password: string;
  end;

App.MapPost('/login', procedure(Ctx: IHttpContext)
  var
    Request: TLoginRequest;
    Token: string;
    Claims: TArray<TClaim>;
  begin
    Request := Ctx.Request.BindBody<TLoginRequest>;
    
    // Validate credentials (replace with real validation)
    if (Request.Username <> 'admin') or (Request.Password <> 'secret') then
    begin
      Results.Unauthorized.Execute(Ctx);
      Exit;
    end;
    
    // Build claims
    Claims := TClaimsBuilder.Create
      .AddSub('user-123')
      .AddName('Admin User')
      .AddEmail('admin@example.com')
      .AddRole('admin')
      .AddClaim('department', 'IT')
      .Build;
    
    // Generate token
    Token := TJwtHelper.GenerateToken(
      'your-secret-key-must-be-at-least-32-characters',
      Claims,
      60  // expiration in minutes
    );
    
    Ctx.Response.Json('{"token": "' + Token + '"}');
  end);
```

### 3. Protect Endpoints

```pascal
// Require authentication
App.MapGet('/protected', procedure(Ctx: IHttpContext)
  begin
    var UserId := Ctx.User.FindFirst('sub');
    var UserName := Ctx.User.FindFirst('name');
    
    Ctx.Response.Json(Format(
      '{"message": "Hello %s!", "userId": "%s"}',
      [UserName, UserId]
    ));
  end)
  .RequireAuthorization;

// Require specific role
App.MapGet('/admin', procedure(Ctx: IHttpContext)
  begin
    Ctx.Response.Json('{"message": "Admin area"}');
  end)
  .RequireRole('admin');
```

## Claims Builder

The `TClaimsBuilder` provides a fluent API for building JWT claims:

```pascal
var Claims := TClaimsBuilder.Create
  .AddSub('user-id')           // Subject (user ID)
  .AddName('John Doe')          // Full name
  .AddEmail('john@example.com') // Email
  .AddRole('user')              // Single role
  .AddRoles(['admin', 'editor']) // Multiple roles
  .AddClaim('custom', 'value')  // Custom claim
  .Build;
```

## Accessing Claims

In protected endpoints, access claims via `Ctx.User`:

```pascal
// Get single claim
var UserId := Ctx.User.FindFirst('sub');
var Email := Ctx.User.FindFirst('email');

// Check role
if Ctx.User.IsInRole('admin') then
  // Admin logic

// Get all claims of a type
var Roles := Ctx.User.FindAll('role');
```

## Token Validation

Tokens are automatically validated by the `UseAuthentication` middleware:

- ✅ Signature verification
- ✅ Expiration check
- ✅ Issuer validation (if configured)
- ✅ Audience validation (if configured)

---

[← Authentication](README.md) | [Next: Claims Builder →](claims-builder.md)
