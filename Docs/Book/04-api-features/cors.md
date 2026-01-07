# CORS

Configure Cross-Origin Resource Sharing for browser clients.

## Quick Setup

```pascal
App.Configure(procedure(App: IApplicationBuilder)
  begin
    App.UseCors(
      TCorsOptions.Create
        .AllowAnyOrigin
        .AllowAnyMethod
        .AllowAnyHeader
    );
    
    // Endpoints...
  end);
```

> ⚠️ `AllowAnyOrigin` is for development only. In production, specify allowed origins.

## Production Configuration

```pascal
App.UseCors(
  TCorsOptions.Create
    .AllowOrigins(['https://myapp.com', 'https://admin.myapp.com'])
    .AllowMethods(['GET', 'POST', 'PUT', 'DELETE'])
    .AllowHeaders(['Content-Type', 'Authorization'])
    .AllowCredentials
    .MaxAge(3600)  // Preflight cache: 1 hour
);
```

## Options Reference

| Method | Description |
|--------|-------------|
| `.AllowAnyOrigin` | Allow all origins (dev only) |
| `.AllowOrigins([...])` | Allow specific origins |
| `.AllowAnyMethod` | Allow all HTTP methods |
| `.AllowMethods([...])` | Allow specific methods |
| `.AllowAnyHeader` | Allow all headers |
| `.AllowHeaders([...])` | Allow specific headers |
| `.AllowCredentials` | Allow cookies/auth headers |
| `.ExposeHeaders([...])` | Headers client can read |
| `.MaxAge(seconds)` | Preflight response cache |

## Per-Endpoint CORS

Override global CORS for specific endpoints:

```pascal
// Disable CORS for this endpoint
App.MapGet('/internal', Handler)
  .DisableCors;

// Different CORS policy
App.MapGet('/public-api', Handler)
  .Cors(
    TCorsOptions.Create
      .AllowAnyOrigin
      .AllowMethods(['GET'])
  );
```

## Preflight Requests

Browsers send `OPTIONS` requests for "non-simple" requests. Dext handles these automatically when CORS is enabled.

A request is "non-simple" if it:
- Uses methods other than GET, HEAD, POST
- Has custom headers
- Uses Content-Type other than form-data, text/plain, application/x-www-form-urlencoded

## Common Issues

### "No 'Access-Control-Allow-Origin' header"

CORS middleware must be **before** your endpoints:

```pascal
// ✅ Correct order
App.UseCors(CorsOptions);
App.MapGet('/api', Handler);

// ❌ Wrong order
App.MapGet('/api', Handler);
App.UseCors(CorsOptions);  // Too late!
```

### Credentials with wildcard origin

```pascal
// ❌ This fails
TCorsOptions.Create
  .AllowAnyOrigin
  .AllowCredentials;  // Can't use both!

// ✅ Use specific origins
TCorsOptions.Create
  .AllowOrigins(['https://myapp.com'])
  .AllowCredentials;
```

---

[← Rate Limiting](rate-limiting.md) | [Next: Caching →](caching.md)
