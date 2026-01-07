# 4. API Features

Essential features for building production-ready APIs.

## Chapters

1. [OpenAPI / Swagger](openapi-swagger.md) - Auto-generated documentation
2. [Rate Limiting](rate-limiting.md) - Request throttling
3. [CORS](cors.md) - Cross-origin resource sharing
4. [Response Caching](cache.md) - Cache headers & strategies
5. [Health Checks](health-checks.md) - Monitoring endpoints

## Quick Examples

### Swagger

```pascal
App.UseSwagger;
App.UseSwaggerUI;
// Visit: /swagger
```

### Rate Limiting

```pascal
App.UseRateLimiting(
  TRateLimitOptions.Create
    .Limit(100)
    .PerMinute
);
```

### CORS

```pascal
App.UseCors(
  TCorsOptions.Create
    .AllowOrigin('https://myapp.com')
    .AllowMethods(['GET', 'POST'])
);
```

### Health Check

```pascal
App.MapGet('/health', procedure(Ctx: IHttpContext)
  begin
    Ctx.Response.Json('{"status": "healthy"}');
  end);
```

---

[← Authentication](../03-authentication/README.md) | [Next: OpenAPI/Swagger →](openapi-swagger.md)
