# Rate Limiting

Protect your API from abuse with request throttling.

> 📦 **Example**: [Web.RateLimitDemo](../../../Examples/Web.RateLimitDemo/)

## Quick Setup

```pascal
App.Configure(procedure(App: IApplicationBuilder)
  begin
    App.UseRateLimiting(
      TRateLimitOptions.Create
        .Limit(100)
        .PerMinute
    );
    
    // Endpoints...
  end);
```

## Configuration Options

### Fixed Window

```pascal
TRateLimitOptions.Create
  .Limit(100)        // 100 requests
  .PerMinute         // per minute
  
TRateLimitOptions.Create
  .Limit(1000)
  .PerHour
  
TRateLimitOptions.Create
  .Limit(10000)
  .PerDay
```

### Token Bucket

```pascal
TRateLimitOptions.Create
  .TokenBucket
  .BucketSize(100)      // Max burst
  .RefillRate(10)       // 10 tokens per second
```

### By Key

```pascal
// By IP (default)
TRateLimitOptions.Create
  .ByIP
  .Limit(100).PerMinute

// By User ID
TRateLimitOptions.Create
  .ByUser
  .Limit(1000).PerHour

// By API Key
TRateLimitOptions.Create
  .ByHeader('X-API-Key')
  .Limit(5000).PerDay

// Custom key extractor
TRateLimitOptions.Create
  .ByKey(function(Ctx: IHttpContext): string
    begin
      Result := Ctx.Request.QueryParam('tenant');
    end)
  .Limit(100).PerMinute
```

## Per-Endpoint Limits

```pascal
// Global limit
App.UseRateLimiting(GlobalOptions);

// Override for specific endpoint
App.MapPost('/api/expensive-operation', Handler)
  .RateLimit(
    TRateLimitOptions.Create
      .Limit(10)
      .PerMinute
  );
```

## Response Headers

Rate limit info is included in response headers:

```
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 95
X-RateLimit-Reset: 1704643200
```

## Exceeded Response

When limit is exceeded, returns `429 Too Many Requests`:

```json
{
  "error": "Rate limit exceeded",
  "retryAfter": 30
}
```

---

[← OpenAPI/Swagger](openapi-swagger.md) | [Next: CORS →](cors.md)
