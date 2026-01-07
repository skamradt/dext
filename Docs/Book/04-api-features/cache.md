# Response Caching

Optimize your API performance with HTTP-level caching.

## [ResponseCache] Attribute

In Controller style, you can use the attribute to set cache headers automatically:

```pascal
type
  TProductsController = class(TController)
  public
    [HttpGet]
    [ResponseCache(Duration := 60)] // Cache for 60 seconds
    function GetAll: IActionResult;
  end;
```

## Using in Minimal APIs

```pascal
App.MapGet('/api/news', Handler)
  .Cache(60); // Cache for 60 seconds
```

## Generated Headers

Dext will add the following headers to the response:
- `Cache-Control: public, max-age=60`
- `Expires: <Current Date + 60s>`

## Cache Profiles

You can define global profiles during setup in `Startup`:

```pascal
Services.AddResponseCaching(procedure(Options: TResponseCachingOptions)
  begin
    Options.AddProfile('Default', procedure(P: TCacheProfile)
      begin
        P.Duration := 300;
        P.Location := clAny;
      end);
  end);

// Usage
[ResponseCache(Profile := 'Default')]
```

## Benefits of Caching

- **Reduced Load**: Fewer requests reach the database or business logic.
- **Speed**: Browsers and Proxies return data instantly without consulting the server.
- **Savings**: Lower bandwidth and CPU consumption.

---

[← CORS](cors.md) | [Next: Health Checks →](health-checks.md)
