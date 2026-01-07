# 10. Advanced Topics

Deep dives into infrastructure and advanced patterns.

## Chapters

1. [Dependency Injection](dependency-injection.md) - Service registration & lifetimes
2. [Background Services](background-services.md) - Hosted services
3. [Configuration](configuration.md) - `IOptions<T>` pattern
4. [Async API](async-api.md) - `TAsyncTask` and concurrency

## Quick Examples

### Dependency Injection

```pascal
// Lifetime options
Services.AddSingleton<ILogger, TConsoleLogger>;    // One instance
Services.AddScoped<IUserService, TUserService>;     // Per request
Services.AddTransient<IValidator, TValidator>;       // New each time
```

### Background Services

```pascal
type
  TCleanupService = class(TInterfacedObject, IHostedService)
  public
    procedure StartAsync(CancellationToken: ICancellationToken);
    procedure StopAsync(CancellationToken: ICancellationToken);
  end;

Services.AddHostedService<TCleanupService>;
```

### Configuration

```pascal
type
  TDatabaseOptions = class
    ConnectionString: string;
    MaxPoolSize: Integer;
  end;

Services.Configure<TDatabaseOptions>(Config.GetSection('Database'));

// Usage
procedure DoWork(Options: IOptions<TDatabaseOptions>);
begin
  var ConnStr := Options.Value.ConnectionString;
end;
```

### Async Tasks

```pascal
TAsyncTask.Run(procedure
  begin
    // Background work
    Sleep(5000);
    Log('Done!');
  end);
```

---

[← CLI](../09-cli/README.md) | [Appendix →](../appendix/type-system.md)
