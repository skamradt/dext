# Dependency Injection

Dext provides a full-featured DI container with constructor injection, scopes, and lifetime management.

## Service Registration

Register services in `ConfigureServices`:

```pascal
TWebHostBuilder.CreateDefault(nil)
  .ConfigureServices(procedure(Services: IServiceCollection)
    begin
      // Interface to implementation
      Services.AddScoped<IUserService, TUserService>;
      Services.AddSingleton<ILogger, TConsoleLogger>;
      Services.AddTransient<IValidator, TValidator>;
      
      // Implementation only (use class directly)
      Services.AddScoped<TUserRepository>;
      
      // Factory function
      Services.AddSingleton<IDbConnection>(
        function(Provider: IServiceProvider): TObject
        begin
          Result := TFireDACConnection.Create(FDConn);
        end);
    end)
  .Configure(...)
  .Build
  .Run;
```

## Service Lifetimes

| Lifetime | Behavior | Use Case |
|----------|----------|----------|
| **Singleton** | One instance for entire app lifetime | Loggers, Configuration, Caches |
| **Scoped** | One instance per HTTP request | DbContext, User session |
| **Transient** | New instance every time | Validators, Factories |

```pascal
Services.AddSingleton<ILogger, TConsoleLogger>;   // Created once
Services.AddScoped<IDbContext, TAppDbContext>;     // Per request
Services.AddTransient<IValidator, TValidator>;     // Always new
```

## Constructor Injection

Services are automatically injected via constructor:

```pascal
type
  TUserService = class(TInterfacedObject, IUserService)
  private
    FRepository: IUserRepository;
    FLogger: ILogger;
  public
    constructor Create(Repository: IUserRepository; Logger: ILogger);
  end;

constructor TUserService.Create(Repository: IUserRepository; Logger: ILogger);
begin
  FRepository := Repository;
  FLogger := Logger;
end;
```

Just register both services:

```pascal
Services.AddScoped<IUserRepository, TUserRepository>;
Services.AddScoped<ILogger, TConsoleLogger>;
Services.AddScoped<IUserService, TUserService>;  // Auto-injects dependencies!
```

## Resolving Services

### In Minimal APIs

```pascal
App.MapGet('/users', procedure(Ctx: IHttpContext)
  var
    UserService: IUserService;
  begin
    UserService := Ctx.Services.GetRequiredService<IUserService>;
    Ctx.Response.Json(UserService.GetAll);
  end);
```

### In Controllers

```pascal
type
  TUsersController = class(TController)
  private
    FUserService: IUserService;
  public
    constructor Create(UserService: IUserService);  // Auto-injected
  end;
```

### Manual Resolution

```pascal
var
  Provider: IServiceProvider;
  Service: IUserService;
begin
  Service := Provider.GetRequiredService<IUserService>;
  // or
  Service := Provider.GetService<IUserService>;  // Returns nil if not found
end;
```

## Custom Constructor

Use `[ServiceConstructor]` when a class has multiple constructors:

```pascal
type
  TUserService = class(TInterfacedObject, IUserService)
  public
    constructor Create; overload;
    
    [ServiceConstructor]  // DI uses this one
    constructor Create(Repo: IUserRepository; Logger: ILogger); overload;
  end;
```

## Registering DbContext

```pascal
Services.AddDbContext<TAppDbContext>(
  function(Provider: IServiceProvider): TAppDbContext
  var
    Connection: IDbConnection;
    Dialect: ISQLDialect;
  begin
    Connection := Provider.GetRequiredService<IDbConnection>;
    Dialect := Provider.GetRequiredService<ISQLDialect>;
    Result := TAppDbContext.Create(Connection, Dialect);
  end);
```

## Service Scope

Create child scopes for background processing:

```pascal
var
  ScopeFactory: IServiceScopeFactory;
  Scope: IServiceScope;
begin
  ScopeFactory := Provider.GetRequiredService<IServiceScopeFactory>;
  Scope := ScopeFactory.CreateScope;
  try
    var Service := Scope.ServiceProvider.GetRequiredService<IUserService>;
    // Use service...
  finally
    // Scope disposes all scoped services
  end;
end;
```

## Best Practices

1. **Prefer constructor injection** over service locator
2. **Use interfaces** for testability
3. **Scoped for DbContext** - one per request
4. **Singleton for stateless** services (loggers, config)
5. **Avoid captive dependencies** - don't inject scoped into singleton

---

[← Advanced Topics](README.md) | [Next: Background Services →](background-services.md)
