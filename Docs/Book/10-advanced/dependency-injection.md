# Dependency Injection

Dext provides a full-featured DI container with constructor injection, scopes, and lifetime management.

## Service Registration

Register services in `ConfigureServices` using the fluent `TDextServices` record:

```pascal
procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  Services
    // Interface to implementation
    .AddScoped<IUserService, TUserService>
    .AddSingleton<ILogger, TConsoleLogger>
    .AddTransient<IValidator, TValidator>
    // Database context
    .AddDbContext<TAppDbContext>(ConfigureDatabase)
    // Controllers
    .AddControllers;
end;
```

> [!IMPORTANT]
> `TDextServices` is a **Record** — never call `.Free` on it. It's managed by the stack.

## Service Lifetimes

| Lifetime | Behavior | Use Case |
|----------|----------|----------|
| **Singleton** | One instance for entire app lifetime | Loggers, Configuration, Caches |
| **Scoped** | One instance per HTTP request | DbContext, User session |
| **Transient** | New instance every time | Validators, Factories |

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

Just register all services:

```pascal
Services
  .AddScoped<IUserRepository, TUserRepository>
  .AddSingleton<ILogger, TConsoleLogger>
  .AddScoped<IUserService, TUserService>;  // Auto-injects dependencies!
```

## Factory Registration

For services that need custom initialization:

```pascal
Services.AddSingleton<IJwtTokenHandler, TJwtTokenHandler>(
  function(Provider: IServiceProvider): TObject
  begin
    Result := TJwtTokenHandler.Create(JWT_SECRET, JWT_ISSUER, JWT_AUDIENCE, JWT_EXPIRATION);
  end);
```

> [!WARNING]
> Use the explicit two-type-parameter form to avoid ambiguity:  
> ✅ `Services.AddSingleton<IAuthService, TAuthService>(FactoryFunc)`  
> ❌ `Services.AddSingleton<IAuthService>(FactoryFunc)` — May fail with E2250/E2003

## Resolving Services

### In Minimal APIs (Recommended: Generic Injection)

```pascal
// ✅ Services are auto-injected via generic overloads
Builder.MapGet<IUserService, IResult>('/api/users',
  function(Svc: IUserService): IResult
  begin
    Result := Results.Ok(Svc.GetAll);
  end);
```

> [!WARNING]
> ⛔ **NEVER** resolve services manually in Minimal APIs:  
> ❌ `var Service := Ctx.RequestServices.GetService<IUserService>;`

### In Controllers (Constructor Injection)

```pascal
type
  [ApiController('/api/users')]
  TUsersController = class
  private
    FUserService: IUserService;
  public
    constructor Create(UserService: IUserService);  // Auto-injected
  end;
```

### Manual Resolution (Seeders, Background Tasks)

```pascal
var Scope := Provider.CreateScope;
try
  var Db := Scope.ServiceProvider.GetService(TAppDbContext) as TAppDbContext;
  // Use service...
finally
  Scope := nil;  // Disposes all scoped services
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

Separate database configuration into a private method:

```pascal
procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  Services
    .AddDbContext<TAppDbContext>(ConfigureDatabase)
    .AddScoped<IUserService, TUserService>;
end;

procedure TStartup.ConfigureDatabase(Options: TDbContextOptions);
begin
  Options
    .UseSQLite('App.db')
    .WithPooling(True);  // REQUIRED for production Web APIs
end;
```

> [!WARNING]
> **Connection Pooling**: Web APIs are multithreaded by nature. **ALWAYS** enable pooling in production to avoid connection exhaustion.

## Service Scope

Create child scopes for background processing or database seeding:

```pascal
var Scope := Provider.CreateScope;
try
  var Service := Scope.ServiceProvider.GetService(TAppDbContext) as TAppDbContext;
  // Use service...
finally
  Scope := nil;  // Scope disposes all scoped services
end;
```

## Best Practices

1. **Prefer constructor injection** over service locator
2. **Use interfaces** for testability
3. **Scoped for DbContext** — one per request
4. **Singleton for stateless** services (loggers, config)
5. **Enable Connection Pooling** for all Web APIs
6. **Use fluent chaining** for service registration
7. **Avoid captive dependencies** — don't inject scoped into singleton

---

[← Advanced Topics](README.md) | [Next: Background Services →](background-services.md)
