# Application Startup

For professional projects, Dext recommends separating configuration from the main `.dpr` file using a **Startup Class**.

## Why use a Startup Class?

- **Clean Code**: Keeps the `.dpr` file minimal and focused only on starting the process.
- **Separation of Concerns**: Services and Middlewares are configured in a dedicated class.
- **Testability**: Easier to mock configurations during integration tests.
- **Maintainability**: Avoids "spaghetti" code in global blocks.

## The Startup Class Pattern

Create a new unit (e.g., `MyProject.Startup.pas`) implementing the `IStartup` interface:

```pascal
unit MyProject.Startup;

interface

uses
  Dext.Entity.Core,     // TDbContextOptions
  // Facades LAST
  Dext,                 // IConfiguration, TDextServices
  Dext.Web;             // IWebApplication, IStartup

type
  TStartup = class(TInterfacedObject, IStartup)
  public
    procedure ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
    procedure Configure(const App: IWebApplication);
  private
    procedure ConfigureDatabase(Options: TDbContextOptions);
  end;

implementation

uses
  MyProject.Data.Context,
  MyProject.Endpoints;

procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  Services
    .AddDbContext<TAppDbContext>(ConfigureDatabase)
    .AddScoped<IUserService, TUserService>
    .AddControllers;
end;

procedure TStartup.Configure(const App: IWebApplication);
begin
  // Global JSON settings
  JsonDefaultSettings(JsonSettings.CamelCase.CaseInsensitive.ISODateFormat);

  App.Builder
    // 1. Exception Handler (always first)
    .UseExceptionHandler
    // 2. HTTP Logging
    .UseHttpLogging
    // 3. CORS
    .UseCors(CorsOptions.AllowAnyOrigin.AllowAnyMethod.AllowAnyHeader)
    // 4. Map Endpoints (Minimal APIs)
    .MapEndpoints(TMyEndpoints.MapEndpoints)
    // 5. Map Controllers (if using)
    .MapControllers
    // 6. Swagger (AFTER routes are mapped)
    .UseSwagger(Swagger.Title('My API').Version('v1'));
end;

procedure TStartup.ConfigureDatabase(Options: TDbContextOptions);
begin
  Options
    .UseSQLite('App.db')
    .WithPooling(True); // REQUIRED for production Web APIs
end;

end.
```

> [!IMPORTANT]
> **Key Points**:
> - `TDextServices` is a **Record** — never call `.Free` on it.
> - `ConfigureServices` signature: `(const Services: TDextServices; const Configuration: IConfiguration)`.
> - `Configure` receives `IWebApplication` not `IApplicationBuilder`.
> - Use `App.Builder` for the **fluent** middleware pipeline.
> - Always separate database configuration into a private method (`ConfigureDatabase`).

## Main Program (.dpr)

With the Startup class, your main file becomes extremely clean:

```pascal
program Web.MyProject;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  Dext,
  Dext.Web,
  MyProject.Startup in 'MyProject.Startup.pas';

var
  App: IWebApplication;
  Provider: IServiceProvider;
begin
  SetConsoleCharSet;
  try
    App := WebApplication;
    App.UseStartup(TStartup.Create);
    Provider := App.BuildServices;
    App.Run(9000);
  except
    on E: Exception do
      WriteLn('Fatal: ' + E.Message);
  end;
  ConsolePause;
end.
```

> [!WARNING]
> **Common Mistakes**:
> - ❌ `var App := WebApplication;` — The compiler may infer the concrete class instead of the interface, causing ARC issues on shutdown.
> - ❌ Declaring `Provider: IServiceProvider` without `Dext` in uses → `E2003 Undeclared identifier`.
> - ❌ Forgetting `SetConsoleCharSet` → broken UTF-8 output.

## Fluent Configuration (Mandatory Pattern)

Both service registration and middleware pipeline **MUST** use fluent (chained) calls:

```pascal
// ✅ CORRECT: Fluent chaining
Services
  .AddDbContext<TMyContext>(ConfigureDatabase)
  .AddScoped<IUserService, TUserService>
  .AddSingleton<ICache, TMemoryCache>;

App.Builder
  .UseExceptionHandler
  .UseHttpLogging
  .UseCors(CorsOptions.AllowAnyOrigin)
  .MapEndpoints(TMyEndpoints.MapEndpoints)
  .UseSwagger(Swagger.Title('My API').Version('v1'));

// ❌ WRONG: Intermediate variables
var Builder := App.Builder;        // Don't do this
Builder.UseExceptionHandler;       // Breaks fluent pattern
Builder.UseHttpLogging;
```

## Execution Models: Run vs Start

| Method | Behavior | Use Case |
| :--- | :--- | :--- |
| **`Run`** | Blocks the calling thread until stopped (Ctrl+C). | **Console Apps**, Services, Daemons. |
| **`Start`** | Non-blocking. Starts the server and returns immediately. | **GUI Apps (VCL/FMX)** like System Tray tools. |

### Example: GUI Application (Sidecar)

In a VCL application, you must use `Start` to avoid freezing the main form:

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  FApp := WebApplication;
  FApp.UseStartup(TStartup.Create);
  FProvider := FApp.BuildServices;
  FApp.Start;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FApp.Stop; // Graceful shutdown
end;
```

## Database Seeding

Seed data in the main `.dpr` **before** `App.Run`:

```pascal
var
  App: IWebApplication;
  Provider: IServiceProvider;
begin
  SetConsoleCharSet;
  try
    App := WebApplication;
    App.UseStartup(TStartup.Create);
    Provider := App.BuildServices;

    // Seed BEFORE running
    TDbSeeder.Seed(Provider);

    App.Run(9000);
  except
    on E: Exception do
      WriteLn('Fatal: ' + E.Message);
  end;
  ConsolePause;
end.
```

### Seeder Pattern

```pascal
class procedure TDbSeeder.Seed(const Provider: IServiceProvider);
begin
  var Scope := Provider.CreateScope;
  try
    var Db := Scope.ServiceProvider.GetService(TAppDbContext) as TAppDbContext;

    if Db.EnsureCreated then // Returns True if schema was created
    begin
      // Use .Any to check existence without loading all records
      if not Db.Users.QueryAll.Any then
      begin
        var Admin := TUser.Create;
        Admin.Name := 'Admin';
        Db.Users.Add(Admin);
        Db.SaveChanges;
        // SaveChanges auto-populates Admin.Id (AutoInc)
      end;
    end;
  finally
    Scope := nil; // Disposes all scoped services
  end;
end;
```

> [!WARNING]
> **SQLite :memory: Warning**: NEVER call `BuildServiceProvider` manually inside the Seeder. This creates a *new* container and a *new* empty in-memory database.

## Advanced: Separating Concerns (Endpoints & Auth)

For larger projects, separate your code into dedicated modules:

### Creating an Endpoints Module

```pascal
unit MyProject.Endpoints;

interface

uses
  Dext.Web; // TAppBuilder, IResult, Results

type
  TMyEndpoints = class
  public
    class procedure MapEndpoints(const Builder: TAppBuilder); static;
  end;

implementation

uses
  MyProject.Auth,
  MyProject.Data.Context;

class procedure TMyEndpoints.MapEndpoints(const Builder: TAppBuilder);
begin
  // Health Check
  Builder.MapGet<IResult>('/health',
    function: IResult
    begin
      Result := Results.Ok('healthy');
    end);

  // Auth - Login with DI + Model Binding
  Builder.MapPost<TLoginRequest, IAuthService, IResult>('/api/auth/login',
    function(Req: TLoginRequest; Auth: IAuthService): IResult
    begin
      Result := Results.Ok(Auth.Login(Req));
    end);
end;

end.
```

> [!IMPORTANT]
> The `MapEndpoints` parameter type is `TAppBuilder` (from `Dext.Web`), **not** `IApplicationBuilder`.

### Creating an Auth Module

```pascal
unit MyProject.Auth;

interface

uses
  Dext.Web, Dext.Auth.JWT;

type
  TLoginRequest = record
    username: string;
    password: string;
  end;

  TLoginResponse = record
    token: string;
  end;

  IAuthService = interface
    ['{...}']
    function Login(const Req: TLoginRequest): TLoginResponse;
  end;

  TAuthConfig = class
  public
    const JWT_SECRET = 'your-secret-key-here-minimum-32-chars';
    const JWT_ISSUER = 'MyApp';
  end;
```

### Benefits

- **Single Responsibility**: Each file has one clear purpose
- **Testability**: Endpoints can be tested independently
- **Scalability**: Easy to add new endpoint groups
- **Maintainability**: Configuration changes don't touch business logic

> 📦 **References**: 
> - [Web.EventHub](../../../Examples/Web.EventHub/) - Modern 2026 patterns
> - [Web.TicketSales](../../../Examples/Web.TicketSales/) - Gold Standard (Controllers + JWT + ORM)
> - [Web.SalesSystem](../../../Examples/Web.SalesSystem/) - Minimal APIs + CQRS

---

[← Project Structure](project-structure.md) | [Next: Web Framework →](../02-web-framework/README.md)
