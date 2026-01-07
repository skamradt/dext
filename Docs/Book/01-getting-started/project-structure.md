# Project Structure

A typical Dext project follows this structure:

## Recommended Layout

```
MyProject/
├── MyProject.dpr              # Main program
├── MyProject.dproj            # Delphi project file
├── src/
│   ├── Controllers/           # Controller classes
│   │   └── UsersController.pas
│   ├── Entities/              # ORM entity classes
│   │   └── User.pas
│   ├── Services/              # Business logic
│   │   └── UserService.pas
│   └── Middleware/            # Custom middleware
│       └── LoggingMiddleware.pas
├── config/
│   └── appsettings.json       # Configuration
├── migrations/                # Database migrations
│   └── 001_CreateUsers.pas
└── tests/
    └── UserServiceTests.pas
```

## Main Program Template

```pascal
program MyProject;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Dext.Web,
  Dext.DependencyInjection,
  Dext.Entity,
  // Your units
  UsersController in 'src\Controllers\UsersController.pas',
  UserService in 'src\Services\UserService.pas',
  User in 'src\Entities\User.pas';

begin
  TWebHostBuilder.CreateDefault(nil)
    .UseUrls('http://localhost:5000')
    .ConfigureServices(procedure(Services: IServiceCollection)
      begin
        // Register services
        Services.AddScoped<IUserService, TUserService>;
        
        // Register DbContext
        Services.AddDbContext<TAppDbContext>;
      end)
    .Configure(procedure(App: IApplicationBuilder)
      begin
        // Middleware
        App.UseExceptionHandler;
        App.UseCors;
        
        // Controllers
        App.MapController<TUsersController>;
        
        // Or Minimal APIs
        App.MapGet('/health', procedure(Ctx: IHttpContext)
          begin
            Ctx.Response.Json('{"status": "healthy"}');
          end);
      end)
    .Build
    .Run;
end.
```

## Configuration File

`config/appsettings.json`:

```json
{
  "Database": {
    "Provider": "PostgreSQL",
    "ConnectionString": "Server=localhost;Database=myapp;User=postgres;Password=secret"
  },
  "Jwt": {
    "SecretKey": "your-secret-key-here",
    "ExpirationMinutes": 60
  }
}
```

Load it with:

```pascal
var
  Config: IConfiguration;
begin
  Config := TConfigurationBuilder.Create
    .AddJsonFile('config/appsettings.json')
    .Build;
    
  var DbConnection := Config.GetValue('Database:ConnectionString');
end;
```

---

[← Hello World](hello-world.md) | [Next: Web Framework →](../02-web-framework/README.md)
