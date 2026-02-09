# Application Startup

For professional projects, Dext recommends separating configuration from the main `.dpr` file using a **Startup Class**.

## Why use a Startup Class?

- **Clean Code**: Keeps the `.dpr` file minimal and focused only on starting the process.
- **Separation of Concerns**: Services and Middlewares are configured in a dedicated class.
- **Testability**: Easier to mock configurations during integration tests.
- **Maintainability**: Avoids "spaghetti" code in global blocks.

## The Startup Class Pattern

Create a new unit (e.g., `App.Startup.pas`) implementing the `IStartup` interface:

```pascal
unit App.Startup;

interface

uses
  Dext.Web, Dext.DependencyInjection;

type
  TStartup = class(TInterfacedObject, IStartup)
  public
    procedure ConfigureServices(const Services: IServiceCollection; const Configuration: IConfiguration);
    procedure Configure(const App: IApplicationBuilder);
  end;

implementation

procedure TStartup.ConfigureServices(const Services: IServiceCollection; const Configuration: IConfiguration);
begin
  // 1. Register your business services
  Services.AddScoped<IUserService, TUserService>;
  
  // 2. Configure Database
  Services.AddDbContext<TAppDbContext>(procedure(Options: TDbContextOptions)
    begin
      Options.UsePostgreSQL(Configuration.GetValue('ConnectionStrings:Default'));
    end);
end;

procedure TStartup.Configure(const App: IApplicationBuilder);
begin
  // 3. Configure Middleware Pipeline
  App.UseExceptionHandler;
  App.UseCors;
  App.UseAuthentication;
  
  // 4. Map Routes/Controllers
  App.MapControllers;
  
  App.MapGet('/', procedure(Ctx: IHttpContext)
    begin
      Ctx.Response.Write('Welcome to Dext API');
    end);
end;

end.
```

## Main Program (.dpr)

With the Startup class, your main file becomes extremely clean:

```pascal
program MyProject;

{$APPTYPE CONSOLE}

uses
  Dext.Web,
  App.Startup in 'src\App.Startup.pas';

begin
  TWebHostBuilder.CreateDefault
    .UseStartup<TStartup>
    .Build
    .Run; // Blocks execution (Console App)
end.
```

## Execution Models: Run vs Start

Dext provides two ways to run the host, designed for different application types:

| Method | Behavior | Use Case |
| :--- | :--- | :--- |
| **`Run`** | Blocks the calling thread until stopped (Ctrl+C). | **Console Apps**, Services, Daemons. |
| **`Start`** | Non-blocking. Starts the server and returns immediately. | **GUI Apps (VCL/FMX)** like System Tray tools or Desktop Shells. |

### Example: GUI Application (Sidecar)

In a VCL application, you must use `Start` to avoid freezing the main form:

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Create and Start the host without blocking the UI
  FHost := TWebHostBuilder.CreateDefault
    .UseStartup<TStartup>
    .Build;
    
  FHost.Start; 
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FHost.Stop; // Graceful shutdown
end;
```

## Advanced: Database Seeding

You can also include a seeding method in your Startup class to populate the database on first run, as seen in the **OrderAPI** example:

```pascal
class procedure TStartup.Seed(const App: IWebApplication);
begin
  using var Scope := App.Services.CreateScope;
  var Context := Scope.ServiceProvider.GetService<TAppDbContext>;
  
  Context.EnsureCreated;
  if Context.Users.Count = 0 then
  begin
    Context.Users.Add(TUser.Create('Admin'));
    Context.SaveChanges;
  end;
end;
```

> 📦 **High-Quality Reference**: Check the [Web.OrderAPI](../../../Examples/Web.OrderAPI/OrderAPI.Startup.pas) example for a complete real-world implementation of this pattern.

## Advanced: Separating Concerns (Endpoints & Auth)

For larger projects, it's recommended to further separate your code into dedicated modules:

### Recommended Project Structure

```
MyProject/
├── Server/
│   ├── MyProject.Startup.pas      # Configuration only
│   ├── MyProject.Auth.pas         # Authentication services & DTOs
│   ├── MyProject.Endpoints.pas    # All API route definitions
│   └── Web.MyProject.dpr          # Entry point
├── Domain/
│   ├── MyProject.Domain.Entities.pas
│   ├── MyProject.Domain.Models.pas
│   └── MyProject.Domain.Enums.pas
└── Data/
    ├── MyProject.Data.Context.pas
    └── MyProject.Data.Seeder.pas
```

### Creating an Endpoints Module

Move all route definitions to a dedicated unit:

```pascal
unit App.Endpoints;

interface

uses Dext.Web;

type
  TAppEndpoints = class
  public
    class procedure MapEndpoints(const Builder: IApplicationBuilder); static;
  end;

implementation

uses
  Dext.Web.Results,
  Dext.Web.DataApi,
  App.Auth,
  App.Data.Context,
  App.Domain.Entities;

class procedure TAppEndpoints.MapEndpoints(const Builder: IApplicationBuilder);
begin
  // Health Check
  Builder.MapGet<IResult>('/health', 
    function: IResult
    begin
      Result := Results.Ok(THealthStatus.Create('healthy'));
    end); 

  // Authentication
  Builder.MapPost<TLoginRequest, IAuthService, IResult>('/auth/login',
    function(Req: TLoginRequest; Auth: IAuthService): IResult
    begin
      var Token := Auth.Login(Req.username, Req.password);
      if Token = '' then
        Exit(Results.StatusCode(401)); 
      Result := Results.Ok(TLoginResponse.Create(Token));
    end);

  // DataApi - Auto CRUD
  TDataApiHandler<TCustomer>.Map(Builder, '/api/customers',
    TDataApiOptions<TCustomer>.Create.DbContext<TAppDbContext>);

  // Custom Endpoints
  Builder.MapPost<TCreateOrderDto, IResult>('/api/orders', ...);
end;

end.
```

### Creating an Auth Module

Isolate authentication-related code:

```pascal
unit App.Auth;

interface

uses Dext.Web, Dext.Auth.JWT;

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
    function Login(const User, Pass: string): string;
  end;

  TAuthConfig = class
  public
    const JWT_SECRET = 'your-secret-key-minimum-32-chars';
    class procedure AddServices(const Services: TDextServices); static;
  end;

implementation

// ... TAuthService implementation
```

### Simplified Startup

Your Startup becomes focused only on configuration:

```pascal
procedure TStartup.Configure(const App: IWebApplication);
begin
  var Builder := App.Builder;

  // Middleware Pipeline
  Builder.UseExceptionHandler;
  Builder.UseHttpLogging;
  Builder.UseCors(...);
  Builder.UseJwtAuthentication(TAuthConfig.JWT_SECRET, ...);

  // Delegate to Endpoints module
  TAppEndpoints.MapEndpoints(Builder);

  Builder.UseSwagger(...);
end;
```

### Benefits

- **Single Responsibility**: Each file has one clear purpose
- **Testability**: Endpoints can be tested independently
- **Scalability**: Easy to add new endpoint groups
- **Maintainability**: Configuration changes don't touch business logic

> 📦 **Reference**: See [Web.SalesSystem](../../../Examples/Web.SalesSystem/) for a complete example using this pattern.

---

[← Project Structure](project-structure.md) | [Next: Web Framework →](../02-web-framework/README.md)
