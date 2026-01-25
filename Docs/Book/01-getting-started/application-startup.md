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

---

[← Project Structure](project-structure.md) | [Next: Web Framework →](../02-web-framework/README.md)
