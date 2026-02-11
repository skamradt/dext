# Project Structure

A typical Dext project follows this structure:

## Recommended Layout

```
MyProject/
├── Server/
│   ├── MyProject.Startup.pas      # Configuration only (IStartup)
│   ├── MyProject.Endpoints.pas    # All API route definitions
│   ├── MyProject.Auth.pas         # Authentication services & DTOs
│   └── Web.MyProject.dpr          # Entry point
├── Domain/
│   ├── MyProject.Domain.Entities.pas
│   ├── MyProject.Domain.Models.pas
│   └── MyProject.Domain.Enums.pas
├── Data/
│   ├── MyProject.Data.Context.pas
│   └── MyProject.Data.Seeder.pas
└── Tests/
    ├── MyProject.Tests.dpr         # Test runner
    └── MyProject.Tests.pas         # Test fixtures
```

## Main Program Template (.dpr)

> [!IMPORTANT]
> Every console project (Web APIs, Test Runners) **MUST** follow these rules:
> 1. Add `Dext.Utils` to uses
> 2. Call `SetConsoleCharSet;` as the **first** instruction
> 3. Call `ConsolePause;` as the **last** instruction
> 4. Declare interface variables (`IWebApplication`, `IServiceProvider`) with **explicit types** in the `var` block

```pascal
program Web.MyProject;

{$APPTYPE CONSOLE}

uses
  Dext.MM,            // Optional: FastMM5 wrapper for leak detection
  Dext.Utils,         // SetConsoleCharSet, ConsolePause
  System.SysUtils,
  // Dext Facades (ALWAYS LAST in Dext group)
  Dext,               // IServiceProvider, IConfiguration
  Dext.Web,           // IWebApplication, WebApplication
  // Project units
  MyProject.Startup in 'MyProject.Startup.pas';

var
  App: IWebApplication;
  Provider: IServiceProvider;
begin
  SetConsoleCharSet;   // 1. Configure UTF-8 for emojis and accents
  try
    App := WebApplication;
    App.UseStartup(TStartup.Create);

    // Build services and seed BEFORE running
    Provider := App.BuildServices;
    TDbSeeder.Seed(Provider);

    App.Run(9000);
  except
    on E: Exception do
      WriteLn('Fatal: ' + E.Message);
  end;
  ConsolePause;        // 2. Pause (only when running in IDE)
end.
```

> [!WARNING]
> **Memory Safety**: Always declare `App` and `Provider` as **explicitly typed** interface variables in the `var` block (not inline `var`). This ensures ARC releases interfaces in the correct order during shutdown, avoiding Access Violations.

## Uses Clause Ordering

Dext uses **Facade Units** with Record Helpers. Since Delphi only applies the last declared helper, facades **must** come last:

```pascal
uses
  // 1. Delphi System Units
  System.SysUtils,
  System.Classes,
  // 2. Third-Party Units
  // ...
  // 3. Dext Specialized Units (alphabetical)
  Dext.Auth.JWT,
  Dext.Entity.Core,      // Required for IDbSet<T>
  Dext.Types.Nullable,   // Required for Nullable<T>
  // 4. Dext Facades (ALWAYS LAST in Dext group)
  Dext,                  // Core facade
  Dext.Entity,           // ORM facade
  Dext.Web,              // Web facade
  // 5. Current Project Units (alphabetical)
  MyProject.Data.Context,
  MyProject.Domain.Entities;
```

> [!IMPORTANT]
> **Generic types** (`IDbSet<T>`, `Nullable<T>`, `Lazy<T>`, `Prop<T>`) are **NOT** re-exported by facades.  
> You **MUST** add the original unit (`Dext.Entity.Core`, `Dext.Types.Nullable`, etc.) to uses.

## Configuration File

`appsettings.json`:

```json
{
  "Database": {
    "Provider": "SQLite",
    "ConnectionString": "App.db"
  },
  "Jwt": {
    "SecretKey": "your-secret-key-minimum-32-chars",
    "ExpirationMinutes": 60
  }
}
```

Load it with:

```pascal
// In ConfigureServices, use the IConfiguration parameter
procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  var DbConn := Configuration.GetValue('Database:ConnectionString');
  // ...
end;
```

## Search Paths (.dproj)

> [!WARNING]
> **NEVER** point search paths to the framework `Sources` directory. Always use compiled DCUs from the `Output` directory. Mixing sources causes the fatal error "Unit was compiled with a different version".

```xml
<DCC_ExeOutput>..\..\Output\</DCC_ExeOutput>
<DCC_DcuOutput>..\..\Output\$(ProductVersion)_$(Platform)_$(Config)</DCC_DcuOutput>
<DCC_UnitSearchPath>..\..\..\Output\$(ProductVersion)_$(Platform)_$(Config);$(DCC_UnitSearchPath)</DCC_UnitSearchPath>
```

---

[← Hello World](hello-world.md) | [Next: Application Startup →](application-startup.md)
