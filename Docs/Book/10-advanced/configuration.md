# Configuration

Load and bind configuration using `IConfiguration` and `IOptions<T>`.

> 📦 **Example**: [Core.TestConfig](../../../Examples/Core.TestConfig/)

## Loading Configuration

### From JSON

```pascal
uses
  Dext.Configuration;

var
  Config: IConfiguration;
begin
  Config := TConfigurationBuilder.Create
    .AddJsonFile('appsettings.json')
    .AddJsonFile('appsettings.Development.json', Optional := True)
    .Build;
end;
```

### appsettings.json

```json
{
  "Database": {
    "Provider": "PostgreSQL",
    "ConnectionString": "Server=localhost;Database=myapp",
    "MaxPoolSize": 10
  },
  "Jwt": {
    "SecretKey": "your-secret-key",
    "ExpirationMinutes": 60
  },
  "Features": {
    "EnableCache": true,
    "CacheTTL": 300
  }
}
```

## Reading Values

```pascal
// Simple values
var DbProvider := Config.GetValue('Database:Provider');
var MaxPool := Config.GetValue<Integer>('Database:MaxPoolSize');

// With defaults
var CacheTTL := Config.GetValue<Integer>('Features:CacheTTL', 60);
```

## Options Pattern

Bind configuration sections to strongly-typed classes:

### 1. Define Options Class

```pascal
type
  TDatabaseOptions = class
  public
    Provider: string;
    ConnectionString: string;
    MaxPoolSize: Integer;
  end;
  
  TJwtOptions = class
  public
    SecretKey: string;
    ExpirationMinutes: Integer;
  end;
```

### 2. Register Options

```pascal
Services.Configure<TDatabaseOptions>(Config.GetSection('Database'));
Services.Configure<TJwtOptions>(Config.GetSection('Jwt'));
```

### 3. Inject and Use

```pascal
type
  TUserService = class
  private
    FDbOptions: IOptions<TDatabaseOptions>;
  public
    constructor Create(DbOptions: IOptions<TDatabaseOptions>);
    procedure Connect;
  end;

procedure TUserService.Connect;
begin
  var ConnStr := FDbOptions.Value.ConnectionString;
  var MaxPool := FDbOptions.Value.MaxPoolSize;
  // Use values...
end;
```

## Environment Variables

Override config with environment variables:

```pascal
Config := TConfigurationBuilder.Create
  .AddJsonFile('appsettings.json')
  .AddEnvironmentVariables  // Override with env vars
  .Build;
```

Environment variable naming:
```
Database__ConnectionString=postgresql://...
Jwt__SecretKey=my-secret
```

(Double underscore `__` represents nesting)

## Configuration in Web Host

```pascal
TWebHostBuilder.CreateDefault(nil)
  .ConfigureAppConfiguration(procedure(Builder: IConfigurationBuilder)
    begin
      Builder.AddJsonFile('appsettings.json');
    end)
  .ConfigureServices(procedure(Services: IServiceCollection)
    var
      Config: IConfiguration;
    begin
      Config := Services.BuildServiceProvider.GetRequiredService<IConfiguration>;
      Services.Configure<TDatabaseOptions>(Config.GetSection('Database'));
    end)
  .Build
  .Run;
```

---

[← Background Services](background-services.md) | [Next: Async API →](async-api.md)
