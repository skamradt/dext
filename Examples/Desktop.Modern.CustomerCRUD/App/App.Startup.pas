{***************************************************************************}
{                                                                           }
{           Dext Framework - Example                                        }
{                                                                           }
{           App Startup - DI Container Configuration                        }
{                                                                           }
{***************************************************************************}
unit App.Startup;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Dext,
  Dext.Configuration.Interfaces,
  Dext.Configuration.Yaml,
  Dext.Entity,
  Customer.Entity,
  Customer.Service,
  Customer.Context;

type
  /// <summary>
  /// Application startup and DI configuration
  /// </summary>
  TAppStartup = class
  private
    class var FServices: TDextServices;
    class var FProvider: IServiceProvider;
    class var FLogger: ILogger;
    class var FConfig: IConfiguration;
    
    class procedure SeedDemoData;
  public
    class procedure Configure;
    class procedure Shutdown;
    
    class function GetCustomerService: ICustomerService;
    class function GetLogger: ILogger;
  end;

implementation

{ TAppStartup }

class procedure TAppStartup.Configure;
begin
  // Load Configuration from YAML
  // Load Configuration from YAML with in-memory defaults
  FConfig := TYamlConfigurationBuilder.Create
    .AddValues([
      TPair<string, string>.Create('Database:DriverName', 'SQLite'),
      TPair<string, string>.Create('Database:Params:Database', 'customers.db'),
      TPair<string, string>.Create('Database:Naming', 'snake_case')
    ])
    .AddYamlFile('appsettings.yaml', True) // True = Optional
    .Build;

  // Create DI Container using the fluent wrapper
  FServices := TDextServices.New;
  
  // Register Logger as a singleton instance
  FLogger := TConsoleLogger.Create('CustomerCRUD');
  FServices.AddSingleton<ILogger>(FLogger);
  
  // Register DB Context from configuration
  FServices.AddDbContext<TCustomerContext>(FConfig.GetSection('Database'));
  
  // Register Customer Service
  // Using auto-injection via [ServiceConstructor]
  FServices.AddSingleton<ICustomerService, TCustomerService>;
  
  // Build provider
  FProvider := FServices.BuildServiceProvider;
  
  FLogger.Info('Application configured successfully (YAML + DB)');
  
  // Seed demo data
  SeedDemoData;
end;

class procedure TAppStartup.SeedDemoData;
var
  Service: ICustomerService;
  Customer: TCustomer;
begin
  Service := GetCustomerService;
  
  // Note: Objects added to the DbContext are managed by the IdentityMap.
  // Do NOT free them manually or the IdentityMap will have dangling pointers.
  
  Customer := TCustomer.Create;
  Customer.Name := 'John Doe';
  Customer.Email := 'john.doe@example.com';
  Customer.Phone := '+1 555-1234';
  Customer.Document := '123.456.789-00';
  Customer.Active := True;
  Service.Save(Customer);
  
  Customer := TCustomer.Create;
  Customer.Name := 'Jane Smith';
  Customer.Email := 'jane.smith@example.com';
  Customer.Phone := '+1 555-5678';
  Customer.Document := '987.654.321-00';
  Customer.Active := True;
  Service.Save(Customer);
  
  Customer := TCustomer.Create;
  Customer.Name := 'Bob Johnson';
  Customer.Email := 'bob.johnson@example.com';
  Customer.Phone := '+1 555-9999';
  Customer.Document := '111.222.333-44';
  Customer.Active := False;
  Service.Save(Customer);
  
  FLogger.Info('Demo data seeded: 3 customers');
end;

class procedure TAppStartup.Shutdown;
begin
  FLogger.Info('Application shutting down');
  FProvider := nil;
  FServices := Default(TDextServices);
end;

class function TAppStartup.GetCustomerService: ICustomerService;
begin
  Result := TServiceProviderExtensions.GetService<ICustomerService>(FProvider);
end;

class function TAppStartup.GetLogger: ILogger;
begin
  Result := FLogger;
end;

end.
