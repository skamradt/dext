program Web.DatabaseAsApi;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  System.Classes,
  
  // Dext Framework
  Dext,
  Dext.Entity,
  Dext.Entity.Attributes,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.Dialects,
  Dext.Web,
  Dext.Web.DataApi,
  Dext.WebHost,
  Dext.Web.Results,
  Dext.Web.Interfaces,
  // Swagger/OpenAPI
  Dext.OpenAPI.Generator,
  Dext.Swagger.Middleware,
  
  // Storage
  FireDAC.Comp.Client,
  Dext.Entity.Drivers.FireDAC.Links;

type
  [Table('Customers')]
  TCustomer = class
  private
    FId: Integer;
    FName: string;
    FEmail: string;
    FActive: Boolean;
    FInternalCode: string;
    FCreatedAt: TDateTime;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
    property Active: Boolean read FActive write FActive;
    
    // This field will be ignored by the API (and DB)
    [NotMapped]
    property InternalCode: string read FInternalCode write FInternalCode;
    
    // Demonstration of SnakeCase conversion
    // CreatedAt -> created_at in JSON
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  end;

  /// <summary>
  ///   Application startup class - handles initialization, configuration and shutdown.
  ///   Avoids global variables and initialization/finalization sections.
  /// </summary>
  TStartup = class
  private
    FConnection: IDbConnection;
    FDbContext: TDbContext;
    FHost: IWebHost;
    
    function CreateSQLiteConnection: IDbConnection;
    procedure SeedData;
    procedure Configure(ABuilder: IApplicationBuilder);
  public
    procedure Initialize;
    procedure Run;
    procedure Finalize;
  end;

// Standalone root handler (cannot be nested due to capture limitations)
procedure HandleRoot(Ctx: IHttpContext);
begin
  Results.Text('Database as API Example. Try /api/customers').Execute(Ctx);
end;

{ TStartup }

function TStartup.CreateSQLiteConnection: IDbConnection;
var
  FDConn: TFDConnection;
begin
  FDConn := TFDConnection.Create(nil);
  FDConn.DriverName := 'SQLite';
  FDConn.Params.Values['Database'] := ':memory:';
  FDConn.Params.Values['LockingMode'] := 'Normal';
  Result := TFireDACConnection.Create(FDConn);
end;

procedure TStartup.SeedData;
var
  Customer: TCustomer;
begin
  Writeln('Seeding database...');
  
  Customer := TCustomer.Create;
  Customer.Name := 'John Doe';
  Customer.Email := 'john@example.com';
  Customer.Active := True;
  Customer.InternalCode := 'SECRET_123';
  Customer.CreatedAt := Now;
  FDbContext.Entities<TCustomer>.Add(Customer);
  
  Customer := TCustomer.Create;
  Customer.Name := 'Jane Smith';
  Customer.Email := 'jane@example.com';
  Customer.Active := False;
  FDbContext.Entities<TCustomer>.Add(Customer);
  
  FDbContext.SaveChanges;
  Writeln('Database seeded with 2 customers.');
end;

procedure TStartup.Configure(ABuilder: IApplicationBuilder);
var
  SwaggerOptions: TOpenAPIOptions;
begin
  // Map the Data API for TCustomer entity
  // Enable Swagger documentation with .UseSwagger
  TDataApiHandler<TCustomer>.Map(ABuilder, '/api/customers', FDbContext,
    TDataApiOptions<TCustomer>.Create
      .UseSnakeCase
      .UseSwagger       // Opt-in to Swagger documentation
      .Tag('Customers') // Optional: custom tag name
  );
  
  // Map root handler
  ABuilder.MapGet('/', HandleRoot);
  
  // Configure Swagger/OpenAPI
  SwaggerOptions := TOpenAPIOptions.Default;
  SwaggerOptions.Title := 'Database as API';
  SwaggerOptions.Description := 'Auto-generated REST API from Dext entities with automatic Swagger documentation';
  SwaggerOptions.Version := '1.0.0';
  SwaggerOptions := SwaggerOptions.WithServer('http://localhost:5000', 'Development server');
  
  TSwaggerExtensions.UseSwagger(ABuilder, SwaggerOptions);
end;

procedure TStartup.Initialize;
begin
  Writeln('Database as API Example');
  Writeln('=======================');
  Writeln('');
  
  // 1. Create database connection
  FConnection := CreateSQLiteConnection;
  
  // 2. Create DbContext with SQLite dialect
  FDbContext := TDbContext.Create(FConnection, TSQLiteDialect.Create);
  
  // 3. Register entities
  FDbContext.Entities<TCustomer>;
  
  // 4. Create schema
  FDbContext.EnsureCreated;
  
  // 5. Seed data
  SeedData;
  
  // 6. Build web host
  Writeln('');
  Writeln('Starting web server...');
  
  FHost := TDextWebHost.CreateDefaultBuilder
    .Configure(Configure)
    .Build;
    
  Writeln('');
  Writeln('Server listening on http://localhost:5000');
  Writeln('');
  Writeln('Endpoints:');
  Writeln('  API:     http://localhost:5000/api/customers');
  Writeln('  Swagger: http://localhost:5000/swagger');
  Writeln('  JSON:    http://localhost:5000/swagger.json');
end;

procedure TStartup.Run;
begin
  FHost.Run;
end;

procedure TStartup.Finalize;
begin
  FDbContext.Free;
end;

var
  Startup: TStartup;
begin
  try
    Startup := TStartup.Create;
    try
      Startup.Initialize;
      Startup.Run;
    finally
      Startup.Finalize;
      Startup.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  
  // Only pause if not running in automated mode
  ConsolePause;
end.
