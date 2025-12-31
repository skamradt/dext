program Web.SwaggerControllerExample;

{$APPTYPE CONSOLE}

(*
  Swagger + Controllers Example

  Demonstrates how to configure Swagger/OpenAPI documentation
  with MVC Controllers using attribute-based configuration.

  Key features:
  - [DextController] with [SwaggerTag] for grouping
  - [SwaggerOperation] for endpoint documentation
  - [SwaggerResponse] for response schema documentation
  - [Authorize] for security documentation in Swagger UI
  - [SwaggerSchema], [SwaggerProperty] for DTO documentation

  Endpoints:
    GET    /api/books          - List all books
    GET    /api/books/{id}     - Get book by ID
    POST   /api/books          - Create a book (requires auth)
    PATCH  /api/books/{id}/availability - Update availability
    DELETE /api/books/{id}     - Delete a book (requires auth)
    GET    /api/health         - Health check
*)

uses
  Dext.MM,
  System.SysUtils,
  Dext,
  Dext.Web,
  Dext.Utils,
  Dext.Swagger.Middleware,
  Dext.OpenAPI.Generator,
  Dext.OpenAPI.Types,
  SwaggerControllerExample.Models in 'SwaggerControllerExample.Models.pas',
  SwaggerControllerExample.Controller in 'SwaggerControllerExample.Controller.pas';

begin
  SetConsoleCharSet(65001);
  try
    WriteLn('🚀 Starting Dext Swagger Controllers Example...');
    WriteLn('');

    var App: IWebApplication := TDextApplication.Create;

    // 1. Register Controllers
    App.Services.AddControllers;

    // 2. Configure Swagger/OpenAPI options
    var Options := TOpenAPIOptions.Default;
    Options.Title := 'Library API';
    Options.Description := 'A sample API demonstrating Swagger with MVC Controllers in Dext Framework';
    Options.Version := '1.0.0';
    Options.ContactName := 'Dext Team';
    Options.ContactEmail := 'contact@dext.dev';
    Options.LicenseName := 'MIT';
    Options.LicenseUrl := 'https://opensource.org/licenses/MIT';

    // Configure servers
    Options := Options.WithServer('http://localhost:8080', 'Development server');

    // Configure Security Schemes
    Options := Options.WithBearerAuth('JWT', 'Enter JWT token');
    Options := Options.WithApiKeyAuth('X-API-Key', aklHeader, 'API Key for administrative access');

    // 3. Map Controllers (discovers and registers all routes)
    App.MapControllers;

    // 4. Add Swagger middleware
    TSwaggerExtensions.UseSwagger(App.Builder, Options);

    // Print instructions
    WriteLn('✅ Server configured successfully!');
    WriteLn('');
    WriteLn('📖 Swagger UI: http://localhost:8080/swagger');
    WriteLn('📄 OpenAPI JSON: http://localhost:8080/swagger.json');
    WriteLn('');
    WriteLn('🔗 Available endpoints:');
    WriteLn('   POST   /api/auth/login              - Demo login endpoint');
    WriteLn('   GET    /api/books                   - List all books');
    WriteLn('   GET    /api/books/{id}              - Get book by ID');
    WriteLn('   POST   /api/books                   - Create a book');
    WriteLn('   PATCH  /api/books/{id}/availability - Update availability');
    WriteLn('   DELETE /api/books/{id}              - Delete a book');
    WriteLn('   GET    /api/health                  - Health check');
    WriteLn('');
    WriteLn('💡 This example demonstrates Swagger documentation with Controllers.');
    WriteLn('   For auth examples, see Web.ControllerExample with JWT middleware.');
    WriteLn('');
    WriteLn('Press Ctrl+C to stop the server...');
    WriteLn('');

    // 5. Run application
    App.Run(8080);

  except
    on E: Exception do
    begin
      WriteLn('❌ Error: ', E.ClassName, ': ', E.Message);
      ReadLn;
    end;
  end;
end.
