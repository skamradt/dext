program Web.ControllerExample.FluentAPI;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Dext,
  Dext.Web,
  ControllerExample.Setup in 'ControllerExample.Setup.pas',
  ControllerExample.Services in 'ControllerExample.Services.pas',
  ControllerExample.Controller in 'ControllerExample.Controller.pas';

begin
  try
    WriteLn('🚀 Starting Dext Controller Example with Fluent API...');
    
    // Create appsettings.json if it doesn't exist
    EnsureAppSettingsExists;
    
    var App: IWebApplication := TDextApplication.Create;

    // 1. Register Configuration (IOptions)
    App.Services.Configure<TMySettings>(
      App.Configuration.GetSection('AppSettings')
    );

    // 2. Register Services
    App.Services
      .AddSingleton<IGreetingService, TGreetingService>
      .AddControllers;
    
    // 3. Register Health Checks
    App.Services.AddHealthChecks
      .AddCheck<TDatabaseHealthCheck>
      .Build;

    // 4. Register Background Services
    App.Services.AddBackgroundServices
      .AddHostedService<TWorkerService>
      .Build;

    // 5. Configure Middleware Pipeline
    var Builder := App.Builder;

    // ✨ CORS with Fluent API
    Builder.UseCors(procedure(Cors: TCorsBuilder)
    begin
      Cors.WithOrigins(['http://localhost:5173'])
          .WithMethods(['GET', 'POST', 'PUT', 'DELETE'])
          .AllowAnyHeader
          .AllowCredentials
          .WithMaxAge(3600);
    end);

    // Static Files
    Builder.UseStaticFiles(Builder.CreateStaticFileOptions);
    
    // Health Checks
    App.UseMiddleware(THealthCheckMiddleware);

    // ✨ JWT Authentication with Fluent API
    Builder.UseJwtAuthentication('dext-secret-key-must-be-very-long-and-secure-at-least-32-chars',
      procedure(Auth: TJwtOptionsBuilder)
      begin
        Auth.WithIssuer('dext-issuer')
            .WithAudience('dext-audience')
            .WithExpirationMinutes(60);
      end
    );
       
    // 6. Map Controllers
    App.MapControllers;

    // 6.1 Map Versioned API Examples
    RegisterVersionedRoutes(App.Builder);

    // 7. Run Application
    PrintFeatureInstructions;
    App.Run(8080);
  except
    on E: Exception do
      Writeln('❌ Error: ', E.ClassName, ': ', E.Message);
  end;
end.
