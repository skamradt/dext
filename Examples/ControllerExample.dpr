program ControllerExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Rtti,
  Dext, // ✅ The only core unit needed!
  ControllerExample.Controller in 'ControllerExample.Controller.pas',
  ControllerExample.Services in 'ControllerExample.Services.pas';

begin
  try
    WriteLn('🚀 Starting Dext Controller Example...');
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

    // CORS
    var corsOptions := Builder.CreateCorsOptions;
    corsOptions.AllowedOrigins := ['http://localhost:5173'];
    corsOptions.AllowCredentials := True;
    Builder.UseCors(corsOptions);

    // Static Files
    Builder.UseStaticFiles(Builder.CreateStaticFileOptions);
    
    // Health Checks
    App.UseMiddleware(THealthCheckMiddleware);

    // JWT Authentication
    var AuthOptions := Builder.CreateJwtOptions('dext-secret-key-must-be-very-long-and-secure-at-least-32-chars');
    AuthOptions.Issuer := 'dext-issuer';
    AuthOptions.Audience := 'dext-audience';
    Builder.UseJwtAuthentication(AuthOptions);
       
    // 6. Map Controllers
    App.MapControllers;

    // 7. Run Application
    App.Run(8080);
  except
    on E: Exception do
      Writeln('❌ Error: ', E.ClassName, ': ', E.Message);
  end;
end.
