program Web.BasicAuthDemo;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  Dext.Web,
  Dext.Auth.BasicAuth,
  Dext.Web.Interfaces;

begin
  try
    WriteLn('🔐 Dext Basic Authentication Demo');
    WriteLn('==================================');
    WriteLn;

    var App: IWebApplication := TWebApplication.Create;

    // 1. Configure Basic Authentication Middleware
    // This defines HOW users are validated
    App.Builder.UseBasicAuthentication(
      'Dext Protected API',
      function(const Username, Password: string): Boolean
      begin
        // Simple static validation for demo purposes
        Result := (Username = 'admin') and (Password = 'secret');
      end);

    WriteLn('📦 Basic Auth middleware configured (admin:secret)');

    // 2. Register Protected Endpoint using Minimal API
    // The .RequireAuthorization() method automatically enforces authentication
    App.Builder.MapGet('/api/privado', procedure(Ctx: IHttpContext)
      begin
        Ctx.Response.Write('🔓 Você acessou uma área privada com sucesso!');
      end)
      .RequireAuthorization;

    WriteLn('🔒 Registered protected endpoint: GET /api/privado');

    // 3. Register Public Endpoint
    App.Builder.MapGet('/api/publico', procedure(Ctx: IHttpContext)
      begin
        Ctx.Response.Write('📖 Este é um endpoint público, livre para todos.');
      end);

    WriteLn('🔓 Registered public endpoint:    GET /api/publico');

    WriteLn;
    WriteLn('═══════════════════════════════════════════');
    WriteLn('🌐 Server running on http://localhost:8080');
    WriteLn('═══════════════════════════════════════════');
    WriteLn;
    WriteLn('📝 Test Commands:');
    WriteLn;
    WriteLn('# 1. Access public endpoint (Ok)');
    WriteLn('curl -i http://localhost:8080/api/publico');
    WriteLn;
    WriteLn('# 2. Access protected endpoint without credentials (Unauthorized 401)');
    WriteLn('curl -i http://localhost:8080/api/privado');
    WriteLn;
    WriteLn('# 3. Access protected endpoint with valid credentials (Ok)');
    WriteLn('curl -i -u admin:secret http://localhost:8080/api/privado');
    WriteLn;
    WriteLn('# 4. Access protected endpoint with invalid credentials (Unauthorized 401)');
    WriteLn('curl -i -u user:wrong http://localhost:8080/api/privado');
    WriteLn;
    WriteLn('═══════════════════════════════════════════');
    WriteLn('Press Enter to stop the server...');
    WriteLn;

    App.Run(8080);
    
    ConsolePause;
    App.Stop;

    WriteLn;
    WriteLn('✅ Server stopped successfully');

  except
    on E: Exception do
      WriteLn('❌ Error: ', E.Message);
  end;
end.
