program Web.SwaggerExample;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  Dext.Web,
  Web.SwaggerExample.Startup in 'Web.SwaggerExample.Startup.pas';

begin
  SetConsoleCharSet;
  try
    Writeln('🚀 Starting Dext Swagger Example...');
    Writeln('');

    var App := TDextApplication.Create;
    App.UseStartup(TStartup.Create);
    
    Writeln('');
    Writeln('✅ Server configured successfully!');
    Writeln('');
    Writeln('📖 Swagger UI: http://localhost:5000/api/swagger');
    Writeln('');
    
    App.Run(5000);
    
    ConsolePause;
    App.Stop;

  except
    on E: Exception do
    begin
      Writeln('❌ Error: ', E.Message);
      Writeln('Press Enter to exit...');
      Readln;
    end;
  end;
end.
