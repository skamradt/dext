program Web.HelpDesk;

{$APPTYPE CONSOLE}

uses
  Dext.MM,      // FastMM5 Wrapper
  Dext.Utils,   // Console Pause/SetCharSet
  System.SysUtils,
  Dext,         // IServiceProvider
  Dext.Web,     // WebApplication Facade
  HelpDesk.Startup in 'HelpDesk.Startup.pas',
  HelpDesk.Endpoints in 'HelpDesk.Endpoints.pas',
  HelpDesk.Services in '..\Services\HelpDesk.Services.pas',
  HelpDesk.Data.Context in '..\Data\HelpDesk.Data.Context.pas',
  HelpDesk.Data.Seeder in '..\Data\HelpDesk.Data.Seeder.pas',
  HelpDesk.Domain.Entities in '..\Domain\HelpDesk.Domain.Entities.pas',
  HelpDesk.Domain.Enums in '..\Domain\HelpDesk.Domain.Enums.pas',
  HelpDesk.Domain.Models in '..\Domain\HelpDesk.Domain.Models.pas';

const
  PORT = 9005;

var
  App: IWebApplication;
  Provider: IServiceProvider;

begin
  SetConsoleCharSet;
  try
    WriteLn('========================================');
    WriteLn('   HelpDesk API Server (Minimal API)');
    WriteLn('========================================');

    // ARC-safe: Declare as interface explicitly (SKILL.md rule)
    App := WebApplication;
    App.UseStartup(TStartup.Create);

    // Build + Seed
    Provider := App.BuildServices;

    TDbSeeder.Seed(Provider);

    WriteLn('Server listening on http://localhost:' + IntToStr(PORT));
    WriteLn('Swagger UI: http://localhost:' + IntToStr(PORT) + '/swagger');

    App.Run(PORT);
  except
    on E: Exception do
      WriteLn('Fatal Error: ' + E.ClassName + ': ' + E.Message);
  end;
  ConsolePause;
end.
