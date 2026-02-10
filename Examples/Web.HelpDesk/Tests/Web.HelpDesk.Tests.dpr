program Web.HelpDesk.Tests;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  Dext.Testing,
  HelpDesk.Tests.Entities in 'HelpDesk.Tests.Entities.pas',
  HelpDesk.Tests.Services in 'HelpDesk.Tests.Services.pas',
  HelpDesk.Domain.Entities in '..\Domain\HelpDesk.Domain.Entities.pas',
  HelpDesk.Domain.Enums in '..\Domain\HelpDesk.Domain.Enums.pas',
  HelpDesk.Domain.Models in '..\Domain\HelpDesk.Domain.Models.pas',
  HelpDesk.Services in '..\Services\HelpDesk.Services.pas',
  HelpDesk.Data.Context in '..\Data\HelpDesk.Data.Context.pas';

begin
  SetConsoleCharSet;
  try
    TTest.SetExitCode(
      TTest.Configure
        .Verbose
        .RegisterFixtures([TTicketEntityTests, TTicketServiceTests])
        .Run
    );
  except
    on E: Exception do
      WriteLn('Test Runner Error: ' + E.ClassName + ': ' + E.Message);
  end;
  ConsolePause;
end.
