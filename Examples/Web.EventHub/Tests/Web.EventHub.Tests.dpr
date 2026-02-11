program Web.EventHub.Tests;

{$APPTYPE CONSOLE}

{***************************************************************************}
{                                                                           }
{           Web.EventHub - Test Runner                                      }
{                                                                           }
{           Unit tests for entities, services, and domain logic             }
{                                                                           }
{***************************************************************************}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  Dext.Testing,
  EventHub.Tests.Entities in 'EventHub.Tests.Entities.pas',
  EventHub.Tests.Services in 'EventHub.Tests.Services.pas',
  EventHub.Domain.Entities in '..\Domain\EventHub.Domain.Entities.pas',
  EventHub.Domain.Enums in '..\Domain\EventHub.Domain.Enums.pas',
  EventHub.Domain.Models in '..\Domain\EventHub.Domain.Models.pas';

begin
  SetConsoleCharSet;
  try
    WriteLn('');
    WriteLn('========================================');
    WriteLn('   EventHub Unit Tests');
    WriteLn('========================================');
    WriteLn('');

    TTest.SetExitCode(
      TTest.Configure
        .Verbose
        .RegisterFixtures([
          // Entity Tests
          TEventTests,
          TRegistrationTests,
          TVenueTests,
          TSpeakerTests,
          TAttendeeTests,
          // Service/DTO Tests
          TEventResponseTests,
          TRegistrationResponseTests,
          TLoginRequestTests,
          TDashboardMetricsTests,
          TCreateEventRequestTests
        ])
        .Run
    );
  except
    on E: Exception do
      Writeln('Error: ', E.Message);
  end;
  ConsolePause;
end.
