program Web.EventHub;

{$APPTYPE CONSOLE}

{***************************************************************************}
{                                                                           }
{           Web.EventHub - Entry Point                                      }
{                                                                           }
{           Event Management & Registration Platform                        }
{           Demonstrates Minimal API, ORM, JWT Auth, Business Rules         }
{           and the WaitList auto-promotion pattern                         }
{                                                                           }
{***************************************************************************}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  Dext,
  Dext.Web,
  EventHub.Startup in 'EventHub.Startup.pas',
  EventHub.Endpoints in 'EventHub.Endpoints.pas',
  EventHub.Services in '..\Services\EventHub.Services.pas',
  EventHub.Data.Context in '..\Data\EventHub.Data.Context.pas',
  EventHub.Data.Seeder in '..\Data\EventHub.Data.Seeder.pas',
  EventHub.Domain.Entities in '..\Domain\EventHub.Domain.Entities.pas',
  EventHub.Domain.Enums in '..\Domain\EventHub.Domain.Enums.pas',
  EventHub.Domain.Models in '..\Domain\EventHub.Domain.Models.pas';

const
  DEFAULT_PORT = 9000;

begin
  SetConsoleCharSet;
  try
    WriteLn('');
    WriteLn('========================================');
    WriteLn('   EventHub API v1.0');
    WriteLn('========================================');
    WriteLn('');

    // Create application with fluent startup
    var App: IWebApplication := TDextApplication.Create;
    App.UseStartup(TStartup.Create);

    // Build services and seed database BEFORE running
    WriteLn('  Initializing services...');
    var Provider := App.BuildServices;

    WriteLn('  Setting up database...');
    TDbSeeder.Seed(Provider);

    WriteLn('  Server starting on http://localhost:' + IntToStr(DEFAULT_PORT));
    WriteLn('');
    WriteLn('  Swagger UI: http://localhost:' + IntToStr(DEFAULT_PORT) + '/swagger');
    WriteLn('');
    WriteLn('Endpoints:');
    WriteLn('  POST   /api/auth/login                        - Login (get JWT token)');
    WriteLn('');
    WriteLn('  GET    /api/events                            - List published events');
    WriteLn('  GET    /api/events/{id}                       - Get event details');
    WriteLn('  POST   /api/events                            - Create event (auth)');
    WriteLn('  PUT    /api/events/{id}                       - Update event (auth)');
    WriteLn('  POST   /api/events/{id}/publish               - Publish draft event (auth)');
    WriteLn('  POST   /api/events/{id}/cancel                - Cancel event (auth)');
    WriteLn('  GET    /api/events/metrics                    - Dashboard metrics (auth)');
    WriteLn('');
    WriteLn('  GET    /api/events/{eventId}/speakers         - List speakers');
    WriteLn('  POST   /api/events/{eventId}/speakers         - Add speaker (auth)');
    WriteLn('');
    WriteLn('  POST   /api/attendees                         - Register attendee');
    WriteLn('  GET    /api/attendees/{id}                    - Get attendee');
    WriteLn('');
    WriteLn('  POST   /api/registrations                     - Register for event');
    WriteLn('  POST   /api/registrations/{id}/cancel         - Cancel registration');
    WriteLn('  GET    /api/events/{eventId}/registrations    - Event registrations');
    WriteLn('  GET    /api/attendees/{id}/registrations      - Attendee registrations');
    WriteLn('');
    WriteLn('  GET    /health                                - Health check');
    WriteLn('');
    WriteLn('Demo Credentials:');
    WriteLn('  admin/admin123 (Admin role)');
    WriteLn('  organizer/org123 (Organizer role)');
    WriteLn('');
    WriteLn('Press Ctrl+C to stop the server...');
    WriteLn('');

    App.Run(DEFAULT_PORT);

  except
    on E: Exception do
    begin
      WriteLn('');
      WriteLn('Error: ' + E.ClassName + ': ' + E.Message);
      WriteLn('');
      ExitCode := 1;
    end;
  end;
  ConsolePause;
end.
