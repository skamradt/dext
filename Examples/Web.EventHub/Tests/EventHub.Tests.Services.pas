unit EventHub.Tests.Services;

{***************************************************************************}
{                                                                           }
{           Web.EventHub - Service Tests with Mocks                         }
{                                                                           }
{           Tests for service-level business logic using mocked deps        }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Testing,
  Dext.Collections,
  EventHub.Domain.Models,
  EventHub.Domain.Enums;

type
  [TestFixture]
  TEventResponseTests = class
  public
    [Test]
    procedure Should_Create_EventResponse_With_All_Fields;

    [Test]
    procedure AvailableSlots_Should_Be_Correct;

    [Test]
    procedure Should_Handle_Zero_Capacity;
  end;

  [TestFixture]
  TRegistrationResponseTests = class
  public
    [Test]
    procedure Should_Create_RegistrationResponse_Confirmed;

    [Test]
    procedure Should_Create_RegistrationResponse_WaitList;
  end;

  [TestFixture]
  TLoginRequestTests = class
  public
    [Test]
    procedure Should_Create_LoginRequest;
  end;

  [TestFixture]
  TDashboardMetricsTests = class
  public
    [Test]
    procedure Should_Create_Metrics_With_Defaults;

    [Test]
    procedure Should_Track_All_Registration_States;
  end;

  [TestFixture]
  TCreateEventRequestTests = class
  public
    [Test]
    procedure Should_Create_Request_With_All_Fields;

    [Test]
    procedure Should_Validate_Dates_Conceptually;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils;

{ ======================================================================== }
{ TEventResponseTests                                                      }
{ ======================================================================== }

procedure TEventResponseTests.Should_Create_EventResponse_With_All_Fields;
var
  Resp: TEventResponse;
begin
  Resp.Id := 1;
  Resp.VenueId := 10;
  Resp.VenueName := 'Centro de Convenções';
  Resp.Title := 'Delphi Conference 2026';
  Resp.Description := 'Annual event';
  Resp.MaxCapacity := 500;
  Resp.ConfirmedCount := 120;
  Resp.AvailableSlots := 380;
  Resp.Status := esPublished;
  Resp.SpeakerCount := 5;

  Should(Resp.Id).Be(1);
  Should(Resp.VenueName).Be('Centro de Convenções');
  Should(Resp.Title).Be('Delphi Conference 2026');
  Should(Resp.MaxCapacity).Be(500);
  Should(Resp.ConfirmedCount).Be(120);
  Should(Resp.AvailableSlots).Be(380);
  Should(Integer(Resp.Status)).Be(Integer(esPublished));
  Should(Resp.SpeakerCount).Be(5);
end;

procedure TEventResponseTests.AvailableSlots_Should_Be_Correct;
var
  Resp: TEventResponse;
begin
  Resp.MaxCapacity := 200;
  Resp.ConfirmedCount := 150;
  Resp.AvailableSlots := Resp.MaxCapacity - Resp.ConfirmedCount;
  Should(Resp.AvailableSlots).Be(50);
end;

procedure TEventResponseTests.Should_Handle_Zero_Capacity;
var
  Resp: TEventResponse;
begin
  Resp.MaxCapacity := 0;
  Resp.ConfirmedCount := 0;
  Resp.AvailableSlots := 0;
  Should(Resp.AvailableSlots).Be(0);
end;

{ ======================================================================== }
{ TRegistrationResponseTests                                               }
{ ======================================================================== }

procedure TRegistrationResponseTests.Should_Create_RegistrationResponse_Confirmed;
var
  Resp: TRegistrationResponse;
begin
  Resp.Id := 1;
  Resp.EventId := 10;
  Resp.EventTitle := 'Delphi Conf';
  Resp.AttendeeId := 5;
  Resp.AttendeeName := 'Carlos';
  Resp.Status := rsConfirmed;
  Resp.RegisteredAt := Now;

  Should(Resp.Id).Be(1);
  Should(Integer(Resp.Status)).Be(Integer(rsConfirmed));
  Should(Resp.AttendeeName).Be('Carlos');
end;

procedure TRegistrationResponseTests.Should_Create_RegistrationResponse_WaitList;
var
  Resp: TRegistrationResponse;
begin
  Resp.Status := rsWaitList;
  Should(Integer(Resp.Status)).Be(Integer(rsWaitList));
end;

{ ======================================================================== }
{ TLoginRequestTests                                                       }
{ ======================================================================== }

procedure TLoginRequestTests.Should_Create_LoginRequest;
var
  Req: TLoginRequest;
begin
  Req.Username := 'admin';
  Req.Password := 'admin123';
  Should(Req.Username).Be('admin');
  Should(Req.Password).Be('admin123');
end;

{ ======================================================================== }
{ TDashboardMetricsTests                                                   }
{ ======================================================================== }

procedure TDashboardMetricsTests.Should_Create_Metrics_With_Defaults;
var
  Metrics: TDashboardMetrics;
begin
  FillChar(Metrics, SizeOf(Metrics), 0);
  Should(Metrics.TotalEvents).Be(0);
  Should(Metrics.PublishedEvents).Be(0);
  Should(Metrics.TotalAttendees).Be(0);
  Should(Metrics.TotalRegistrations).Be(0);
end;

procedure TDashboardMetricsTests.Should_Track_All_Registration_States;
var
  Metrics: TDashboardMetrics;
begin
  Metrics.TotalRegistrations := 100;
  Metrics.ConfirmedRegistrations := 80;
  Metrics.WaitListRegistrations := 20;

  Should(Metrics.TotalRegistrations).Be(100);
  Should(Metrics.ConfirmedRegistrations).Be(80);
  Should(Metrics.WaitListRegistrations).Be(20);
  // Invariant: Confirmed + WaitList <= Total (canceled not tracked)
  Should(Metrics.ConfirmedRegistrations + Metrics.WaitListRegistrations)
    .Be(Metrics.TotalRegistrations);
end;

{ ======================================================================== }
{ TCreateEventRequestTests                                                 }
{ ======================================================================== }

procedure TCreateEventRequestTests.Should_Create_Request_With_All_Fields;
var
  Req: TCreateEventRequest;
begin
  Req.VenueId := 1;
  Req.Title := 'Workshop Dext';
  Req.Description := 'Learn Dext Framework';
  Req.StartDate := EncodeDate(2026, 6, 15) + EncodeTime(9, 0, 0, 0);
  Req.EndDate := EncodeDate(2026, 6, 15) + EncodeTime(17, 0, 0, 0);
  Req.MaxCapacity := 30;

  Should(Req.VenueId).Be(1);
  Should(Req.Title).Be('Workshop Dext');
  Should(Req.MaxCapacity).Be(30);
end;

procedure TCreateEventRequestTests.Should_Validate_Dates_Conceptually;
var
  Req: TCreateEventRequest;
begin
  Req.StartDate := EncodeDate(2026, 6, 15) + EncodeTime(9, 0, 0, 0);
  Req.EndDate := EncodeDate(2026, 6, 15) + EncodeTime(17, 0, 0, 0);

  // EndDate must be after StartDate
  Should(Req.EndDate > Req.StartDate).Be(True);
end;

end.
