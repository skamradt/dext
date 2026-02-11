unit EventHub.Tests.Entities;

{***************************************************************************}
{                                                                           }
{           Web.EventHub - Entity Unit Tests                                }
{                                                                           }
{           Tests for entity business logic and domain rules                }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Testing,
  EventHub.Domain.Entities,
  EventHub.Domain.Enums;

type
  [TestFixture]
  TEventTests = class
  private
    FEvent: TEvent;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure Should_Create_Event_With_Default_Values;

    [Test]
    procedure CanRegister_Should_Return_True_For_Published_Event;

    [Test]
    procedure CanRegister_Should_Return_False_For_Draft_Event;

    [Test]
    procedure CanRegister_Should_Return_False_For_Canceled_Event;

    [Test]
    procedure CanRegister_Should_Return_False_For_Finished_Event;

    [Test]
    procedure AvailableSlots_Should_Return_MaxCapacity_When_No_Registrations;

    [Test]
    procedure IsFullyBooked_Should_Return_False_When_Slots_Available;
  end;

  [TestFixture]
  TRegistrationTests = class
  private
    FRegistration: TRegistration;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure Cancel_Should_Succeed_When_More_Than_24h_Before_Event;

    [Test]
    procedure Cancel_Should_Fail_When_Less_Than_24h_Before_Event;

    [Test]
    procedure Cancel_Should_Fail_When_Already_Canceled;

    [Test]
    procedure Cancel_Should_Set_Status_To_Canceled;

    [Test]
    procedure Cancel_Should_Set_CanceledAt;
  end;

  [TestFixture]
  TVenueTests = class
  public
    [Test]
    procedure Should_Create_Venue_With_Properties;
  end;

  [TestFixture]
  TSpeakerTests = class
  public
    [Test]
    procedure Should_Create_Speaker_With_Properties;
  end;

  [TestFixture]
  TAttendeeTests = class
  public
    [Test]
    procedure Should_Create_Attendee_With_Properties;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils;

{ ======================================================================== }
{ TEventTests                                                              }
{ ======================================================================== }

procedure TEventTests.Setup;
begin
  FEvent := TEvent.Create;
end;

procedure TEventTests.TearDown;
begin
  FEvent.Free;
end;

procedure TEventTests.Should_Create_Event_With_Default_Values;
begin
  Should(Integer(FEvent.Id)).Be(0);
  Should(string(FEvent.Title)).Be('');
  Should(FEvent.Speakers.Count).Be(0);
  Should(FEvent.Registrations.Count).Be(0);
end;

procedure TEventTests.CanRegister_Should_Return_True_For_Published_Event;
begin
  FEvent.Status := esPublished;
  Should(FEvent.CanRegister).Be(True);
end;

procedure TEventTests.CanRegister_Should_Return_False_For_Draft_Event;
begin
  FEvent.Status := esDraft;
  Should(FEvent.CanRegister).Be(False);
end;

procedure TEventTests.CanRegister_Should_Return_False_For_Canceled_Event;
begin
  FEvent.Status := esCanceled;
  Should(FEvent.CanRegister).Be(False);
end;

procedure TEventTests.CanRegister_Should_Return_False_For_Finished_Event;
begin
  FEvent.Status := esFinished;
  Should(FEvent.CanRegister).Be(False);
end;

procedure TEventTests.AvailableSlots_Should_Return_MaxCapacity_When_No_Registrations;
begin
  FEvent.MaxCapacity := 100;
  // No registrations in the list, so ConfirmedCount = 0
  Should(FEvent.AvailableSlots).Be(100);
end;

procedure TEventTests.IsFullyBooked_Should_Return_False_When_Slots_Available;
begin
  FEvent.MaxCapacity := 50;
  Should(FEvent.IsFullyBooked).Be(False);
end;

{ ======================================================================== }
{ TRegistrationTests                                                       }
{ ======================================================================== }

procedure TRegistrationTests.Setup;
begin
  FRegistration := TRegistration.Create;
  FRegistration.Status := rsConfirmed;
end;

procedure TRegistrationTests.TearDown;
begin
  FRegistration.Free;
end;

procedure TRegistrationTests.Cancel_Should_Succeed_When_More_Than_24h_Before_Event;
var
  EventStart: TDateTime;
begin
  // Event starts 3 days from now
  EventStart := IncDay(Now, 3);
  Should(FRegistration.Cancel(EventStart)).Be(True);
end;

procedure TRegistrationTests.Cancel_Should_Fail_When_Less_Than_24h_Before_Event;
var
  EventStart: TDateTime;
begin
  // Event starts 12 hours from now
  EventStart := IncHour(Now, 12);
  Should(FRegistration.Cancel(EventStart)).Be(False);
end;

procedure TRegistrationTests.Cancel_Should_Fail_When_Already_Canceled;
var
  EventStart: TDateTime;
begin
  EventStart := IncDay(Now, 3);
  FRegistration.Status := rsCanceled;
  Should(FRegistration.Cancel(EventStart)).Be(False);
end;

procedure TRegistrationTests.Cancel_Should_Set_Status_To_Canceled;
var
  EventStart: TDateTime;
begin
  EventStart := IncDay(Now, 3);
  FRegistration.Cancel(EventStart);
  Should(Integer(TRegistrationStatus(FRegistration.Status))).Be(Integer(rsCanceled));
end;

procedure TRegistrationTests.Cancel_Should_Set_CanceledAt;
var
  EventStart: TDateTime;
begin
  EventStart := IncDay(Now, 3);
  FRegistration.Cancel(EventStart);
  Should(FRegistration.CanceledAt.HasValue).Be(True);
end;

{ ======================================================================== }
{ TVenueTests                                                              }
{ ======================================================================== }

procedure TVenueTests.Should_Create_Venue_With_Properties;
var
  Venue: TVenue;
begin
  Venue := TVenue.Create;
  try
    Venue.Name := 'Test Venue';
    Venue.Address := 'Test Address 123';
    Venue.City := 'São Paulo';
    Venue.Capacity := 500;

    Should(string(Venue.Name)).Be('Test Venue');
    Should(string(Venue.City)).Be('São Paulo');
    Should(Integer(Venue.Capacity)).Be(500);
  finally
    Venue.Free;
  end;
end;

{ ======================================================================== }
{ TSpeakerTests                                                            }
{ ======================================================================== }

procedure TSpeakerTests.Should_Create_Speaker_With_Properties;
var
  Speaker: TSpeaker;
begin
  Speaker := TSpeaker.Create;
  try
    Speaker.Name := 'John Doe';
    Speaker.Bio := 'Expert in Delphi';
    Speaker.Email := 'john@example.com';

    Should(string(Speaker.Name)).Be('John Doe');
    Should(string(Speaker.Bio)).Be('Expert in Delphi');
    Should(string(Speaker.Email)).Be('john@example.com');
  finally
    Speaker.Free;
  end;
end;

{ ======================================================================== }
{ TAttendeeTests                                                           }
{ ======================================================================== }

procedure TAttendeeTests.Should_Create_Attendee_With_Properties;
var
  Attendee: TAttendee;
begin
  Attendee := TAttendee.Create;
  try
    Attendee.Name := 'Maria Silva';
    Attendee.Email := 'maria@dev.io';
    Attendee.Phone := '(11) 99999-0000';

    Should(string(Attendee.Name)).Be('Maria Silva');
    Should(string(Attendee.Email)).Be('maria@dev.io');
    Should(string(Attendee.Phone)).Be('(11) 99999-0000');
  finally
    Attendee.Free;
  end;
end;

end.
