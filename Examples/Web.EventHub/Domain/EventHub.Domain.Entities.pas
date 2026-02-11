unit EventHub.Domain.Entities;

{***************************************************************************}
{                                                                           }
{           Web.EventHub - Domain Entities                                  }
{                                                                           }
{           ORM Entities with Smart Properties, validation attributes,      }
{           and embedded business logic for the Event Hub platform          }
{                                                                           }
{***************************************************************************}

interface

uses
  // 1. Delphi Units
  System.SysUtils,
  // 2. Dext Specialized Units
  Dext.Collections,
  Dext.Core.SmartTypes,
  Dext.Types.Nullable,
  // 3. Dext Facades
  Dext.Entity,
  // 4. Project Units
  EventHub.Domain.Enums;

type
  { Forward Declarations }
  TVenue = class;
  TEvent = class;
  TSpeaker = class;
  TAttendee = class;
  TRegistration = class;

  { -------------------------------------------------------------------------- }
  { TVenue - Physical location where events take place                         }
  { -------------------------------------------------------------------------- }
  [Table('Venues')]
  TVenue = class
  private
    FId: IntType;
    FName: StringType;
    FAddress: StringType;
    FCity: StringType;
    FCapacity: IntType;
    FCreatedAt: DateTimeType;
  public
    [PK, AutoInc]
    property Id: IntType read FId write FId;

    [Required, MaxLength(200)]
    property Name: StringType read FName write FName;

    [Required, MaxLength(500)]
    property Address: StringType read FAddress write FAddress;

    [Required, MaxLength(100)]
    property City: StringType read FCity write FCity;

    /// <summary>Maximum number of people the venue can hold</summary>
    [Required]
    property Capacity: IntType read FCapacity write FCapacity;

    [CreatedAt]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;

    class function Props: TVenue; static;
  end;

  { -------------------------------------------------------------------------- }
  { TEvent - A scheduled event (conference, meetup, workshop, etc.)            }
  { -------------------------------------------------------------------------- }
  [Table('Events')]
  TEvent = class
  private
    FId: IntType;
    FVenueId: IntType;
    FTitle: StringType;
    FDescription: StringType;
    FStartDate: DateTimeType;
    FEndDate: DateTimeType;
    FMaxCapacity: IntType;
    FStatus: TEventStatusType;
    FCreatedAt: DateTimeType;
    FUpdatedAt: DateTimeType;
    FSpeakers: IList<TSpeaker>;
    FRegistrations: IList<TRegistration>;

    // Navigation
    FVenue: TVenue;
  public
    constructor Create;
    destructor Destroy; override;

    [PK, AutoInc]
    property Id: IntType read FId write FId;

    [ForeignKey('Venue')]
    property VenueId: IntType read FVenueId write FVenueId;

    [Required, MaxLength(300)]
    property Title: StringType read FTitle write FTitle;

    [MaxLength(5000)]
    property Description: StringType read FDescription write FDescription;

    [Required]
    property StartDate: DateTimeType read FStartDate write FStartDate;

    [Required]
    property EndDate: DateTimeType read FEndDate write FEndDate;

    /// <summary>Maximum attendees allowed (may be less than venue capacity)</summary>
    [Required]
    property MaxCapacity: IntType read FMaxCapacity write FMaxCapacity;

    property Status: TEventStatusType read FStatus write FStatus;

    [CreatedAt]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;

    [UpdatedAt]
    property UpdatedAt: DateTimeType read FUpdatedAt write FUpdatedAt;

    // Navigation properties
    property Venue: TVenue read FVenue write FVenue;
    property Speakers: IList<TSpeaker> read FSpeakers;
    property Registrations: IList<TRegistration> read FRegistrations;

    /// <summary>Returns how many confirmed registrations exist</summary>
    function ConfirmedCount: Integer;

    /// <summary>Returns number of available slots</summary>
    function AvailableSlots: Integer;

    /// <summary>Returns true if event is fully booked (confirmed = max)</summary>
    function IsFullyBooked: Boolean;

    /// <summary>Returns true if new registrations are allowed</summary>
    function CanRegister: Boolean;

    class function Props: TEvent; static;
  end;

  { -------------------------------------------------------------------------- }
  { TSpeaker - A speaker presenting at an event                                }
  { -------------------------------------------------------------------------- }
  [Table('Speakers')]
  TSpeaker = class
  private
    FId: IntType;
    FEventId: IntType;
    FName: StringType;
    FBio: StringType;
    FEmail: StringType;
    FCreatedAt: DateTimeType;

    // Navigation
    FEvent: TEvent;
  public
    [PK, AutoInc]
    property Id: IntType read FId write FId;

    [ForeignKey('Event')]
    property EventId: IntType read FEventId write FEventId;

    [Required, MaxLength(200)]
    property Name: StringType read FName write FName;

    [MaxLength(2000)]
    property Bio: StringType read FBio write FBio;

    [Required, MaxLength(200)]
    property Email: StringType read FEmail write FEmail;

    [CreatedAt]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;

    // Navigation
    property Event: TEvent read FEvent write FEvent;

    class function Props: TSpeaker; static;
  end;

  { -------------------------------------------------------------------------- }
  { TAttendee - A person who can register for events                           }
  { -------------------------------------------------------------------------- }
  [Table('Attendees')]
  TAttendee = class
  private
    FId: IntType;
    FName: StringType;
    FEmail: StringType;
    FPhone: StringType;
    FCreatedAt: DateTimeType;
  public
    [PK, AutoInc]
    property Id: IntType read FId write FId;

    [Required, MaxLength(200)]
    property Name: StringType read FName write FName;

    [Required, MaxLength(200)]
    property Email: StringType read FEmail write FEmail;

    [MaxLength(20)]
    property Phone: StringType read FPhone write FPhone;

    [CreatedAt]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;

    class function Props: TAttendee; static;
  end;

  { -------------------------------------------------------------------------- }
  { TRegistration - Links an attendee to an event (with status)                }
  { -------------------------------------------------------------------------- }
  [Table('Registrations')]
  TRegistration = class
  private
    FId: IntType;
    FEventId: IntType;
    FAttendeeId: IntType;
    FStatus: TRegistrationStatusType;
    FRegisteredAt: DateTimeType;
    FCanceledAt: Nullable<TDateTime>;

    // Navigation
    FEvent: TEvent;
    FAttendee: TAttendee;
  public
    [PK, AutoInc]
    property Id: IntType read FId write FId;

    [ForeignKey('Event')]
    property EventId: IntType read FEventId write FEventId;

    [ForeignKey('Attendee')]
    property AttendeeId: IntType read FAttendeeId write FAttendeeId;

    property Status: TRegistrationStatusType read FStatus write FStatus;

    [CreatedAt]
    property RegisteredAt: DateTimeType read FRegisteredAt write FRegisteredAt;

    /// <summary>When the registration was canceled (null if still active)</summary>
    property CanceledAt: Nullable<TDateTime> read FCanceledAt write FCanceledAt;

    // Navigation
    property Event: TEvent read FEvent write FEvent;
    property Attendee: TAttendee read FAttendee write FAttendee;

    /// <summary>
    ///   Cancels this registration. Returns False if event starts in less
    ///   than 24 hours or registration is already canceled.
    /// </summary>
    function Cancel(EventStartDate: TDateTime): Boolean;

    class function Props: TRegistration; static;
  end;

implementation

uses
  System.DateUtils;

{ TVenue }

class function TVenue.Props: TVenue;
begin
  Result := Prototype.Entity<TVenue>;
end;

{ TEvent }

constructor TEvent.Create;
begin
  inherited;
  FSpeakers := TCollections.CreateList<TSpeaker>(False);
  FRegistrations := TCollections.CreateList<TRegistration>(False);
end;

destructor TEvent.Destroy;
begin
  inherited;
end;

function TEvent.ConfirmedCount: Integer;
var
  Reg: TRegistration;
begin
  Result := 0;
  for Reg in FRegistrations do
    if TRegistrationStatus(Reg.Status) = rsConfirmed then
      Inc(Result);
end;

function TEvent.AvailableSlots: Integer;
begin
  Result := Integer(FMaxCapacity) - ConfirmedCount;
  if Result < 0 then
    Result := 0;
end;

function TEvent.IsFullyBooked: Boolean;
begin
  Result := AvailableSlots <= 0;
end;

function TEvent.CanRegister: Boolean;
begin
  Result := (FStatus.Value = esPublished);
end;

class function TEvent.Props: TEvent;
begin
  Result := Prototype.Entity<TEvent>;
end;

{ TSpeaker }

class function TSpeaker.Props: TSpeaker;
begin
  Result := Prototype.Entity<TSpeaker>;
end;

{ TAttendee }

class function TAttendee.Props: TAttendee;
begin
  Result := Prototype.Entity<TAttendee>;
end;

{ TRegistration }

function TRegistration.Cancel(EventStartDate: TDateTime): Boolean;
begin
  // Rule: Cannot cancel if already canceled
  if TRegistrationStatus(FStatus) = rsCanceled then
    Exit(False);

  // Rule: Cannot cancel less than 24 hours before the event
  if HoursBetween(Now, EventStartDate) < 24 then
    Exit(False);

  FStatus := rsCanceled;
  FCanceledAt := Now;
  Result := True;
end;

class function TRegistration.Props: TRegistration;
begin
  Result := Prototype.Entity<TRegistration>;
end;

end.
