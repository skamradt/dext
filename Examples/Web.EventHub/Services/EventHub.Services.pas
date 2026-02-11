unit EventHub.Services;

{***************************************************************************}
{                                                                           }
{           Web.EventHub - Business Services                                }
{                                                                           }
{           Service interfaces and implementations with business rules      }
{                                                                           }
{***************************************************************************}

interface

uses
  // 1. Delphi Units
  System.SysUtils,
  System.DateUtils,
  // 2. Dext Specialized Units
  Dext.Collections,
  Dext.Core.SmartTypes,
  Dext.Auth.JWT,
  Dext.Auth.Middleware,
  // 3. Dext Facades
  Dext,
  Dext.Entity,
  Dext.Web,
  // 4. Project Units
  EventHub.Data.Context,
  EventHub.Domain.Entities,
  EventHub.Domain.Enums,
  EventHub.Domain.Models;

type
  { ======================================================================== }
  { Service Interfaces                                                       }
  { ======================================================================== }

  {$M+}
  IEventService = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function GetAllPublished: IList<TEventResponse>;
    function GetById(Id: Integer): TEventResponse;
    function CreateEvent(const Req: TCreateEventRequest): TEventResponse;
    function UpdateEvent(const Req: TUpdateEventRequest): TEventResponse;
    function PublishEvent(Id: Integer): TEventResponse;
    function CancelEvent(Id: Integer): TEventResponse;
    function GetMetrics: TDashboardMetrics;
  end;

  ISpeakerService = interface
    ['{B2C3D4E5-F6A7-8901-BCDE-F12345678901}']
    function GetByEvent(EventId: Integer): IList<TSpeakerResponse>;
    function AddSpeaker(const Req: TAddSpeakerRequest): TSpeakerResponse;
  end;

  IAttendeeService = interface
    ['{C3D4E5F6-A7B8-9012-CDEF-123456789012}']
    function RegisterAttendee(const Req: TRegisterAttendeeRequest): TAttendeeResponse;
    function GetById(Id: Integer): TAttendeeResponse;
  end;

  IRegistrationService = interface
    ['{D4E5F6A7-B8C9-0123-DEF1-234567890123}']
    function CreateRegistration(const Req: TCreateRegistrationRequest): TRegistrationResponse;
    function CancelRegistration(Id: Integer): TRegistrationResponse;
    function GetByEvent(EventId: Integer): IList<TRegistrationResponse>;
    function GetByAttendee(AttendeeId: Integer): IList<TRegistrationResponse>;
  end;
  {$M-}

  { ======================================================================== }
  { Service Implementations                                                  }
  { ======================================================================== }

  TEventService = class(TInterfacedObject, IEventService)
  private
    FDb: TEventHubDbContext;
    function MapToResponse(Event: TEvent): TEventResponse;
  public
    constructor Create(Db: TEventHubDbContext);
    function GetAllPublished: IList<TEventResponse>;
    function GetById(Id: Integer): TEventResponse;
    function CreateEvent(const Req: TCreateEventRequest): TEventResponse;
    function UpdateEvent(const Req: TUpdateEventRequest): TEventResponse;
    function PublishEvent(Id: Integer): TEventResponse;
    function CancelEvent(Id: Integer): TEventResponse;
    function GetMetrics: TDashboardMetrics;
  end;

  TSpeakerService = class(TInterfacedObject, ISpeakerService)
  private
    FDb: TEventHubDbContext;
  public
    constructor Create(Db: TEventHubDbContext);
    function GetByEvent(EventId: Integer): IList<TSpeakerResponse>;
    function AddSpeaker(const Req: TAddSpeakerRequest): TSpeakerResponse;
  end;

  TAttendeeService = class(TInterfacedObject, IAttendeeService)
  private
    FDb: TEventHubDbContext;
  public
    constructor Create(Db: TEventHubDbContext);
    function RegisterAttendee(const Req: TRegisterAttendeeRequest): TAttendeeResponse;
    function GetById(Id: Integer): TAttendeeResponse;
  end;

  TRegistrationService = class(TInterfacedObject, IRegistrationService)
  private
    FDb: TEventHubDbContext;
    function MapToResponse(Reg: TRegistration): TRegistrationResponse;
    procedure PromoteFromWaitList(EventId: Integer);
  public
    constructor Create(Db: TEventHubDbContext);
    function CreateRegistration(const Req: TCreateRegistrationRequest): TRegistrationResponse;
    function CancelRegistration(Id: Integer): TRegistrationResponse;
    function GetByEvent(EventId: Integer): IList<TRegistrationResponse>;
    function GetByAttendee(AttendeeId: Integer): IList<TRegistrationResponse>;
  end;


  { ======================================================================== }
  { Claims Builder                                                           }
  { ======================================================================== }

  IClaimsBuilder = interface
    ['{F6A7B8C9-D0E1-2345-F123-456789012345}']
    function BuildClaims(const Username, Role: string): TArray<TClaim>;
  end;

  TClaimsBuilder = class(TInterfacedObject, IClaimsBuilder)
  public
    function BuildClaims(const Username, Role: string): TArray<TClaim>;
  end;

implementation

{ ======================================================================== }
{ TEventService                                                            }
{ ======================================================================== }

constructor TEventService.Create(Db: TEventHubDbContext);
begin
  inherited Create;
  FDb := Db;
end;

function TEventService.MapToResponse(Event: TEvent): TEventResponse;
var
  SpeakerCount, ConfirmedCount: Integer;
  Venue: TVenue;
begin
  // Count speakers for this event
  var s := TSpeaker.Props;
  SpeakerCount := FDb.Speakers.Where(s.EventId = Event.Id.Value).Count;

  // Count confirmed registrations
  var r := TRegistration.Props;
  ConfirmedCount := FDb.Registrations
    .Where((r.EventId = Event.Id.Value) and
           (r.Status = rsConfirmed))
    .Count;

  // Get venue name
  Venue := FDb.Venues.Find(Event.VenueId.Value);

  Result.Id := Event.Id;
  Result.VenueId := Event.VenueId;
  if Assigned(Venue) then
    Result.VenueName := Venue.Name
  else
    Result.VenueName := '';
  Result.Title := Event.Title;
  Result.Description := Event.Description;
  Result.StartDate := Event.StartDate;
  Result.EndDate := Event.EndDate;
  Result.MaxCapacity := Event.MaxCapacity;
  Result.ConfirmedCount := ConfirmedCount;
  Result.AvailableSlots := Integer(Result.MaxCapacity) - ConfirmedCount;
  if Result.AvailableSlots < 0 then
    Result.AvailableSlots := 0;
  Result.Status := Event.Status.Value;
  Result.SpeakerCount := SpeakerCount;
end;

function TEventService.GetAllPublished: IList<TEventResponse>;
var
  Events: IList<TEvent>;
  Event: TEvent;
begin
  Result := TCollections.CreateList<TEventResponse>;
  var e := TEvent.Props;
  Events := FDb.Events.Where(e.Status = esPublished).ToList;
  for Event in Events do
    Result.Add(MapToResponse(Event));
end;

function TEventService.GetById(Id: Integer): TEventResponse;
var
  Event: TEvent;
begin
  Event := FDb.Events.Find(Id);
  if not Assigned(Event) then
    raise Exception.CreateFmt('Event with ID %d not found', [Id]);
  Result := MapToResponse(Event);
end;

function TEventService.CreateEvent(const Req: TCreateEventRequest): TEventResponse;
var
  Event: TEvent;
  Venue: TVenue;
begin
  // Validate venue exists
  Venue := FDb.Venues.Find(Req.VenueId);
  if not Assigned(Venue) then
    raise Exception.CreateFmt('Venue with ID %d not found', [Req.VenueId]);

  // Validate capacity
  if Req.MaxCapacity > Integer(Venue.Capacity) then
    raise Exception.CreateFmt('MaxCapacity (%d) exceeds venue capacity (%d)',
      [Req.MaxCapacity, Integer(Venue.Capacity)]);

  // Validate dates
  if Req.EndDate <= Req.StartDate then
    raise Exception.Create('EndDate must be after StartDate');

  Event := TEvent.Create;
  Event.VenueId := Venue.Id;
  Event.Title := Req.Title;
  Event.Description := Req.Description;
  Event.StartDate := Req.StartDate;
  Event.EndDate := Req.EndDate;
  Event.MaxCapacity := Req.MaxCapacity;
  Event.Status := esDraft;  // Always starts as Draft

  FDb.Events.Add(Event);
  FDb.SaveChanges;
  Result := MapToResponse(Event);
end;

function TEventService.UpdateEvent(const Req: TUpdateEventRequest): TEventResponse;
var
  Event: TEvent;
begin
  Event := FDb.Events.Find(Req.EventId);
  if not Assigned(Event) then
    raise Exception.CreateFmt('Event with ID %d not found', [Req.EventId]);

  // Cannot update canceled or finished events
  if Event.Status.Value in [esCanceled, esFinished] then
    raise Exception.Create('Cannot update a canceled or finished event');

  Event.Title := Req.Title;
  Event.Description := Req.Description;
  Event.StartDate := Req.StartDate;
  Event.EndDate := Req.EndDate;
  Event.MaxCapacity := Req.MaxCapacity;
  
  FDb.Events.Update(Event);
  FDb.SaveChanges;
  Result := MapToResponse(Event);
  // Event.Free;
end;

function TEventService.PublishEvent(Id: Integer): TEventResponse;
var
  Event: TEvent;
begin
  Event := FDb.Events.Find(Id);
  if not Assigned(Event) then
    raise Exception.CreateFmt('Event with ID %d not found', [Id]);

  if Event.Status.Value <> esDraft then
    raise Exception.Create('Only Draft events can be published');

  Event.Status := esPublished;
  FDb.Events.Update(Event);
  FDb.SaveChanges;
  Result := MapToResponse(Event);
  // Event.Free;
end;

function TEventService.CancelEvent(Id: Integer): TEventResponse;
var
  Event: TEvent;
begin
  Event := FDb.Events.Find(Id);
  if not Assigned(Event) then
    raise Exception.CreateFmt('Event with ID %d not found', [Id]);

  if Event.Status.Value = esCanceled then
    raise Exception.Create('Event is already canceled');

  if Event.Status.Value = esFinished then
    raise Exception.Create('Cannot cancel a finished event');

  Event.Status := esCanceled;
  FDb.Events.Update(Event);
  FDb.SaveChanges;
  Result := MapToResponse(Event);
//   Event.Free;
end;

function TEventService.GetMetrics: TDashboardMetrics;
begin
  var e := TEvent.Props;
  var r := TRegistration.Props;
  Result.TotalEvents := FDb.Events.QueryAll.Count;
  Result.PublishedEvents := FDb.Events.Where(e.Status = esPublished).Count;
  Result.TotalAttendees := FDb.Attendees.QueryAll.Count;
  Result.TotalRegistrations := FDb.Registrations.QueryAll.Count;
  Result.ConfirmedRegistrations := FDb.Registrations
    .Where(r.Status = rsConfirmed).Count;
  Result.WaitListRegistrations := FDb.Registrations
    .Where(r.Status = rsWaitList).Count;
end;

{ ======================================================================== }
{ TSpeakerService                                                          }
{ ======================================================================== }

constructor TSpeakerService.Create(Db: TEventHubDbContext);
begin
  inherited Create;
  FDb := Db;
end;

function TSpeakerService.GetByEvent(EventId: Integer): IList<TSpeakerResponse>;
var
  Speakers: IList<TSpeaker>;
  Speaker: TSpeaker;
  Resp: TSpeakerResponse;
begin
  Result := TCollections.CreateList<TSpeakerResponse>;
  var s := TSpeaker.Props;
  Speakers := FDb.Speakers.Where(s.EventId = EventId).ToList;
  for Speaker in Speakers do
  begin
    Resp.Id := Speaker.Id;
    Resp.EventId := Speaker.EventId;
    Resp.Name := Speaker.Name;
    Resp.Bio := Speaker.Bio;
    Resp.Email := Speaker.Email;
    Result.Add(Resp);
  end;
end;

function TSpeakerService.AddSpeaker(const Req: TAddSpeakerRequest): TSpeakerResponse;
var
  Event: TEvent;
  Speaker: TSpeaker;
begin
  // Validate event exists
  Event := FDb.Events.Find(Req.EventId);
  if not Assigned(Event) then
    raise Exception.CreateFmt('Event with ID %d not found', [Req.EventId]);

  Speaker := TSpeaker.Create;
  Speaker.EventId := Event.Id;
  Speaker.Name := Req.Name;
  Speaker.Bio := Req.Bio;
  Speaker.Email := Req.Email;

  FDb.Speakers.Add(Speaker);
  FDb.SaveChanges;

  // Refresh
  var s := TSpeaker.Props;
  Speaker := FDb.Speakers.Where(
    (s.EventId = Req.EventId) and
    (s.Email = Req.Email)
  ).FirstOrDefault;

  Result.Id := Speaker.Id;
  Result.EventId := Speaker.EventId;
  Result.Name := Speaker.Name;
  Result.Bio := Speaker.Bio;
  Result.Email := Speaker.Email;
end;

{ ======================================================================== }
{ TAttendeeService                                                         }
{ ======================================================================== }

constructor TAttendeeService.Create(Db: TEventHubDbContext);
begin
  inherited Create;
  FDb := Db;
end;

function TAttendeeService.RegisterAttendee(const Req: TRegisterAttendeeRequest): TAttendeeResponse;
var
  Existing: TAttendee;
  Attendee: TAttendee;
begin
  // Check if email already registered
  var a := TAttendee.Props;
  Existing := FDb.Attendees.Where(a.Email = Req.Email).FirstOrDefault;
  if Assigned(Existing) then
    raise Exception.CreateFmt('An attendee with email "%s" already exists', [Req.Email]);

  Attendee := TAttendee.Create;
  Attendee.Name := Req.Name;
  Attendee.Email := Req.Email;
  Attendee.Phone := Req.Phone;

  FDb.Attendees.Add(Attendee);
  FDb.SaveChanges;

  // Refresh
  var a2 := TAttendee.Props;
  Attendee := FDb.Attendees.Where(a2.Email = Req.Email).FirstOrDefault;

  Result.Id := Attendee.Id;
  Result.Name := Attendee.Name;
  Result.Email := Attendee.Email;
  Result.Phone := Attendee.Phone;
end;

function TAttendeeService.GetById(Id: Integer): TAttendeeResponse;
var
  Attendee: TAttendee;
begin
  Attendee := FDb.Attendees.Find(Id);
  if not Assigned(Attendee) then
    raise Exception.CreateFmt('Attendee with ID %d not found', [Id]);

  Result.Id := Attendee.Id;
  Result.Name := Attendee.Name;
  Result.Email := Attendee.Email;
  Result.Phone := Attendee.Phone;
end;

{ ======================================================================== }
{ TRegistrationService                                                     }
{ ======================================================================== }

constructor TRegistrationService.Create(Db: TEventHubDbContext);
begin
  inherited Create;
  FDb := Db;
end;

function TRegistrationService.MapToResponse(Reg: TRegistration): TRegistrationResponse;
var
  Event: TEvent;
  Attendee: TAttendee;
begin
  Event := FDb.Events.Find(Reg.EventId.Value);
  Attendee := FDb.Attendees.Find(Reg.AttendeeId.Value);

  Result.Id := Reg.Id;
  Result.EventId := Reg.EventId;
  if Assigned(Event) then
    Result.EventTitle := Event.Title
  else
    Result.EventTitle := '';
  Result.AttendeeId := Reg.AttendeeId;
  if Assigned(Attendee) then
    Result.AttendeeName := Attendee.Name
  else
    Result.AttendeeName := '';
  Result.Status := Reg.Status.Value;
  Result.RegisteredAt := Reg.RegisteredAt;
end;

function TRegistrationService.CreateRegistration(const Req: TCreateRegistrationRequest): TRegistrationResponse;
var
  Event: TEvent;
  Attendee: TAttendee;
  ExistingReg: TRegistration;
  ConfirmedCount: Integer;
  Reg: TRegistration;
begin
  // Validate event exists
  Event := FDb.Events.Find(Req.EventId);
  if not Assigned(Event) then
    raise Exception.CreateFmt('Event with ID %d not found', [Req.EventId]);

  // Rule 1: Cannot register for Draft or Canceled events
  if not Event.CanRegister then
    raise Exception.Create('This event is not accepting registrations');

  // Validate attendee exists
  Attendee := FDb.Attendees.Find(Req.AttendeeId);
  if not Assigned(Attendee) then
    raise Exception.CreateFmt('Attendee with ID %d not found', [Req.AttendeeId]);

  // Check for duplicate active registration
  var r := TRegistration.Props;
  ExistingReg := FDb.Registrations
    .Where((r.EventId = Req.EventId) and
           (r.AttendeeId = Req.AttendeeId) and
           (r.Status <> rsCanceled))
    .FirstOrDefault;
  if Assigned(ExistingReg) then
    raise Exception.Create('Attendee is already registered for this event');

  // Rule 2: If MaxCapacity reached, go to WaitList
  ConfirmedCount := FDb.Registrations
    .Where((r.EventId = Req.EventId) and
           (r.Status = rsConfirmed))
    .Count;

  Reg := TRegistration.Create;
  Reg.EventId := Event.Id;
  Reg.AttendeeId := Attendee.Id;

  if ConfirmedCount >= Integer(Event.MaxCapacity) then
    Reg.Status := rsWaitList   // Rule 2: Auto-WaitList when full
  else
    Reg.Status := rsConfirmed;

  FDb.Registrations.Add(Reg);
  FDb.SaveChanges;

  // Refresh
  var r2 := TRegistration.Props;
  Reg := FDb.Registrations
    .Where((r2.EventId = Req.EventId) and
           (r2.AttendeeId = Req.AttendeeId) and
           (r2.Status <> rsCanceled))
    .FirstOrDefault;

  Result := MapToResponse(Reg);
end;

function TRegistrationService.CancelRegistration(Id: Integer): TRegistrationResponse;
var
  Reg: TRegistration;
  Event: TEvent;
begin
  Reg := FDb.Registrations.Find(Id);
  if not Assigned(Reg) then
    raise Exception.CreateFmt('Registration with ID %d not found', [Id]);

  Event := FDb.Events.Find(Reg.EventId.Value);
  if not Assigned(Event) then
    raise Exception.Create('Associated event not found');

  // Rule 4: Cannot cancel less than 24h before event
  if not Reg.Cancel(Event.StartDate.Value) then
    raise Exception.Create('Cannot cancel: registration is already canceled or event starts in less than 24 hours');

  FDb.SaveChanges;

  // Rule 3: Promote first person from WaitList
  if Reg.Status.Value = rsCanceled then
    PromoteFromWaitList(Reg.EventId.Value);

  Result := MapToResponse(Reg);
end;

procedure TRegistrationService.PromoteFromWaitList(EventId: Integer);
var
  NextInLine: TRegistration;
begin
  // Find the first WaitList registration (FIFO by RegisteredAt)
  var r := TRegistration.Props;
  NextInLine := FDb.Registrations
    .Where((r.EventId = EventId) and
           (r.Status = rsWaitList))
    .OrderBy(r.RegisteredAt.Asc)
    .FirstOrDefault;

  if Assigned(NextInLine) then
  begin
    NextInLine.Status := rsConfirmed;
    FDb.SaveChanges;
  end;
end;

function TRegistrationService.GetByEvent(EventId: Integer): IList<TRegistrationResponse>;
var
  Regs: IList<TRegistration>;
  Reg: TRegistration;
begin
  Result := TCollections.CreateList<TRegistrationResponse>;
  var r := TRegistration.Props;
  Regs := FDb.Registrations
    .Where(r.EventId = EventId)
    .ToList;
  for Reg in Regs do
    Result.Add(MapToResponse(Reg));
end;

function TRegistrationService.GetByAttendee(AttendeeId: Integer): IList<TRegistrationResponse>;
var
  Regs: IList<TRegistration>;
  Reg: TRegistration;
begin
  Result := TCollections.CreateList<TRegistrationResponse>;
  var r := TRegistration.Props;
  Regs := FDb.Registrations
    .Where(r.AttendeeId = AttendeeId)
    .ToList;
  for Reg in Regs do
    Result.Add(MapToResponse(Reg));
end;

{ ======================================================================== }
{ TClaimsBuilder                                                           }
{ ======================================================================== }

function TClaimsBuilder.BuildClaims(const Username, Role: string): TArray<TClaim>;
begin
  SetLength(Result, 3);
  Result[0] := TClaim.Create(TClaimTypes.NameIdentifier, Username);
  Result[1] := TClaim.Create(TClaimTypes.Role, Role);
  Result[2] := TClaim.Create(TClaimTypes.Name, Username);
end;

end.
