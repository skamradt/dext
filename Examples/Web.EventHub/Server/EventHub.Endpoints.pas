unit EventHub.Endpoints;

{***************************************************************************}
{                                                                           }
{           Web.EventHub - Minimal API Endpoints                            }
{                                                                           }
{           All route definitions using the Minimal API pattern              }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Web;

type
  /// <summary>
  ///   Maps all Minimal API endpoints for the EventHub.
  ///   Called from TStartup.Configure.
  /// </summary>
  TEventHubEndpoints = class
  public
    class procedure MapEndpoints(Builder: TAppBuilder); static;
  end;

implementation

uses
  // 1. Delphi Units
  System.SysUtils,
  // 2. Dext Specialized Units
  Dext.Auth.JWT,
  Dext.Collections,
  // 3. Project Units
  EventHub.Domain.Models,
  EventHub.Services;

{ TEventHubEndpoints }

class procedure TEventHubEndpoints.MapEndpoints(Builder: TAppBuilder);
begin
  // ========================================================================
  // Health Check
  // ========================================================================
  Builder.MapGet<IResult>('/health',
    function: IResult
    begin
      Result := Results.Ok('EventHub API is healthy');
    end)
    .WithTag('Health');

  // ========================================================================
  // Auth Endpoints (Public)
  // ========================================================================
  Builder.MapPost<TLoginRequest, IJwtTokenHandler, IClaimsBuilder, IResult>('/api/auth/login',
    function(Req: TLoginRequest; Jwt: IJwtTokenHandler; Claims: IClaimsBuilder): IResult
    begin
      // Demo credentials: admin/admin123 or organizer/org123
      if ((Req.Username = 'admin') and (Req.Password = 'admin123')) then
      begin
        var Resp: TLoginResponse;
        Resp.Token := Jwt.GenerateToken(Claims.BuildClaims(Req.Username, 'Admin'));
        Resp.ExpiresIn := 120;
        Result := Results.Ok<TLoginResponse>(Resp);
      end
      else if ((Req.Username = 'organizer') and (Req.Password = 'org123')) then
      begin
        var Resp: TLoginResponse;
        Resp.Token := Jwt.GenerateToken(Claims.BuildClaims(Req.Username, 'Organizer'));
        Resp.ExpiresIn := 120;
        Result := Results.Ok<TLoginResponse>(Resp);
      end
      else
        Result := Results.StatusCode(401);
    end)
    .WithTag('Auth');

  // ========================================================================
  // Event Endpoints
  // ========================================================================

  // GET /api/events - List all published events (Public)
  Builder.MapGet<IEventService, IResult>('/api/events',
    function(Svc: IEventService): IResult
    begin
      Result := Results.Ok<IList<TEventResponse>>(Svc.GetAllPublished);
    end)
    .WithTag('Events');

  // GET /api/events/{id} - Get event by ID (Public)
  Builder.MapGet<IEventService, Integer, IResult>('/api/events/{id}',
    function(Svc: IEventService; Id: Integer): IResult
    begin
      try
        Result := Results.Ok<TEventResponse>(Svc.GetById(Id));
      except
        on E: Exception do
          Result := Results.NotFound(E.Message);
      end;
    end)
    .WithTag('Events');

  // POST /api/events - Create event (Auth required)
  Builder.MapPost<TCreateEventRequest, IEventService, IResult>('/api/events',
    function(Req: TCreateEventRequest; Svc: IEventService): IResult
    begin
      try
        Result := Results.Created<TEventResponse>('/api/events', Svc.CreateEvent(Req));
      except
        on E: Exception do
          Result := Results.BadRequest(E.Message);
      end;
    end)
    .WithTag('Events');

  // PUT /api/events/{id} - Update event (Auth required)
  Builder.MapPut<TUpdateEventRequest, IEventService, IResult>('/api/events/{id}',
    function(Req: TUpdateEventRequest; Svc: IEventService): IResult
    begin
      try
        Result := Results.Ok<TEventResponse>(Svc.UpdateEvent(Req));
      except
        on E: Exception do
          Result := Results.BadRequest(E.Message);
      end;
    end)
    .WithTag('Events');

  // POST /api/events/{id}/publish - Publish event (Auth required)
  Builder.MapPost<TPublishEventRequest, IEventService, IResult>('/api/events/{id}/publish',
    function(Req: TPublishEventRequest; Svc: IEventService): IResult
    begin
      try
        Result := Results.Ok<TEventResponse>(Svc.PublishEvent(Req.EventId));
      except
        on E: Exception do
          Result := Results.BadRequest(E.Message);
      end;
    end)
    .WithTag('Events');

  // POST /api/events/{id}/cancel - Cancel event (Auth required)
  Builder.MapPost<TCancelEventRequest, IEventService, IResult>('/api/events/{id}/cancel',
    function(Req: TCancelEventRequest; Svc: IEventService): IResult
    begin
      try
        Result := Results.Ok<TEventResponse>(Svc.CancelEvent(Req.EventId));
      except
        on E: Exception do
          Result := Results.BadRequest(E.Message);
      end;
    end)
    .WithTag('Events');

  // GET /api/events/metrics - Dashboard metrics (Auth required)
  Builder.MapGet<IEventService, IResult>('/api/events/metrics',
    function(Svc: IEventService): IResult
    begin
      Result := Results.Ok<TDashboardMetrics>(Svc.GetMetrics);
    end)
    .WithTag('Events');

  // ========================================================================
  // Speaker Endpoints
  // ========================================================================

  // GET /api/events/{eventId}/speakers - List speakers for an event (Public)
  Builder.MapGet<ISpeakerService, Integer, IResult>('/api/events/{eventId}/speakers',
    function(Svc: ISpeakerService; EventId: Integer): IResult
    begin
      Result := Results.Ok<IList<TSpeakerResponse>>(Svc.GetByEvent(EventId));
    end)
    .WithTag('Speakers');

  // POST /api/events/{eventId}/speakers - Add speaker (Auth required)
  Builder.MapPost<TAddSpeakerRequest, ISpeakerService, IResult>('/api/events/{eventId}/speakers',
    function(Req: TAddSpeakerRequest; Svc: ISpeakerService): IResult
    begin
      try
        Result := Results.Created<TSpeakerResponse>('/api/events/' + IntToStr(Req.EventId) + '/speakers',
          Svc.AddSpeaker(Req));
      except
        on E: Exception do
          Result := Results.BadRequest(E.Message);
      end;
    end)
    .WithTag('Speakers');

  // ========================================================================
  // Attendee Endpoints (Public)
  // ========================================================================

  // POST /api/attendees - Register new attendee
  Builder.MapPost<TRegisterAttendeeRequest, IAttendeeService, IResult>('/api/attendees',
    function(Req: TRegisterAttendeeRequest; Svc: IAttendeeService): IResult
    begin
      try
        Result := Results.Created<TAttendeeResponse>('/api/attendees', Svc.RegisterAttendee(Req));
      except
        on E: Exception do
          Result := Results.BadRequest(E.Message);
      end;
    end)
    .WithTag('Attendees');

  // GET /api/attendees/{id} - Get attendee by ID
  Builder.MapGet<IAttendeeService, Integer, IResult>('/api/attendees/{id}',
    function(Svc: IAttendeeService; Id: Integer): IResult
    begin
      try
        Result := Results.Ok<TAttendeeResponse>(Svc.GetById(Id));
      except
        on E: Exception do
          Result := Results.NotFound(E.Message);
      end;
    end)
    .WithTag('Attendees');

  // ========================================================================
  // Registration Endpoints
  // ========================================================================

  // POST /api/registrations - Create registration (auto Confirmed or WaitList)
  Builder.MapPost<TCreateRegistrationRequest, IRegistrationService, IResult>('/api/registrations',
    function(Req: TCreateRegistrationRequest; Svc: IRegistrationService): IResult
    begin
      try
        Result := Results.Created<TRegistrationResponse>('/api/registrations',
          Svc.CreateRegistration(Req));
      except
        on E: Exception do
          Result := Results.BadRequest(E.Message);
      end;
    end)
    .WithTag('Registrations');

  // POST /api/registrations/{id}/cancel - Cancel registration (24h rule + auto-promote)
  Builder.MapPost<TCancelRegistrationRequest, IRegistrationService, IResult>('/api/registrations/{id}/cancel',
    function(Req: TCancelRegistrationRequest; Svc: IRegistrationService): IResult
    begin
      try
        Result := Results.Ok<TRegistrationResponse>(Svc.CancelRegistration(Req.RegistrationId));
      except
        on E: Exception do
          Result := Results.BadRequest(E.Message);
      end;
    end)
    .WithTag('Registrations');

  // GET /api/events/{eventId}/registrations - List registrations for event
  Builder.MapGet<IRegistrationService, Integer, IResult>('/api/events/{eventId}/registrations',
    function(Svc: IRegistrationService; EventId: Integer): IResult
    begin
      Result := Results.Ok<IList<TRegistrationResponse>>(Svc.GetByEvent(EventId));
    end)
    .WithTag('Registrations');

  // GET /api/attendees/{attendeeId}/registrations - List registrations for attendee
  Builder.MapGet<IRegistrationService, Integer, IResult>('/api/attendees/{attendeeId}/registrations',
    function(Svc: IRegistrationService; AttendeeId: Integer): IResult
    begin
      Result := Results.Ok<IList<TRegistrationResponse>>(Svc.GetByAttendee(AttendeeId));
    end)
    .WithTag('Registrations');
end;

end.
