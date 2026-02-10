unit HelpDesk.Endpoints;

{***************************************************************************}
{                                                                           }
{           Web.HelpDesk - Minimal API Endpoints                            }
{                                                                           }
{           All routes use DI + Model Binding (FromRoute, FromHeader)       }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Web,         // TAppBuilder, IResult, Results, [FromRoute], [FromHeader]
  Dext.Web.Results, // Results class
  HelpDesk.Domain.Enums;

type
  // ==========================================================================
  // ENDPOINT BINDING DTOs (FromRoute, FromHeader, Body)
  // ==========================================================================

  /// <summary>Authenticated request with user from header</summary>
  TAuthenticatedRequest = record
    [FromHeader('X-User-Id')]
    UserId: Integer;
  end;

  /// <summary>Create Ticket: Header userId + Body fields</summary>
  TCreateTicketBindRequest = record
    [FromHeader('X-User-Id')]
    UserId: Integer;
    // Body fields
    Subject: string;
    Description: string;
    Priority: TTicketPriority;
    Channel: TTicketChannel;
  end;

  /// <summary>Get Ticket by ID from route</summary>
  TTicketIdRequest = record
    [FromRoute('id')]
    Id: Integer;
  end;

  /// <summary>Update Status: Route id + Header userId + Body</summary>
  TUpdateStatusBindRequest = record
    [FromRoute('id')]
    TicketId: Integer;
    [FromHeader('X-User-Id')]
    UserId: Integer;
    // Body fields
    NewStatus: TTicketStatus;
    Reason: string;
  end;

  /// <summary>Assign Ticket: Route id + Header userId + Body</summary>
  TAssignTicketBindRequest = record
    [FromRoute('id')]
    TicketId: Integer;
    [FromHeader('X-User-Id')]
    UserId: Integer;
    // Body fields
    AssigneeId: Integer;
  end;

  /// <summary>Add Comment: Route id + Header userId + Body</summary>
  TAddCommentBindRequest = record
    [FromRoute('id')]
    TicketId: Integer;
    [FromHeader('X-User-Id')]
    UserId: Integer;
    // Body fields
    Text: string;
    IsInternal: Boolean;
  end;

  TEndpoints = class
  public
    class procedure MapEndpoints(const Builder: TAppBuilder); static;
  end;

implementation

uses
  System.SysUtils,
  HelpDesk.Domain.Models,
  HelpDesk.Services;

{ TEndpoints }

class procedure TEndpoints.MapEndpoints(const Builder: TAppBuilder);
begin
  // ==========================================================================
  // AUTH ENDPOINTS
  // ==========================================================================

  Builder.MapPost<TLoginRequest, IUserService, IResult>('/api/auth/login',
    function(Req: TLoginRequest; Svc: IUserService): IResult
    begin
      try
        var Token := Svc.Login(Req);
        Result := Results.Ok<TTokenResponse>(Token);
      except
        on E: EAccessDenied do
          Result := Results.StatusCode(401);
        on E: Exception do
          Result := Results.BadRequest(E.Message);
      end;
    end);

  Builder.MapPost<TRegisterUserRequest, IUserService, IResult>('/api/auth/register',
    function(Req: TRegisterUserRequest; Svc: IUserService): IResult
    begin
      try
        var User := Svc.Register(Req);
        Result := Results.Created('/api/users/' + IntToStr(Integer(User.Id)), User);
      except
        on E: Exception do
          Result := Results.BadRequest(E.Message);
      end;
    end);

  // ==========================================================================
  // TICKET ENDPOINTS
  // ==========================================================================

  // List My Tickets (X-User-Id from header via Model Binding)
  Builder.MapGet<ITicketService, TAuthenticatedRequest, IResult>('/api/tickets',
    function(Svc: ITicketService; Req: TAuthenticatedRequest): IResult
    begin
      Result := Results.Ok(Svc.GetMyTickets(Req.UserId, True));
    end);

  // Create Ticket (Header + Body via mixed Model Binding)
  Builder.MapPost<TCreateTicketBindRequest, ITicketService, IResult>('/api/tickets',
    function(Req: TCreateTicketBindRequest; Svc: ITicketService): IResult
    var
      SvcReq: TCreateTicketRequest;
    begin
      SvcReq.Subject := Req.Subject;
      SvcReq.Description := Req.Description;
      SvcReq.Priority := Req.Priority;
      SvcReq.Channel := Req.Channel;

      try
        var Ticket := Svc.CreateTicket(SvcReq, Req.UserId);
        Result := Results.Created('/api/tickets/' + IntToStr(Integer(Ticket.Id)), Ticket);
      except
        on E: EUserNotFound do
          Result := Results.BadRequest(E.Message);
        on E: Exception do
          Result := Results.InternalServerError(E.Message);
      end;
    end);

  // Get Ticket by ID (Route param via Model Binding)
  Builder.MapGet<ITicketService, TTicketIdRequest, IResult>('/api/tickets/{id}',
    function(Svc: ITicketService; Req: TTicketIdRequest): IResult
    begin
      try
        Result := Results.Ok(Svc.GetTicket(Req.Id));
      except
        on E: ETicketNotFound do
          Result := Results.NotFound(E.Message);
        on E: Exception do
          Result := Results.BadRequest(E.Message);
      end;
    end);

  // Update Status (Route + Header + Body via mixed Model Binding)
  Builder.MapPost<TUpdateStatusBindRequest, ITicketService, IResult>('/api/tickets/{id}/status',
    function(Req: TUpdateStatusBindRequest; Svc: ITicketService): IResult
    begin
      try
        Result := Results.Ok(Svc.UpdateStatus(Req.TicketId, Req.NewStatus, Req.Reason, Req.UserId));
      except
        on E: EAccessDenied do
          Result := Results.StatusCode(401);
        on E: ETicketNotFound do
          Result := Results.NotFound(E.Message);
        on E: Exception do
          Result := Results.BadRequest(E.Message);
      end;
    end);

  // Assign Ticket (Route + Header + Body via mixed Model Binding)
  Builder.MapPost<TAssignTicketBindRequest, ITicketService, IResult>('/api/tickets/{id}/assign',
    function(Req: TAssignTicketBindRequest; Svc: ITicketService): IResult
    begin
      try
        Result := Results.Ok(Svc.AssignTicket(Req.TicketId, Req.AssigneeId, Req.UserId));
      except
        on E: EAccessDenied do
          Result := Results.StatusCode(401);
        on E: Exception do
          Result := Results.BadRequest(E.Message);
      end;
    end);

  // Add Comment (Route + Header + Body via mixed Model Binding)
  Builder.MapPost<TAddCommentBindRequest, ITicketService, IResult>('/api/tickets/{id}/comments',
    function(Req: TAddCommentBindRequest; Svc: ITicketService): IResult
    var
      SvcReq: TAddCommentRequest;
    begin
      SvcReq.Text := Req.Text;
      SvcReq.IsInternal := Req.IsInternal;
      try
        Result := Results.Ok(Svc.AddComment(Req.TicketId, SvcReq, Req.UserId));
      except
        on E: EAccessDenied do
          Result := Results.StatusCode(401);
        on E: Exception do
          Result := Results.BadRequest(E.Message);
      end;
    end);

  // ==========================================================================
  // METRICS ENDPOINT
  // ==========================================================================

  Builder.MapGet<ITicketService, IResult>('/api/metrics',
    function(Svc: ITicketService): IResult
    begin
      Result := Results.Ok<TMetricsResponse>(Svc.GetMetrics);
    end);
end;

end.
