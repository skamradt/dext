unit HelpDesk.Services;

{***************************************************************************}
{                                                                           }
{           Web.HelpDesk - Business Services                                }
{                                                                           }
{           Core logic for Tickets, Users, and SLA calculations             }
{                                                                           }
{***************************************************************************}

interface

uses
  System.SysUtils,
  System.TypInfo,
  Dext.Collections,  // IList<T>
  Dext,              // Facade
  Dext.Entity,       // Facade: TDbContext
  HelpDesk.Domain.Entities,
  HelpDesk.Domain.Enums,
  HelpDesk.Domain.Models,
  HelpDesk.Data.Context;

type
  EHelpDeskException = class(Exception);
  ETicketNotFound = class(EHelpDeskException);
  EUserNotFound = class(EHelpDeskException);
  EAccessDenied = class(EHelpDeskException);

  {$M+}
  ITicketService = interface
    ['{A1B2C3D4-1111-2222-3333-444455556666}']
    function CreateTicket(const Request: TCreateTicketRequest; RequesterId: Integer): TTicket;
    function GetTicket(Id: Integer): TTicket;
    function GetMyTickets(UserId: Integer; AsRequester: Boolean): IList<TTicket>;
    function UpdateStatus(Id: Integer; NewStatus: TTicketStatus; const Reason: string; ByUser: Integer): TTicket;
    function AssignTicket(Id: Integer; AgentId: Integer; ByUser: Integer): TTicket;
    function AddComment(TicketId: Integer; const Request: TAddCommentRequest; AuthorId: Integer): TComment;
    function GetMetrics: TMetricsResponse;
  end;

  IUserService = interface
    ['{B2C3D4E5-2222-3333-4444-555566667777}']
    function Login(const Request: TLoginRequest): TTokenResponse;
    function Register(const Request: TRegisterUserRequest): TUser;
    function GetUser(Id: Integer): TUser;
    function VerifyAgentAccess(UserId: Integer): Boolean;
  end;
  {$M-}

  TTicketService = class(TInterfacedObject, ITicketService)
  private
    FDb: THelpDeskContext;
    FUserService: IUserService;
  public
    constructor Create(Db: THelpDeskContext; UserService: IUserService);
    function CreateTicket(const Request: TCreateTicketRequest; RequesterId: Integer): TTicket;
    function GetTicket(Id: Integer): TTicket;
    function GetMyTickets(UserId: Integer; AsRequester: Boolean): IList<TTicket>;
    function UpdateStatus(Id: Integer; NewStatus: TTicketStatus; const Reason: string; ByUser: Integer): TTicket;
    function AssignTicket(Id: Integer; AgentId: Integer; ByUser: Integer): TTicket;
    function AddComment(TicketId: Integer; const Request: TAddCommentRequest; AuthorId: Integer): TComment;
    function GetMetrics: TMetricsResponse;
  end;

  TUserService = class(TInterfacedObject, IUserService)
  private
    FDb: THelpDeskContext;
  public
    constructor Create(Db: THelpDeskContext);
    function Login(const Request: TLoginRequest): TTokenResponse;
    function Register(const Request: TRegisterUserRequest): TUser;
    function GetUser(Id: Integer): TUser;
    function VerifyAgentAccess(UserId: Integer): Boolean;
  end;

implementation

uses
  System.DateUtils,
  Dext.Core.SmartTypes; // Smart Props

{ TTicketService }

constructor TTicketService.Create(Db: THelpDeskContext; UserService: IUserService);
begin
  inherited Create;
  FDb := Db;
  FUserService := UserService;
end;

function TTicketService.CreateTicket(const Request: TCreateTicketRequest; RequesterId: Integer): TTicket;
var
  Requester: TUser;
begin
  Requester := FDb.Users.Find(RequesterId);
  if Requester = nil then
    raise EUserNotFound.CreateFmt('Requester %d not found', [RequesterId]);

  Result := TTicket.Create;
  Result.RequesterId := RequesterId;
  Result.Subject := Request.Subject;
  Result.Description := Request.Description;
  Result.Priority := Request.Priority;
  Result.Channel := Request.Channel;
  Result.Status := tsNew;

  if Length(Request.Tags) > 0 then
    Result.Tags := '[' + string.Join(',', Request.Tags) + ']'
  else
    Result.Tags := '[]';

  Result.CreatedAt := Now;
  Result.DueDate := Result.CalculateSLA(Result.CreatedAt);

  FDb.Tickets.Add(Result);
  FDb.SaveChanges;
end;

function TTicketService.GetTicket(Id: Integer): TTicket;
begin
  Result := FDb.Tickets.Find(Id);
  if Result = nil then
    raise ETicketNotFound.CreateFmt('Ticket %d not found', [Id]);
end;

function TTicketService.GetMyTickets(UserId: Integer; AsRequester: Boolean): IList<TTicket>;
var
  t: TTicket;
begin
  t := TTicket.Props;
  if AsRequester then
    Result := FDb.Tickets.Where(t.RequesterId = UserId).OrderBy(t.CreatedAt.Desc).ToList
  else
    Result := FDb.Tickets.Where(t.AssigneeId = UserId).OrderBy(t.Priority.Desc).ToList;
end;

function TTicketService.UpdateStatus(Id: Integer; NewStatus: TTicketStatus; const Reason: string; ByUser: Integer): TTicket;
var
  Ticket: TTicket;
  LogComment: TComment;
begin
  Ticket := GetTicket(Id);

  // Permission: Only Agents can resolve/progress tickets
  if (NewStatus in [tsInProgress, tsResolved]) and (not FUserService.VerifyAgentAccess(ByUser)) then
    raise EAccessDenied.Create('Only agents can resolve tickets');

  Ticket.Status := NewStatus;
  if NewStatus in [tsResolved, tsClosed, tsRejected] then
    Ticket.ClosedAt := Now;

  // Log status change as internal comment
  LogComment := TComment.Create;
  LogComment.TicketId := Ticket.Id;
  LogComment.AuthorId := ByUser;
  LogComment.Text := Format('Status changed to %s. Reason: %s',
    [GetEnumName(TypeInfo(TTicketStatus), Integer(NewStatus)), Reason]);
  LogComment.IsInternal := True;
  LogComment.CreatedAt := Now;

  FDb.Tickets.Update(Ticket);
  FDb.Comments.Add(LogComment);
  FDb.SaveChanges;

  Result := Ticket;
end;

function TTicketService.AssignTicket(Id: Integer; AgentId: Integer; ByUser: Integer): TTicket;
var
  Ticket: TTicket;
  Agent: TUser;
begin
  if not FUserService.VerifyAgentAccess(ByUser) then
    raise EAccessDenied.Create('Only agents can assign tickets');

  Ticket := GetTicket(Id);
  Agent := FDb.Users.Find(AgentId);

  if Agent = nil then
    raise EUserNotFound.Create('Target agent not found');

  if TUserRole(Agent.Role) = urCustomer then
    raise EAccessDenied.Create('Cannot assign ticket to a customer');

  Ticket.AssigneeId := AgentId;
  Ticket.Status := tsOpen;

  FDb.Tickets.Update(Ticket);
  FDb.SaveChanges;

  Result := Ticket;
end;

function TTicketService.AddComment(TicketId: Integer; const Request: TAddCommentRequest; AuthorId: Integer): TComment;
begin
  GetTicket(TicketId); // Validates existence

  // Customers cannot post internal notes
  if Request.IsInternal and (not FUserService.VerifyAgentAccess(AuthorId)) then
    raise EAccessDenied.Create('Customers cannot post internal notes');

  Result := TComment.Create;
  Result.TicketId := TicketId;
  Result.AuthorId := AuthorId;
  Result.Text := Request.Text;
  Result.IsInternal := Request.IsInternal;
  Result.CreatedAt := Now;

  FDb.Comments.Add(Result);
  FDb.SaveChanges;
end;

function TTicketService.GetMetrics: TMetricsResponse;
var
  t: TTicket;
  Tickets: IList<TTicket>;
  Ticket: TTicket;
  TotalHours: Double;
begin
  t := TTicket.Props;

  // 1. Total Open
  Result.TotalOpen := FDb.Tickets
    .Where(t.Status in [tsNew, tsOpen, tsInProgress])
    .Count;

  // 2. Overdue (DueDate < Now and still open)
  Result.OverdueCount := FDb.Tickets
    .Where((t.Status in [tsNew, tsOpen, tsInProgress]) and (t.DueDate < Now))
    .Count;

  // 3. Avg Resolution Time (in-memory calculation)
  Tickets := FDb.Tickets
    .Where(t.Status = tsResolved)
    .ToList;

  TotalHours := 0;
  for Ticket in Tickets do
  begin
    if (Ticket.ClosedAt > Ticket.CreatedAt) then
      TotalHours := TotalHours + HoursBetween(Ticket.ClosedAt, Ticket.CreatedAt);
  end;

  if Tickets.Count > 0 then
    Result.AvgResolutionHours := TotalHours / Tickets.Count
  else
    Result.AvgResolutionHours := 0;
end;

{ TUserService }

constructor TUserService.Create(Db: THelpDeskContext);
begin
  inherited Create;
  FDb := Db;
end;

function TUserService.Login(const Request: TLoginRequest): TTokenResponse;
var
  u: TUser;
  User: TUser;
begin
  u := TUser.Props;
  User := FDb.Users
    .Where((u.Email = Request.Email) and (u.PasswordHash = Request.Password))
    .FirstOrDefault;

  if User = nil then
    raise EAccessDenied.Create('Invalid credentials');

  // Mock Token: In real app use JwtTokenHandler
  Result.AccessToken := 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.mock-token';
  Result.TokenType := 'Bearer';
  Result.ExpiresIn := 3600;
  Result.UserRole := GetEnumName(TypeInfo(TUserRole), Integer(TUserRole(User.Role)));
end;

function TUserService.Register(const Request: TRegisterUserRequest): TUser;
var
  u: TUser;
begin
  u := TUser.Props;
  if FDb.Users.Where(u.Email = Request.Email).Any then
    raise EHelpDeskException.Create('Email already registered');

  Result := TUser.Create;
  Result.Name := Request.FullName;
  Result.Email := Request.Email;
  Result.PasswordHash := Request.Password;
  Result.Role := urCustomer; // Default role
  Result.IsActive := True;
  Result.CreatedAt := Now;

  FDb.Users.Add(Result);
  FDb.SaveChanges;
end;

function TUserService.GetUser(Id: Integer): TUser;
begin
  Result := FDb.Users.Find(Id);
  if Result = nil then
    raise EUserNotFound.CreateFmt('User %d not found', [Id]);
end;

function TUserService.VerifyAgentAccess(UserId: Integer): Boolean;
var
  User: TUser;
begin
  User := FDb.Users.Find(UserId);
  Result := (User <> nil) and (TUserRole(User.Role) in [urAgent, urManager, urAdmin]);
end;

end.
