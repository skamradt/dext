unit HelpDesk.Domain.Entities;

{***************************************************************************}
{                                                                           }
{           Web.HelpDesk - Domain Entities                                  }
{                                                                           }
{           Smart Entities with JSON Columns and Business Logic             }
{                                                                           }
{***************************************************************************}

interface

uses
  System.SysUtils,
  Dext.Collections,
  Dext.Core.SmartTypes,
  Dext.Types.Nullable,    // Required for Nullable<T>
  Dext.Entity.Attributes, // Required for [JsonColumn]
  Dext.Entity,            // Facade: [Table], [Column], [PK], [Required]
  HelpDesk.Domain.Enums;

type
  { Forward Declarations }
  TUser = class;
  TTicket = class;
  TComment = class;

  { -------------------------------------------------------------------------- }
  { TUser - System Users (Agents and Customers)                                }
  { -------------------------------------------------------------------------- }
  [Table('Users')]
  TUser = class
  private
    FId: IntType;
    FName: StringType;
    FEmail: StringType;
    FPasswordHash: StringType;
    FRole: TUserRoleType;
    FMetadata: StringType;
    FCreatedAt: DateTimeType;
    FIsActive: BoolType;

    // Navigation
    FTicketsCreated: IList<TTicket>;
    FTicketsAssigned: IList<TTicket>;
  public
    constructor Create;
    destructor Destroy; override;

    [PK, AutoInc]
    property Id: IntType read FId write FId;

    [Required, MaxLength(100)]
    property Name: StringType read FName write FName;

    [Required, MaxLength(100)]
    property Email: StringType read FEmail write FEmail;

    [Required, MaxLength(255)]
    property PasswordHash: StringType read FPasswordHash write FPasswordHash;

    [Required]
    property Role: TUserRoleType read FRole write FRole;

    /// <summary>
    ///   Stores flexible attributes like Department, Phone, Location.
    ///   Example: {"dept": "Sales", "manager": "John Doe"}
    /// </summary>
    [JsonColumn]
    property Metadata: StringType read FMetadata write FMetadata;

    [CreatedAt]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;

    property IsActive: BoolType read FIsActive write FIsActive;

    // Navigation (One-to-Many)
    property TicketsCreated: IList<TTicket> read FTicketsCreated;
    property TicketsAssigned: IList<TTicket> read FTicketsAssigned;

    class function Props: TUser; static;
  end;

  { -------------------------------------------------------------------------- }
  { TTicket - The Issue/Ticket                                                 }
  { -------------------------------------------------------------------------- }
  [Table('Tickets')]
  TTicket = class
  private
    FId: IntType;
    FSubject: StringType;
    FDescription: StringType;
    FStatus: TTicketStatusType;
    FPriority: TTicketPriorityType;
    FChannel: TTicketChannelType;

    // Foreign Keys
    FRequesterId: IntType;
    FAssigneeId: Nullable<Integer>; // Null = Unassigned

    // Data
    FTags: StringType;
    FCustomFields: StringType;

    // Dates
    FCreatedAt: DateTimeType;
    FDueDate: DateTimeType;
    FClosedAt: DateTimeType;

    // Navigation
    FRequester: TUser;
    FAssignee: TUser;
    FComments: IList<TComment>;
  public
    constructor Create;
    destructor Destroy; override;

    [PK, AutoInc]
    property Id: IntType read FId write FId;

    [Required, MaxLength(250)]
    property Subject: StringType read FSubject write FSubject;

    [Required, MaxLength(4000)]
    property Description: StringType read FDescription write FDescription;

    [Required]
    property Status: TTicketStatusType read FStatus write FStatus;

    [Required]
    property Priority: TTicketPriorityType read FPriority write FPriority;

    property Channel: TTicketChannelType read FChannel write FChannel;

    // Relationships
    [ForeignKey('Requester')]
    property RequesterId: IntType read FRequesterId write FRequesterId;

    [ForeignKey('Assignee')]
    property AssigneeId: Nullable<Integer> read FAssigneeId write FAssigneeId;

    // JSON Columns
    [JsonColumn]
    property Tags: StringType read FTags write FTags;

    [JsonColumn]
    property CustomFields: StringType read FCustomFields write FCustomFields;

    // Timestamps
    [CreatedAt]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;

    property DueDate: DateTimeType read FDueDate write FDueDate;
    property ClosedAt: DateTimeType read FClosedAt write FClosedAt;

    // Navigation Properties
    property Requester: TUser read FRequester write FRequester;
    property Assignee: TUser read FAssignee write FAssignee;
    property Comments: IList<TComment> read FComments;

    // Business Logic Helpers
    function IsOverdue: Boolean;
    function CalculateSLA(CreateDate: TDateTime): TDateTime;
    procedure Resolve;

    class function Props: TTicket; static;
  end;

  { -------------------------------------------------------------------------- }
  { TComment - Interaction Log                                                 }
  { -------------------------------------------------------------------------- }
  [Table('Comments')]
  TComment = class
  private
    FId: IntType;
    FTicketId: IntType;
    FAuthorId: IntType;
    FText: StringType;
    FIsInternal: BoolType;
    FCreatedAt: DateTimeType;

    // Navigation
    FTicket: TTicket;
    FAuthor: TUser;
  public
    [PK, AutoInc]
    property Id: IntType read FId write FId;

    [ForeignKey('Ticket')]
    property TicketId: IntType read FTicketId write FTicketId;

    [ForeignKey('Author')]
    property AuthorId: IntType read FAuthorId write FAuthorId;

    [Required, MaxLength(2000)]
    property Text: StringType read FText write FText;

    /// <summary>Internal notes are visible only to Agents/Managers</summary>
    property IsInternal: BoolType read FIsInternal write FIsInternal;

    [CreatedAt]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;

    // Navigation
    property Ticket: TTicket read FTicket write FTicket;
    property Author: TUser read FAuthor write FAuthor;

    class function Props: TComment; static;
  end;

implementation

uses
  System.DateUtils;

{ TUser }

constructor TUser.Create;
begin
  inherited;
  // OwnsObjects = False because DbContext manages lifecycle
  FTicketsCreated := TCollections.CreateList<TTicket>(False);
  FTicketsAssigned := TCollections.CreateList<TTicket>(False);
end;

destructor TUser.Destroy;
begin
  inherited;
end;

class function TUser.Props: TUser;
begin
  Result := Prototype.Entity<TUser>;
end;

{ TTicket }

constructor TTicket.Create;
begin
  inherited;
  FComments := TCollections.CreateList<TComment>(False);
end;

destructor TTicket.Destroy;
begin
  inherited;
end;

function TTicket.IsOverdue: Boolean;
begin
  if (TTicketStatus(FStatus) in [tsResolved, tsClosed, tsRejected]) then
    Exit(False);

  Result := (DueDate > 0) and (DueDate < Now);
end;

procedure TTicket.Resolve;
begin
  FStatus := tsResolved;
  FClosedAt := Now;
end;

function TTicket.CalculateSLA(CreateDate: TDateTime): TDateTime;
var
  HoursToAdd: Integer;
begin
  case TTicketPriority(FPriority) of
    tpCritical: HoursToAdd := 4;
    tpHigh:     HoursToAdd := 24;
    tpMedium:   HoursToAdd := 48;
    tpLow:      HoursToAdd := 72;
  else
    HoursToAdd := 48;
  end;

  Result := IncHour(CreateDate, HoursToAdd);
end;

class function TTicket.Props: TTicket;
begin
  Result := Prototype.Entity<TTicket>;
end;

{ TComment }

class function TComment.Props: TComment;
begin
  Result := Prototype.Entity<TComment>;
end;

end.
