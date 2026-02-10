unit HelpDesk.Domain.Models;

{***************************************************************************}
{                                                                           }
{           Web.HelpDesk - Domain Models (DTOs)                             }
{                                                                           }
{           Data structures for API requests and response contracts         }
{                                                                           }
{***************************************************************************}

interface

uses
  System.SysUtils,
  Dext.Entity, // Required: MaxLength, Required (Validation Attributes)
  HelpDesk.Domain.Enums;

type
  // ===================================
  // Auth DTOs
  // ===================================
  TLoginRequest = record
    [Required]
    Email: string;

    [Required]
    Password: string;
  end;

  TRegisterUserRequest = record
    [Required, MaxLength(100)]
    FullName: string;

    [Required, MaxLength(100)]
    Email: string;

    [Required, MaxLength(50)]
    Password: string;
  end;

  TTokenResponse = record
    AccessToken: string;
    TokenType: string;
    ExpiresIn: Integer;
    UserRole: string;
  end;

  // ===================================
  // Ticket DTOs
  // ===================================
  TCreateTicketRequest = record
    [Required, MaxLength(250)]
    Subject: string;

    [Required, MaxLength(4000)]
    Description: string;

    [Required]
    Priority: TTicketPriority;

    [Required]
    Channel: TTicketChannel;

    Tags: TArray<string>;
  end;

  TUpdateTicketStatusRequest = record
    [Required]
    NewStatus: TTicketStatus;

    [Required, MaxLength(1000)]
    Reason: string;
  end;

  TAssignTicketRequest = record
    [Required]
    AssigneeId: Integer;
  end;

  TTicketResponse = record
    Id: Integer;
    Subject: string;
    Status: string;
    Priority: string;
    RequesterName: string;
    AssigneeName: string;
    CreatedAt: TDateTime;
    DueDate: TDateTime;
    IsOverdue: Boolean;
    Tags: TArray<string>;
  end;

  // ===================================
  // Comment DTOs
  // ===================================
  TAddCommentRequest = record
    [Required, MaxLength(2000)]
    Text: string;

    IsInternal: Boolean;
  end;

  TCommentResponse = record
    Id: Integer;
    AuthorName: string;
    CreatedAt: TDateTime;
    Text: string;
    IsInternal: Boolean;
  end;

  // ===================================
  // Metrics DTO
  // ===================================
  TMetricsResponse = record
    TotalOpen: Integer;
    OverdueCount: Integer;
    AvgResolutionHours: Double;
  end;

implementation

end.
