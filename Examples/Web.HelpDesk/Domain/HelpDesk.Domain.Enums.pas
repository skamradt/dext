unit HelpDesk.Domain.Enums;

{***************************************************************************}
{                                                                           }
{           Web.HelpDesk - Domain Enums                                     }
{                                                                           }
{           Key enumerations for business logic: Status, Priority, Role     }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Core.SmartTypes;

type
  /// <summary>Ticket workflow status</summary>
  TTicketStatus = (
    tsNew,          // Initial state
    tsOpen,         // Assigned to agent
    tsInProgress,   // Being worked on
    tsResolved,     // Solution provided
    tsClosed,       // Confirmed by user or auto-closed
    tsRejected      // Invalid or duplicate
  );
  TTicketStatusType = Prop<TTicketStatus>;

  /// <summary>Priority levels for SLAs</summary>
  TTicketPriority = (
    tpLow,          // Default SLA: 72 hours
    tpMedium,       // SLA: 48 hours
    tpHigh,         // SLA: 24 hours
    tpCritical      // SLA: 4 hours
  );
  TTicketPriorityType = Prop<TTicketPriority>;

  /// <summary>User role within the system</summary>
  TUserRole = (
    urCustomer,     // Can only view own tickets
    urAgent,        // Can be assigned tickets
    urManager,      // Can reassign and view reports
    urAdmin         // System access
  );
  TUserRoleType = Prop<TUserRole>;

  /// <summary>Source channel of the ticket</summary>
  TTicketChannel = (
    tcWeb,          // Created via Portal
    tcEmail,        // Created via Email parser
    tcPhone,        // Created by agent via call
    tcChat          // Created via Live Chat
  );
  TTicketChannelType = Prop<TTicketChannel>;

implementation

end.
