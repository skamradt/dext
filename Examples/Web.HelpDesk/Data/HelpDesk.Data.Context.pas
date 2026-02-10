unit HelpDesk.Data.Context;

{***************************************************************************}
{                                                                           }
{           Web.HelpDesk - Database Context                                 }
{                                                                           }
{           Access point for Users, Tickets, and Comments                   }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Entity.Core,  // Required for IDbSet<T>
  Dext.Entity,       // Required for TDbContext
  HelpDesk.Domain.Entities;

type
  /// <summary>
  ///   Main database context for the HelpDesk application.
  /// </summary>
  THelpDeskContext = class(TDbContext)
  private
    function GetUsers: IDbSet<TUser>;
    function GetTickets: IDbSet<TTicket>;
    function GetComments: IDbSet<TComment>;
  public
    /// <summary>System users (Agents, Customers, Admins)</summary>
    property Users: IDbSet<TUser> read GetUsers;
    
    /// <summary>Issue tickets</summary>
    property Tickets: IDbSet<TTicket> read GetTickets;
    
    /// <summary>Ticket comments and history</summary>
    property Comments: IDbSet<TComment> read GetComments;
  end;

implementation

{ THelpDeskContext }

function THelpDeskContext.GetUsers: IDbSet<TUser>;
begin
  Result := Entities<TUser>;
end;

function THelpDeskContext.GetTickets: IDbSet<TTicket>;
begin
  Result := Entities<TTicket>;
end;

function THelpDeskContext.GetComments: IDbSet<TComment>;
begin
  Result := Entities<TComment>;
end;

end.
