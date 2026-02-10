unit TicketSales.Data.Context;

{***************************************************************************}
{                                                                           }
{           Web.TicketSales - Database Context                              }
{                                                                           }
{           DbContext with entity sets for ORM operations                   }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Entity.Core,
  Dext.Entity,
  TicketSales.Domain.Entities;

type
  /// <summary>
  ///   Database context for the Ticket Sales system.
  ///   Provides access to all entity sets.
  /// </summary>
  TTicketSalesDbContext = class(TDbContext)
  private
    function GetEvents: IDbSet<TEvent>;
    function GetTicketTypes: IDbSet<TTicketType>;
    function GetCustomers: IDbSet<TCustomer>;
    function GetOrders: IDbSet<TOrder>;
    function GetOrderItems: IDbSet<TOrderItem>;
    function GetTickets: IDbSet<TTicket>;
  public
    /// <summary>Events (concerts, shows, etc.)</summary>
    property Events: IDbSet<TEvent> read GetEvents;
    
    /// <summary>Ticket types for events (VIP, Standard, etc.)</summary>
    property TicketTypes: IDbSet<TTicketType> read GetTicketTypes;
    
    /// <summary>Registered customers</summary>
    property Customers: IDbSet<TCustomer> read GetCustomers;
    
    /// <summary>Purchase orders</summary>
    property Orders: IDbSet<TOrder> read GetOrders;
    
    /// <summary>Order line items</summary>
    property OrderItems: IDbSet<TOrderItem> read GetOrderItems;
    
    /// <summary>Issued tickets</summary>
    property Tickets: IDbSet<TTicket> read GetTickets;
  end;

implementation

{ TTicketSalesDbContext }

function TTicketSalesDbContext.GetEvents: IDbSet<TEvent>;
begin
  Result := Entities<TEvent>;
end;

function TTicketSalesDbContext.GetTicketTypes: IDbSet<TTicketType>;
begin
  Result := Entities<TTicketType>;
end;

function TTicketSalesDbContext.GetCustomers: IDbSet<TCustomer>;
begin
  Result := Entities<TCustomer>;
end;

function TTicketSalesDbContext.GetOrders: IDbSet<TOrder>;
begin
  Result := Entities<TOrder>;
end;

function TTicketSalesDbContext.GetOrderItems: IDbSet<TOrderItem>;
begin
  Result := Entities<TOrderItem>;
end;

function TTicketSalesDbContext.GetTickets: IDbSet<TTicket>;
begin
  Result := Entities<TTicket>;
end;

end.

