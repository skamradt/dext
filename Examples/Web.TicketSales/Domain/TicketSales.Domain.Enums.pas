unit TicketSales.Domain.Enums;

{***************************************************************************}
{                                                                           }
{           Web.TicketSales - Domain Enums                                  }
{                                                                           }
{           Enumeration types for the Ticket Sales system                   }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Core.SmartTypes;

type
  /// <summary>
  ///   Status of an event
  /// </summary>
  TEventStatus = (
    esScheduled,    // Event is scheduled for the future
    esOnSale,       // Tickets are available for purchase
    esSoldOut,      // All tickets sold
    esCancelled,    // Event was cancelled
    esCompleted     // Event has finished
  );
  TEventStatusType = Prop<TEventStatus>;

  /// <summary>
  ///   Type of customer (affects pricing)
  /// </summary>
  TCustomerType = (
    ctRegular,      // Full price
    ctStudent,      // 50% discount (meia-entrada)
    ctSenior,       // 50% discount (idoso)
    ctChild         // 50% discount (criança)
  );
  TCustomerTypeType = Prop<TCustomerType>;

  /// <summary>
  ///   Status of an order
  /// </summary>
  TOrderStatus = (
    osPending,      // Order created, awaiting payment
    osPaid,         // Payment confirmed
    osCompleted,    // Tickets generated and sent
    osCancelled,    // Order cancelled
    osRefunded      // Order was refunded
  );
  TOrderStatusType = Prop<TOrderStatus>;

  /// <summary>
  ///   Status of a ticket
  /// </summary>
  TTicketStatus = (
    tsValid,        // Ticket is valid and can be used
    tsUsed,         // Ticket has been scanned/used
    tsCancelled,    // Ticket was cancelled
    tsExpired       // Event has passed
  );
  TTicketStatusType = Prop<TTicketStatus>;

implementation

end.
