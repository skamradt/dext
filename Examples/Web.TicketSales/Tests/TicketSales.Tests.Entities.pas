unit TicketSales.Tests.Entities;

{***************************************************************************}
{                                                                           }
{           Web.TicketSales - Entity Unit Tests                             }
{                                                                           }
{           Tests for entity business methods                               }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Testing;

type
  /// <summary>
  ///   Tests for TEvent entity methods
  /// </summary>
  [TestFixture]
  TEventEntityTests = class
  public
    [Test]
    procedure AvailableTickets_Should_Return_Capacity_Minus_Sold;

    [Test]
    procedure AvailableTickets_Should_Not_Return_Negative;
  end;

  /// <summary>
  ///   Tests for TCustomer entity methods
  /// </summary>
  [TestFixture]
  TCustomerEntityTests = class
  public
    [Test]
    procedure IsHalfPriceEligible_Should_Return_True_For_Student;

    [Test]
    procedure IsHalfPriceEligible_Should_Return_True_For_Senior;

    [Test]
    procedure IsHalfPriceEligible_Should_Return_True_For_Child;

    [Test]
    procedure IsHalfPriceEligible_Should_Return_False_For_Regular;
  end;

  /// <summary>
  ///   Tests for TOrder entity methods
  /// </summary>
  [TestFixture]
  TOrderEntityTests = class
  public
    [Test]
    procedure CalculateTotal_Should_Sum_All_Items;

    [Test]
    procedure CalculateTotal_Should_Return_Zero_For_Empty_Order;
  end;

  /// <summary>
  ///   Tests for TTicket entity methods
  /// </summary>
  [TestFixture]
  TTicketEntityTests = class
  public
    [Test]
    procedure Use_Should_Mark_Ticket_As_Used;

    [Test]
    procedure Use_Should_Return_False_For_Already_Used_Ticket;

    [Test]
    procedure Use_Should_Return_False_For_Cancelled_Ticket;

    [Test]
    procedure Use_Should_Set_UsedAt_DateTime;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils,
  TicketSales.Domain.Entities,
  TicketSales.Domain.Enums;

{ TEventEntityTests }

procedure TEventEntityTests.AvailableTickets_Should_Return_Capacity_Minus_Sold;
var
  Event: TEvent;
begin
  Event := TEvent.Create;
  try
    Event.Capacity := 1000;
    Event.SoldCount := 350;

    Should(Event.AvailableTickets).Be(650)
      .Because('Available = Capacity - Sold');
  finally
    Event.Free;
  end;
end;

procedure TEventEntityTests.AvailableTickets_Should_Not_Return_Negative;
var
  Event: TEvent;
begin
  Event := TEvent.Create;
  try
    Event.Capacity := 100;
    Event.SoldCount := 150; // Over-sold scenario

    Should(Event.AvailableTickets).Be(0)
      .Because('Available tickets should never be negative');
  finally
    Event.Free;
  end;
end;

{ TCustomerEntityTests }

procedure TCustomerEntityTests.IsHalfPriceEligible_Should_Return_True_For_Student;
var
  Customer: TCustomer;
begin
  Customer := TCustomer.Create;
  try
    Customer.CustomerType := ctStudent;

    Should(Customer.IsHalfPriceEligible).Be(True)
      .Because('Students are eligible for half-price (meia-entrada)');
  finally
    Customer.Free;
  end;
end;

procedure TCustomerEntityTests.IsHalfPriceEligible_Should_Return_True_For_Senior;
var
  Customer: TCustomer;
begin
  Customer := TCustomer.Create;
  try
    Customer.CustomerType := ctSenior;

    Should(Customer.IsHalfPriceEligible).Be(True)
      .Because('Seniors (60+) are eligible for half-price');
  finally
    Customer.Free;
  end;
end;

procedure TCustomerEntityTests.IsHalfPriceEligible_Should_Return_True_For_Child;
var
  Customer: TCustomer;
begin
  Customer := TCustomer.Create;
  try
    Customer.CustomerType := ctChild;

    Should(Customer.IsHalfPriceEligible).Be(True)
      .Because('Children are eligible for half-price');
  finally
    Customer.Free;
  end;
end;

procedure TCustomerEntityTests.IsHalfPriceEligible_Should_Return_False_For_Regular;
var
  Customer: TCustomer;
begin
  Customer := TCustomer.Create;
  try
    Customer.CustomerType := ctRegular;

    Should(Customer.IsHalfPriceEligible).Be(False)
      .Because('Regular customers pay full price');
  finally
    Customer.Free;
  end;
end;

{ TOrderEntityTests }

procedure TOrderEntityTests.CalculateTotal_Should_Sum_All_Items;
var
  Order: TOrder;
  Item1, Item2, Item3: TOrderItem;
begin
  Order := TOrder.Create;
  Item1 := TOrderItem.Create;
  Item2 := TOrderItem.Create;
  Item3 := TOrderItem.Create;
  try
    Item1.Total := 100.00;
    Order.Items.Add(Item1);

    Item2.Total := 250.50;
    Order.Items.Add(Item2);

    Item3.Total := 75.25;
    Order.Items.Add(Item3);

    Order.CalculateTotal;

    Should(Currency(Order.Total))
      .Be(Currency(425.75))
      .Because('Total should be sum of all item totals');
  finally
    Order.Free;
    Item1.Free;
    Item2.Free;
    Item3.Free;
  end;
end;

procedure TOrderEntityTests.CalculateTotal_Should_Return_Zero_For_Empty_Order;
var
  Order: TOrder;
begin
  Order := TOrder.Create;
  try
    Order.CalculateTotal;

    Should(Currency(Order.Total)).Be(Currency(0))
      .Because('Empty order should have zero total');
  finally
    Order.Free;
  end;
end;

{ TTicketEntityTests }

procedure TTicketEntityTests.Use_Should_Mark_Ticket_As_Used;
var
  Ticket: TTicket;
  Success: Boolean;
begin
  Ticket := TTicket.Create;
  try
    Ticket.Status := tsValid;

    Success := Ticket.Use;

    Should(Success).Be(True)
      .Because('Valid ticket should be usable');
    Should(TTicketStatus(Ticket.Status) = tsUsed).Be(True)
      .Because('Status should change to Used');
  finally
    Ticket.Free;
  end;
end;

procedure TTicketEntityTests.Use_Should_Return_False_For_Already_Used_Ticket;
var
  Ticket: TTicket;
  Success: Boolean;
begin
  Ticket := TTicket.Create;
  try
    Ticket.Status := tsUsed;

    Success := Ticket.Use;

    Should(Success).Be(False)
      .Because('Already used ticket cannot be used again');
  finally
    Ticket.Free;
  end;
end;

procedure TTicketEntityTests.Use_Should_Return_False_For_Cancelled_Ticket;
var
  Ticket: TTicket;
  Success: Boolean;
begin
  Ticket := TTicket.Create;
  try
    Ticket.Status := tsCancelled;

    Success := Ticket.Use;

    Should(Success).Be(False)
      .Because('Cancelled ticket cannot be used');
  finally
    Ticket.Free;
  end;
end;

procedure TTicketEntityTests.Use_Should_Set_UsedAt_DateTime;
var
  Ticket: TTicket;
  BeforeUse: TDateTime;
begin
  Ticket := TTicket.Create;
  try
    Ticket.Status := tsValid;
    BeforeUse := Now;

    Ticket.Use;

    Should(TDateTime(Ticket.UsedAt) >= BeforeUse).Be(True)
      .Because('UsedAt should be set to current time');
  finally
    Ticket.Free;
  end;
end;

end.
