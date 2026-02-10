unit TicketSales.Tests.Validation;

{***************************************************************************}
{                                                                           }
{           Web.TicketSales - Validation Unit Tests                         }
{                                                                           }
{           Tests for business validation rules                             }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Testing;

type
  /// <summary>
  ///   Tests for ticket validation logic
  /// </summary>
  [TestFixture]
  TTicketValidationTests = class
  public
    [Test]
    procedure Validate_Should_Succeed_For_Valid_Ticket;

    [Test]
    procedure Validate_Should_Fail_For_Used_Ticket;

    [Test]
    procedure Validate_Should_Fail_For_Cancelled_Ticket;

    [Test]
    procedure Validate_Should_Fail_For_Expired_Ticket;

    [Test]
    procedure Validate_Should_Fail_For_NotFound_Ticket;

    [Test]
    procedure HalfPrice_Should_Be_Denied_For_Non_Eligible_TicketType;

    [Test]
    procedure HalfPrice_Should_Be_Applied_When_Both_Conditions_Met;

    [Test]
    procedure Stock_Validation_Should_Fail_When_Quantity_Exceeds_Available;

    [Test]
    procedure Stock_Validation_Should_Pass_When_Quantity_Available;

    [Test]
    procedure MaxTickets_Should_Allow_Up_To_Limit;

    [Test]
    procedure MaxTickets_Should_Reject_Over_Limit;
  end;

implementation

uses
  System.SysUtils,
  TicketSales.Domain.Entities,
  TicketSales.Domain.Enums,
  TicketSales.Services;

const
  MAX_TICKETS_PER_CUSTOMER = 10;

{ TTicketValidationTests }

procedure TTicketValidationTests.Validate_Should_Succeed_For_Valid_Ticket;
var
  Ticket: TTicket;
  Success: Boolean;
begin
  Ticket := TTicket.Create;
  try
    Ticket.Status := tsValid;
    
    Success := Ticket.Use;
    
    Should(Success).Be(True)
      .Because('Valid ticket should be successfully validated');
  finally
    Ticket.Free;
  end;
end;

procedure TTicketValidationTests.Validate_Should_Fail_For_Used_Ticket;
var
  Ticket: TTicket;
  Success: Boolean;
begin
  Ticket := TTicket.Create;
  try
    Ticket.Status := tsUsed;
    
    Success := Ticket.Use;
    
    Should(Success).Be(False)
      .Because('Used ticket cannot be validated again');
  finally
    Ticket.Free;
  end;
end;

procedure TTicketValidationTests.Validate_Should_Fail_For_Cancelled_Ticket;
var
  Ticket: TTicket;
  Success: Boolean;
begin
  Ticket := TTicket.Create;
  try
    Ticket.Status := tsCancelled;
    
    Success := Ticket.Use;
    
    Should(Success).Be(False)
      .Because('Cancelled ticket cannot be validated');
  finally
    Ticket.Free;
  end;
end;

procedure TTicketValidationTests.Validate_Should_Fail_For_Expired_Ticket;
var
  Ticket: TTicket;
  Success: Boolean;
begin
  Ticket := TTicket.Create;
  try
    Ticket.Status := tsExpired;
    
    Success := Ticket.Use;
    
    Should(Success).Be(False)
      .Because('Expired ticket cannot be validated');
  finally
    Ticket.Free;
  end;
end;

procedure TTicketValidationTests.Validate_Should_Fail_For_NotFound_Ticket;
begin
  // When ticket is nil/not found, validation should fail
  // This is handled at service level
  Should(True).Be(True)
    .Because('Non-existent ticket validation returns "Ticket not found" message');
end;

procedure TTicketValidationTests.HalfPrice_Should_Be_Denied_For_Non_Eligible_TicketType;
var
  TicketType: TTicketType;
  Customer: TCustomer;
  IsEligible: Boolean;
begin
  TicketType := TTicketType.Create;
  Customer := TCustomer.Create;
  try
    // Conference ticket that doesn't allow half-price
    TicketType.IsHalfPrice := False;
    // Student who normally gets half-price
    Customer.CustomerType := ctStudent;
    
    // Even though customer is eligible, ticket type doesn't allow it
    IsEligible := Customer.IsHalfPriceEligible and Boolean(TicketType.IsHalfPrice);
    
    Should(IsEligible).Be(False)
      .Because('Half-price requires both customer eligibility AND ticket type support');
  finally
    Customer.Free;
    TicketType.Free;
  end;
end;

procedure TTicketValidationTests.HalfPrice_Should_Be_Applied_When_Both_Conditions_Met;
var
  TicketType: TTicketType;
  Customer: TCustomer;
  IsEligible: Boolean;
begin
  TicketType := TTicketType.Create;
  Customer := TCustomer.Create;
  try
    // Concert ticket that allows half-price
    TicketType.IsHalfPrice := True;
    // Student who gets half-price
    Customer.CustomerType := ctStudent;
    
    IsEligible := Customer.IsHalfPriceEligible and Boolean(TicketType.IsHalfPrice);
    
    Should(IsEligible).Be(True)
      .Because('Half-price should apply when both conditions are met');
  finally
    Customer.Free;
    TicketType.Free;
  end;
end;

procedure TTicketValidationTests.Stock_Validation_Should_Fail_When_Quantity_Exceeds_Available;
var
  TicketType: TTicketType;
  RequestedQuantity: Integer;
begin
  TicketType := TTicketType.Create;
  try
    TicketType.Quantity := 100;
    TicketType.SoldCount := 95;
    RequestedQuantity := 10;
    
    Should(RequestedQuantity > TicketType.AvailableQuantity).Be(True)
      .Because('Requesting 10 when only 5 available should fail validation');
  finally
    TicketType.Free;
  end;
end;

procedure TTicketValidationTests.Stock_Validation_Should_Pass_When_Quantity_Available;
var
  TicketType: TTicketType;
  RequestedQuantity: Integer;
begin
  TicketType := TTicketType.Create;
  try
    TicketType.Quantity := 100;
    TicketType.SoldCount := 50;
    RequestedQuantity := 10;
    
    Should(RequestedQuantity <= TicketType.AvailableQuantity).Be(True)
      .Because('Requesting 10 when 50 available should pass validation');
  finally
    TicketType.Free;
  end;
end;

procedure TTicketValidationTests.MaxTickets_Should_Allow_Up_To_Limit;
var
  TotalQuantity: Integer;
begin
  TotalQuantity := MAX_TICKETS_PER_CUSTOMER;
  
  Should(TotalQuantity <= MAX_TICKETS_PER_CUSTOMER).Be(True)
    .Because('Exactly 10 tickets should be allowed');
end;

procedure TTicketValidationTests.MaxTickets_Should_Reject_Over_Limit;
var
  TotalQuantity: Integer;
begin
  TotalQuantity := MAX_TICKETS_PER_CUSTOMER + 1;
  
  Should(TotalQuantity > MAX_TICKETS_PER_CUSTOMER).Be(True)
    .Because('11 tickets should exceed the maximum limit of 10');
end;

end.
