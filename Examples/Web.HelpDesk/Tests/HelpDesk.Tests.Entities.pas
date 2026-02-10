unit HelpDesk.Tests.Entities;

{***************************************************************************}
{                                                                           }
{           Web.HelpDesk - Entity Unit Tests                                }
{                                                                           }
{           Tests for Ticket SLA, Overdue logic                             }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Testing,
  HelpDesk.Domain.Entities,
  HelpDesk.Domain.Enums;

type
  [TestFixture]
  TTicketEntityTests = class
  public
    [Test]
    procedure SLA_High_Priority_Should_Be_24Hours;

    [Test]
    procedure SLA_Critical_Priority_Should_Be_4Hours;
    
    [Test]
    procedure IsOverdue_Should_Return_True_If_DueDate_Passed;
    
    [Test]
    procedure IsOverdue_Should_Return_False_If_Resolved;
  end;

implementation

uses
  System.SysUtils,
  System.DateUtils;

{ TTicketEntityTests }

procedure TTicketEntityTests.SLA_High_Priority_Should_Be_24Hours;
var
  Ticket: TTicket;
  BaseDate: TDateTime;
begin
  Ticket := TTicket.Create;
  try
    Ticket.Priority := tpHigh;
    BaseDate := EncodeDate(2023, 1, 1) + EncodeTime(10, 0, 0, 0); // 10 AM
    
    var DueDate := Ticket.CalculateSLA(BaseDate);
    
    // 24h later = Next day 10 AM
    var Expected := IncHour(BaseDate, 24);
    
    Should(DueDate).Be(Expected)
      .Because('High priority SLA must be 24 hours');
  finally
    Ticket.Free;
  end;
end;

procedure TTicketEntityTests.SLA_Critical_Priority_Should_Be_4Hours;
var
  Ticket: TTicket;
  BaseDate: TDateTime;
begin
  Ticket := TTicket.Create;
  try
    Ticket.Priority := tpCritical;
    BaseDate := EncodeDate(2023, 1, 1) + EncodeTime(10, 0, 0, 0);
    
    var DueDate := Ticket.CalculateSLA(BaseDate);
    var Expected := IncHour(BaseDate, 4);
    
    Should(DueDate).Be(Expected)
      .Because('Critical priority SLA must be 4 hours');
  finally
    Ticket.Free;
  end;
end;

procedure TTicketEntityTests.IsOverdue_Should_Return_True_If_DueDate_Passed;
var
  Ticket: TTicket;
begin
  Ticket := TTicket.Create;
  try
    Ticket.Status := tsOpen;
    Ticket.DueDate := IncHour(Now, -1); // 1 hour ago
    
    Should(Ticket.IsOverdue).Be(True)
      .Because('Ticket is past due date and open');
  finally
    Ticket.Free;
  end;
end;

procedure TTicketEntityTests.IsOverdue_Should_Return_False_If_Resolved;
var
  Ticket: TTicket;
begin
  Ticket := TTicket.Create;
  try
    Ticket.DueDate := IncHour(Now, -1); // 1 hour ago (would be overdue)
    Ticket.Status := tsResolved;        // But it is resolved
    
    Should(Ticket.IsOverdue).Be(False)
      .Because('Resolved tickets are never overdue');
  finally
    Ticket.Free;
  end;
end;

end.
