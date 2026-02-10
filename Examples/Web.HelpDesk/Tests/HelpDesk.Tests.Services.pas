unit HelpDesk.Tests.Services;

{***************************************************************************}
{                                                                           }
{           Web.HelpDesk - Service Unit Tests                               }
{                                                                           }
{           Tests for TicketService Logic using Mocks                       }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Entity,       // TDbContext, TDbContextOptions
  Dext.Mocks,        // Mock<T> (generic, not in facade)
  Dext.Testing,      // Facade: Should, TestFixture
  HelpDesk.Services,
  HelpDesk.Domain.Entities,
  HelpDesk.Domain.Enums,
  HelpDesk.Domain.Models,
  HelpDesk.Data.Context;

type
  [TestFixture]
  TTicketServiceTests = class
  private
    FService: ITicketService;
    FMockUser: Mock<IUserService>;
    FDb: THelpDeskContext;
    FOptions: TDbContextOptions;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure CreateTicket_Should_Save_And_Calculate_SLA;

    [Test]
    procedure AssignTicket_Should_Throw_If_Not_Agent;
  end;

implementation

uses
  System.SysUtils;

{ TTicketServiceTests }

procedure TTicketServiceTests.Setup;
begin
  // 1. In-Memory DbContext (SQLite :memory:)
  FOptions := TDbContextOptions.Create
    .UseSQLite(':memory:');
  FDb := THelpDeskContext.Create(FOptions);
  FDb.EnsureCreated;

  // 2. Mock UserService
  FMockUser := Mock<IUserService>.Create;

  // 3. System Under Test
  FService := TTicketService.Create(FDb, FMockUser.Instance);
end;

procedure TTicketServiceTests.TearDown;
begin
  FService := nil; // Release interface ref before freeing Db
  FDb.Free;
  FOptions.Free;
end;

procedure TTicketServiceTests.CreateTicket_Should_Save_And_Calculate_SLA;
var
  User: TUser;
  Req: TCreateTicketRequest;
  Ticket: TTicket;
begin
  // Arrange: Seed requester
  User := TUser.Create;
  User.Name := 'Requester';
  User.Email := 'req@test.com';
  User.Role := urCustomer;
  User.IsActive := True;
  User.PasswordHash := '123';
  FDb.Users.Add(User);
  FDb.SaveChanges;

  Req.Subject := 'Test Ticket';
  Req.Description := 'Test Description';
  Req.Priority := tpHigh;
  Req.Channel := tcWeb;
  SetLength(Req.Tags, 1);
  Req.Tags[0] := 'bug';

  // Act
  Ticket := FService.CreateTicket(Req, Integer(User.Id));

  // Assert
  Should(Integer(Ticket.Id) > 0).Be(True)
    .Because('Ticket should be saved with auto-generated ID');

  Should(Integer(Ticket.RequesterId) = Integer(User.Id)).Be(True)
    .Because('Requester ID should match the creator');

  // SLA: High priority = 24 hours
  Should(Ticket.DueDate > Ticket.CreatedAt).Be(True)
    .Because('DueDate must be after CreatedAt');
end;

procedure TTicketServiceTests.AssignTicket_Should_Throw_If_Not_Agent;
begin
  // Arrange: Mock VerifyAgentAccess to return False
  FMockUser.Setup.Returns(False).When.VerifyAgentAccess(Arg.Any<Integer>);

  // Act & Assert
  Should(
    procedure
    begin
      FService.AssignTicket(1, 2, 999);
    end)
    .Throw<EAccessDenied>
    .Because('Non-agent users cannot assign tickets');
end;

end.
