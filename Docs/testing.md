# Dext Testing (Modern Unit Tests)

Fluent assertions, Interface mocking, and Snapshot testing built for Delphi.

> 📚 **Full Documentation**: See [Book: Testing](Book/08-testes/README.md)

## Key Features

1. **Fluent Assertions**: `Should(Name).Be('John')`
2. **Interface Mocking**: `Mock<IService>.Create` without external libraries
3. **Snapshot Testing**: `CompareSnapshot('orders_v1')` for complex objects

## Quick Start (Attributes)

```pascal
unit MyTests;

interface

uses
  Dext.Testing, Dext.Mocks; // Facade

type
  [TestFixture]
  TUserTests = class(TTestCase)
  private
    FService: TUserService;
    FRepo: Mock<IUserRepository>;
  public
    [Setup]
    procedure SetUp;
    
    [Test]
    procedure GetById_Should_Return_User;
  end;

implementation

procedure TUserTests.SetUp;
begin
  FRepo := Mock<IUserRepository>.Create;
  FService := TUserService.Create(FRepo.Instance);
end;

procedure TUserTests.GetById_Should_Return_User;
begin
  // Arrange
  var User := TUser.Create;
  User.Name := 'John';
  FRepo.Setup.Returns(User).When.FindById(1);

  // Act
  var Found := FService.GetById(1);

  // Assert
  Should(Found).NotBeNil;
  Should(Found.Name).Be('John');

  // Verify
  FRepo.Received(Times.Once).FindById(1);
end;
```

## Running Tests

`TTest.Configure.Verbose.RegisterFixtures([TUserTests]).Run` in your console app.

## CLI

```bash
dext test
dext test --coverage
```

## Mock Syntax

```pascal
// Setup Return
Mock.Setup.Returns(42).When.Calculate(Arg.Any<Integer>);

// Verify Call
Mock.Received(Times.Once).Calculate(10);

// Setup Property
Mock.Setup.Returns('Admin').When.Role; // getter

// Setup Exception
Mock.Setup.Throws(EAccessDenied).When.DeleteUser(Arg.Is<Integer>(function(id: Integer): Boolean begin Result = 1; end));
```

## Assertion Examples

```pascal
Should(List).Contain(Item);
Should(Value).BeGreaterThan(10);
Should(MyDate).BeCloseTo(Now, 1000);
Should(Action).Throw<EInvalidOp>;
```
