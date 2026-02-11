# 8. Testing

Dext includes a powerful testing framework inspired by NUnit/xUnit, with mocking, fluent assertions, and built-in test runners.

## Chapters

1. [Mocking](mocking.md) - `Mock<T>` for interfaces
2. [Assertions](assertions.md) - Fluent `Should()` syntax
3. [Snapshots](snapshots.md) - JSON snapshot testing

## Test Project Structure (.dpr)

Create a separate Console Application project for tests:

```pascal
program MyProject.Tests;

{$APPTYPE CONSOLE}

uses
  Dext.MM,             // Optional: FastMM5 wrapper
  Dext.Utils,          // SetConsoleCharSet, ConsolePause
  System.SysUtils,
  Dext.Testing,        // Main test facade (Assert, TTest, Should)
  MyTests in 'MyTests.pas';

begin
  SetConsoleCharSet;   // REQUIRED for all console projects
  try
    // Fluent Runner Configuration
    // TTest.SetExitCode auto-sets ExitCode=0 (success) or 1 (failure)
    TTest.SetExitCode(
      TTest.Configure
        .Verbose            // Always include: without this, output is empty
        .RegisterFixtures([TCalculatorTests, TUserServiceTests])
        .Run
    );
  except
    on E: Exception do
      Writeln('Error: ', E.Message);
  end;
  ConsolePause;        // REQUIRED: keeps console open when running in IDE
end.
```

## Writing Tests (Attributes)

```pascal
uses
  Dext.Testing; // Single facade

type
  [TestFixture]
  TCalculatorTests = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure Should_Add_Numbers;

    [Test]
    [TestCase(1, 2, 3)]
    [TestCase(10, 5, 15)]
    procedure Should_Add_With_Params(A, B, Expected: Integer);
  end;
```

## Quick Start

```pascal
uses
  Dext.Testing,   // Facade: Assert, Should, TTest
  Dext.Mocks;     // Mock<T> - generic record, NOT in facade

type
  [TestFixture]
  TUserServiceTests = class
  private
    FService: TUserService;
    FMockRepo: Mock<IUserRepository>;
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure GetUser_ReturnsUser_WhenExists;
  end;

procedure TUserServiceTests.Setup;
begin
  FMockRepo := Mock<IUserRepository>.Create;
  FService := TUserService.Create(FMockRepo.Instance);
end;

procedure TUserServiceTests.GetUser_ReturnsUser_WhenExists;
var
  User: TUser;
begin
  // Arrange
  User := TUser.Create;
  User.Name := 'John';
  FMockRepo.Setup.Returns(User).When.FindById(Arg.Any<Integer>);

  // Act
  var Result := FService.GetById(1);

  // Assert
  Should(Result).NotBeNil;
  Should(Result.Name).Be('John');

  // Verify
  FMockRepo.Received(Times.Once).FindById(1);
end;
```

> [!IMPORTANT]
> `Mock<T>` is a **generic Record** — it lives in `Dext.Mocks` (NOT in the `Dext.Testing` facade) and does **NOT** need `.Free`.

## Fluent Assertions

```pascal
// Simple values
Should(Total).Be(100);
Should(Name).StartWith('John').AndAlso.EndWith('Doe');

// Collections
Should(List).Contain(Item);
Should(List).HaveCount(5);

// Exceptions
Should(procedure begin Calc.DivByZero end).Throw<EInvalidOp>;

// Smart Assertions (Strongly Typed)
var u := Prototype.Entity<TUser>; // Ghost entity for metadata
Should(User).HaveValue(u.Name, 'Alice');
```

## Unit Testing Entities (Memory Warning)

When testing entities with `OwnsObjects=False` child collections (required for ORM compatibility), **you must manually free child items** in your test's `finally` block:

```pascal
var Order := TOrder.Create;
var Item := TOrderItem.Create;  // Created manually
try
  Order.Items.Add(Item);
  Order.CalculateTotal;
  // Assert...
finally
  Order.Free; // Frees Order and the List (but NOT the Item)
  Item.Free;  // REQUIRED: Free child manually
end;
```

**Why?** Entities use `OwnsObjects = False` to avoid Double Free when tracked by a DbContext. In unit tests (without DbContext), this means child objects must be freed manually.

## Integration Testing (PowerShell Scripts)

Every Web API must have a PowerShell integration test script (e.g., `Test.MyProject.ps1`) in the project root.

### Recommended Structure

1. Configuration (BaseURL, UTF-8 encoding)
2. Health Check (validate server is online)
3. Auth / Token Generation
4. Use case tests (CRUD, Business Flows)
5. Result validation (HTTP codes, JSON content)

### Tips

- **IPv6/404 Errors**: Always use `$baseUrl = "http://127.0.0.1:9000"` instead of `localhost`
- **Headers**: Set `Accept: application/json` and `Content-Type: application/json; charset=utf-8` explicitly
- **Enum values**: By default, Dext serializes enums as strings (`"tsOpen"` not `1`)
- **JWT testing**: If the API uses JWT, include a `New-JwtToken` function in the script

## Run Tests

```bash
dext test
dext test --coverage
dext test --html --output TestReport.html
```

---

[← Real-Time](../07-real-time/README.md) | [Next: Mocking →](mocking.md)
