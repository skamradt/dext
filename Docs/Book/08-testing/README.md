# 8. Testing

Dext includes a powerful testing framework with mocking, assertions, and test runners.

## Chapters

1. [Mocking](mocking.md) - `Mock<T>` for interfaces and classes
2. [Assertions](assertions.md) - Fluent `Should()` syntax
3. [Snapshots](snapshots.md) - JSON snapshot testing

## Quick Start

```pascal
uses
  Dext.Testing;

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
  
  FMockRepo.Setup.WhenCalling('FindById').Returns(User);
  
  // Act
  var Result := FService.GetById(1);
  
  // Assert
  Result.Should.NotBeNil;
  Result.Name.Should.Be('John');
  
  // Verify
  FMockRepo.Received(Times.Once).FindById(1);
end;
```

## Unit Testing Entities (Warning)

When testing entities that use `OwnsObjects=False` for child collections (required for ORM compatibility), **you must manually free the child items** in your test's `finally` block. `Entity.Free` will only free the list container, not the items.

```pascal
var Order := TOrder.Create;
var Item  := TOrderItem.Create;
try
  Order.Items.Add(Item);
  // Test changes...
finally
  Order.Free;
  Item.Free; // MUST be freed manually!
end;
```

## Integration Testing

We recommend using PowerShell scripts for full-stack integration testing.

- Use `http://127.0.0.1:9000` instead of `localhost` to avoid IPv6 resolution issues.
- Set `Accept: application/json` headers explicitly.
- Generate local JWTs if needed for protected routes.

## Run Tests

```bash
dext test
dext test --coverage
dext test --html --output TestReport.html
```

---

[← Real-Time](../07-real-time/README.md) | [Next: Mocking →](mocking.md)
</Parameter>
<parameter name="Complexity">2
