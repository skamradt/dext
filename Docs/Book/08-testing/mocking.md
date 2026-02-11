# Mocking

Create test doubles with `Mock<T>` for interfaces.

> [!IMPORTANT]
> `Mock<T>` is a **generic Record** that lives in `Dext.Mocks` — it is **NOT** part of the `Dext.Testing` facade. It does **NOT** need `.Free`.

## Interface Mocking

Interfaces must have RTTI enabled (`{$M+}`) to be mockable:

```pascal
uses
  Dext.Mocks; // Mock<T>, Arg, Times

type
  {$M+} // REQUIRED for mockable interfaces
  IService = interface
    ['{...}']
    function Calculate(A: Integer): Integer;
  end;
  {$M-}

procedure TestMock;
begin
  // 1. Create Mock
  var MyMock := Mock<IService>.Create;

  // 2. Setup (fluent definition)
  MyMock.Setup.Returns(42).When.Calculate(Arg.Any<Integer>);

  // 3. Act
  var Result := MyMock.Instance.Calculate(10); // Returns 42

  // 4. Verify
  MyMock.Received(Times.Once).Calculate(10);
end;
```

## Setup Methods

```pascal
// Return a specific value
MyMock.Setup.Returns(42).When.Calculate(Arg.Any<Integer>);

// Return different values in sequence
MyMock.Setup.Returns(User1).When.GetNext;
// On second call, returns User2, etc.

// Throw exception
MyMock.Setup.Throws(ENotFound).When.GetById(Arg.Any<Integer>);
```

## Argument Matching

```pascal
// Any value of a type
MyMock.Setup.Returns(User).When.FindById(Arg.Any<Integer>);

// Exact value
MyMock.Received(Times.Once).FindById(42);
```

## Verification

```pascal
// Verify called exactly once
MyMock.Received(Times.Once).FindById(1);

// Verify called N times
MyMock.Received(Times.Exactly(3)).Save(Arg.Any<TUser>);

// Verify never called
MyMock.DidNotReceive.Delete(Arg.Any<Integer>);

// Verify at least/at most
MyMock.Received(Times.AtLeast(1)).GetAll;
MyMock.Received(Times.AtMost(5)).GetAll;

// Verify no other calls were made
MyMock.VerifyNoOtherCalls;
```

## Test Fixture Example

```pascal
uses
  Dext.Testing,  // Facade: Should, [TestFixture], [Test]
  Dext.Mocks;    // Mock<T>

type
  [TestFixture]
  TUserServiceTests = class
  private
    FMockRepo: Mock<IUserRepository>;
    FService: TUserService;
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure Should_Find_User;
  end;

procedure TUserServiceTests.Setup;
begin
  FMockRepo := Mock<IUserRepository>.Create;
  FService := TUserService.Create(FMockRepo.Instance);
end;

procedure TUserServiceTests.Should_Find_User;
begin
  // Arrange
  var User := TUser.Create;
  User.Name := 'Alice';
  FMockRepo.Setup.Returns(User).When.FindById(1);

  // Act
  var Found := FService.GetById(1);

  // Assert
  Should(Found).NotBeNil;
  Should(Found.Name).Be('Alice');

  // Verify
  FMockRepo.Received(Times.Once).FindById(1);
end;
```

---

[← Testing](README.md) | [Next: Assertions →](assertions.md)
