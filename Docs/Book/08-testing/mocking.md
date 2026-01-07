# Mocking

Create test doubles with `Mock<T>` for interfaces and classes.

## Interface Mocking

```pascal
uses
  Dext.Testing.Mock;

var
  MockRepo: Mock<IUserRepository>;
  Service: TUserService;
begin
  // Create mock
  MockRepo := Mock<IUserRepository>.Create;
  
  // Setup behavior
  MockRepo.Setup.WhenCalling('FindById').WithArgs([1]).Returns(SomeUser);
  MockRepo.Setup.WhenCalling('GetAll').Returns(UserList);
  
  // Use in production code
  Service := TUserService.Create(MockRepo.Instance);
  
  // Verify calls
  MockRepo.Received(Times.Once).FindById(1);
  MockRepo.DidNotReceive.Delete(Arg.Any<Integer>);
end;
```

## Setup Methods

```pascal
// Return value
MockRepo.Setup.WhenCalling('GetById').Returns(User);

// Return different values in sequence
MockRepo.Setup.WhenCalling('GetNext')
  .Returns(User1)
  .Then.Returns(User2)
  .Then.Returns(User3);

// Throw exception
MockRepo.Setup.WhenCalling('GetById').Throws(ENotFound);

// Execute callback
MockRepo.Setup.WhenCalling('Save').Executes(procedure(Args: TArray<TValue>)
  begin
    WriteLn('Save called with: ', Args[0].AsObject.ClassName);
  end);
```

## Argument Matching

```pascal
// Exact value
MockRepo.Setup.WhenCalling('FindById').WithArgs([42]).Returns(User);

// Any value
MockRepo.Setup.WhenCalling('FindById').WithArgs([Arg.Any<Integer>]).Returns(User);

// Predicate
MockRepo.Setup.WhenCalling('FindById')
  .WithArgs([Arg.Is<Integer>(function(I: Integer): Boolean
    begin
      Result := I > 0;
    end)])
  .Returns(User);
```

## Verification

```pascal
// Verify called exactly once
MockRepo.Received(Times.Once).FindById(1);

// Verify called N times
MockRepo.Received(Times.Exactly(3)).Save(Arg.Any<TUser>);

// Verify never called
MockRepo.DidNotReceive.Delete(Arg.Any<Integer>);

// Verify at least/at most
MockRepo.Received(Times.AtLeast(1)).GetAll;
MockRepo.Received(Times.AtMost(5)).GetAll;

// Verify no other calls were made
MockRepo.VerifyNoOtherCalls;
```

## Class Mocking

For classes with virtual methods:

```pascal
var
  MockService: Mock<TUserService>;
begin
  MockService := Mock<TUserService>.Create;
  
  MockService.Setup.WhenCalling('Calculate').Returns(42);
  
  var Result := MockService.Instance.Calculate;
  Assert(Result = 42);
end;
```

---

[← Testing](README.md) | [Next: Assertions →](assertions.md)
