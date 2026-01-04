# 🧪 Dext Testing Framework

> *"Depois de usar Dext Testing, você não vai querer outra ferramenta na vida."*

The **Dext Testing Framework** is a native Delphi testing library designed for modern patterns like TDD (Test Driven Development) and BDD (Behavior Driven Development). It provides a fluent, expressive API for **Mocking** and **Assertions**, eliminating the need for external dependencies like DUnitX extensions or third-party mocking libraries.

## 💡 Why Dext Testing?

### The Problem with Testing in Delphi

Testing in Delphi has traditionally been challenging:
- **DUnit/DUnitX** are great for test execution, but lack fluent assertions and mocking
- **Writing mocks manually** is tedious and error-prone
- **No snapshot testing** means complex object assertions require tons of boilerplate

### What Dext Testing Provides

Dext Testing is designed from the ground up for modern Delphi development, bringing the **developer experience of .NET testing frameworks** (FluentAssertions, Moq, xUnit) to Delphi:

✅ **Fluent Assertions** — `Should(Value).Be(Expected)` readable syntax  
✅ **Interface Mocking** — `Mock<IService>` with full argument matching  
✅ **Class Mocking** — Virtual methods can be intercepted  
✅ **Auto-Mocking (DI)** — `TAutoMocker` injects dependencies automatically  
✅ **Snapshot Testing** — `MatchSnapshot` for regression testing  
✅ **GUID/UUID Native Support** — Assertions for Dext's TUUID type  
✅ **Moq-Compatible Syntax** — Familiar `.Setup.Returns.When` pattern  
✅ **Zero External Dependencies** — Works with DUnit/DUnitX or standalone

### One Framework, Zero Compromises

```pascal
// This is what testing SHOULD look like in Delphi:
procedure TestUserService;
var
  Mocker: TAutoMocker;
  Service: TUserService;
begin
  Mocker := TAutoMocker.Create;
  try
    // Auto-inject all dependencies as mocks
    Service := Mocker.CreateInstance<TUserService>;
    
    // Setup behavior with fluent syntax
    Mocker.GetMock<IUserRepository>.Setup.Returns(TUser.Create('John')).When.FindById(1);
    
    // Execute
    var User := Service.GetUser(1);
    
    // Assert with readable syntax
    Should(User.Name).Be('John').And.NotBeEmpty;
    
    // Verify interactions
    Mocker.GetMock<IUserRepository>.Received(Times.Once).FindById(1);
  finally
    Mocker.Free;
  end;
end;
```

## 📦 Features

### 1. Fluent Assertions
Inspired by .NET's *FluentAssertions*, write readable and expressive tests:

```pascal
// String
Should('Hello World').StartWith('Hello').And.EndWith('World');

// Numbers
Should(Order.Total).BeGreaterThan(0);

// Collections
ShouldList<string>.Create(Items).HaveCount(3).Contain('Dext');

// Exceptions
Should(procedure begin raise EInvalidOp.Create('Error'); end)
  .Throw<EInvalidOp>;
```

### 2. Expressive Mocking
A powerful mocking engine using `TVirtualInterface` to create dynamic proxies for interfaces.

```pascal
var
  EmailEngine: Mock<IEmailEngine>;
begin
  EmailEngine := Mock<IEmailEngine>.Create;
  
  // Setup behavior
  EmailEngine.Setup.Returns(True).When.Send('john@doe.com', Arg.Any<string>);
  
  // Use the proxy
  MyService.Process(EmailEngine.Instance);
  
  // Verify calls
  EmailEngine.Received(Times.Once).Send('john@doe.com', Arg.Any<string>);
end;
```

## 🚀 Getting Started

### Installation
The testing framework is part of the core Dext distribution.
1. Ensure `Dext.Testing.dpk` is compiled.
2. Add `Dext.Mocks`, `Dext.Assertions`, and `Dext.Interception` to your unit uses clause.

> ⚠️ **Important:** Interfaces to be mocked MUST have the `{$M+}` directive (RTTI generation) enabled.

### Writing your first test

```pascal
program MyTests;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Dext.Assertions,
  Dext.Mocks;

type
  {$M+} // Enable RTTI for mocking
  ICalculator = interface
    ['{GUID}']
    function Add(A, B: Integer): Integer;
  end;
  {$M-}

procedure TestCalculator;
var
  MockCalc: Mock<ICalculator>;
begin
  // Arrange
  MockCalc := Mock<ICalculator>.Create;
  MockCalc.Setup.Returns(10).When.Add(5, 5);

  // Act
  var Result := MockCalc.Instance.Add(5, 5);

  // Assert
  Should(Result).Be(10);
  MockCalc.Received.Add(5, 5);
end;

begin
  try
    TestCalculator;
    WriteLn('All tests passed!');
  except
    on E: Exception do WriteLn('Test Failed: ', E.Message);
  end;
end.
```

## 🔍 Assertions API

The `Dext.Assertions` unit provides a global `Should()` helper for most types.

### Strings
```pascal
Should(Name).Be('John');
Should(Name).NotBe('Doe');
Should(Name).StartWith('Jo');
Should(Name).EndWith('hn');
Should(Name).Contain('oh');
Should(Name).BeEmpty;
Should(Name).NotBeEmpty;
Should(Name).BeEquivalentTo('JOHN'); // Case insensitive
```

### Numbers (Integer, Double, Int64)
```pascal
Should(Age).Be(18);
Should(Age).BeGreaterThan(10);
Should(Age).BeLessThan(100);
Should(Age).BeInRange(18, 99);
Should(Age).BePositive;
Should(Age).BeNegative;
Should(Age).BeZero;
```

### Booleans
```pascal
Should(IsActive).BeTrue;
Should(IsActive).BeFalse;
```

### Date & Time
Use `ShouldDate()` for clarity and to avoid ambiguity with numbers.
```pascal
ShouldDate(Now).BeCloseTo(Now, 1000); // within 1000ms
ShouldDate(DueDate).BeAfter(SomeDate);
ShouldDate(DueDate).BeBefore(SomeDate);
ShouldDate(EventDate).BeSameDateAs(Now); // Ignores time
```

### Objects
```pascal
Should(User).BeNil;
Should(User).NotBeNil;
Should(User).BeOfType<TAdmin>;

// Deep Comparison (using JSON serialization)
Should(Dto1).BeEquivalentTo(Dto2);
```

### Actions (Exceptions)
```pascal
Should(procedure begin ... end).Throw<EInvalidOp>;
Should(procedure begin ... end).NotThrow;
```

### Lists & Collections
For collections, use `ShouldList<T>.Create(...)` or `Should<T>(Array)` (global syntax currently limited).

```pascal
var List: TList<Integer>;
...
ShouldList<Integer>.Create(List).HaveCount(5)
  .Contain(10)
  .NotContain(99);
```

## 🎭 Mocking API

The `Dext.Mocks` unit allows defining behavior and verifying interactions.

### Setup Returns
```pascal
// Return specific value
Repo.Setup.Returns(User).When.GetById(1);

// Basic Return
Calculator.Setup.Returns(42).When.Add(Arg.Any<Integer>, Arg.Any<Integer>);

// Sequence of Returns (1st call -> 10, 2nd call -> 20)
Calculator.Setup.ReturnsInSequence([10, 20]).When.Add(1, 1);

// Callback (Side effects or Argument Capture)
Calculator.Setup.Callback(procedure(Args: TArray<TValue>)
  begin
    Log('Called with ' + Args[0].ToString);
  end).When.DoSomething(Arg.IsAny);

// Return based on arguments (stubbing)
Repo.Setup.Returns(nil).When.GetById(Arg.Is<Integer>(function(Id: Integer): Boolean
  begin
    Result := Id < 0;
  end));
```

### Overloaded Returns
Simplified syntax for common types:
```pascal
Mock.Setup.Returns(10).When.GetInt;      // Integer
Mock.Setup.Returns('Data').When.GetString; // String
Mock.Setup.Returns(True).When.GetBool;   // Boolean
```

### Argument Matchers
- `Arg.Any<T>`: Matches any value of type T.
- `Arg.Is<T>(Predicate)`: Matches if predicate returns true.
- `Arg.Matches<T>(Value)`: Matches if equal to Value.

### Verification
```pascal
// Ensure method was called exactly once
Mock.Received(Times.Once).Save(Arg.Any<TUser>);

// Alias: You can also use .Verify() if you prefer Moq syntax
Mock.Verify(Times.Once).Save(Arg.Any<TUser>);

// Ensure method was never called
Mock.Received(Times.Never).Delete(Arg.Any<Integer>);

// Ensure method was called at least N times
Mock.Received(Times.AtLeast(2)).Log(Arg.Any<string>);
```

### Strict Mocks
By default, mocks are **Loose** (methods return default values if not setup). You can create **Strict** mocks that raise exceptions for unconfigured calls.

```pascal
var M := Mock<IFaa>.Create(TMockBehavior.Strict);
```

### Mocking Classes
You can mock `virtual` methods of regular classes similar to interfaces.
```pascal
type
  TCustomerRepo = class
  public
    function Count: Integer; virtual; // Must be virtual
  end;
  
var 
  Repo: Mock<TCustomerRepo>;
begin
  Repo := Mock<TCustomerRepo>.Create;
  Repo.Setup.Returns(10).When.Count;
end;
```

## 🌟 Advanced Features

### Snapshot Testing
Simplify testing of complex objects or large strings by comparing them against a stored "snapshot" file.

```pascal
// First run: Creates 'Snapshots/User_V1.json'
// Next runs: Compares result with file content
Should(UserDTO).MatchSnapshot('User_V1');
```

To update snapshots, set environment variable `SNAPSHOT_UPDATE=1`.

### Auto-Mocking Container
Reduce boilerplate in your tests by automatically creating mocks and injecting them into your System Under Test (SUT) constructor.

```pascal
uses Dext.Mocks.Auto;

var
  Mocker: TAutoMocker;
  Service: TMyService;
begin
  Mocker := TAutoMocker.Create;
  try
    // Automatically creates mocks and injects them (Interfaces and Virtual Classes)
    Service := Mocker.CreateInstance<TMyService>;
    
    // Access the injected mock to setup behavior
    Mocker.GetMock<IRepo>.Setup.Returns(User).When.GetById(1);
    
    Service.DoWork;
  finally
    Mocker.Free;
  end;
end;
```

### DI Integration for Tests

Use `TTestServiceProvider` to create a mock-aware dependency injection container:

```pascal
uses Dext.Testing.DI;

var
  Provider: TTestServiceProvider;
  EmailMock: Mock<IEmailService>;
  Service: TOrderService;
begin
  EmailMock := Mock<IEmailService>.Create;
  EmailMock.Setup.Returns(True).When.Send(Arg.Any<string>, Arg.Any<string>);
  
  Provider := TTestServiceProvider.Create;
  try
    // Add mock
    Provider.AddMock<IEmailService>(EmailMock);
    
    // Add real implementations
    Provider.AddSingleton<TOrderService>;
    
    // Get service - uses mock for IEmailService, real for others
    Service := Provider.GetService<TOrderService>;
    
    Service.ProcessOrder(Order);
    
    EmailMock.Received(Times.Once).Send(Arg.Any<string>, Arg.Any<string>);
  finally
    Provider.Free;
  end;
end;
```

---

## ⚠️ Common Gotchas & FAQ

### 1. "Interface not found" when mocking

**Problem:** You get an error when trying to create a mock.

**Solution:** Ensure the interface has `{$M+}` RTTI enabled:

```pascal
type
  {$M+}  // REQUIRED for mocking!
  IMyService = interface
    ['{GUID-HERE}']
    procedure DoWork;
  end;
  {$M-}
```

### 2. Class methods not being intercepted

**Problem:** Mocked class methods still execute the real code.

**Solution:** The method MUST be declared as `virtual`:

```pascal
type
  TMyClass = class
  public
    function GetData: string; virtual;  // ✅ Will be intercepted
    function Process: Boolean;           // ❌ Will NOT be intercepted
  end;
```

### 3. Mock returns nil/0 when I expected a value

**Problem:** You set up a return value but get default values.

**Solution:** Ensure your argument matchers match exactly what's being called:

```pascal
// Setup
Mock.Setup.Returns('Hello').When.GetValue(1);

// This will NOT match:
Mock.Instance.GetValue(2);  // Returns '' (default)

// Use Arg.Any<T> for flexible matching:
Mock.Setup.Returns('Hello').When.GetValue(Arg.Any<Integer>);
```

### 4. Snapshot test fails after code changes

**Problem:** Snapshot comparison fails after legitimate changes.

**Solution:** Set environment variable `SNAPSHOT_UPDATE=1` to regenerate snapshots:

```
set SNAPSHOT_UPDATE=1
MyTests.exe
set SNAPSHOT_UPDATE=
```

---

## 🔄 Migrating from DUnitX

If you're currently using DUnitX, Dext Testing works alongside it. You can use DUnitX for test discovery and execution while leveraging Dext for assertions and mocking.

### Before (DUnitX + Manual Assertions)

```pascal
procedure TMyTests.TestCalculation;
begin
  Assert.AreEqual(42, FCalculator.Add(40, 2));
  Assert.IsTrue(FCalculator.IsPositive(5));
  Assert.IsNotNull(FResult);
end;
```

### After (DUnitX + Dext Assertions)

```pascal
uses Dext.Assertions;

procedure TMyTests.TestCalculation;
begin
  Should(FCalculator.Add(40, 2)).Be(42);
  Should(FCalculator.IsPositive(5)).BeTrue;
  Should(FResult).NotBeNil;
end;
```

### Before (Manual Stub)

```pascal
type
  TStubRepository = class(TInterfacedObject, IRepository)
    function GetById(Id: Integer): TEntity;
  end;

function TStubRepository.GetById(Id: Integer): TEntity;
begin
  Result := TEntity.Create;
  Result.Name := 'Test';
end;
```

### After (Dext Mock)

```pascal
uses Dext.Mocks;

var
  Repo: Mock<IRepository>;
begin
  Repo := Mock<IRepository>.Create;
  Repo.Setup.Returns(TEntity.Create('Test')).When.GetById(Arg.Any<Integer>);
end;
```

---

## 📚 See Also

- [Main Documentation](../README.md)
- [Dext.Mocks API Reference](../Sources/Testing/README.md)
- [Framework Comparison](testing-framework-comparison.md)
- [Examples: TestNewFeatures.dpr](../Tests/Mocking/TestNewFeatures.dpr)
