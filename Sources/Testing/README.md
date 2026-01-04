# Dext Testing Framework

A comprehensive testing framework for Delphi, designed for modern development. It features a Fluent API for Mocking and Assertions, heavily inspired by .NET ecosystem tools (NUnit, Moq, FluentAssertions).

## Features

### Dext.Testing.Attributes ⭐ NEW
Attribute-based test runner inspired by NUnit/xUnit - no base class required.

```pascal
[TestFixture('Calculator Tests')]
TCalculatorTests = class
public
  [Setup]
  procedure SetUp;
  
  [Test]
  procedure TestAddition;
  
  [Test]
  [TestCase(1, 2, 3)]
  [TestCase(-1, 1, 0)]
  procedure TestAddWithParams(A, B, Expected: Integer);
end;
```

**Available Attributes:**
- `[TestFixture]`, `[Test]` - Core test markers
- `[Setup]`, `[TearDown]` - Per-test lifecycle
- `[BeforeAll]`, `[AfterAll]` - Per-fixture lifecycle  
- `[AssemblyInitialize]`, `[AssemblyCleanup]` - Global lifecycle
- `[TestCase(...)]` - Parameterized tests
- `[Category('...')]`, `[Ignore('...')]`, `[Explicit]` - Filtering
- `[Timeout(ms)]`, `[MaxTime(ms)]`, `[Repeat(n)]`, `[Priority(n)]` - Execution control

### Dext.Testing.Fluent
Fluent API for test configuration and execution.

```pascal
if TTest.Configure
  .Verbose
  .RegisterFixtures([TMyTests])
  .FilterByCategory('Unit')
  .ExportToJUnit('results.xml')
  .ExportToHtml('report.html')
  .Run then
  ExitCode := 0;
```

### Dext.Testing.Report ⭐ NEW
CI/CD report generation for multiple formats:
- **JUnit XML** - Jenkins, GitHub Actions, GitLab CI
- **xUnit XML** - .NET ecosystem
- **TRX** - Azure DevOps, Visual Studio
- **HTML** - Beautiful standalone dark-themed report
- **JSON** - Custom tooling
- **SonarQube** - Quality gates integration

### Dext.Interception
Core interception library supporting both Interfaces and Classes.
- `TProxy.CreateInterface<T>`: Creates dynamic proxies for interfaces (`TVirtualInterface`).
- `TClassProxy`: Creates proxies for class virtual methods (`TVirtualMethodInterceptor`).
> **Note**: Interfaces must have `{$M+}` enabled. Classes use RTTI for virtual methods.

### Dext.Mocks
Fluent mocking framework inspired by Moq.
- `Mock<T>`: Create mocks for **Interfaces** and **Classes**.
- `Setup`: Configure method behavior (Returns, Throws, Callback).
- `When`: Fluent chaining.
- `Arg`: Argument matchers (`Arg.Any<T>`, `Arg.Is<T>`).
- `Received`: Verification (`Times.Once`, `Times.Never`).

**Example (Interface):**
```pascal
var Calc: Mock<ICalculator>;
...
Calc.Setup.Returns(42).When.Add(10, 20);
```

**Example (Class):**
```pascal
var Repo: Mock<TCustomerRepo>;
...
Repo.Setup.Returns(100).When.GetCount;
```

### Dext.Mocks.Auto (Auto-Mocking)
Automatically creates mocks and injects them into your System Under Test (SUT) constructor.
```pascal
var Mocker: TAutoMocker;
...
Service := Mocker.CreateInstance<TMyService>; // Injects Mock<IRepo> automatically
Mocker.GetMock<IRepo>.Setup.Returns(True).When.Save;
```

### Dext.Assertions
Fluent assertions library inspired by FluentAssertions.
- `Should(Value)` syntax.
- Supports all primitive types, Objects, Lists, Dates, and Actions.
- **Snapshot Testing**: `Should(Json).MatchSnapshot('MySnapshot');`

**Example:**
```pascal
Should(User.Name).Be('John');
Should(List).HaveCount(5).Contain(10);
Should(procedure begin RaiseError end).Throw<EException>;
```

#### Multiple Assertions (Soft Asserts)
Execute multiple assertions in a scope and collect all failures at the end, instead of stopping at the first failure.

```pascal
Assert.Multiple(procedure
begin
  Should(Person.Name).Be('John');       // If this fails...
  Should(Person.Age).BeGreaterThan(18); // ...this executes too!
end);
// Reports all failures combined at the end of the block
```

### ITestContext Injection ⭐ NEW
Tests can receive runtime context automatically:

```pascal
[Test]
procedure TestWithContext(Context: ITestContext);
begin
  Context.WriteLine('Current test: %s', [Context.CurrentTest]);
  Context.AttachFile('screenshot.png');
end;
```

## Requirements
- Delphi 10.4 or newer (Recommended: Delphi 11/12).
- FastMM5 (Optional).

## See Also
- [Full Testing Documentation](../../Docs/testing.md)
- [Live Dashboard Guide](../../Docs/testing-dashboard.md)
- [Testing Enhancements Roadmap](../../Docs/testing-enhancements.md)

