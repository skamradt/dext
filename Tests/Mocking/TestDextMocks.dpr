{***************************************************************************}
{                                                                           }
{           Dext Framework - Mock Framework Test                            }
{                                                                           }
{***************************************************************************}
program TestDextMocks;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  Dext.Utils,
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  Dext.Interception,
  Dext.Interception.Proxy,
  Dext.Mocks,
  Dext.Mocks.Interceptor,
  Dext.Mocks.Matching;

type
  // IMPORTANT: {$M+} is required for TVirtualInterface to work!
  // Without it, the interface has no RTTI and mocking will fail.
  {$M+}
  ICalculator = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function Add(A, B: Integer): Integer;
    function Subtract(A, B: Integer): Integer;
    function Multiply(A, B: Integer): Integer;
    function GetName: string;
    procedure SetValue(Value: Integer);
  end;

  IGreeter = interface
    ['{B2C3D4E5-F6A7-8901-BCDE-F12345678901}']
    function Greet(const Name: string): string;
    function GreetWithTitle(const Title, Name: string): string;
  end;

  // Simple interceptor to test direct proxy creation (without Mock<T>)
  TSimpleInterceptor = class(TInterfacedObject, IInterceptor)
  private
    FCallCount: Integer;
    FLastMethod: string;
  public
    procedure Intercept(const Invocation: IInvocation);
    property CallCount: Integer read FCallCount;
    property LastMethod: string read FLastMethod;
  end;
  {$M-}

{ TSimpleInterceptor }

procedure TSimpleInterceptor.Intercept(const Invocation: IInvocation);
begin
  Inc(FCallCount);
  FLastMethod := Invocation.Method.Name;
  // Simple pass-through behavior or default result
  if Invocation.Method.ReturnType.TypeKind = tkInteger then
    Invocation.Result := TValue.From<Integer>(123); // Always return 123
end;

procedure TestBasicMocking;
var
  CalculatorMock: Mock<ICalculator>;
begin
  WriteLn('=== Test 1: Basic Mocking ===');

  CalculatorMock := Mock<ICalculator>.Create;

  // Setup: Add should return 42 for any arguments (fluent syntax)
  CalculatorMock.Setup.Returns(42).When.Add(0, 0);

  // Act
  var Result := CalculatorMock.Instance.Add(10, 20);
  
  // Assert
  if Result = 42 then
    WriteLn('  PASS: Add returned 42')
  else
    WriteLn('  FAIL: Add returned ', Result, ' (expected 42)');
end;

procedure TestArgumentMatchers;
var
  CalculatorMock: Mock<ICalculator>;
begin
  WriteLn('');
  WriteLn('=== Test 2: Argument Matchers ===');
  
  CalculatorMock := Mock<ICalculator>.Create;
  
// Setup with Arg.Any
  CalculatorMock.Setup.Returns(100).When.Add(Arg.Any<Integer>, Arg.Any<Integer>);
  
  // Act - different arguments
  var R1 := CalculatorMock.Instance.Add(1, 2);
  var R2 := CalculatorMock.Instance.Add(50, 100);
  
  // Assert
  if (R1 = 100) and (R2 = 100) then
    WriteLn('  PASS: Arg.Any matched all calls')
  else
    WriteLn('  FAIL: Expected 100, got ', R1, ' and ', R2);
end;

procedure TestStringMatching;
var
  Greeter: Mock<IGreeter>;
begin
  WriteLn('');
  WriteLn('=== Test 3: String Matching ===');
  
  Greeter := Mock<IGreeter>.Create;
  
  // Setup with string matcher
  Greeter.Setup.Returns('Hello, World!').When.Greet(Arg.Any<string>);
  
  // Act
  var Result := Greeter.Instance.Greet('John');
  
  // Assert
  if Result = 'Hello, World!' then
    WriteLn('  PASS: Greet returned expected string')
  else
    WriteLn('  FAIL: Greet returned "', Result, '"');
end;

procedure TestVerification;
var
  CalculatorMock: Mock<ICalculator>;
begin
  WriteLn('');
  WriteLn('=== Test 4: Verification ===');
  
  CalculatorMock := Mock<ICalculator>.Create;
  CalculatorMock.Setup.Returns(0).When.Add(Arg.Any<Integer>, Arg.Any<Integer>);
  
  // Call Add 3 times
  CalculatorMock.Instance.Add(1, 2);
  CalculatorMock.Instance.Add(3, 4);
  CalculatorMock.Instance.Add(5, 6);
  
  // Verify: should have been called at least once
  try
    CalculatorMock.Received(Times.AtLeast(1)).Add(Arg.Any<Integer>, Arg.Any<Integer>);
    WriteLn('  PASS: Verification passed for AtLeast(1)');
  except
    on E: EMockException do
      WriteLn('  FAIL: Verification failed - ', E.Message);
  end;

  // Verify exact count
  try
    CalculatorMock.Received(Times.Exactly(3)).Add(Arg.Any<Integer>, Arg.Any<Integer>);
    WriteLn('  PASS: Verification passed for Exactly(3)');
  except
    on E: EMockException do
      WriteLn('  FAIL: Verification failed - ', E.Message);
  end;
  // Verify with Alias (.Verify)
  try
    CalculatorMock.Verify(Times.Exactly(3)).Add(Arg.Any<Integer>, Arg.Any<Integer>);
    WriteLn('  PASS: Verification alias Verify(Exactly(3)) passed');
  except
    on E: EMockException do
      WriteLn('  FAIL: Verification alias failed - ', E.Message);
  end;
end;

procedure TestStrictBehavior;
var
  CalculatorMock: Mock<ICalculator>;
begin
  WriteLn('');
  WriteLn('=== Test 5: Strict Behavior ===');
  
  CalculatorMock := Mock<ICalculator>.Create(TMockBehavior.Strict);
  
  // Don't setup anything - strict should throw
  try
    CalculatorMock.Instance.Add(1, 2);
    WriteLn('  FAIL: Strict mode should have thrown exception');
  except
    on E: EMockException do
      WriteLn('  PASS: Strict mode threw exception: ', E.Message);
  end;
end;

procedure TestThrowsSetup;
var
  CalculatorMock: Mock<ICalculator>;
begin
  WriteLn('');
  WriteLn('=== Test 6: Throws Setup ===');
  
  CalculatorMock := Mock<ICalculator>.Create;
  
  // Setup to throw exception
  CalculatorMock.Setup.Throws(EInvalidOp, 'Cannot divide by zero').When.Add(0, 0);
  
  try
    CalculatorMock.Instance.Add(0, 0);
    WriteLn('  FAIL: Should have thrown EInvalidOp');
  except
    on E: EInvalidOp do
      WriteLn('  PASS: Threw expected exception: ', E.Message);
    on E: Exception do
      WriteLn('  FAIL: Wrong exception type: ', E.ClassName, ' - ', E.Message);
  end;
end;

procedure TestMultipleReturns;
var
  CalculatorMock: Mock<ICalculator>;
begin
  WriteLn('');
  WriteLn('=== Test 7: Multiple Returns (Sequence) ===');
  
  CalculatorMock := Mock<ICalculator>.Create;
  
  // Setup to return values in sequence
  CalculatorMock.Setup.ReturnsInSequence([1, 2, 3]).When.Add(Arg.Any<Integer>, Arg.Any<Integer>);
  
  var R1 := CalculatorMock.Instance.Add(0, 0);
  var R2 := CalculatorMock.Instance.Add(0, 0);
  var R3 := CalculatorMock.Instance.Add(0, 0);
  var R4 := CalculatorMock.Instance.Add(0, 0); // Should return last value again
  
  if (R1 = 1) and (R2 = 2) and (R3 = 3) and (R4 = 3) then
    WriteLn('  PASS: Sequence returns worked correctly')
  else
    WriteLn('  FAIL: Expected 1,2,3,3 but got ', R1, ',', R2, ',', R3, ',', R4);
end;

procedure TestVerifyNoOtherCalls;
var
  CalculatorMock: Mock<ICalculator>;
begin
  WriteLn('');
  WriteLn('=== Test 8: VerifyNoOtherCalls ===');
  
  CalculatorMock := Mock<ICalculator>.Create;
  
  // Setup
  CalculatorMock.Setup.Returns(0).When.Add(1, 2);
  
  // Act
  CalculatorMock.Instance.Add(1, 2);
  
  // Verify specific call
  CalculatorMock.Received.Add(1, 2);
  
  // Verify no other calls - should PASS
  try
    CalculatorMock.VerifyNoOtherCalls;
    WriteLn('  PASS: VerifyNoOtherCalls passed when only expected calls made');
  except
    on E: Exception do
      WriteLn('  FAIL: VerifyNoOtherCalls threw exception: ', E.Message);
  end;
  
  // Act again (unexpected call)
  CalculatorMock.Instance.Subtract(5, 5);
  
  // Verify no other calls - should FAIL
  try
    CalculatorMock.VerifyNoOtherCalls;
    WriteLn('  FAIL: VerifyNoOtherCalls passed but unexpected Subtract was called');
  except
    on E: EMockException do
      WriteLn('  PASS: VerifyNoOtherCalls caught unexpected call: ', E.Message);
  end;
end;

procedure TestVerificationVariants;
var
  CalculatorMock: Mock<ICalculator>;
begin
  WriteLn('');
  WriteLn('=== Test 9: Verification Variants ===');
  
  CalculatorMock := Mock<ICalculator>.Create;
  CalculatorMock.Setup.Returns(0).When.Add(Arg.Any<Integer>, Arg.Any<Integer>);
  
  // Call 2 times
  CalculatorMock.Instance.Add(1, 1);
  CalculatorMock.Instance.Add(2, 2);
  
  try
    // AtLeastOnce (2 >= 1) - PASS
    CalculatorMock.Verify(Times.AtLeastOnce).Add(Arg.Any<Integer>, Arg.Any<Integer>);
    
    // AtMost(5) (2 <= 5) - PASS
    CalculatorMock.Verify(Times.AtMost(5)).Add(Arg.Any<Integer>, Arg.Any<Integer>);
    
    // Between(1, 3) (1 <= 2 <= 3) - PASS
    CalculatorMock.Verify(Times.Between(1, 3)).Add(Arg.Any<Integer>, Arg.Any<Integer>);
    
    // Once - FAIL
    // We expect this to fail, so we wrap in sub-try
    try
        CalculatorMock.Verify(Times.Once).Add(Arg.Any<Integer>, Arg.Any<Integer>);
        WriteLn('  FAIL: Times.Once passed but called 2 times');
    except
        on E: EMockException do
            WriteLn('  PASS: Times.Once failed as expected');
    end;

    WriteLn('  PASS: All verification variants behave as expected');
  except
    on E: Exception do
      WriteLn('  FAIL: Verification variant error: ', E.Message);
  end;
end;

procedure TestInterceptionDirectly;
var
  Interceptor: TSimpleInterceptor;
  Calculator: ICalculator;
begin
  WriteLn('');
  WriteLn('=== Test 8: Direct Interception ===');
  
  // 1. Create the interceptor manually
  Interceptor := TSimpleInterceptor.Create;
  
  // 2. Create the proxy manually using Dext.Interception
  Calculator := TProxy.CreateInterface<ICalculator>(Interceptor);
  
  // 3. Act
  var Res := Calculator.Add(5, 5);
  
  // 4. Assert
  if (Res = 123) and (Interceptor.CallCount = 1) and (Interceptor.LastMethod = 'Add') then
    WriteLn('  PASS: Direct interception worked (Result=123, Calls=1)')
  else
    WriteLn('  FAIL: Expected 123/1/Add but got ', Res, '/', Interceptor.CallCount, '/', Interceptor.LastMethod);
    
  // Test another call
  Calculator.Subtract(10, 2);
  
  if (Interceptor.CallCount = 2) and (Interceptor.LastMethod = 'Subtract') then
    WriteLn('  PASS: Second call intercepted correctly')
  else
    WriteLn('  FAIL: Second call failed expectation');
end;

begin
  try
    WriteLn('======================================');
    WriteLn('   Dext Mocks Framework Test Suite   ');
    WriteLn('======================================');
    WriteLn;
    
    TestBasicMocking;
    TestArgumentMatchers;
    TestStringMatching;
    TestVerification;
    TestStrictBehavior;
    TestThrowsSetup;
    TestMultipleReturns;
    TestVerifyNoOtherCalls;
    TestVerificationVariants;
    TestInterceptionDirectly;
    
    WriteLn;
    WriteLn('======================================');
    WriteLn('   All tests completed!              ');
    WriteLn('======================================');
    
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  ConsolePause;
end.
