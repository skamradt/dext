program TestSequenceAndCallback;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Rtti,
  Dext.Utils,
  Dext.Mocks,
  Dext.Mocks.Matching,
  Dext.Assertions;

type
  {$M+}
  ICalculator = interface
    ['{20B11EE2-7F01-4470-9883-8472570F4229}']
    function Add(A, B: Integer): Integer;
    function GetName: string;
    function IsValid: Boolean;
  end;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure Pass(const TestName: string);
begin
  Inc(TestsPassed);
  WriteLn('  PASS: ', TestName);
end;

procedure Fail(const TestName, Error: string);
begin
  Inc(TestsFailed);
  WriteLn('  FAIL: ', TestName, ' - ', Error);
end;

procedure TestSequence;
var
  CalculatorMock: Mock<ICalculator>;
  Calculator: ICalculator;
begin
  WriteLn('=== TestSequence ===');
  CalculatorMock := Mock<ICalculator>.Create;
  // Setup sequence: 10, 20, 30
  CalculatorMock.Setup
    .ReturnsInSequence([10, 20, 30]).When.Add(Arg.Any<Integer>, Arg.Any<Integer>);

  Calculator := CalculatorMock.Instance;
  try
    Should(Calculator.Add(1, 1)).Be(10);
    Should(Calculator.Add(1, 1)).Be(20);
    Should(Calculator.Add(1, 1)).Be(30);
    Should(Calculator.Add(1, 1)).Be(30); // Repeats last value
    Pass('ReturnsInSequence Integer');
  except
    on E: Exception do Fail('ReturnsInSequence Integer', E.Message);
  end;
  
  // Setup sequence: "Result 1", "Result 2"
  CalculatorMock.Setup.ReturnsInSequence(['Result 1', 'Result 2']).When.GetName;
  
  try
    Should(Calculator.GetName).Be('Result 1');
    Should(Calculator.GetName).Be('Result 2');
    Should(Calculator.GetName).Be('Result 2');
    Pass('ReturnsInSequence String');
  except
    on E: Exception do Fail('ReturnsInSequence String', E.Message);
  end;
end;

procedure TestCallback;
var
  CalculatorMock: Mock<ICalculator>;
  Calculator: ICalculator;
  LastA, LastB: Integer;
begin
  WriteLn('=== TestCallback ===');
  CalculatorMock := Mock<ICalculator>.Create;

  LastA := 0;
  LastB := 0;
  // Setup Callback to capture arguments
  CalculatorMock.Setup.Callback(
    procedure(Args: TArray<TValue>)
    begin
      LastA := Args[0].AsInteger;
      LastB := Args[1].AsInteger;
    end).When.Add(Arg.Any<Integer>, Arg.Any<Integer>);

  Calculator := CalculatorMock.Instance;
  try
    Calculator.Add(5, 10);
    
    Should(LastA).Be(5);
    Should(LastB).Be(10);
    Pass('Callback captured arguments');
    
    Should(Calculator.Add(99, 1)).Be(0); // Default return
    Should(LastA).Be(99);
    Pass('Callback executed successfully');
  except
    on E: Exception do Fail('Callback', E.Message);
  end;
end;

begin
  try
    TestSequence;
    TestCallback;
    
    WriteLn;
    WriteLn(Format('Tests Passed: %d, Failed: %d', [TestsPassed, TestsFailed]));
    if TestsFailed > 0 then
      ExitCode := 1
    else
      ExitCode := 0;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
  ConsolePause;
end.
