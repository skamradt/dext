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
  MyMock: Mock<ICalculator>;
  Calc: ICalculator;
begin
  WriteLn('=== TestSequence ===');
  MyMock := Mock<ICalculator>.Create;
  Calc := MyMock.Instance;

  // Setup sequence: 10, 20, 30
  MyMock.Setup.ReturnsInSequence([10, 20, 30]).When.Add(Arg.Any<Integer>, Arg.Any<Integer>);

  try
    Should(Calc.Add(1, 1)).Be(10);
    Should(Calc.Add(1, 1)).Be(20);
    Should(Calc.Add(1, 1)).Be(30);
    Should(Calc.Add(1, 1)).Be(30); // Repeats last value
    Pass('ReturnsInSequence Integer');
  except
    on E: Exception do Fail('ReturnsInSequence Integer', E.Message);
  end;
  
  // Setup sequence: "Result 1", "Result 2"
  MyMock.Setup.ReturnsInSequence(['Result 1', 'Result 2']).When.GetName;
  
  try
    Should(Calc.GetName).Be('Result 1');
    Should(Calc.GetName).Be('Result 2');
    Should(Calc.GetName).Be('Result 2');
    Pass('ReturnsInSequence String');
  except
    on E: Exception do Fail('ReturnsInSequence String', E.Message);
  end;
end;

procedure TestCallback;
var
  MyMock: Mock<ICalculator>;
  Calc: ICalculator;
  LastA, LastB: Integer;
begin
  WriteLn('=== TestCallback ===');
  MyMock := Mock<ICalculator>.Create;
  Calc := MyMock.Instance;
  
  LastA := 0;
  LastB := 0;

  // Setup Callback to capture arguments
  MyMock.Setup.Callback(procedure(Args: TArray<TValue>)
    begin
      LastA := Args[0].AsInteger;
      LastB := Args[1].AsInteger;
    end).When.Add(Arg.Any<Integer>, Arg.Any<Integer>);

  try
    Calc.Add(5, 10);
    
    Should(LastA).Be(5);
    Should(LastB).Be(10);
    Pass('Callback captured arguments');
    
    Should(Calc.Add(99, 1)).Be(0); // Default return
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
