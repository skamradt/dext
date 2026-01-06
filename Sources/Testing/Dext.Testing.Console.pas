unit Dext.Testing.Console;

interface

uses
  System.SysUtils;

type
  TTestRunner = class
  private
    class var FPassed: Integer;
    class var FFailed: Integer;
  public
    class procedure Run(const TestName: string; const Action: TProc);
    class procedure PrintSummary;
    class property Passed: Integer read FPassed;
    class property Failed: Integer read FFailed;
  end;

implementation

uses
  Dext.Utils;


{ TTestRunner }

class procedure TTestRunner.Run(const TestName: string; const Action: TProc);
begin
  Write('Running ', TestName, '... ');
  try
    Action;
    Inc(FPassed);
    SafeWriteLn('PASS');
  except
    on E: Exception do
    begin
      Inc(FFailed);
      SafeWriteLn('FAIL');
      SafeWriteLn('  Error: ' + E.Message);
    end;
  end;
end;

class procedure TTestRunner.PrintSummary;
begin
  SafeWriteLn;
  SafeWriteLn('=========================================');
  SafeWriteLn(Format('Tests Summary: %d Passed, %d Failed', [FPassed, FFailed]));
  SafeWriteLn('=========================================');
  
  if FFailed > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end;

end.
