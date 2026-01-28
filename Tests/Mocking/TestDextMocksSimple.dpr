{***************************************************************************}
{                                                                           }
{           Dext Framework - Mock Framework Simple Test                     }
{                                                                           }
{***************************************************************************}
program TestDextMocksSimple;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Rtti,
  Dext.Interception,
  Dext.Interception.Proxy,
  Dext.Mocks,
  Dext.Mocks.Interceptor,
  Dext.Mocks.Matching,
  Dext.Utils;

type
  {$M+}
  ICalculator = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function Add(A, B: Integer): Integer;
  end;

var
  Calculator: Mock<ICalculator>;
  Instance: ICalculator;
  SetupResult: ISetup<ICalculator>;
  WhenResult: IWhen<ICalculator>;
  CalcForSetup: ICalculator;
begin
  try
    WriteLn('Step 1: Creating Mock...');
    Calculator := Mock<ICalculator>.Create;
    WriteLn('  OK');

    WriteLn('Step 2: Getting Instance...');
    Instance := Calculator.Instance;
    WriteLn('  OK - Instance assigned');

    WriteLn('Step 3: Calling Setup...');
    SetupResult := Calculator.Setup;
    WriteLn('  OK - Setup returned');

    WriteLn('Step 4: Calling Returns...');
    WhenResult := SetupResult.Returns(TValue.From<Integer>(42));
    WriteLn('  OK - Returns returned');

    WriteLn('Step 5: Calling When...');
    CalcForSetup := WhenResult.When;
    WriteLn('  OK - When returned');

    WriteLn('Step 6: Calling Add on setup interface...');
    CalcForSetup.Add(0, 0);
    WriteLn('  OK - Add called during setup');

    WriteLn('Step 7: Calling Add on Instance...');
    var Result := Instance.Add(10, 20);
    WriteLn('  OK - Add returned: ', Result);

    if Result = 42 then
      WriteLn('SUCCESS!')
    else
      WriteLn('FAIL - Expected 42, got ', Result);

  except
    on E: Exception do
      WriteLn('ERROR at current step: ', E.ClassName, ': ', E.Message);
  end;

  ConsolePause;
end.
