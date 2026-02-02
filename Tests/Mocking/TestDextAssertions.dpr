{***************************************************************************}
{                                                                           }
{           Dext Framework - Fluent Assertions Test                         }
{                                                                           }
{***************************************************************************}
program TestDextAssertions;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  System.Generics.Collections,
  System.DateUtils,
  Dext.Assertions;

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

procedure TestListAssertions;
var
  List: TList<Integer>;
  Arr: TArray<string>;
begin
  WriteLn('');
  WriteLn('=== List Assertions ===');

  List := TList<Integer>.Create;
  try
    List.Add(1);
    List.Add(2);
    List.Add(3);
    
    // ShouldList<T>.Create(List.ToArray)
    try
      ShouldList<Integer>.Create(List.ToArray).HaveCount(3);
      Pass('HaveCount (TList)');
    except on E: Exception do Fail('HaveCount (TList)', E.Message); end;
    
    try
      ShouldList<Integer>.Create(List.ToArray).Contain(2);
      Pass('Contain (TList)');
    except on E: Exception do Fail('Contain (TList)', E.Message); end;
    
    try
      ShouldList<Integer>.Create(List.ToArray).NotContain(4);
      Pass('NotContain (TList)');
    except on E: Exception do Fail('NotContain (TList)', E.Message); end;
    
  finally
    List.Free;
  end;
  
  Arr := ['a', 'b', 'c'];
  try
      ShouldList<string>.Create(Arr).HaveCount(3);
      Pass('HaveCount (Array)');
  except on E: Exception do Fail('HaveCount (Array)', E.Message); end;
  
  try
      ShouldList<string>.Create(Arr).Contain('b');
      Pass('Contain (Array)');
  except on E: Exception do Fail('Contain (Array)', E.Message); end;
end;

procedure TestStringAssertions;
begin
  WriteLn('');
  WriteLn('=== String Assertions ===');
  
  // Be
  try
    Should('Hello').Be('Hello');
    Pass('Be - same string (Global Should)');
  except
    on E: Exception do Fail('Be - same string', E.Message);
  end;
  
  try
    ShouldString.Create('Hello').Be('World');
    Fail('Be - different string', 'Should have thrown');
  except
    on E: EAssertionFailed do Pass('Be - different string throws');
    on E: Exception do Fail('Be - different string', 'Wrong exception: ' + E.Message);
  end;
  
  // BeEquivalentTo (case insensitive)
  try
    ShouldString.Create('HELLO').BeEquivalentTo('hello');
    Pass('BeEquivalentTo - case insensitive');
  except
    on E: Exception do Fail('BeEquivalentTo', E.Message);
  end;
  
  // Contain
  try
    ShouldString.Create('Hello World').Contain('World');
    Pass('Contain - found');
  except
    on E: Exception do Fail('Contain', E.Message);
  end;
  
  // StartWith
  try
    ShouldString.Create('Hello World').StartWith('Hello');
    Pass('StartWith - found');
  except
    on E: Exception do Fail('StartWith', E.Message);
  end;
  
  // EndWith
  try
    ShouldString.Create('Hello World').EndWith('World');
    Pass('EndWith - found');
  except
    on E: Exception do Fail('EndWith', E.Message);
  end;
  
  // HaveLength
  try
    ShouldString.Create('Hello').HaveLength(5);
    Pass('HaveLength - correct');
  except
    on E: Exception do Fail('HaveLength', E.Message);
  end;
  
  // BeEmpty
  try
    ShouldString.Create('').BeEmpty;
    Pass('BeEmpty - is empty');
  except
    on E: Exception do Fail('BeEmpty', E.Message);
  end;
  
  // NotBeEmpty
  try
    ShouldString.Create('Hello').NotBeEmpty;
    Pass('NotBeEmpty - is not empty');
  except
    on E: Exception do Fail('NotBeEmpty', E.Message);
  end;
  
  // Chaining with Because
  try
    ShouldString.Create('Hello').Because('it is a greeting').Be('World');
    Fail('Because context', 'Should have thrown');
  except
    on E: EAssertionFailed do
    begin
      if E.Message.Contains('because') then
        Pass('Because - context included in error')
      else
        Fail('Because - context missing', E.Message);
    end;
  end;
end;

procedure TestIntegerAssertions;
begin
  WriteLn('');
  WriteLn('=== Integer Assertions ===');
  
  // Be
  try
    ShouldInteger.Create(42).Be(42);
    Pass('Be - same value');
  except
    on E: Exception do Fail('Be', E.Message);
  end;
  
  // NotBe
  try
    ShouldInteger.Create(42).NotBe(100);
    Pass('NotBe - different value');
  except
    on E: Exception do Fail('NotBe', E.Message);
  end;
  
  // BeGreaterThan
  try
    ShouldInteger.Create(10).BeGreaterThan(5);
    Pass('BeGreaterThan - 10 > 5');
  except
    on E: Exception do Fail('BeGreaterThan', E.Message);
  end;
  
  // BeLessThan
  try
    ShouldInteger.Create(5).BeLessThan(10);
    Pass('BeLessThan - 5 < 10');
  except
    on E: Exception do Fail('BeLessThan', E.Message);
  end;
  
  // BeInRange
  try
    ShouldInteger.Create(5).BeInRange(1, 10);
    Pass('BeInRange - 5 in [1, 10]');
  except
    on E: Exception do Fail('BeInRange', E.Message);
  end;
  
  // BePositive
  try
    ShouldInteger.Create(5).BePositive;
    Pass('BePositive - 5 is positive');
  except
    on E: Exception do Fail('BePositive', E.Message);
  end;
  
  // BeNegative
  try
    ShouldInteger.Create(-5).BeNegative;
    Pass('BeNegative - -5 is negative');
  except
    on E: Exception do Fail('BeNegative', E.Message);
  end;
  
  // BeZero
  try
    ShouldInteger.Create(0).BeZero;
    Pass('BeZero - 0 is zero');
  except
    on E: Exception do Fail('BeZero', E.Message);
  end;
  
  // Global Should(Integer)
  try
    Should(123).Be(123);
    Pass('Global Should(Integer)');
  except
    on E: Exception do Fail('Global Should(Integer)', E.Message);
  end;
end;

procedure TestBooleanAssertions;
begin
  WriteLn('');
  WriteLn('=== Boolean Assertions ===');
  
  // BeTrue
  try
    ShouldBoolean.Create(True).BeTrue;
    Pass('BeTrue - value is True');
  except
    on E: Exception do Fail('BeTrue', E.Message);
  end;
  
  // Global Should(Boolean)
  try
    Should(True).BeTrue;
    Pass('Global Should(Boolean)');
  except
    on E: Exception do Fail('Global Should(Boolean)', E.Message);
  end;
  
  // BeFalse
  try
    ShouldBoolean.Create(False).BeFalse;
    Pass('BeFalse - value is False');
  except
    on E: Exception do Fail('BeFalse', E.Message);
  end;
  
  // Be
  try
    ShouldBoolean.Create(True).Be(True);
    Pass('Be - True = True');
  except
    on E: Exception do Fail('Be', E.Message);
  end;
end;

procedure TestActionAssertions;
begin
  WriteLn('');
  WriteLn('=== Action Assertions ===');
  
  // Throw
  try
    ShouldAction.Create(
      procedure
      begin
        raise EInvalidOp.Create('Test error');
      end).Throw<EInvalidOp>;
    Pass('Throw - caught expected exception');
  except
    on E: EAssertionFailed do Fail('Throw', E.Message);
    on E: Exception do Fail('Throw', 'Unexpected: ' + E.Message);
  end;
  
  // Global Should(Action)
  try
    Should(procedure begin raise EInvalidOp.Create('Global'); end).Throw<EInvalidOp>;
    Pass('Global Should(Action)');
  except
    on E: Exception do Fail('Global Should(Action)', E.Message);
  end;
  
  // NotThrow
  try
    ShouldAction.Create(
      procedure
      begin
        // Do nothing - no exception
      end).NotThrow;
    Pass('NotThrow - no exception thrown');
  except
    on E: Exception do Fail('NotThrow', E.Message);
  end;
  
  // Throw wrong type
  try
    ShouldAction.Create(
      procedure
      begin
        raise EInvalidOp.Create('Test error');
      end).Throw<EAbort>;
    Fail('Throw wrong type', 'Should have thrown EAssertionFailed');
  except
    on E: EAssertionFailed do Pass('Throw - wrong exception type detected');
  end;
end;

procedure TestDoubleAssertions;
begin
  WriteLn('');
  WriteLn('=== Double Assertions ===');
  
  // BeApproximately
  try
    ShouldDouble.Create(3.14159).BeApproximately(3.14, 0.01);
    Pass('BeApproximately - within tolerance');
  except
    on E: Exception do Fail('BeApproximately', E.Message);
  end;
  
  // Global Should(Double)
  try
    Should(Double(3.14)).BeApproximately(3.14, 0.001);
    Pass('Global Should(Double)');
  except
    on E: Exception do Fail('Global Should(Double)', E.Message);
  end;
  
  // BePositive
  try
    ShouldDouble.Create(1.5).BePositive;
    Pass('BePositive - 1.5 is positive');
  except
    on E: Exception do Fail('BePositive', E.Message);
  end;
  
  // BeInRange
  try
    ShouldDouble.Create(5.5).BeInRange(5.0, 6.0);
    Pass('BeInRange - 5.5 in [5.0, 6.0]');
  except
    on E: Exception do Fail('BeInRange', E.Message);
  end;
end;

procedure TestDateTimeAssertions;
var
  NowTime: TDateTime;
begin
  WriteLn('');
  WriteLn('=== DateTime Assertions ===');
  NowTime := Now;
  
  // BeAround
  try
    ShouldDate(NowTime).BeCloseTo(NowTime + (1.0 / 86400.0), 1500); // 1.5s tolerance
    Pass('BeCloseTo - within tolerance');
  except
    on E: Exception do Fail('BeCloseTo', E.Message);
  end;
  
  // BeAfter
  try
    ShouldDate(NowTime).BeAfter(NowTime - 1.0);
    Pass('BeAfter - today is after yesterday');
  except
    on E: Exception do Fail('BeAfter', E.Message);
  end;
  
  // BeSameDateAs
  try
    // Use fixed date to avoid midnight crossing issues
    NowTime := EncodeDateTime(2026, 1, 1, 12, 0, 0, 0);
    ShouldDate(NowTime).BeSameDateAs(NowTime + 0.1); // Same day, different time
    Pass('BeSameDateAs - ignoring time');
  except
    on E: Exception do Fail('BeSameDateAs', E.Message);
  end;
end;

procedure TestObjectAssertions;
var
  Obj: TObject;
  List1, List2: TList<Integer>;
begin
  WriteLn('');
  WriteLn('=== Object Assertions ===');
  Obj := TObject.Create;
  try
    Should(Obj).NotBeNil;
    Pass('Global Should(Object) NotBeNil');
    
    Should(nil).BeNil;
    Pass('Global Should(Object) BeNil');
  finally
    Obj.Free;
  end;
  
  // Deep Equality Check (BeEquivalentTo)
  List1 := TList<Integer>.Create;
  List2 := TList<Integer>.Create;
  try
    List1.Add(10); List1.Add(20);
    List2.Add(10); List2.Add(20);
    
    try
      Should(List1).BeEquivalentTo(List2);
      Pass('BeEquivalentTo - Deep equal lists (different pointers)');
    except
      on E: Exception do Fail('BeEquivalentTo', E.Message);
    end;
    
    List2.Add(30);
    try
      Should(List1).BeEquivalentTo(List2);
      Fail('BeEquivalentTo - Different lists', 'Should have failed');
    except
      on E: EAssertionFailed do Pass('BeEquivalentTo - Detected difference');
    end;
    
  finally
    List1.Free;
    List2.Free;
  end;
end;

begin
  try
    WriteLn('======================================');
    WriteLn('   Dext Assertions Test Suite        ');
    WriteLn('======================================');
    
    TestStringAssertions;
    TestIntegerAssertions;
    TestBooleanAssertions;
    TestActionAssertions;
    TestDoubleAssertions;
    TestListAssertions;
    TestDateTimeAssertions;
    TestObjectAssertions;
    
    WriteLn('');
    WriteLn('======================================');
    WriteLn(Format('   Results: %d passed, %d failed', [TestsPassed, TestsFailed]));
    WriteLn('======================================');
    
    if TestsFailed > 0 then
      ExitCode := 1;
      
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  ConsolePause;
end.
