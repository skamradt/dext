{***************************************************************************}
{                                                                           }
{           Dext Framework - New Assertions Test (2026-01-03)               }
{                                                                           }
{           Tests for: ShouldInt64, ShouldGuid, ShouldUUID, ShouldVariant   }
{                      New string/list methods, HaveProperty, And chaining  }
{                                                                           }
{***************************************************************************}
program TestNewAssertions;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  System.SysUtils,
  System.Generics.Collections,
  System.Rtti,
  System.Variants,
  Dext.Assertions,
  Dext.Types.UUID;

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

// ============================================================================
// TEST: ShouldInt64
// ============================================================================
procedure TestShouldInt64;
var
  BigValue: Int64;
begin
  WriteLn('');
  WriteLn('=== ShouldInt64 Tests ===');
  
  BigValue := 9223372036854775807; // Max Int64
  
  try
    Should(BigValue).Be(9223372036854775807);
    Pass('Int64.Be (max value)');
  except on E: Exception do Fail('Int64.Be', E.Message); end;
  
  try
    Should(Int64(100)).NotBe(200);
    Pass('Int64.NotBe');
  except on E: Exception do Fail('Int64.NotBe', E.Message); end;
  
  try
    Should(Int64(50)).BeGreaterThan(10);
    Pass('Int64.BeGreaterThan');
  except on E: Exception do Fail('Int64.BeGreaterThan', E.Message); end;
  
  try
    Should(Int64(50)).BeLessThan(100);
    Pass('Int64.BeLessThan');
  except on E: Exception do Fail('Int64.BeLessThan', E.Message); end;
  
  try
    Should(Int64(50)).BeInRange(40, 60);
    Pass('Int64.BeInRange');
  except on E: Exception do Fail('Int64.BeInRange', E.Message); end;
  
  try
    Should(Int64(100)).BePositive;
    Pass('Int64.BePositive');
  except on E: Exception do Fail('Int64.BePositive', E.Message); end;
  
  try
    Should(Int64(-50)).BeNegative;
    Pass('Int64.BeNegative');
  except on E: Exception do Fail('Int64.BeNegative', E.Message); end;
  
  try
    Should(Int64(0)).BeZero;
    Pass('Int64.BeZero');
  except on E: Exception do Fail('Int64.BeZero', E.Message); end;
  
  try
    Should(Int64(5)).BeOneOf([1, 3, 5, 7, 9]);
    Pass('Int64.BeOneOf');
  except on E: Exception do Fail('Int64.BeOneOf', E.Message); end;
  
  try
    Should(Int64(42)).Satisfy(function(V: Int64): Boolean
                              begin
                                Result := V > 40;
                              end);
    Pass('Int64.Satisfy');
  except on E: Exception do Fail('Int64.Satisfy', E.Message); end;
  
  // And chaining
  try
    Should(Int64(50)).BePositive.&And.BeLessThan(100);
    Pass('Int64.And chaining');
  except on E: Exception do Fail('Int64.And chaining', E.Message); end;
end;

// ============================================================================
// TEST: ShouldGuid
// ============================================================================
procedure TestShouldGuid;
var
  G1, G2: TGUID;
begin
  WriteLn('');
  WriteLn('=== ShouldGuid Tests ===');
  
  G1 := TGUID.Create('{12345678-1234-1234-1234-123456789ABC}');
  G2 := G1;
  
  try
    Should(G1).Be(G2);
    Pass('GUID.Be');
  except on E: Exception do Fail('GUID.Be', E.Message); end;
  
  try
    Should(G1).NotBe(TGUID.Empty);
    Pass('GUID.NotBe');
  except on E: Exception do Fail('GUID.NotBe', E.Message); end;
  
  try
    Should(TGUID.Empty).BeEmpty;
    Pass('GUID.BeEmpty');
  except on E: Exception do Fail('GUID.BeEmpty', E.Message); end;
  
  try
    Should(G1).NotBeEmpty;
    Pass('GUID.NotBeEmpty');
  except on E: Exception do Fail('GUID.NotBeEmpty', E.Message); end;
  
  // And chaining
  try
    Should(G1).NotBeEmpty.&And.NotBe(TGUID.Empty);
    Pass('GUID.And chaining');
  except on E: Exception do Fail('GUID.And chaining', E.Message); end;
end;

// ============================================================================
// TEST: ShouldUUID
// ============================================================================
procedure TestShouldUUID;
var
  U1, U2: TUUID;
begin
  WriteLn('');
  WriteLn('=== ShouldUUID Tests ===');
  
  U1 := TUUID.NewV4;
  U2 := U1;
  
  try
    Should(U1).Be(U2);
    Pass('UUID.Be');
  except on E: Exception do Fail('UUID.Be', E.Message); end;
  
  try
    Should(U1).NotBe(TUUID.Empty);
    Pass('UUID.NotBe');
  except on E: Exception do Fail('UUID.NotBe', E.Message); end;
  
  try
    Should(TUUID.Empty).BeEmpty;
    Pass('UUID.BeEmpty');
  except on E: Exception do Fail('UUID.BeEmpty', E.Message); end;
  
  try
    Should(U1).NotBeEmpty;
    Pass('UUID.NotBeEmpty');
  except on E: Exception do Fail('UUID.NotBeEmpty', E.Message); end;
  
  // And chaining
  try
    Should(U1).NotBeEmpty.&And.NotBe(TUUID.Empty);
    Pass('UUID.And chaining');
  except on E: Exception do Fail('UUID.And chaining', E.Message); end;
end;

// ============================================================================
// TEST: ShouldVariant
// ============================================================================
procedure TestShouldVariant;
var
  V: Variant;
begin
  WriteLn('');
  WriteLn('=== ShouldVariant Tests ===');
  
  V := 'Hello World';
  
  try
    Should(V).Be('Hello World');
    Pass('Variant.Be (string)');
  except on E: Exception do Fail('Variant.Be', E.Message); end;
  
  V := 42;
  try
    Should(V).Be(42);
    Pass('Variant.Be (integer)');
  except on E: Exception do Fail('Variant.Be', E.Message); end;
  
  try
    Should(V).NotBe(100);
    Pass('Variant.NotBe');
  except on E: Exception do Fail('Variant.NotBe', E.Message); end;
  
  V := Null;
  try
    Should(V).BeNull;
    Pass('Variant.BeNull');
  except on E: Exception do Fail('Variant.BeNull', E.Message); end;
  
  V := 'Test';
  try
    Should(V).NotBeNull;
    Pass('Variant.NotBeNull');
  except on E: Exception do Fail('Variant.NotBeNull', E.Message); end;
  
  V := Unassigned;
  try
    Should(V).BeEmpty;
    Pass('Variant.BeEmpty');
  except on E: Exception do Fail('Variant.BeEmpty', E.Message); end;
  
  V := 123;
  try
    Should(V).NotBeEmpty;
    Pass('Variant.NotBeEmpty');
  except on E: Exception do Fail('Variant.NotBeEmpty', E.Message); end;
end;

// ============================================================================
// TEST: New String Methods
// ============================================================================
procedure TestNewStringMethods;
begin
  WriteLn('');
  WriteLn('=== New String Methods ===');
  
  try
    Should('Hello').NotBe('World');
    Pass('String.NotBe');
  except on E: Exception do Fail('String.NotBe', E.Message); end;
  
  try
    Should('test@example.com').MatchRegex('^\w+@\w+\.\w+$');
    Pass('String.MatchRegex');
  except on E: Exception do Fail('String.MatchRegex', E.Message); end;
  
  try
    Should('Hello').HaveLengthGreaterThan(3);
    Pass('String.HaveLengthGreaterThan');
  except on E: Exception do Fail('String.HaveLengthGreaterThan', E.Message); end;
  
  try
    Should('Hi').HaveLengthLessThan(10);
    Pass('String.HaveLengthLessThan');
  except on E: Exception do Fail('String.HaveLengthLessThan', E.Message); end;
  
  try
    Should('HELLO').BeUpperCase;
    Pass('String.BeUpperCase');
  except on E: Exception do Fail('String.BeUpperCase', E.Message); end;
  
  try
    Should('hello').BeLowerCase;
    Pass('String.BeLowerCase');
  except on E: Exception do Fail('String.BeLowerCase', E.Message); end;
  
  try
    Should('B').BeOneOf(['A', 'B', 'C']);
    Pass('String.BeOneOf');
  except on E: Exception do Fail('String.BeOneOf', E.Message); end;
  
  try
    Should('Test').Satisfy(function(S: string): Boolean begin Result := Length(S) = 4; end);
    Pass('String.Satisfy');
  except on E: Exception do Fail('String.Satisfy', E.Message); end;
  
  // And chaining
  try
    Should('HELLO').BeUpperCase.&And.HaveLengthGreaterThan(3);
    Pass('String.And chaining');
  except on E: Exception do Fail('String.And chaining', E.Message); end;
end;

// ============================================================================
// TEST: New List Methods
// ============================================================================
procedure TestNewListMethods;
var
  Arr: TArray<Integer>;
begin
  WriteLn('');
  WriteLn('=== New List Methods ===');
  
  Arr := [1, 2, 3, 4, 5];
  
  try
    ShouldList<Integer>.Create(Arr).HaveCountLessThan(10);
    Pass('List.HaveCountLessThan');
  except on E: Exception do Fail('List.HaveCountLessThan', E.Message); end;
  
  try
    ShouldList<Integer>.Create(Arr).ContainInOrder([1, 3, 5]);
    Pass('List.ContainInOrder');
  except on E: Exception do Fail('List.ContainInOrder', E.Message); end;
  
  try
    ShouldList<Integer>.Create([3, 1, 2]).BeEquivalentTo([1, 2, 3]);
    Pass('List.BeEquivalentTo (order-independent)');
  except on E: Exception do Fail('List.BeEquivalentTo', E.Message); end;
  
  try
    ShouldList<Integer>.Create(Arr).OnlyContain(function(X: Integer): Boolean begin Result := X > 0; end);
    Pass('List.OnlyContain');
  except on E: Exception do Fail('List.OnlyContain', E.Message); end;
  
  // And chaining
  try
    ShouldList<Integer>.Create(Arr).HaveCount(5).&And.Contain(3);
    Pass('List.And chaining');
  except on E: Exception do Fail('List.And chaining', E.Message); end;
end;

// ============================================================================
// TEST: HaveProperty
// ============================================================================
type
  TTestPerson = class
  private
    FName: string;
    FAge: Integer;
  public
    constructor Create(const AName: string; AAge: Integer);
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
  end;
  
constructor TTestPerson.Create(const AName: string; AAge: Integer);
begin
  inherited Create;
  FName := AName;
  FAge := AAge;
end;

procedure TestHaveProperty;
var
  Person: TTestPerson;
begin
  WriteLn('');
  WriteLn('=== HaveProperty Tests ===');
  
  Person := TTestPerson.Create('John', 30);
  try
    try
      Should(Person).HaveProperty('Name');
      Pass('HaveProperty (exists)');
    except on E: Exception do Fail('HaveProperty (exists)', E.Message); end;
    
    try
      Should(Person).HavePropertyValue('Name', TValue.From<string>('John'));
      Pass('HavePropertyValue (string)');
    except on E: Exception do Fail('HavePropertyValue (string)', E.Message); end;
    
    try
      Should(Person).HavePropertyValue('Age', TValue.From<Integer>(30));
      Pass('HavePropertyValue (integer)');
    except on E: Exception do Fail('HavePropertyValue (integer)', E.Message); end;
    
    // And chaining with other assertions
    try
      Should(Person).NotBeNil.&And.HaveProperty('Name');
      Pass('HaveProperty.And chaining');
    except on E: Exception do Fail('HaveProperty.And chaining', E.Message); end;
  finally
    Person.Free;
  end;
end;

// ============================================================================
// TEST: Other And Chaining
// ============================================================================
procedure TestAndChaining;
begin
  WriteLn('');
  WriteLn('=== And Chaining Tests ===');
  
  try
    Should(50).BeGreaterThan(10).&And.BeLessThan(100);
    Pass('Integer.And chaining');
  except on E: Exception do Fail('Integer.And chaining', E.Message); end;
  
  try
    Should(Double(3.14)).BeGreaterThan(3.0).&And.BeLessThan(4.0);
    Pass('Double.And chaining');
  except on E: Exception do Fail('Double.And chaining', E.Message); end;
  
  try
    Should(True).BeTrue.&And.NotBe(False);
    Pass('Boolean.And chaining');
  except on E: Exception do Fail('Boolean.And chaining', E.Message); end;
end;

// ============================================================================
// MAIN
// ============================================================================
begin
  try
    WriteLn('');
    WriteLn('========================================');
    WriteLn('  Dext New Assertions Test Suite');
    WriteLn('  2026-01-03');
    WriteLn('========================================');
    
    TestShouldInt64;
    TestShouldGuid;
    TestShouldUUID;
    TestShouldVariant;
    TestNewStringMethods;
    TestNewListMethods;
    TestHaveProperty;
    TestAndChaining;
    
    WriteLn('');
    WriteLn('========================================');
    WriteLn(Format('  Results: %d passed, %d failed', [TestsPassed, TestsFailed]));
    WriteLn('========================================');
    
    if TestsFailed > 0 then
      ExitCode := 1;
      
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  WriteLn('');
  WriteLn('Press ENTER to exit...');
  ReadLn;
end.
