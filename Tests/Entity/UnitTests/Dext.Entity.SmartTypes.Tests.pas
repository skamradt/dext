unit Dext.Entity.SmartTypes.Tests;

interface

uses
  System.SysUtils,
  Dext.Assertions,
  Dext.Testing.Attributes,
  Dext.Core.SmartTypes;

type
  [TestFixture('SmartTypes (Prop<T>) Tests')]
  TSmartTypesTests = class
  public
    [Test]
    [Description('Verify explicit casts from Prop<T> to common types')]
    procedure TestExplicitCasts;

    [Test]
    [Description('Verify fluent .AsXXXX methods for conversion')]
    procedure TestAsMethods;

    [Test]
    [Description('Verify generic .As<T> conversion')]
    procedure TestGenericAs;

    [Test]
    [Description('Verify assertions working directly with Prop<T>')]
    procedure TestSmartAssertions;

    [Test]
    [Description('Verify assertions working with Nullable<T>')]
    procedure TestNullableAssertions;
  end;

implementation

{ TSmartTypesTests }

procedure TSmartTypesTests.TestExplicitCasts;
var
  IntProp: Prop<Integer>;
  StrProp: Prop<string>;
  NumStrProp: Prop<string>;
  BoolProp: Prop<Boolean>;
begin
  IntProp := 10;
  Should(Integer(IntProp)).Be(10);
  Should(string(IntProp)).Be('10');

  StrProp := 'Dext';
  Should(string(StrProp)).Be('Dext');
  
  NumStrProp := '123';
  Should(Integer(NumStrProp)).Be(123);
  
  BoolProp := True;
  Should(Boolean(BoolProp)).BeTrue;
  Should(string(BoolProp)).BeEquivalentTo('True');
end;

procedure TSmartTypesTests.TestAsMethods;
var
  Age: Prop<Integer>;
  Price: Prop<Double>;
begin
  Age := 25;
  Should(Age.AsInteger).Be(25);
  Should(Age.AsString).Be('25');
  
  Price := 1500.50;
  Should(Price.AsDouble).Be(1500.50);
  Should(Price.AsString).StartWith('1500');
end;

procedure TSmartTypesTests.TestGenericAs;
var
  Value: Prop<Integer>;
begin
  Value := 100;
  Should(Value.As<Int64>).Be(100);
end;

procedure TSmartTypesTests.TestSmartAssertions;
var
  S: StringType;
  I: IntType;
begin
  S := 'Framework';
  Should(S).StartWith('Frame');
  Should(S).NotBeEmpty;
  
  I := 100;
  Should(I).BeGreaterThan(50);
  Should(I).Be(100);
end;

procedure TSmartTypesTests.TestNullableAssertions;
var
  N: Nullable<Integer>;
  D: Nullable<TDateTime>;
begin
  N := 42;
  Should(N).Be(42);
  
  D := Now;
  ShouldDate(D).BeToday;
  
  // Test failure case (Manual check if needed, but here we just verify success)
  N := Nullable<Integer>.Null;
  // Should(N).Be(0); // This would call Assert.Fail as expected
end;

initialization
  // Fixtures are registered in the main runner
end.
