/// <summary>
/// Counter.MVU.Tests - Unit Tests for MVU Counter
///
/// This unit demonstrates the excellent testability of the MVU pattern.
/// Since the Update function is PURE (no side effects), tests are trivial:
/// - Setup initial state
/// - Call Update with a message
/// - Assert the result
///
/// No mocking required! No UI dependencies!
/// </summary>
unit Counter.MVU.Tests;

interface

uses
  DUnitX.TestFramework,
  Counter.MVU;

type
  [TestFixture]
  TCounterUpdateTests = class
  public
    [Setup]
    procedure Setup;
    
    [Test]
    procedure TestInit_ShouldReturnZeroCount;
    
    [Test]
    procedure TestInit_ShouldReturnStepOne;
    
    [Test]
    procedure TestIncrement_ShouldAddOne;
    
    [Test]
    procedure TestDecrement_ShouldSubtractOne;
    
    [Test]
    procedure TestIncrementByStep_WithStep5_ShouldAdd5;
    
    [Test]
    procedure TestDecrementByStep_WithStep10_ShouldSubtract10;
    
    [Test]
    procedure TestReset_ShouldReturnToZero;
    
    [Test]
    procedure TestSetStep_ShouldChangeStep;
    
    [Test]
    procedure TestMultipleOperations_ShouldAccumulate;
    
    [Test]
    procedure TestImmutability_OriginalShouldNotChange;
    
    [Test]
    procedure TestHistory_ShouldRecordActions;
  end;

implementation

uses
  System.SysUtils;

{ TCounterUpdateTests }

procedure TCounterUpdateTests.Setup;
begin
  // Nothing to setup - MVU is stateless!
end;

procedure TCounterUpdateTests.TestInit_ShouldReturnZeroCount;
var
  Model: TCounterModel;
begin
  Model := TCounterModel.Init;
  
  Assert.AreEqual(0, Model.Count);
end;

procedure TCounterUpdateTests.TestInit_ShouldReturnStepOne;
var
  Model: TCounterModel;
begin
  Model := TCounterModel.Init;
  
  Assert.AreEqual(1, Model.Step);
end;

procedure TCounterUpdateTests.TestIncrement_ShouldAddOne;
var
  Initial, Result: TCounterModel;
begin
  // Arrange
  Initial := TCounterModel.Init;
  
  // Act
  Result := TCounterUpdate.Update(Initial, IncrementMsg);
  
  // Assert
  Assert.AreEqual(1, Result.Count);
end;

procedure TCounterUpdateTests.TestDecrement_ShouldSubtractOne;
var
  Initial, Result: TCounterModel;
begin
  Initial := TCounterModel.Init;
  Initial := Initial.WithCount(10);
  
  Result := TCounterUpdate.Update(Initial, DecrementMsg);
  
  Assert.AreEqual(9, Result.Count);
end;

procedure TCounterUpdateTests.TestIncrementByStep_WithStep5_ShouldAdd5;
var
  Initial, Result: TCounterModel;
begin
  Initial := TCounterModel.Init;
  Initial := Initial.WithStep(5);
  
  Result := TCounterUpdate.Update(Initial, IncrementByStepMsg);
  
  Assert.AreEqual(5, Result.Count);
end;

procedure TCounterUpdateTests.TestDecrementByStep_WithStep10_ShouldSubtract10;
var
  Initial, Result: TCounterModel;
begin
  Initial := TCounterModel.Init;
  Initial := Initial.WithCount(100).WithStep(10);
  
  Result := TCounterUpdate.Update(Initial, DecrementByStepMsg);
  
  Assert.AreEqual(90, Result.Count);
end;

procedure TCounterUpdateTests.TestReset_ShouldReturnToZero;
var
  Initial, Result: TCounterModel;
begin
  Initial := TCounterModel.Init;
  Initial := Initial.WithCount(999).WithStep(50);
  
  Result := TCounterUpdate.Update(Initial, ResetMsg);
  
  Assert.AreEqual(0, Result.Count);
  Assert.AreEqual(1, Result.Step); // Reset also resets step
end;

procedure TCounterUpdateTests.TestSetStep_ShouldChangeStep;
var
  Initial, R1, R5, R10: TCounterModel;
begin
  Initial := TCounterModel.Init;
  
  R1 := TCounterUpdate.Update(Initial, SetStep1Msg);
  R5 := TCounterUpdate.Update(Initial, SetStep5Msg);
  R10 := TCounterUpdate.Update(Initial, SetStep10Msg);
  
  Assert.AreEqual(1, R1.Step);
  Assert.AreEqual(5, R5.Step);
  Assert.AreEqual(10, R10.Step);
end;

procedure TCounterUpdateTests.TestMultipleOperations_ShouldAccumulate;
var
  Model: TCounterModel;
begin
  Model := TCounterModel.Init;
  
  // +1, +1, +1, -1, set step 5, +5 = 8
  Model := TCounterUpdate.Update(Model, IncrementMsg);
  Model := TCounterUpdate.Update(Model, IncrementMsg);
  Model := TCounterUpdate.Update(Model, IncrementMsg);
  Model := TCounterUpdate.Update(Model, DecrementMsg);
  Model := TCounterUpdate.Update(Model, SetStep5Msg);
  Model := TCounterUpdate.Update(Model, IncrementByStepMsg);
  
  Assert.AreEqual(7, Model.Count);
  Assert.AreEqual(5, Model.Step);
end;

procedure TCounterUpdateTests.TestImmutability_OriginalShouldNotChange;
var
  Original, Updated: TCounterModel;
begin
  // This test proves that the original model is NEVER modified
  Original := TCounterModel.Init;
  Original := Original.WithCount(100);
  
  // Update returns a NEW model
  Updated := TCounterUpdate.Update(Original, IncrementMsg);
  
  // Original is unchanged!
  Assert.AreEqual(100, Original.Count, 'Original should be unchanged');
  Assert.AreEqual(101, Updated.Count, 'Updated should have new value');
end;

procedure TCounterUpdateTests.TestHistory_ShouldRecordActions;
var
  Model: TCounterModel;
begin
  Model := TCounterModel.Init;
  
  Model := TCounterUpdate.Update(Model, IncrementMsg);
  
  Assert.IsTrue(Model.History.Contains('+1'), 'History should contain action');
end;

initialization
  TDUnitX.RegisterTestFixture(TCounterUpdateTests);

end.
