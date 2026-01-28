{***************************************************************************}
{                                                                           }
{           Dext Framework                                                  }
{                                                                           }
{           Copyright (C) 2025 Cesar Romero & Dext Contributors             }
{                                                                           }
{           Licensed under the Apache License, Version 2.0 (the "License"); }
{           you may not use this file except in compliance with the License.}
{           You may obtain a copy of the License at                         }
{                                                                           }
{               http://www.apache.org/licenses/LICENSE-2.0                  }
{                                                                           }
{           Unless required by applicable law or agreed to in writing,      }
{           software distributed under the License is distributed on an     }
{           "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    }
{           either express or implied. See the License for the specific     }
{           language governing permissions and limitations under the        }
{           License.                                                        }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Author:  Cesar Romero                                                    }
{  Created: 2026-01-04                                                      }
{                                                                           }
{  TestAttributeRunner - Demo for Attribute-Based Test Framework            }
{                                                                           }
{  Demonstrates the new [TestFixture] / [Test] attribute system             }
{                                                                           }
{***************************************************************************}
program TestAttributeRunner;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  System.SysUtils,
  System.Rtti,
  Dext.Assertions,
  Dext.Testing.Attributes,
  Dext.Testing.Runner,   // ITestContext
  Dext.Testing.Fluent,   // Fluent API
  Dext.Utils,
  Dext.Core.SmartTypes,
  Dext.Entity.Prototype,
  Dext.Testing.Listeners.Telemetry,
  Dext.Logging,
  Dext.Logging.Global;

type
  TSmartUser = class
  public
    FName: StringType; // Smart Property
    FAge: IntType;     // Smart Property
  end;

  TAddress = class
  private
    FCity: string;
    FZip: Integer;
  public
    constructor Create(const ACity: string; AZip: Integer);
    property City: string read FCity;
    property Zip: Integer read FZip;
  end;

  TPerson = class
  private
    FName: string;
    FAddress: TAddress;
  public
    constructor Create(const AName: string; AAddress: TAddress);
    destructor Destroy; override;
    property Name: string read FName;
    property Address: TAddress read FAddress;
  end;
  
  /// <summary>
  ///   Global setup/cleanup fixture - runs once for entire test suite.
  /// </summary>
  [TestFixture]
  TGlobalSetup = class
  public
    [AssemblyInitialize]
    class procedure GlobalSetup;
    
    [AssemblyCleanup]
    class procedure GlobalCleanup;
  end;
  /// <summary>
  ///   Example test fixture demonstrating basic attribute usage.
  /// </summary>
  [TestFixture('Calculator Tests')]
  TCalculatorTests = class
  private
    FInitialized: Boolean;
  public
    [BeforeAll]
    procedure BeforeAll;
    
    [AfterAll]
    procedure AfterAll;
    
    [Setup]
    procedure Setup;
    
    [TearDown]
    procedure TearDown;
    
    [Test]
    procedure TestAddition;
    
    [Test('Subtraction should handle negative results')]
    procedure TestSubtraction;
    
    [Test]
    [Category('Math')]
    procedure TestMultiplication;
    
    [Test]
    [Category('Math')]
    [Category('Division')]
    procedure TestDivision;
    
    [Test]
    [Ignore('Not implemented yet')]
    procedure TestModulo;
    
    // Parameterized tests with [TestCase]
    [Test]
    [TestCase(2, 3, 5)]
    [TestCase(0, 0, 0)]
    [TestCase(-1, 1, 0)]
    [TestCase(100, 200, 300)]
    procedure TestAddWithCases(A, B, Expected: Integer);
    
    [Test]
    [TestCase(10, 2, 5)]
    [TestCase(9, 3, 3)]
    [TestCase(100, 10, 10)]
    procedure TestDivideWithCases(A, B, Expected: Integer);
  end;

  /// <summary>
  ///   String manipulation tests.
  /// </summary>
  [TestFixture]
  TStringTests = class
  public
    [Test]
    procedure TestUpperCase;
    
    [Test]
    procedure TestLowerCase;
    
    [Test]
    procedure TestTrim;
    
    [Test]
    [TestCase('Hello', 'HELLO')]
    [TestCase('World', 'WORLD')]
    [TestCase('Dext', 'DEXT')]
    procedure TestUpperCaseParameterized(const Input, Expected: string);
    
    [Test]
    [Priority(1)]  // Run first
    procedure TestConcatenation;
    
    [Test]
    [Explicit('Integration test - run manually')]
    procedure TestFileRead;
  end;

  /// <summary>
  ///   Tests demonstrating assertion integration.
  /// </summary>
  [TestFixture('Assertion Integration')]
  TAssertionTests = class
  public
    [Test]
    procedure TestShouldString;
    
    [Test]
    procedure TestShouldInteger;
    
    [Test]
    procedure TestShouldBoolean;
    
    [Test]
    procedure TestShouldList;
    
    [Test]
    [Description('Verifies that fluent chaining works correctly')]
    procedure TestFluentChaining;

    [Test]
    procedure TestDeepAssertions;

    [Test]
    [Description('Verifies strongly typed assertions using Prototype.Entity<T>')]
    procedure TestSmartAssertions;
    
    [Test]
    [Description('Demonstrates automatic ITestContext injection')]
    procedure TestContextInjection(Context: ITestContext);

    [Test]
    procedure TestMultipleAssertions;
  end;

  /// <summary>
  ///   Data provider for TestCaseSource tests.
  /// </summary>
  TExternalData = class
  public
    class function GetValues: TArray<TArray<TValue>>; static;
  end;

  /// <summary>
  ///   Demonstrates [TestCaseSource] with external class.
  /// </summary>
  [TestFixture('External Data Tests')]
  TExternalDataTests = class
  public
    [Test]
    [TestCaseSource(TExternalData, 'GetValues')]
    procedure TestWithExternalData(A, B, Expected: Integer);
  end;

{ TGlobalSetup }

class procedure TGlobalSetup.GlobalSetup;
begin
  WriteLn('  🌐 [AssemblyInitialize] Global test environment setup...');
  // Initialize global resources here (database connections, config, etc.)
end;

class procedure TGlobalSetup.GlobalCleanup;
begin
  WriteLn('  🌐 [AssemblyCleanup] Global test environment cleanup...');
  // Cleanup global resources here
end;

{ TAddress }

constructor TAddress.Create(const ACity: string; AZip: Integer);
begin
  FCity := ACity;
  FZip := AZip;
end;

{ TPerson }

constructor TPerson.Create(const AName: string; AAddress: TAddress);
begin
  FName := AName;
  FAddress := AAddress;
end;

destructor TPerson.Destroy;
begin
  FAddress.Free;
  inherited;
end;

{ TCalculatorTests }

procedure TCalculatorTests.BeforeAll;
begin
  WriteLn('    [BeforeAll] Calculator tests starting...');
  FInitialized := True;
end;

procedure TCalculatorTests.AfterAll;
begin
  WriteLn('    [AfterAll] Calculator tests completed.');
end;

procedure TCalculatorTests.Setup;
begin
  // Called before each test
end;

procedure TCalculatorTests.TearDown;
begin
  // Called after each test
end;

procedure TCalculatorTests.TestAddition;
begin
  Should(2 + 2).Be(4);
end;

procedure TCalculatorTests.TestSubtraction;
begin
  Should(5 - 10).Be(-5);
end;

procedure TCalculatorTests.TestMultiplication;
begin
  Should(3 * 4).Be(12);
end;

procedure TCalculatorTests.TestDivision;
begin
  Should(10 div 2).Be(5);
end;

procedure TCalculatorTests.TestModulo;
begin
  // This test is ignored
  Should(10 mod 3).Be(1);
end;

procedure TCalculatorTests.TestAddWithCases(A, B, Expected: Integer);
begin
  Should(A + B).Be(Expected);
end;

procedure TCalculatorTests.TestDivideWithCases(A, B, Expected: Integer);
begin
  Should(A div B).Be(Expected);
end;

{ TStringTests }

procedure TStringTests.TestUpperCase;
begin
  Should('hello').BeEquivalentTo('HELLO');
end;

procedure TStringTests.TestLowerCase;
begin
  Should('WORLD'.ToLower).Be('world');
end;

procedure TStringTests.TestTrim;
begin
  Should('  trimmed  '.Trim).Be('trimmed');
end;

procedure TStringTests.TestUpperCaseParameterized(const Input, Expected: string);
begin
  Should(Input.ToUpper).Be(Expected);
end;

procedure TStringTests.TestConcatenation;
begin
  Should('Hello' + ' ' + 'World').Be('Hello World');
end;

procedure TStringTests.TestFileRead;
begin
  // Explicit test - would be skipped in normal run
  Should(True).BeTrue;
end;

{ TAssertionTests }

procedure TAssertionTests.TestShouldString;
begin
  Should('Dext Framework')
    .StartWith('Dext')
    .AndAlso.EndWith('Framework')
    .AndAlso.Contain('Frame');
end;

procedure TAssertionTests.TestShouldInteger;
begin
  Should(42)
    .BeGreaterThan(40)
    .AndAlso.BeLessThan(50)
    .AndAlso.BeInRange(40, 45);
end;

procedure TAssertionTests.TestShouldBoolean;
begin
  Should(True).BeTrue;
  Should(False).BeFalse;
  Should(1 = 1).BeTrue;
end;

procedure TAssertionTests.TestShouldList;
var
  Numbers: TArray<Integer>;
begin
  Numbers := [1, 2, 3, 4, 5];
  // List assertions - simple tests for this demo
  Should(Length(Numbers)).Be(5);
  Should(Numbers[2]).Be(3);
end;

procedure TAssertionTests.TestFluentChaining;
begin
  Should('Hello World')
    .NotBeEmpty
    .AndAlso.HaveLength(11)
    .AndAlso.StartWith('Hello')
    .AndAlso.EndWith('World');
end;

procedure TAssertionTests.TestDeepAssertions;
var
  Addr: TAddress;
  Person: TPerson;
begin
  Addr := TAddress.Create('New York', 10001);
  Person := TPerson.Create('John Doe', Addr);
  try
    // Verify Name property directly
    Should(Person).HavePropertyValue('Name', 'John Doe');
    
    // Test Deep Graph Assertion: Person.Address
    Should(Person)
      .HaveProperty('Address')
        .WhichObject // Focus shifts to Address object
          .HavePropertyValue('City', 'New York')
          .AndAlso // Still on Address object
          .HavePropertyValue('Zip', 10001);
  finally
    Person.Free;
  end;
end;

procedure TAssertionTests.TestSmartAssertions;
var
  User: TSmartUser;
  u: TSmartUser;
begin
  User := TSmartUser.Create;
  try
    User.FName := 'Alice';
    User.FAge := 30;

    // Create Prototype for Strong Typing
    u := Prototype.Entity<TSmartUser>;
    
    // Assert using Strongly Typed Property Metadata
    Should(User)
      .HaveValue(u.FName, 'Alice')
      .AndAlso
      .HaveValue(u.FAge, 30);
  finally
    User.Free;
  end;
end;

procedure TAssertionTests.TestContextInjection(Context: ITestContext);
begin
  // Demonstrate ITestContext features
  Context.WriteLine('This test demonstrates ITestContext injection');
  Context.WriteLine('Current Fixture: %s', [Context.CurrentFixture]);
  Context.WriteLine('Current Test: %s', [Context.CurrentTest]);
  
  // Verify context was properly injected
  Should(Context).NotBeNil;
  Should(Context.CurrentFixture).Be('TAssertionTests');
  Should(Context.CurrentTest).Contain('TestContextInjection');
end;

procedure TAssertionTests.TestMultipleAssertions;
begin
  // 1. Success case: All assertions pass
  Assert.Multiple(procedure
  begin
    Should(10).BeGreaterThan(5);
    Should('Dext').StartWith('D');
  end);

  // 2. Failure aggregator verification
  // We expect this block to throw an aggregated exception.
  try
    Assert.Multiple(procedure
    begin
      Should(10).Be(20);        // Fail 1
      Should('A').Be('B');      // Fail 2
      Should(True).BeTrue;      // Pass
      Should(50).BeLessThan(10);// Fail 3
    end);
    
    // If we get here, Multiple failed to throw
    Assert.Fail('Assert.Multiple should have raised an exception with collected failures');
  except
    on E: EAssertionFailed do
    begin
      // Verify the exception contains all failures
      Should(E.Message).Contain('Multiple failures (3)'); // Header
      Should(E.Message).Contain('Expected 20 but was 10');
      Should(E.Message).Contain('Expected "B" but was "A"');
      Should(E.Message).Contain('Expected 50 to be less than 10');
    end;
  end;
end;

{ TExternalData }

class function TExternalData.GetValues: TArray<TArray<TValue>>;
begin
  Result := [
    [TValue.From(1), TValue.From(2), TValue.From(3)],
    [TValue.From(10), TValue.From(20), TValue.From(30)]
  ];
end;

{ TExternalDataTests }

procedure TExternalDataTests.TestWithExternalData(A, B, Expected: Integer);
begin
  Should(A + B).Be(Expected);
end;

begin
  SetConsoleCharSet();
  try
    WriteLn;
    WriteLn('🧪 Dext Attribute-Based Testing Demo');
    WriteLn('=====================================');
    WriteLn;

    // Register Telemetry Listener
    TTestRunner.RegisterListener(TTelemetryTestListener.Create(Log.Logger));
    
    // ✨ NEW FLUENT API - Clean and intuitive!
    if TTest.Configure
      .Verbose
////      .UseDashboard(9000)
      .RegisterFixtures([TGlobalSetup, TCalculatorTests, TStringTests, TAssertionTests, TExternalDataTests])
      .ExportToJUnit('test-results.xml')
      .ExportToJson('test-results.json')
      .ExportToHtml('test-results.html')
      .Run then
      ExitCode := 0
    else
      ExitCode := 1;
      
    // Give time for async logs (Sidecar Sink) to flush before process kill
    Sleep(1000);
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  ConsolePause;
end.
