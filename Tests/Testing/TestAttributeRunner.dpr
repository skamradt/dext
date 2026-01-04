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
  Dext.MM, // Memory manager first
  System.SysUtils,
  System.Rtti,
  Dext.Assertions,
  Dext.Testing.Attributes,
  Dext.Testing.Fluent,  // Fluent API
  Dext.Utils;

type
  // Enable RTTI for attribute-based test discovery
  {$RTTI EXPLICIT METHODS([vcPublic, vcPublished]) PROPERTIES([vcPublic, vcPublished])}
  {$M+}
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

begin
  SetConsoleCharSet();
  try
    WriteLn;
    WriteLn('🧪 Dext Attribute-Based Testing Demo');
    WriteLn('=====================================');
    WriteLn;
    
    // ✨ NEW FLUENT API - Clean and intuitive!
    // Everything in one elegant chain:
    if TTest.Configure
      .Verbose
      .RegisterFixtures([TCalculatorTests, TStringTests, TAssertionTests])
      .ExportToJUnit('test-results.xml')
      .ExportToJson('test-results.json')
      .Run then
      ExitCode := 0
    else
      ExitCode := 1;
      
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
