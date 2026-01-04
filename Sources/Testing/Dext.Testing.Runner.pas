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
{  Dext.Testing.Runner - Attribute-Based Test Discovery and Execution       }
{                                                                           }
{  Provides automatic test discovery via RTTI scanning and execution        }
{  of tests marked with [TestFixture] and [Test] attributes.                }
{                                                                           }
{***************************************************************************}

unit Dext.Testing.Runner;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TimeSpan,
  System.TypInfo,
  System.Generics.Collections,
  System.Diagnostics,
  System.Classes,
  Dext.Testing.Attributes;

type
  /// <summary>
  ///   Result of a single test execution.
  /// </summary>
  TTestResult = (trPassed, trFailed, trSkipped, trTimeout, trError);

  /// <summary>
  ///   Detailed information about a test execution.
  /// </summary>
  TTestInfo = record
    FixtureName: string;
    TestName: string;
    DisplayName: string;
    Result: TTestResult;
    Duration: TTimeSpan;
    ErrorMessage: string;
    StackTrace: string;
    Categories: TArray<string>;
  end;

  /// <summary>
  ///   Summary of test run results.
  /// </summary>
  TTestSummary = record
    TotalTests: Integer;
    Passed: Integer;
    Failed: Integer;
    Skipped: Integer;
    Errors: Integer;
    TotalDuration: TTimeSpan;
    procedure Reset;
  end;

  /// <summary>
  ///   Filter options for test discovery and execution.
  /// </summary>
  TTestFilter = record
    Categories: TArray<string>;
    TestNamePattern: string;
    FixtureNamePattern: string;
    IncludeExplicit: Boolean;
    function Matches(const AFixtureName, ATestName: string;
      const ACategories: TArray<string>; AIsExplicit: Boolean): Boolean;
  end;

  /// <summary>
  ///   Event fired before each test runs.
  /// </summary>
  TTestStartEvent = procedure(const Fixture, Test: string) of object;

  /// <summary>
  ///   Event fired after each test completes.
  /// </summary>
  TTestCompleteEvent = procedure(const Info: TTestInfo) of object;

  /// <summary>
  ///   Event fired when a fixture starts.
  /// </summary>
  TFixtureStartEvent = procedure(const FixtureName: string; TestCount: Integer) of object;

  /// <summary>
  ///   Event fired when a fixture completes.
  /// </summary>
  TFixtureCompleteEvent = procedure(const FixtureName: string) of object;

  /// <summary>
  ///   Output format for test results.
  /// </summary>
  TOutputFormat = (ofConsole, ofXUnit, ofJUnit);

  /// <summary>
  ///   Discovered test fixture information.
  /// </summary>
  TTestFixtureInfo = class
  private
    FRttiType: TRttiType;
    FFixtureClass: TClass;
    FName: string;
    FDescription: string;
    FSetupMethod: TRttiMethod;
    FTearDownMethod: TRttiMethod;
    FBeforeAllMethod: TRttiMethod;
    FAfterAllMethod: TRttiMethod;
    FTestMethods: TList<TRttiMethod>;
  public
    constructor Create(ARttiType: TRttiType);
    destructor Destroy; override;
    property RttiType: TRttiType read FRttiType;
    property FixtureClass: TClass read FFixtureClass;
    property Name: string read FName;
    property Description: string read FDescription;
    property SetupMethod: TRttiMethod read FSetupMethod;
    property TearDownMethod: TRttiMethod read FTearDownMethod;
    property BeforeAllMethod: TRttiMethod read FBeforeAllMethod;
    property AfterAllMethod: TRttiMethod read FAfterAllMethod;
    property TestMethods: TList<TRttiMethod> read FTestMethods;
  end;

  /// <summary>
  ///   Context object injected into tests that need runtime information.
  /// </summary>
  ITestContext = interface
    ['{F1E2D3C4-B5A6-7890-1234-567890ABCDEF}']
    function GetCurrentTest: string;
    function GetCurrentFixture: string;
    procedure WriteLine(const Msg: string); overload;
    procedure WriteLine(const Fmt: string; const Args: array of const); overload;
    procedure AttachFile(const FilePath: string);
    property CurrentTest: string read GetCurrentTest;
    property CurrentFixture: string read GetCurrentFixture;
  end;

  /// <summary>
  ///   Main test runner that discovers and executes attribute-based tests.
  /// </summary>
  TTestRunner = class
  private
    class var FContext: TRttiContext;
    class var FFixtures: TObjectList<TTestFixtureInfo>;
    class var FSummary: TTestSummary;
    class var FFilter: TTestFilter;
    class var FVerbose: Boolean;
    class var FDebugDiscovery: Boolean;
    class var FOutputFormat: TOutputFormat;
    class var FReportFileName: string;
    class var FReportFormat: TOutputFormat;
    class var FTestResults: TList<TTestInfo>;

    class var FOnTestStart: TTestStartEvent;
    class var FOnTestComplete: TTestCompleteEvent;
    class var FOnFixtureStart: TFixtureStartEvent;
    class var FOnFixtureComplete: TFixtureCompleteEvent;

    class procedure DiscoverFixtures;
    class procedure DiscoverTestMethods(Fixture: TTestFixtureInfo);
    class function HasAttribute<T: TCustomAttribute>(
      const Attrs: TArray<TCustomAttribute>): Boolean; overload;
    class function GetAttribute<T: TCustomAttribute>(
      const Attrs: TArray<TCustomAttribute>): T; overload;
    class function GetAttributes<T: TCustomAttribute>(
      const Attrs: TArray<TCustomAttribute>): TArray<T>;
    class function GetCategories(Method: TRttiMethod): TArray<string>;
    class function IsExplicit(Method: TRttiMethod): Boolean;
    class function GetIgnoreReason(Method: TRttiMethod): string;
    class function GetTimeout(Method: TRttiMethod): Integer;
    class function GetRepeatCount(Method: TRttiMethod): Integer;
    class function GetMaxTime(Method: TRttiMethod): Integer;
    class function GetPriority(Method: TRttiMethod): Integer;
    class function ShouldRunOnPlatform(Method: TRttiMethod): Boolean;
    class procedure ExecuteFixture(Fixture: TTestFixtureInfo);
    class procedure ExecuteTest(Fixture: TTestFixtureInfo;
      Method: TRttiMethod; Instance: TObject;
      const TestCaseValues: TArray<TValue>;
      const TestCaseDisplayName: string);
    class function GetTestCases(Method: TRttiMethod): TArray<TArray<TValue>>;
    class function GetTestCaseDisplayNames(Method: TRttiMethod): TArray<string>;
    class procedure PrintTestResult(const Info: TTestInfo);
    class procedure PrintSummary;
    class procedure PrintResultChar(Result: TTestResult);
  public
    /// <summary>
    ///   Discovers all test fixtures in the application.
    /// </summary>
    class procedure Discover;

    /// <summary>
    ///   Runs all discovered tests.
    /// </summary>
    class procedure RunAll;

    /// <summary>
    ///   Runs tests matching the specified filter.
    /// </summary>
    class procedure RunFiltered(const AFilter: TTestFilter);

    /// <summary>
    ///   Runs tests in a specific category.
    /// </summary>
    class procedure RunCategory(const Category: string);

    /// <summary>
    ///   Runs a single test fixture by class.
    /// </summary>
    class procedure RunFixture(AFixtureClass: TClass);

    /// <summary>
    ///   Runs a single test by name pattern.
    /// </summary>
    class procedure RunTest(const ATestNamePattern: string);

    /// <summary>
    ///   Returns the test summary after a run.
    /// </summary>
    class function Summary: TTestSummary;

    /// <summary>
    ///   Sets verbose output mode.
    /// </summary>
    class procedure SetVerbose(AValue: Boolean);

    /// <summary>
    ///   Sets the output format.
    /// </summary>
    class procedure SetOutputFormat(AFormat: TOutputFormat);

    /// <summary>
    ///   Returns the discovered fixture count.
    /// </summary>
    class function FixtureCount: Integer;

    /// <summary>
    ///   Returns the total discovered test count.
    /// </summary>
    class function TestCount: Integer;

    /// <summary>
    ///   Clears discovered fixtures (for re-discovery).
    /// </summary>
    class procedure Clear;

    /// <summary>
    ///   Registers a test fixture class manually.
    ///   Use this for classes defined in the main program (.dpr)
    ///   where RTTI discovery may not work automatically.
    /// </summary>
    class procedure RegisterFixture(AClass: TClass);

    /// <summary>
    ///   Enables debug output during discovery phase.
    /// </summary>
    class procedure SetDebugDiscovery(AValue: Boolean);

    /// <summary>
    ///   Configures automatic report file generation after test run.
    /// </summary>
    /// <param name="FileName">Output file path (e.g., 'test-results.xml')</param>
    /// <param name="Format">Report format (ofConsole, ofXUnit, ofJUnit)</param>
    class procedure SetReportFile(const FileName: string; Format: TOutputFormat = ofJUnit);

    /// <summary>
    ///   Saves test results to a JUnit XML file.
    ///   Call this after RunAll or RunFiltered.
    /// </summary>
    class procedure SaveJUnitReport(const FileName: string);

    /// <summary>
    ///   Saves test results to a JSON file.
    ///   Call this after RunAll or RunFiltered.
    /// </summary>
    class procedure SaveJsonReport(const FileName: string);

    // Events
    class property OnTestStart: TTestStartEvent read FOnTestStart write FOnTestStart;
    class property OnTestComplete: TTestCompleteEvent read FOnTestComplete write FOnTestComplete;
    class property OnFixtureStart: TFixtureStartEvent read FOnFixtureStart write FOnFixtureStart;
    class property OnFixtureComplete: TFixtureCompleteEvent read FOnFixtureComplete write FOnFixtureComplete;
  end;

  /// <summary>
  ///   Console output helpers for test results.
  /// </summary>
  TTestConsole = class
  public
    class procedure WriteColored(const Text: string; Color: Word);
    class procedure WritePass(const Text: string);
    class procedure WriteFail(const Text: string);
    class procedure WriteSkip(const Text: string);
    class procedure WriteInfo(const Text: string);
    class procedure WriteHeader(const Text: string);
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.StrUtils,
  System.RegularExpressions,
  System.IOUtils,
  Dext.Utils,
  Dext.Testing.Report;

const
  CONSOLE_COLOR_GREEN = 10;
  CONSOLE_COLOR_RED = 12;
  CONSOLE_COLOR_YELLOW = 14;
  CONSOLE_COLOR_CYAN = 11;
  CONSOLE_COLOR_WHITE = 15;
  CONSOLE_COLOR_GRAY = 8;

{ TTestSummary }

procedure TTestSummary.Reset;
begin
  TotalTests := 0;
  Passed := 0;
  Failed := 0;
  Skipped := 0;
  Errors := 0;
  TotalDuration := TTimeSpan.Zero;
end;

{ TTestFilter }

function TTestFilter.Matches(const AFixtureName, ATestName: string;
  const ACategories: TArray<string>; AIsExplicit: Boolean): Boolean;
var
  Cat, FilterCat: string;
  CategoryMatch: Boolean;
begin
  // Explicit tests only run when explicitly requested
  if AIsExplicit and not IncludeExplicit then
    Exit(False);

  // Check fixture name pattern
  if (FixtureNamePattern <> '') and
     not ContainsText(AFixtureName, FixtureNamePattern) then
    Exit(False);

  // Check test name pattern
  if (TestNamePattern <> '') and
     not ContainsText(ATestName, TestNamePattern) then
    Exit(False);

  // Check categories
  if Length(Categories) > 0 then
  begin
    CategoryMatch := False;
    for FilterCat in Categories do
    begin
      for Cat in ACategories do
      begin
        if SameText(Cat, FilterCat) then
        begin
          CategoryMatch := True;
          Break;
        end;
      end;
      if CategoryMatch then
        Break;
    end;
    if not CategoryMatch then
      Exit(False);
  end;

  Result := True;
end;

{ TTestFixtureInfo }

constructor TTestFixtureInfo.Create(ARttiType: TRttiType);
var
  Attr: TCustomAttribute;
begin
  inherited Create;
  FRttiType := ARttiType;
  FFixtureClass := ARttiType.AsInstance.MetaclassType;
  FName := ARttiType.Name;
  FTestMethods := TList<TRttiMethod>.Create;

  // Get description from attribute
  for Attr in ARttiType.GetAttributes do
  begin
    if Attr is TestFixtureAttribute then
    begin
      FDescription := TestFixtureAttribute(Attr).Description;
      Break;
    end;
  end;
end;

destructor TTestFixtureInfo.Destroy;
begin
  FTestMethods.Free;
  inherited;
end;

{ TTestRunner }

class procedure TTestRunner.Discover;
begin
  if FFixtures = nil then
    FFixtures := TObjectList<TTestFixtureInfo>.Create(True);

  FFixtures.Clear;
  FSummary.Reset;

  FContext := TRttiContext.Create;
  try
    DiscoverFixtures;
  finally
    // Context is kept for execution
  end;
end;

class procedure TTestRunner.DiscoverFixtures;
var
  RttiType: TRttiType;
  Attr: TCustomAttribute;
  Fixture: TTestFixtureInfo;
  TypeCount, InstanceCount, FixtureAttrCount: Integer;
begin
  TypeCount := 0;
  InstanceCount := 0;
  FixtureAttrCount := 0;
  
  for RttiType in FContext.GetTypes do
  begin
    Inc(TypeCount);
    
    if not (RttiType is TRttiInstanceType) then
      Continue;
      
    Inc(InstanceCount);
    
    if FDebugDiscovery then
      WriteLn('  [Discovery] Checking: ', RttiType.QualifiedName);

    for Attr in RttiType.GetAttributes do
    begin
      if FDebugDiscovery then
        WriteLn('    - Attribute: ', Attr.ClassName);
        
      if (Attr is TestFixtureAttribute) then
      begin
        Inc(FixtureAttrCount);
        if FDebugDiscovery then
          WriteLn('    ** Found TestFixture!');
          
        Fixture := TTestFixtureInfo.Create(RttiType);
        DiscoverTestMethods(Fixture);
        if Fixture.TestMethods.Count > 0 then
          FFixtures.Add(Fixture)
        else
          Fixture.Free;
        Break;
      end;
    end;
  end;
  
  if FDebugDiscovery then
  begin
    WriteLn;
    WriteLn('  [Discovery Summary]');
    WriteLn('    Total types scanned: ', TypeCount);
    WriteLn('    Instance types: ', InstanceCount);
    WriteLn('    Fixtures found: ', FixtureAttrCount);
    WriteLn;
  end;
end;

class procedure TTestRunner.DiscoverTestMethods(Fixture: TTestFixtureInfo);
var
  Method: TRttiMethod;
  Attr: TCustomAttribute;
  I, J: Integer;
  TempMethod: TRttiMethod;
begin
  for Method in Fixture.RttiType.GetMethods do
  begin
    for Attr in Method.GetAttributes do
    begin
      // Test methods
      if Attr is TestAttribute then
      begin
        Fixture.TestMethods.Add(Method);
        Break;
      end
      // Setup
      else if Attr is SetupAttribute then
        Fixture.FSetupMethod := Method
      // TearDown
      else if Attr is TearDownAttribute then
        Fixture.FTearDownMethod := Method
      // BeforeAll
      else if Attr is BeforeAllAttribute then
        Fixture.FBeforeAllMethod := Method
      // AfterAll
      else if Attr is AfterAllAttribute then
        Fixture.FAfterAllMethod := Method;
    end;
  end;

  // Sort tests by priority (simple bubble sort)
  for I := 0 to Fixture.TestMethods.Count - 2 do
  begin
    for J := I + 1 to Fixture.TestMethods.Count - 1 do
    begin
      if GetPriority(Fixture.TestMethods[J]) < GetPriority(Fixture.TestMethods[I]) then
      begin
        TempMethod := Fixture.TestMethods[I];
        Fixture.TestMethods[I] := Fixture.TestMethods[J];
        Fixture.TestMethods[J] := TempMethod;
      end;
    end;
  end;
end;

class function TTestRunner.HasAttribute<T>(
  const Attrs: TArray<TCustomAttribute>): Boolean;
var
  Attr: TCustomAttribute;
begin
  for Attr in Attrs do
    if Attr is T then
      Exit(True);
  Result := False;
end;

class function TTestRunner.GetAttribute<T>(
  const Attrs: TArray<TCustomAttribute>): T;
var
  Attr: TCustomAttribute;
begin
  for Attr in Attrs do
    if Attr is T then
      Exit(T(Attr));
  Result := nil;
end;

class function TTestRunner.GetAttributes<T>(
  const Attrs: TArray<TCustomAttribute>): TArray<T>;
var
  Attr: TCustomAttribute;
  List: TList<T>;
begin
  List := TList<T>.Create;
  try
    for Attr in Attrs do
      if Attr is T then
        List.Add(T(Attr));
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

class function TTestRunner.GetCategories(Method: TRttiMethod): TArray<string>;
var
  Attrs: TArray<TCustomAttribute>;
  Attr: TCustomAttribute;
  Categories: TList<string>;
begin
  Attrs := Method.GetAttributes;
  Categories := TList<string>.Create;
  try
    for Attr in Attrs do
    begin
      if Attr is CategoryAttribute then
        Categories.Add(CategoryAttribute(Attr).Name)
      else if Attr is TraitAttribute then
        Categories.Add(TraitAttribute(Attr).Name + '=' + TraitAttribute(Attr).Value);
    end;
    Result := Categories.ToArray;
  finally
    Categories.Free;
  end;
end;

class function TTestRunner.IsExplicit(Method: TRttiMethod): Boolean;
begin
  Result := HasAttribute<ExplicitAttribute>(Method.GetAttributes);
end;

class function TTestRunner.GetIgnoreReason(Method: TRttiMethod): string;
var
  IgnoreAttr: IgnoreAttribute;
begin
  IgnoreAttr := GetAttribute<IgnoreAttribute>(Method.GetAttributes);
  if Assigned(IgnoreAttr) then
    Result := IgnoreAttr.Reason
  else
    Result := '';
end;

class function TTestRunner.GetTimeout(Method: TRttiMethod): Integer;
var
  TimeoutAttr: TimeoutAttribute;
begin
  TimeoutAttr := GetAttribute<TimeoutAttribute>(Method.GetAttributes);
  if Assigned(TimeoutAttr) then
    Result := TimeoutAttr.Milliseconds
  else
    Result := 0; // No timeout
end;

class function TTestRunner.GetRepeatCount(Method: TRttiMethod): Integer;
var
  RepeatAttr: RepeatAttribute;
begin
  RepeatAttr := GetAttribute<RepeatAttribute>(Method.GetAttributes);
  if Assigned(RepeatAttr) then
    Result := RepeatAttr.Count
  else
    Result := 1;
end;

class function TTestRunner.GetMaxTime(Method: TRttiMethod): Integer;
var
  MaxTimeAttr: MaxTimeAttribute;
begin
  MaxTimeAttr := GetAttribute<MaxTimeAttribute>(Method.GetAttributes);
  if Assigned(MaxTimeAttr) then
    Result := MaxTimeAttr.Milliseconds
  else
    Result := 0;
end;

class function TTestRunner.GetPriority(Method: TRttiMethod): Integer;
var
  PriorityAttr: PriorityAttribute;
begin
  PriorityAttr := GetAttribute<PriorityAttribute>(Method.GetAttributes);
  if Assigned(PriorityAttr) then
    Result := PriorityAttr.Priority
  else
    Result := 999; // Default low priority
end;

class function TTestRunner.ShouldRunOnPlatform(Method: TRttiMethod): Boolean;
var
  PlatformAttr: PlatformAttribute;
begin
  PlatformAttr := GetAttribute<PlatformAttribute>(Method.GetAttributes);
  if Assigned(PlatformAttr) then
    Result := PlatformAttr.ShouldRun
  else
    Result := True; // No platform restriction
end;

class function TTestRunner.GetTestCases(Method: TRttiMethod): TArray<TArray<TValue>>;
var
  TestCaseAttrs: TArray<TestCaseAttribute>;
  Attr: TestCaseAttribute;
  List: TList<TArray<TValue>>;
begin
  TestCaseAttrs := GetAttributes<TestCaseAttribute>(Method.GetAttributes);
  if Length(TestCaseAttrs) = 0 then
  begin
    // No test cases, return single empty array for parameterless execution
    SetLength(Result, 1);
    SetLength(Result[0], 0);
    Exit;
  end;

  List := TList<TArray<TValue>>.Create;
  try
    for Attr in TestCaseAttrs do
      List.Add(Attr.Values);
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

class function TTestRunner.GetTestCaseDisplayNames(Method: TRttiMethod): TArray<string>;
var
  TestCaseAttrs: TArray<TestCaseAttribute>;
  Attr: TestCaseAttribute;
  List: TList<string>;
  I: Integer;
  Values: string;
  V: TValue;
begin
  TestCaseAttrs := GetAttributes<TestCaseAttribute>(Method.GetAttributes);
  if Length(TestCaseAttrs) = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := '';
    Exit;
  end;

  List := TList<string>.Create;
  try
    for I := 0 to High(TestCaseAttrs) do
    begin
      Attr := TestCaseAttrs[I];
      if Attr.DisplayName <> '' then
        List.Add(Attr.DisplayName)
      else
      begin
        // Generate display name from values
        Values := '';
        for V in Attr.Values do
        begin
          if Values <> '' then
            Values := Values + ', ';
          Values := Values + V.ToString;
        end;
        List.Add('(' + Values + ')');
      end;
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

class procedure TTestRunner.RunAll;
var
  Fixture: TTestFixtureInfo;
  Stopwatch: TStopwatch;
begin
  // Enable UTF-8 for Unicode symbols in console
  SetConsoleCharSet(CP_UTF8);
  
  if FFixtures = nil then
    Discover;

  FSummary.Reset;
  FFilter := Default(TTestFilter);
  FFilter.IncludeExplicit := False;

  Stopwatch := TStopwatch.StartNew;

  TTestConsole.WriteHeader('Dext Test Runner');
  WriteLn(Format('Discovered %d fixtures with %d tests', [FixtureCount, TestCount]));
  WriteLn;

  for Fixture in FFixtures do
    ExecuteFixture(Fixture);

  Stopwatch.Stop;
  FSummary.TotalDuration := Stopwatch.Elapsed;

  PrintSummary;
end;

class procedure TTestRunner.RunFiltered(const AFilter: TTestFilter);
var
  Fixture: TTestFixtureInfo;
  Stopwatch: TStopwatch;
begin
  if FFixtures = nil then
    Discover;

  FSummary.Reset;
  FFilter := AFilter;

  Stopwatch := TStopwatch.StartNew;

  TTestConsole.WriteHeader('Dext Test Runner (Filtered)');
  WriteLn;

  for Fixture in FFixtures do
    ExecuteFixture(Fixture);

  Stopwatch.Stop;
  FSummary.TotalDuration := Stopwatch.Elapsed;

  PrintSummary;
end;

class procedure TTestRunner.RunCategory(const Category: string);
var
  Filter: TTestFilter;
begin
  Filter := Default(TTestFilter);
  SetLength(Filter.Categories, 1);
  Filter.Categories[0] := Category;
  RunFiltered(Filter);
end;

class procedure TTestRunner.RunFixture(AFixtureClass: TClass);
var
  Fixture: TTestFixtureInfo;
  Stopwatch: TStopwatch;
begin
  if FFixtures = nil then
    Discover;

  FSummary.Reset;
  Stopwatch := TStopwatch.StartNew;

  for Fixture in FFixtures do
  begin
    if Fixture.FixtureClass = AFixtureClass then
    begin
      ExecuteFixture(Fixture);
      Break;
    end;
  end;

  Stopwatch.Stop;
  FSummary.TotalDuration := Stopwatch.Elapsed;

  PrintSummary;
end;

class procedure TTestRunner.RunTest(const ATestNamePattern: string);
var
  Filter: TTestFilter;
begin
  Filter := Default(TTestFilter);
  Filter.TestNamePattern := ATestNamePattern;
  RunFiltered(Filter);
end;

class procedure TTestRunner.ExecuteFixture(Fixture: TTestFixtureInfo);
var
  Instance: TObject;
  Method: TRttiMethod;
  TestCases: TArray<TArray<TValue>>;
  DisplayNames: TArray<string>;
  I: Integer;
begin
  if Assigned(FOnFixtureStart) then
    FOnFixtureStart(Fixture.Name, Fixture.TestMethods.Count);

  if FVerbose then
  begin
    WriteLn;
    TTestConsole.WriteInfo('Fixture: ' + Fixture.Name);
    if Fixture.Description <> '' then
      Write('  ' + Fixture.Description);
    WriteLn;
  end;

  // Create fixture instance
  Instance := Fixture.FixtureClass.Create;
  try
    // BeforeAll (class-level setup)
    if Assigned(Fixture.BeforeAllMethod) then
    begin
      try
        Fixture.BeforeAllMethod.Invoke(Instance, []);
      except
        on E: Exception do
        begin
          TTestConsole.WriteFail('BeforeAll failed: ' + E.Message);
          Exit;
        end;
      end;
    end;

    // Execute each test method
    for Method in Fixture.TestMethods do
    begin
      // Get test cases
      TestCases := GetTestCases(Method);
      DisplayNames := GetTestCaseDisplayNames(Method);

      for I := 0 to High(TestCases) do
        ExecuteTest(Fixture, Method, Instance, TestCases[I], DisplayNames[I]);
    end;

    // AfterAll (class-level cleanup)
    if Assigned(Fixture.AfterAllMethod) then
    begin
      try
        Fixture.AfterAllMethod.Invoke(Instance, []);
      except
        on E: Exception do
          TTestConsole.WriteFail('AfterAll failed: ' + E.Message);
      end;
    end;
  finally
    Instance.Free;
  end;

  if Assigned(FOnFixtureComplete) then
    FOnFixtureComplete(Fixture.Name);
end;

class procedure TTestRunner.ExecuteTest(Fixture: TTestFixtureInfo;
  Method: TRttiMethod; Instance: TObject;
  const TestCaseValues: TArray<TValue>;
  const TestCaseDisplayName: string);
var
  Info: TTestInfo;
  Stopwatch: TStopwatch;
  IgnoreReason: string;
  RepeatCount, I: Integer;
  Categories: TArray<string>;
  MaxTime: Integer;
begin
  Info.FixtureName := Fixture.Name;
  Info.TestName := Method.Name;
  Info.DisplayName := Method.Name;
  if TestCaseDisplayName <> '' then
    Info.DisplayName := Info.DisplayName + TestCaseDisplayName;
  Info.Categories := GetCategories(Method);
  Categories := Info.Categories;

  // Check filters
  if not FFilter.Matches(Fixture.Name, Method.Name, Categories, IsExplicit(Method)) then
    Exit;

  Inc(FSummary.TotalTests);

  if Assigned(FOnTestStart) then
    FOnTestStart(Fixture.Name, Info.DisplayName);

  // Check platform
  if not ShouldRunOnPlatform(Method) then
  begin
    Info.Result := trSkipped;
    Info.ErrorMessage := 'Platform not supported';
    Inc(FSummary.Skipped);
    PrintResultChar(trSkipped);
    PrintTestResult(Info);
    if Assigned(FOnTestComplete) then
      FOnTestComplete(Info);
    Exit;
  end;

  // Check ignore
  IgnoreReason := GetIgnoreReason(Method);
  if IgnoreReason <> '' then
  begin
    Info.Result := trSkipped;
    Info.ErrorMessage := IgnoreReason;
    Inc(FSummary.Skipped);
    PrintResultChar(trSkipped);
    PrintTestResult(Info);
    if Assigned(FOnTestComplete) then
      FOnTestComplete(Info);
    Exit;
  end;

  // Repeat count
  RepeatCount := GetRepeatCount(Method);

  for I := 1 to RepeatCount do
  begin
    Stopwatch := TStopwatch.StartNew;

    try
      // Setup
      if Assigned(Fixture.SetupMethod) then
        Fixture.SetupMethod.Invoke(Instance, []);

      try
        // Execute test
        if Length(TestCaseValues) > 0 then
          Method.Invoke(Instance, TestCaseValues)
        else
          Method.Invoke(Instance, []);

        Info.Result := trPassed;
        Inc(FSummary.Passed);

        // Check MaxTime warning
        MaxTime := GetMaxTime(Method);
        if (MaxTime > 0) and (Stopwatch.ElapsedMilliseconds > MaxTime) then
        begin
          Info.ErrorMessage := Format('Test passed but exceeded MaxTime (%dms > %dms)',
            [Stopwatch.ElapsedMilliseconds, MaxTime]);
        end;
      finally
        // TearDown
        if Assigned(Fixture.TearDownMethod) then
          Fixture.TearDownMethod.Invoke(Instance, []);
      end;
    except
      on E: Exception do
      begin
        Info.Result := trFailed;
        Info.ErrorMessage := E.Message;
        // Try to get stack trace if available
        Info.StackTrace := E.StackTrace;
        Inc(FSummary.Failed);
      end;
    end;

    Stopwatch.Stop;
    Info.Duration := Stopwatch.Elapsed;

    PrintResultChar(Info.Result);
    PrintTestResult(Info);

    // Store result for report generation
    if FTestResults = nil then
      FTestResults := TList<TTestInfo>.Create;
    FTestResults.Add(Info);

    if Assigned(FOnTestComplete) then
      FOnTestComplete(Info);
  end;
end;

class procedure TTestRunner.PrintResultChar(Result: TTestResult);
begin
  if FVerbose then
    Exit;

  case Result of
    trPassed:  TTestConsole.WritePass('●');
    trFailed:  TTestConsole.WriteFail('✖');
    trSkipped: TTestConsole.WriteSkip('○');
    trTimeout: TTestConsole.WriteFail('⏱');
    trError:   TTestConsole.WriteFail('⚠');
  end;
end;

class procedure TTestRunner.PrintTestResult(const Info: TTestInfo);
begin
  if not FVerbose then
    Exit;

  case Info.Result of
    trPassed:
      begin
        Write('  ✅  ');
        WriteLn(Format('%s (%dms)', [Info.DisplayName, Info.Duration.Milliseconds]));
        if Info.ErrorMessage <> '' then
        begin
          Write('      ⚠️  Warning: ' + Info.ErrorMessage);
          WriteLn;
        end;
      end;
    trFailed:
      begin
        Write('  ❌  ');
        WriteLn(Info.DisplayName);
        Write('      ');
        TTestConsole.WriteFail('>> ' + Info.ErrorMessage);
        WriteLn;
      end;
    trSkipped:
      begin
        Write('  ⚠️  ');
        Write(Info.DisplayName);
        if Info.ErrorMessage <> '' then
          Write('  [' + Info.ErrorMessage + ']');
        WriteLn;
      end;
    trTimeout:
      begin
        Write('  ⏱️  ');
        WriteLn(Info.DisplayName + ' (TIMEOUT)');
      end;
    trError:
      begin
        Write('  ⛔  ');
        WriteLn(Info.DisplayName);
        Write('      ');
        TTestConsole.WriteFail('>> ' + Info.ErrorMessage);
        WriteLn;
      end;
  end;
end;

class procedure TTestRunner.PrintSummary;
var
  PassPercent: Double;
begin
  WriteLn;
  WriteLn;
  TTestConsole.WriteHeader('Test Summary');
  WriteLn;

  // Calculate pass percentage
  if FSummary.TotalTests > 0 then
    PassPercent := (FSummary.Passed / FSummary.TotalTests) * 100
  else
    PassPercent := 100;

  // Total
  WriteLn(Format('  📊  Total:     %d', [FSummary.TotalTests]));
  
  // Passed - with green emoji
  WriteLn(Format('  ✅  Passed:    %d', [FSummary.Passed]));
  
  // Failed - with red emoji
  WriteLn(Format('  ❌  Failed:    %d', [FSummary.Failed]));
    
  // Skipped - with warning emoji (yellow)
  WriteLn(Format('  ⚠️  Skipped:   %d', [FSummary.Skipped]));
  
  WriteLn;
  
  // Duration
  WriteLn(Format('  ⏱️  Duration:  %.3fs', [FSummary.TotalDuration.TotalSeconds]));
  
  // Pass rate - colored based on percentage
  Write('  📈  Pass Rate: ');
  if PassPercent = 100 then
    TTestConsole.WritePass(Format('%.1f%%', [PassPercent]))
  else if PassPercent >= 80 then
    TTestConsole.WriteSkip(Format('%.1f%%', [PassPercent]))
  else
    TTestConsole.WriteFail(Format('%.1f%%', [PassPercent]));
  WriteLn;
  
  WriteLn;

  // Final result banner
  if FSummary.Failed = 0 then
  begin
    WriteLn('  🎉  All tests passed!');
  end
  else
  begin
    Write('  💥  ');
    TTestConsole.WriteFail(Format('%d test(s) failed!', [FSummary.Failed]));
    WriteLn;
  end;
end;

class function TTestRunner.Summary: TTestSummary;
begin
  Result := FSummary;
end;

class procedure TTestRunner.SetVerbose(AValue: Boolean);
begin
  FVerbose := AValue;
end;

class procedure TTestRunner.SetOutputFormat(AFormat: TOutputFormat);
begin
  FOutputFormat := AFormat;
end;

class function TTestRunner.FixtureCount: Integer;
begin
  if FFixtures = nil then
    Result := 0
  else
    Result := FFixtures.Count;
end;

class function TTestRunner.TestCount: Integer;
var
  Fixture: TTestFixtureInfo;
begin
  Result := 0;
  if FFixtures = nil then
    Exit;
  for Fixture in FFixtures do
    Inc(Result, Fixture.TestMethods.Count);
end;

class procedure TTestRunner.Clear;
begin
  if FFixtures <> nil then
  begin
    FFixtures.Free;
    FFixtures := nil;
  end;
  
  // Free test results list
  if FTestResults <> nil then
  begin
    FTestResults.Free;
    FTestResults := nil;
  end;
  
  FContext.Free;
end;

class procedure TTestRunner.RegisterFixture(AClass: TClass);
var
  RttiType: TRttiType;
  Fixture: TTestFixtureInfo;
begin
  if FFixtures = nil then
    FFixtures := TObjectList<TTestFixtureInfo>.Create(True);
    
  FContext := TRttiContext.Create;
  RttiType := FContext.GetType(AClass);
  
  if RttiType = nil then
  begin
    if FDebugDiscovery then
      WriteLn('  [RegisterFixture] RTTI not available for: ', AClass.ClassName);
    Exit;
  end;
  
  if FDebugDiscovery then
    WriteLn('  [RegisterFixture] Registering: ', AClass.ClassName);
    
  Fixture := TTestFixtureInfo.Create(RttiType);
  DiscoverTestMethods(Fixture);
  
  if Fixture.TestMethods.Count > 0 then
  begin
    FFixtures.Add(Fixture);
    if FDebugDiscovery then
      WriteLn('    Found ', Fixture.TestMethods.Count, ' test methods');
  end
  else
  begin
    if FDebugDiscovery then
      WriteLn('    No test methods found');
    Fixture.Free;
  end;
end;

class procedure TTestRunner.SetDebugDiscovery(AValue: Boolean);
begin
  FDebugDiscovery := AValue;
end;

class procedure TTestRunner.SetReportFile(const FileName: string; Format: TOutputFormat);
begin
  FReportFileName := FileName;
  FReportFormat := Format;
end;

class procedure TTestRunner.SaveJUnitReport(const FileName: string);
var
  Reporter: TJUnitReporter;
  Fixture: TTestFixtureInfo;
  TestInfo: TTestInfo;
begin
  if FTestResults = nil then
  begin
    WriteLn('Warning: No test results available. Run tests first.');
    Exit;
  end;

  Reporter := TJUnitReporter.Create;
  try
    // Group results by fixture
    for Fixture in FFixtures do
    begin
      Reporter.BeginSuite(Fixture.Name);
      for TestInfo in FTestResults do
      begin
        if TestInfo.FixtureName = Fixture.Name then
          Reporter.AddTestCase(TestInfo);
      end;
      Reporter.EndSuite;
    end;
    
    Reporter.SaveToFile(FileName);
    
    if FVerbose then
      WriteLn('📄 JUnit report saved: ', FileName);
  finally
    Reporter.Free;
  end;
end;

class procedure TTestRunner.SaveJsonReport(const FileName: string);
var
  Reporter: TJsonReporter;
  Fixture: TTestFixtureInfo;
  TestInfo: TTestInfo;
begin
  if FTestResults = nil then
  begin
    WriteLn('Warning: No test results available. Run tests first.');
    Exit;
  end;

  Reporter := TJsonReporter.Create;
  try
    // Group results by fixture
    for Fixture in FFixtures do
    begin
      Reporter.BeginSuite(Fixture.Name);
      for TestInfo in FTestResults do
      begin
        if TestInfo.FixtureName = Fixture.Name then
          Reporter.AddTestCase(TestInfo);
      end;
      Reporter.EndSuite;
    end;
    
    Reporter.SaveToFile(FileName);
    
    if FVerbose then
      WriteLn('📄 JSON report saved: ', FileName);
  finally
    Reporter.Free;
  end;
end;

{ TTestConsole }

class procedure TTestConsole.WriteColored(const Text: string; Color: Word);
{$IFDEF MSWINDOWS}
var
  Handle: THandle;
  Info: TConsoleScreenBufferInfo;
begin
  Handle := GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleScreenBufferInfo(Handle, Info);
  SetConsoleTextAttribute(Handle, Color);
  Write(Text);
  SetConsoleTextAttribute(Handle, Info.wAttributes);
end;
{$ELSE}
begin
  Write(Text);
end;
{$ENDIF}

class procedure TTestConsole.WritePass(const Text: string);
begin
  WriteColored(Text, CONSOLE_COLOR_GREEN);
end;

class procedure TTestConsole.WriteFail(const Text: string);
begin
  WriteColored(Text, CONSOLE_COLOR_RED);
end;

class procedure TTestConsole.WriteSkip(const Text: string);
begin
  WriteColored(Text, CONSOLE_COLOR_YELLOW);
end;

class procedure TTestConsole.WriteInfo(const Text: string);
begin
  WriteColored(Text, CONSOLE_COLOR_CYAN);
end;

class procedure TTestConsole.WriteHeader(const Text: string);
begin
  WriteLn;
  WriteColored('═══════════════════════════════════════════════════════════', CONSOLE_COLOR_CYAN);
  WriteLn;
  WriteColored('  ' + Text, CONSOLE_COLOR_WHITE);
  WriteLn;
  WriteColored('═══════════════════════════════════════════════════════════', CONSOLE_COLOR_CYAN);
  WriteLn;
end;

initialization

finalization
  TTestRunner.Clear;

end.
