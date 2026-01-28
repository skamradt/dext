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
  ///   Interface for listening to test execution events.
  /// </summary>
  ITestListener = interface
    ['{88439A1D-F6E2-4D5C-8A9B-1234567890AB}']
    procedure OnRunStart(TotalTests: Integer);
    procedure OnRunComplete(const Summary: TTestSummary);
    procedure OnFixtureStart(const FixtureName: string; TestCount: Integer);
    procedure OnFixtureComplete(const FixtureName: string);
    procedure OnTestStart(const Fixture, Test: string);
    procedure OnTestComplete(const Info: TTestInfo);
  end;

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
  ///   Implementation of ITestContext for runtime test information.
  /// </summary>
  TTestContext = class(TInterfacedObject, ITestContext)
  private
    FCurrentTest: string;
    FCurrentFixture: string;
    FOutput: TStringBuilder;
    FAttachedFiles: TList<string>;
  public
    constructor Create(const AFixture, ATest: string);
    destructor Destroy; override;
    
    function GetCurrentTest: string;
    function GetCurrentFixture: string;
    procedure WriteLine(const Msg: string); overload;
    procedure WriteLine(const Fmt: string; const Args: array of const); overload;
    procedure AttachFile(const FilePath: string);
    
    function GetOutput: string;
    function GetAttachedFiles: TArray<string>;
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
    
    // Assembly-level hooks (execute once for entire test suite)
    class var FAssemblyInitMethod: TRttiMethod;
    class var FAssemblyCleanupMethod: TRttiMethod;
    class var FAssemblyHookClass: TClass;

    class var FOnTestStart: TTestStartEvent;
    class var FOnTestComplete: TTestCompleteEvent;
    class var FOnFixtureStart: TFixtureStartEvent;
    class var FOnFixtureComplete: TFixtureCompleteEvent;
    
    class var FListeners: TList<ITestListener>;
    
    class procedure NotifyRunStart(TotalTests: Integer);
    class procedure NotifyRunComplete(const Summary: TTestSummary);
    class procedure NotifyFixtureStart(const FixtureName: string; TestCount: Integer);
    class procedure NotifyFixtureComplete(const FixtureName: string);
    class procedure NotifyTestStart(const Fixture, Test: string);
    class procedure NotifyTestComplete(const Info: TTestInfo);

    class procedure DiscoverFixtures;
    class procedure DiscoverTestMethods(Fixture: TTestFixtureInfo);
    class procedure DiscoverAssemblyHooks;
    class procedure ExecuteAssemblyInit;
    class procedure ExecuteAssemblyCleanup;
    class function HasAttribute<T: TCustomAttribute>(
      const Attrs: TArray<TCustomAttribute>): Boolean; overload;
    class function GetAttribute<T: TCustomAttribute>(
      const Attrs: TArray<TCustomAttribute>): T; overload;
    class function GetAttributes<T: TCustomAttribute>(
      const Attrs: TArray<TCustomAttribute>): TArray<T>;
    class function GetCategories(Method: TRttiMethod): TArray<string>;
    class function IsExplicit(Method: TRttiMethod): Boolean;
    class function GetIgnoreReason(Method: TRttiMethod): string;
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
  protected
    // TODO: Implement timeout enforcement using TTask + TCancellationTokenSource
    class function GetTimeout(Method: TRttiMethod): Integer;
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
    ///   Registers a listener for test execution events.
    /// </summary>
    class procedure RegisterListener(const Listener: ITestListener);

    /// <summary>
    ///   Unregisters and clears all listeners.
    /// </summary>
    class procedure ClearListeners;

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

    /// <summary>
    ///   Saves test results to a xUnit.net v2 XML file.
    /// </summary>
    class procedure SaveXUnitReport(const FileName: string);

    /// <summary>
    ///   Saves test results to a Microsoft TRX file (Azure DevOps compatible).
    /// </summary>
    class procedure SaveTRXReport(const FileName: string);

    /// <summary>
    ///   Saves test results to a beautiful standalone HTML file.
    /// </summary>
    class procedure SaveHTMLReport(const FileName: string);

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
  Dext.Testing.Report,
  Dext.Logging,
  Dext.Logging.Global,
  Dext.Types.UUID;

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

{ TTestContext }

constructor TTestContext.Create(const AFixture, ATest: string);
begin
  inherited Create;
  FCurrentFixture := AFixture;
  FCurrentTest := ATest;
  FOutput := TStringBuilder.Create;
  FAttachedFiles := TList<string>.Create;
end;

destructor TTestContext.Destroy;
begin
  FOutput.Free;
  FAttachedFiles.Free;
  inherited;
end;

function TTestContext.GetCurrentTest: string;
begin
  Result := FCurrentTest;
end;

function TTestContext.GetCurrentFixture: string;
begin
  Result := FCurrentFixture;
end;

procedure TTestContext.WriteLine(const Msg: string);
begin
  FOutput.AppendLine(Msg);
  // Also write to console in verbose mode
  SafeWriteLn('      📝 ' + Msg);
end;

procedure TTestContext.WriteLine(const Fmt: string; const Args: array of const);
begin
  WriteLine(Format(Fmt, Args));
end;

procedure TTestContext.AttachFile(const FilePath: string);
begin
  FAttachedFiles.Add(FilePath);
  SafeWriteLn('      📎 Attached: ' + FilePath);
end;

function TTestContext.GetOutput: string;
begin
  Result := FOutput.ToString;
end;

function TTestContext.GetAttachedFiles: TArray<string>;
begin
  Result := FAttachedFiles.ToArray;
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
      SafeWriteLn('  [Discovery] Checking: ' + RttiType.QualifiedName);

    for Attr in RttiType.GetAttributes do
    begin
      if FDebugDiscovery then
        SafeWriteLn('    - Attribute: ' + Attr.ClassName);
        
      if (Attr is TestFixtureAttribute) then
      begin
        Inc(FixtureAttrCount);
        if FDebugDiscovery then
          SafeWriteLn('    ** Found TestFixture!');
          
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
    SafeWriteLn;
    SafeWriteLn('  [Discovery Summary]');
    SafeWriteLn('    Total types scanned: ' + IntToStr(TypeCount));
    SafeWriteLn('    Instance types: ' + IntToStr(InstanceCount));
    SafeWriteLn('    Fixtures found: ' + IntToStr(FixtureAttrCount));
    SafeWriteLn;
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

class procedure TTestRunner.DiscoverAssemblyHooks;
var
  Fixture: TTestFixtureInfo;
  Method: TRttiMethod;
  Attr: TCustomAttribute;
begin
  FAssemblyInitMethod := nil;
  FAssemblyCleanupMethod := nil;
  FAssemblyHookClass := nil;
  
  // Scan all fixtures for assembly-level hooks
  for Fixture in FFixtures do
  begin
    for Method in Fixture.RttiType.GetMethods do
    begin
      for Attr in Method.GetAttributes do
      begin
        if Attr is AssemblyInitializeAttribute then
        begin
          if FAssemblyInitMethod <> nil then
            raise Exception.Create('[AssemblyInitialize] can only be applied to ONE method across all fixtures.');
          FAssemblyInitMethod := Method;
          FAssemblyHookClass := Fixture.FixtureClass;
        end
        else if Attr is AssemblyCleanupAttribute then
        begin
          if FAssemblyCleanupMethod <> nil then
            raise Exception.Create('[AssemblyCleanup] can only be applied to ONE method across all fixtures.');
          FAssemblyCleanupMethod := Method;
          if FAssemblyHookClass = nil then
            FAssemblyHookClass := Fixture.FixtureClass;
        end;
      end;
    end;
  end;
end;

class procedure TTestRunner.ExecuteAssemblyInit;
var
  Instance: TObject;
begin
  if FAssemblyInitMethod = nil then
    Exit;
    
  if FVerbose then
  begin
    SafeWriteLn;
    TTestConsole.WriteInfo('🌐 [AssemblyInitialize] Running global setup...');
  end;
  
  try
    // Create instance and invoke (or invoke as class method if static)
    if FAssemblyInitMethod.IsClassMethod then
    begin
      FAssemblyInitMethod.Invoke(FAssemblyHookClass, []);
    end
    else
    begin
      Instance := FAssemblyHookClass.Create;
      try
        FAssemblyInitMethod.Invoke(Instance, []);
      finally
        Instance.Free;
      end;
    end;
    
    if FVerbose then
      SafeWriteLn('    ✅ Global setup complete.');
  except
    on E: Exception do
    begin
      TTestConsole.WriteFail('    ❌ [AssemblyInitialize] failed: ' + E.Message);
      raise; // Stop test execution if global setup fails
    end;
  end;
end;

class procedure TTestRunner.ExecuteAssemblyCleanup;
var
  Instance: TObject;
begin
  if FAssemblyCleanupMethod = nil then
    Exit;
    
  if FVerbose then
  begin
    SafeWriteLn;
    TTestConsole.WriteInfo('🌐 [AssemblyCleanup] Running global cleanup...');
  end;
  
  try
    if FAssemblyCleanupMethod.IsClassMethod then
    begin
      FAssemblyCleanupMethod.Invoke(FAssemblyHookClass, []);
    end
    else
    begin
      Instance := FAssemblyHookClass.Create;
      try
        FAssemblyCleanupMethod.Invoke(Instance, []);
      finally
        Instance.Free;
      end;
    end;
    
    if FVerbose then
      SafeWriteLn('    ✅ Global cleanup complete.');
  except
    on E: Exception do
      TTestConsole.WriteFail('    ⚠️ [AssemblyCleanup] failed: ' + E.Message);
      // Don't re-raise - we want test results to be reported even if cleanup fails
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
  // TODO: Use this to enforce test timeout (requires TTask/TThread implementation)
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
  SourceAttrs: TArray<TestCaseSourceAttribute>;
  Attr: TestCaseAttribute;
  SourceAttr: TestCaseSourceAttribute;
  List: TList<TArray<TValue>>;
  SourceType: TRttiType;
  SourceMethod: TRttiMethod;
  SourceResult: TValue;
  SourceValues: TArray<TArray<TValue>>;
  V: TArray<TValue>;
begin
  TestCaseAttrs := GetAttributes<TestCaseAttribute>(Method.GetAttributes);
  SourceAttrs := GetAttributes<TestCaseSourceAttribute>(Method.GetAttributes);

  if (Length(TestCaseAttrs) = 0) and (Length(SourceAttrs) = 0) then
  begin
    // No test cases, return single empty array for parameterless execution
    SetLength(Result, 1);
    SetLength(Result[0], 0);
    Exit;
  end;

  List := TList<TArray<TValue>>.Create;
  try
    // 1. [TestCase]
    for Attr in TestCaseAttrs do
      List.Add(Attr.Values);

    // 2. [TestCaseSource]
    for SourceAttr in SourceAttrs do
    begin
      if SourceAttr.SourceType <> nil then
        SourceType := FContext.GetType(SourceAttr.SourceType)
      else
        SourceType := Method.Parent;

      if SourceType = nil then Continue;

      SourceMethod := SourceType.GetMethod(SourceAttr.SourceMethodName);
      if Assigned(SourceMethod) and SourceMethod.IsClassMethod then
      begin
        // Invoke static method
        if SourceType is TRttiInstanceType then
          SourceResult := SourceMethod.Invoke(TRttiInstanceType(SourceType).MetaclassType, [])
        else
          Continue;

        if SourceResult.TryAsType<TArray<TArray<TValue>>>(SourceValues) then
        begin
          for V in SourceValues do
            List.Add(V);
        end;
      end;
    end;

    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

class function TTestRunner.GetTestCaseDisplayNames(Method: TRttiMethod): TArray<string>;
var
  TestCaseAttrs: TArray<TestCaseAttribute>;
  SourceAttrs: TArray<TestCaseSourceAttribute>;
  Attr: TestCaseAttribute;
  SourceAttr: TestCaseSourceAttribute;
  List: TList<string>;
  I: Integer;
  Values: string;
  V: TValue;
  SourceType: TRttiType;
  SourceMethod: TRttiMethod;
  SourceResult: TValue;
  SourceValues: TArray<TArray<TValue>>;
  CaseVals: TArray<TValue>;
begin
  TestCaseAttrs := GetAttributes<TestCaseAttribute>(Method.GetAttributes);
  SourceAttrs := GetAttributes<TestCaseSourceAttribute>(Method.GetAttributes);

  if (Length(TestCaseAttrs) = 0) and (Length(SourceAttrs) = 0) then
  begin
    SetLength(Result, 1);
    Result[0] := '';
    Exit;
  end;

  List := TList<string>.Create;
  try
    // 1. [TestCase] - Legacy handling
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

    // 2. [TestCaseSource] - Generate names from values
    for SourceAttr in SourceAttrs do
    begin
      if SourceAttr.SourceType <> nil then
        SourceType := FContext.GetType(SourceAttr.SourceType)
      else
        SourceType := Method.Parent;

      if SourceType = nil then Continue;

      SourceMethod := SourceType.GetMethod(SourceAttr.SourceMethodName);
      if Assigned(SourceMethod) and SourceMethod.IsClassMethod then
      begin
        if SourceType is TRttiInstanceType then
          SourceResult := SourceMethod.Invoke(TRttiInstanceType(SourceType).MetaclassType, [])
        else
          Continue;

        if SourceResult.TryAsType<TArray<TArray<TValue>>>(SourceValues) then
        begin
          for CaseVals in SourceValues do
          begin
            Values := '';
            for V in CaseVals do
            begin
              if Values <> '' then
                Values := Values + ', ';
              Values := Values + V.ToString;
            end;
            List.Add('(' + Values + ')');
          end;
        end;
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

  // Discover assembly-level hooks
  DiscoverAssemblyHooks;

  FSummary.Reset;
  FFilter := Default(TTestFilter);
  FFilter.IncludeExplicit := False;

  Stopwatch := TStopwatch.StartNew;

  NotifyRunStart(TestCount);
  // Log.Info('Run Started: %d tests', [TestCount]);

  TTestConsole.WriteHeader('Dext Test Runner');
  SafeWriteLn(Format('Discovered %d fixtures with %d tests', [FixtureCount, TestCount]));
  SafeWriteLn;

  // Execute global setup (if defined)
  ExecuteAssemblyInit;

  try
    for Fixture in FFixtures do
      ExecuteFixture(Fixture);
  finally
    // Execute global cleanup (if defined) - always runs
    ExecuteAssemblyCleanup;
  end;

  Stopwatch.Stop;
  FSummary.TotalDuration := Stopwatch.Elapsed;

  PrintSummary;
  
  // Log.Info('Run Completed: %d Tests, %d Passed, %d Failed, %d Skipped. Duration: %.3fs', 
  //  [FSummary.TotalTests, FSummary.Passed, FSummary.Failed, FSummary.Skipped, FSummary.TotalDuration.TotalSeconds]);
    
  NotifyRunComplete(FSummary);
end;

class procedure TTestRunner.RunFiltered(const AFilter: TTestFilter);
var
  Fixture: TTestFixtureInfo;
  Stopwatch: TStopwatch;
begin
  if FFixtures = nil then
    Discover;

  // Discover assembly-level hooks
  DiscoverAssemblyHooks;

  FSummary.Reset;
  FFilter := AFilter;

  Stopwatch := TStopwatch.StartNew;

  NotifyRunStart(0);
  Log.Info('Run Started (Filtered)');
  TTestConsole.WriteHeader('Dext Test Runner (Filtered)');
  SafeWriteLn;

  // Execute global setup (if defined)
  ExecuteAssemblyInit;

  try
    for Fixture in FFixtures do
      ExecuteFixture(Fixture);
  finally
    // Execute global cleanup (if defined) - always runs
    ExecuteAssemblyCleanup;
  end;

  Stopwatch.Stop;
  FSummary.TotalDuration := Stopwatch.Elapsed;

  PrintSummary;
  NotifyRunComplete(FSummary);
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
  Scope: IDisposable;
begin
  // Start Scope for Fixture
  Scope := Log.Logger.BeginScope('Fixture {Fixture}', [Fixture.Name]);
  try
    // Log.Info('Starting Fixture: %s (%d tests)', [Fixture.Name, Fixture.TestMethods.Count]);
  
    NotifyFixtureStart(Fixture.Name, Fixture.TestMethods.Count);
    if Assigned(FOnFixtureStart) then
      FOnFixtureStart(Fixture.Name, Fixture.TestMethods.Count);
  
    if FVerbose then
    begin
      SafeWriteLn;
      TTestConsole.WriteInfo('Fixture: ' + Fixture.Name);
      if Fixture.Description <> '' then
        Write('  ' + Fixture.Description);
      SafeWriteLn;
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
            Log.Error('BeforeAll failed for %s: %s', [Fixture.Name, E.Message]);
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
          begin
            Log.Error('AfterAll failed for %s: %s', [Fixture.Name, E.Message]);
            TTestConsole.WriteFail('AfterAll failed: ' + E.Message);
          end;
        end;
      end;
    finally
      Instance.Free;
    end;
  
    // Log.Info('Completed Fixture: %s', [Fixture.Name]);
    NotifyFixtureComplete(Fixture.Name);
    if Assigned(FOnFixtureComplete) then
      FOnFixtureComplete(Fixture.Name);
      
  finally
    Scope.Dispose;
  end;
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
  TestContext: ITestContext;
  InvokeArgs: TArray<TValue>;
  Params: TArray<TRttiParameter>;
  P: Integer;
  NeedsContext: Boolean;
  Scope: IDisposable;
begin
  Info.FixtureName := Fixture.Name;
  Info.TestName := Method.Name;
  Info.DisplayName := Method.Name;
  if TestCaseDisplayName <> '' then
    Info.DisplayName := Info.DisplayName + TestCaseDisplayName;
  Info.Categories := GetCategories(Method);
  Categories := Info.Categories;

  // Logging Instrumentation
  Scope := Log.Logger.BeginScope('Test {Test}', [Info.DisplayName]);
  try

  // Check filters
  if not FFilter.Matches(Fixture.Name, Method.Name, Categories, IsExplicit(Method)) then
    Exit;

  Inc(FSummary.TotalTests);

  NotifyTestStart(Fixture.Name, Info.DisplayName);
  if Assigned(FOnTestStart) then
    FOnTestStart(Fixture.Name, Info.DisplayName);
    
  // Log.Info('Started Test: %s.%s', [Fixture.Name, Info.DisplayName]);

  // Check platform
  if not ShouldRunOnPlatform(Method) then
  begin
    Info.Result := trSkipped;
    Info.ErrorMessage := 'Platform not supported';
    Inc(FSummary.Skipped);
    PrintResultChar(trSkipped);
    PrintTestResult(Info);
    Log.Warn('Skipped Test: %s (Platform not supported)', [Info.DisplayName]);
    NotifyTestComplete(Info);
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
    Log.Warn('Skipped Test: %s (%s)', [Info.DisplayName, IgnoreReason]);
    NotifyTestComplete(Info);
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
        // Check if method needs ITestContext injection
        Params := Method.GetParameters;
        NeedsContext := False;
        for P := 0 to High(Params) do
        begin
          if (Params[P].ParamType <> nil) and 
             (Params[P].ParamType.TypeKind = tkInterface) and
             (Params[P].ParamType.Name = 'ITestContext') then
          begin
            NeedsContext := True;
            Break;
          end;
        end;
        
        // Build invoke arguments
        if NeedsContext then
        begin
          TestContext := TTestContext.Create(Fixture.Name, Info.DisplayName);
          // Combine TestContext with TestCaseValues
          SetLength(InvokeArgs, Length(TestCaseValues) + 1);
          InvokeArgs[0] := TValue.From<ITestContext>(TestContext);
          for P := 0 to High(TestCaseValues) do
            InvokeArgs[P + 1] := TestCaseValues[P];
          Method.Invoke(Instance, InvokeArgs);
        end
        else if Length(TestCaseValues) > 0 then
          Method.Invoke(Instance, TestCaseValues)
        else
          Method.Invoke(Instance, []);

        Info.Result := trPassed;
        Inc(FSummary.Passed);
        // Log.Info('Passed Test: %s', [Info.DisplayName]);

        // Check MaxTime warning
        MaxTime := GetMaxTime(Method);
        if (MaxTime > 0) and (Stopwatch.ElapsedMilliseconds > MaxTime) then
        begin
          Info.ErrorMessage := Format('Test passed but exceeded MaxTime (%dms > %dms)',
            [Stopwatch.ElapsedMilliseconds, MaxTime]);
          Log.Warn('Test exceeded MaxTime: %s', [Info.ErrorMessage]);
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
        // Log.Error('Failed Test: %s. Error: %s', [Info.DisplayName, E.Message]);
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

    NotifyTestComplete(Info);
    if Assigned(FOnTestComplete) then
      FOnTestComplete(Info);
  end;
  finally
    Scope.Dispose;
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
        SafeWriteLn(Format('%s (%dms)', [Info.DisplayName, Round(Info.Duration.TotalMilliseconds)]));
        if Info.ErrorMessage <> '' then
        begin
          Write('      ⚠️  Warning: ' + Info.ErrorMessage);
          SafeWriteLn;
        end;
      end;
    trFailed:
      begin
        Write('  ❌  ');
        SafeWriteLn(Info.DisplayName);
        Write('      ');
        TTestConsole.WriteFail('>> ' + Info.ErrorMessage);
        SafeWriteLn;
      end;
    trSkipped:
      begin
        SafeWrite('  ⚠️  ');
        SafeWrite(Info.DisplayName);
        if Info.ErrorMessage <> '' then
          SafeWrite('  [' + Info.ErrorMessage + ']');
        SafeWriteLn;
      end;
    trTimeout:
      begin
        Write('  ⏱️  ');
        SafeWriteLn(Info.DisplayName + ' (TIMEOUT)');
      end;
    trError:
      begin
        Write('  ⛔  ');
        SafeWriteLn(Info.DisplayName);
        Write('      ');
        TTestConsole.WriteFail('>> ' + Info.ErrorMessage);
        SafeWriteLn;
      end;
  end;
end;

class procedure TTestRunner.PrintSummary;
var
  PassPercent: Double;
begin
  SafeWriteLn;
  SafeWriteLn;
  TTestConsole.WriteHeader('Test Summary');
  SafeWriteLn;

  // Calculate pass percentage
  if FSummary.TotalTests > 0 then
    PassPercent := (FSummary.Passed / FSummary.TotalTests) * 100
  else
    PassPercent := 100;

  // Total
  SafeWriteLn(Format('  📊  Total:     %d', [FSummary.TotalTests]));
  
  // Passed - with green emoji
  SafeWriteLn(Format('  ✅  Passed:    %d', [FSummary.Passed]));
  
  // Failed - with red emoji
  SafeWriteLn(Format('  ❌  Failed:    %d', [FSummary.Failed]));
    
  // Skipped - with warning emoji (yellow)
  SafeWriteLn(Format('  ⚠️  Skipped:   %d', [FSummary.Skipped]));
  
  SafeWriteLn;
  
  // Duration - show ms if under 1 second, otherwise show seconds
  if FSummary.TotalDuration.TotalSeconds < 1 then
    SafeWriteLn(Format('  ⏱️  Duration:  %dms', [Round(FSummary.TotalDuration.TotalMilliseconds)]))
  else
    SafeWriteLn(Format('  ⏱️  Duration:  %.3fs', [FSummary.TotalDuration.TotalSeconds]));
  
  // Pass rate - colored based on percentage
  Write('  📈  Pass Rate: ');
  if PassPercent = 100 then
    TTestConsole.WritePass(Format('%.1f%%', [PassPercent]))
  else if PassPercent >= 80 then
    TTestConsole.WriteSkip(Format('%.1f%%', [PassPercent]))
  else
    TTestConsole.WriteFail(Format('%.1f%%', [PassPercent]));
  SafeWriteLn;
  
  SafeWriteLn;

  // Final result banner
  if FSummary.Failed = 0 then
  begin
    SafeWriteLn('  🎉  All tests passed!');
  end
  else
  begin
    Write('  💥  ');
    TTestConsole.WriteFail(Format('%d test(s) failed!', [FSummary.Failed]));
    SafeWriteLn;
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
      SafeWriteLn('  [RegisterFixture] RTTI not available for: ' + AClass.ClassName);
    Exit;
  end;
  
  if FDebugDiscovery then
    SafeWriteLn('  [RegisterFixture] Registering: ' + AClass.ClassName);
    
  Fixture := TTestFixtureInfo.Create(RttiType);
  DiscoverTestMethods(Fixture);
  
  if Fixture.TestMethods.Count > 0 then
  begin
    FFixtures.Add(Fixture);
    if FDebugDiscovery then
      SafeWriteLn('    Found ' + IntToStr(Fixture.TestMethods.Count) + ' test methods');
  end
  else
  begin
    if FDebugDiscovery then
      SafeWriteLn('    No test methods found');
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
    SafeWriteLn('Warning: No test results available. Run tests first.');
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
      SafeWriteLn('📄 JUnit report saved: ' + FileName);
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
    SafeWriteLn('Warning: No test results available. Run tests first.');
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
      SafeWriteLn('📄 JSON report saved: ' + FileName);
  finally
    Reporter.Free;
  end;
end;

class procedure TTestRunner.SaveXUnitReport(const FileName: string);
var
  Reporter: TXUnitReporter;
  Fixture: TTestFixtureInfo;
  TestInfo: TTestInfo;
begin
  if FTestResults = nil then
  begin
    SafeWriteLn('Warning: No test results available. Run tests first.');
    Exit;
  end;

  Reporter := TXUnitReporter.Create;
  try
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
      SafeWriteLn('📄 xUnit report saved: ' + FileName);
  finally
    Reporter.Free;
  end;
end;

class procedure TTestRunner.SaveTRXReport(const FileName: string);
var
  Reporter: TTRXReporter;
  Fixture: TTestFixtureInfo;
  TestInfo: TTestInfo;
begin
  if FTestResults = nil then
  begin
    SafeWriteLn('Warning: No test results available. Run tests first.');
    Exit;
  end;

  Reporter := TTRXReporter.Create;
  try
    Reporter.BeginRun('Dext Test Run');
    
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
      SafeWriteLn('📄 TRX report saved: ' + FileName);
  finally
    Reporter.Free;
  end;
end;

class procedure TTestRunner.SaveHTMLReport(const FileName: string);
var
  Reporter: THTMLReporter;
  Fixture: TTestFixtureInfo;
  TestInfo: TTestInfo;
begin
  if FTestResults = nil then
  begin
    SafeWriteLn('Warning: No test results available. Run tests first.');
    Exit;
  end;

  Reporter := THTMLReporter.Create;
  try
    Reporter.SetTitle('Dext Test Report');
    
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
      SafeWriteLn('📄  HTML report saved: ' + FileName);
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
  SafeWriteLn;
  WriteColored('-----------------------------------------------------------', CONSOLE_COLOR_CYAN);
  SafeWriteLn;
  WriteColored('  ' + Text, CONSOLE_COLOR_WHITE);
  SafeWriteLn;
  WriteColored('-----------------------------------------------------------', CONSOLE_COLOR_CYAN);
  SafeWriteLn;
end;

class procedure TTestRunner.ClearListeners;
begin
  if FListeners <> nil then
  begin
    FListeners.Free;
    FListeners := nil;
  end;
end;

class procedure TTestRunner.RegisterListener(const Listener: ITestListener);
begin
  if FListeners = nil then
    FListeners := TList<ITestListener>.Create;
  FListeners.Add(Listener);
end;

class procedure TTestRunner.NotifyRunStart(TotalTests: Integer);
var
  L: ITestListener;
begin
  if FListeners <> nil then
    for L in FListeners do L.OnRunStart(TotalTests);
end;

class procedure TTestRunner.NotifyRunComplete(const Summary: TTestSummary);
var
  L: ITestListener;
begin
  if FListeners <> nil then
    for L in FListeners do L.OnRunComplete(Summary);
end;

class procedure TTestRunner.NotifyFixtureStart(const FixtureName: string; TestCount: Integer);
var
  L: ITestListener;
begin
  if FListeners <> nil then
    for L in FListeners do L.OnFixtureStart(FixtureName, TestCount);
end;

class procedure TTestRunner.NotifyFixtureComplete(const FixtureName: string);
var
  L: ITestListener;
begin
  if FListeners <> nil then
    for L in FListeners do L.OnFixtureComplete(FixtureName);
end;

class procedure TTestRunner.NotifyTestStart(const Fixture, Test: string);
var
  L: ITestListener;
begin
  if FListeners <> nil then
    for L in FListeners do L.OnTestStart(Fixture, Test);
end;

class procedure TTestRunner.NotifyTestComplete(const Info: TTestInfo);
var
  L: ITestListener;
begin
  if FListeners <> nil then
    for L in FListeners do L.OnTestComplete(Info);
end;

initialization

finalization
  TTestRunner.FListeners.Free;
  TTestRunner.Clear;

end.
