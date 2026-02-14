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
{  Dext.Testing.Report - Test Result Report Generators                      }
{                                                                           }
{  Provides test result report generation for CI/CD integration:            }
{    - JUnit XML format (Jenkins, GitHub Actions, GitLab CI, Azure DevOps)  }
{    - xUnit XML format                                                     }
{    - JSON format for custom tooling                                       }
{    - SonarQube Generic format                                             }
{                                                                           }
{***************************************************************************}

unit Dext.Testing.Report;

interface

uses
  System.Classes,
  System.DateUtils,
  System.Generics.Collections,
  System.IOUtils,
  System.SysUtils,
  Dext.Testing.Runner;

type
  /// <summary>
  ///   Report format type.
  /// </summary>
  TReportFormat = (rfJUnit, rfXUnit, rfJSON, rfSonarQube, rfTRX);

  /// <summary>
  ///   Test case result for reporting.
  /// </summary>
  TTestCaseReport = record
    ClassName: string;
    TestName: string;
    Duration: Double;  // seconds
    Status: TTestResult;
    ErrorMessage: string;
    StackTrace: string;
  end;

  /// <summary>
  ///   Test suite result for reporting.
  /// </summary>
  TTestSuiteReport = record
    Name: string;
    Tests: Integer;
    Failures: Integer;
    Errors: Integer;
    Skipped: Integer;
    Duration: Double;  // seconds
    Timestamp: TDateTime;
    TestCases: TArray<TTestCaseReport>;
  end;

  /// <summary>
  ///   JUnit XML report generator.
  ///   Generates reports compatible with Jenkins, GitHub Actions, GitLab CI, Azure DevOps.
  /// </summary>
  TJUnitReporter = class
  private
    FTestSuites: TList<TTestSuiteReport>;
    FCurrentSuite: TTestSuiteReport;
    FCurrentTestCases: TList<TTestCaseReport>;
    function EscapeXml(const S: string): string;
    function FormatDuration(Seconds: Double): string;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   Starts a new test suite.
    /// </summary>
    procedure BeginSuite(const Name: string);

    /// <summary>
    ///   Adds a test case result.
    /// </summary>
    procedure AddTestCase(const Info: TTestInfo);

    /// <summary>
    ///   Ends the current test suite.
    /// </summary>
    procedure EndSuite;

    /// <summary>
    ///   Generates the JUnit XML report.
    /// </summary>
    function GenerateXml: string;

    /// <summary>
    ///   Saves the report to a file.
    /// </summary>
    procedure SaveToFile(const FileName: string);

    /// <summary>
    ///   Clears all recorded data.
    /// </summary>
    procedure Clear;
  end;

  /// <summary>
  ///   JSON report generator for custom tooling.
  /// </summary>
  TJsonReporter = class
  private
    FTestSuites: TList<TTestSuiteReport>;
    FCurrentSuite: TTestSuiteReport;
    FCurrentTestCases: TList<TTestCaseReport>;
    function EscapeJson(const S: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure BeginSuite(const Name: string);
    procedure AddTestCase(const Info: TTestInfo);
    procedure EndSuite;
    function GenerateJson: string;
    procedure SaveToFile(const FileName: string);
    procedure Clear;
  end;

  /// <summary>
  ///   SonarQube Generic Test Data format reporter.
  /// </summary>
  TSonarQubeReporter = class
  private
    FTestCases: TList<TTestCaseReport>;
    FCurrentClassName: string;
    function EscapeXml(const S: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetCurrentClassName(const Name: string);
    procedure AddTestCase(const Info: TTestInfo);
    function GenerateXml: string;
    procedure SaveToFile(const FileName: string);
    procedure Clear;
  end;

  /// <summary>
  ///   xUnit.net v2 XML report generator.
  ///   Compatible with .NET ecosystem tools and CI systems.
  /// </summary>
  TXUnitReporter = class
  private
    FTestSuites: TList<TTestSuiteReport>;
    FCurrentSuite: TTestSuiteReport;
    FCurrentTestCases: TList<TTestCaseReport>;
    function EscapeXml(const S: string): string;
    function FormatDuration(Seconds: Double): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure BeginSuite(const Name: string);
    procedure AddTestCase(const Info: TTestInfo);
    procedure EndSuite;
    function GenerateXml: string;
    procedure SaveToFile(const FileName: string);
    procedure Clear;
  end;

  /// <summary>
  ///   Microsoft TRX (Visual Studio Test Results) report generator.
  ///   Compatible with Azure DevOps, Visual Studio, and TFS.
  /// </summary>
  TTRXReporter = class
  private
    FTestSuites: TList<TTestSuiteReport>;
    FCurrentSuite: TTestSuiteReport;
    FCurrentTestCases: TList<TTestCaseReport>;
    FRunId: TGUID;
    FRunName: string;
    FStartTime: TDateTime;
    function EscapeXml(const S: string): string;
    function FormatDurationTS(Seconds: Double): string;
    function CreateGuid: TGUID;
    function GuidToString(const G: TGUID): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure BeginRun(const RunName: string);
    procedure BeginSuite(const Name: string);
    procedure AddTestCase(const Info: TTestInfo);
    procedure EndSuite;
    function GenerateXml: string;
    procedure SaveToFile(const FileName: string);
    procedure Clear;
  end;

  /// <summary>
  ///   HTML report generator with modern styling and charts.
  ///   Creates a beautiful standalone HTML file for test results.
  /// </summary>
  THTMLReporter = class
  private
    FTestSuites: TList<TTestSuiteReport>;
    FCurrentSuite: TTestSuiteReport;
    FCurrentTestCases: TList<TTestCaseReport>;
    FReportTitle: string;
    function EscapeHtml(const S: string): string;
    function GetStatusClass(Status: TTestResult): string;
    function GetStatusIcon(Status: TTestResult): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetTitle(const Title: string);
    procedure BeginSuite(const Name: string);
    procedure AddTestCase(const Info: TTestInfo);
    procedure EndSuite;
    function GenerateHtml: string;
    procedure SaveToFile(const FileName: string);
    procedure Clear;
  end;

implementation

{ TJUnitReporter }

constructor TJUnitReporter.Create;
begin
  inherited Create;
  FTestSuites := TList<TTestSuiteReport>.Create;
  FCurrentTestCases := TList<TTestCaseReport>.Create;
end;

destructor TJUnitReporter.Destroy;
begin
  FTestSuites.Free;
  FCurrentTestCases.Free;
  inherited;
end;

function TJUnitReporter.EscapeXml(const S: string): string;
begin
  Result := S;
  Result := Result.Replace('&', '&amp;', [rfReplaceAll]);
  Result := Result.Replace('<', '&lt;', [rfReplaceAll]);
  Result := Result.Replace('>', '&gt;', [rfReplaceAll]);
  Result := Result.Replace('"', '&quot;', [rfReplaceAll]);
  Result := Result.Replace('''', '&apos;', [rfReplaceAll]);
end;

function TJUnitReporter.FormatDuration(Seconds: Double): string;
begin
  Result := FormatFloat('0.000', Seconds);
end;

procedure TJUnitReporter.BeginSuite(const Name: string);
begin
  FCurrentSuite := Default(TTestSuiteReport);
  FCurrentSuite.Name := Name;
  FCurrentSuite.Timestamp := Now;
  FCurrentTestCases.Clear;
end;

procedure TJUnitReporter.AddTestCase(const Info: TTestInfo);
var
  TC: TTestCaseReport;
begin
  TC.ClassName := Info.FixtureName;
  TC.TestName := Info.DisplayName;
  TC.Duration := Info.Duration.TotalSeconds;
  TC.Status := Info.Result;
  TC.ErrorMessage := Info.ErrorMessage;
  TC.StackTrace := Info.StackTrace;

  FCurrentTestCases.Add(TC);

  Inc(FCurrentSuite.Tests);
  case Info.Result of
    trFailed: Inc(FCurrentSuite.Failures);
    trError: Inc(FCurrentSuite.Errors);
    trSkipped: Inc(FCurrentSuite.Skipped);
  end;
  FCurrentSuite.Duration := FCurrentSuite.Duration + TC.Duration;
end;

procedure TJUnitReporter.EndSuite;
begin
  FCurrentSuite.TestCases := FCurrentTestCases.ToArray;
  FTestSuites.Add(FCurrentSuite);
end;

function TJUnitReporter.GenerateXml: string;
var
  SB: TStringBuilder;
  Suite: TTestSuiteReport;
  TC: TTestCaseReport;
  TotalTests, TotalFailures, TotalErrors, TotalSkipped: Integer;
  TotalTime: Double;
begin
  SB := TStringBuilder.Create;
  try
    // Calculate totals
    TotalTests := 0;
    TotalFailures := 0;
    TotalErrors := 0;
    TotalSkipped := 0;
    TotalTime := 0;
    
    for Suite in FTestSuites do
    begin
      Inc(TotalTests, Suite.Tests);
      Inc(TotalFailures, Suite.Failures);
      Inc(TotalErrors, Suite.Errors);
      Inc(TotalSkipped, Suite.Skipped);
      TotalTime := TotalTime + Suite.Duration;
    end;

    // XML header
    SB.AppendLine('<?xml version="1.0" encoding="UTF-8"?>');
    
    // Testsuites root element
    SB.AppendFormat('<testsuites tests="%d" failures="%d" errors="%d" skipped="%d" time="%s">',
      [TotalTests, TotalFailures, TotalErrors, TotalSkipped, FormatDuration(TotalTime)]);
    SB.AppendLine;

    // Each test suite
    for Suite in FTestSuites do
    begin
      SB.AppendFormat('  <testsuite name="%s" tests="%d" failures="%d" errors="%d" skipped="%d" time="%s" timestamp="%s">',
        [EscapeXml(Suite.Name), Suite.Tests, Suite.Failures, Suite.Errors, Suite.Skipped,
         FormatDuration(Suite.Duration), FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Suite.Timestamp)]);
      SB.AppendLine;

      // Test cases
      for TC in Suite.TestCases do
      begin
        SB.AppendFormat('    <testcase classname="%s" name="%s" time="%s"',
          [EscapeXml(TC.ClassName), EscapeXml(TC.TestName), FormatDuration(TC.Duration)]);

        case TC.Status of
          trPassed:
            SB.AppendLine('/>');
          trFailed:
            begin
              SB.AppendLine('>');
              SB.AppendFormat('      <failure message="%s">%s</failure>',
                [EscapeXml(TC.ErrorMessage), EscapeXml(TC.StackTrace)]);
              SB.AppendLine;
              SB.AppendLine('    </testcase>');
            end;
          trError:
            begin
              SB.AppendLine('>');
              SB.AppendFormat('      <error message="%s">%s</error>',
                [EscapeXml(TC.ErrorMessage), EscapeXml(TC.StackTrace)]);
              SB.AppendLine;
              SB.AppendLine('    </testcase>');
            end;
          trSkipped:
            begin
              SB.AppendLine('>');
              if TC.ErrorMessage <> '' then
                SB.AppendFormat('      <skipped message="%s"/>', [EscapeXml(TC.ErrorMessage)])
              else
                SB.Append('      <skipped/>');
              SB.AppendLine;
              SB.AppendLine('    </testcase>');
            end;
          trTimeout:
            begin
              SB.AppendLine('>');
              SB.AppendFormat('      <failure message="Test timed out">%s</failure>',
                [EscapeXml(TC.ErrorMessage)]);
              SB.AppendLine;
              SB.AppendLine('    </testcase>');
            end;
        end;
      end;

      SB.AppendLine('  </testsuite>');
    end;

    SB.AppendLine('</testsuites>');

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TJUnitReporter.SaveToFile(const FileName: string);
var
  Content: string;
begin
  Content := GenerateXml;
  TFile.WriteAllText(FileName, Content, TEncoding.UTF8);
end;

procedure TJUnitReporter.Clear;
begin
  FTestSuites.Clear;
  FCurrentTestCases.Clear;
end;

{ TJsonReporter }

constructor TJsonReporter.Create;
begin
  inherited Create;
  FTestSuites := TList<TTestSuiteReport>.Create;
  FCurrentTestCases := TList<TTestCaseReport>.Create;
end;

destructor TJsonReporter.Destroy;
begin
  FTestSuites.Free;
  FCurrentTestCases.Free;
  inherited;
end;

function TJsonReporter.EscapeJson(const S: string): string;
begin
  Result := S;
  Result := Result.Replace('\', '\\', [rfReplaceAll]);
  Result := Result.Replace('"', '\"', [rfReplaceAll]);
  Result := Result.Replace(#13, '\r', [rfReplaceAll]);
  Result := Result.Replace(#10, '\n', [rfReplaceAll]);
  Result := Result.Replace(#9, '\t', [rfReplaceAll]);
end;

procedure TJsonReporter.BeginSuite(const Name: string);
begin
  FCurrentSuite := Default(TTestSuiteReport);
  FCurrentSuite.Name := Name;
  FCurrentSuite.Timestamp := Now;
  FCurrentTestCases.Clear;
end;

procedure TJsonReporter.AddTestCase(const Info: TTestInfo);
var
  TC: TTestCaseReport;
begin
  TC.ClassName := Info.FixtureName;
  TC.TestName := Info.DisplayName;
  TC.Duration := Info.Duration.TotalSeconds;
  TC.Status := Info.Result;
  TC.ErrorMessage := Info.ErrorMessage;
  TC.StackTrace := Info.StackTrace;

  FCurrentTestCases.Add(TC);

  Inc(FCurrentSuite.Tests);
  case Info.Result of
    trFailed: Inc(FCurrentSuite.Failures);
    trError: Inc(FCurrentSuite.Errors);
    trSkipped: Inc(FCurrentSuite.Skipped);
  end;
  FCurrentSuite.Duration := FCurrentSuite.Duration + TC.Duration;
end;

procedure TJsonReporter.EndSuite;
begin
  FCurrentSuite.TestCases := FCurrentTestCases.ToArray;
  FTestSuites.Add(FCurrentSuite);
end;

function TJsonReporter.GenerateJson: string;
var
  SB: TStringBuilder;
  Suite: TTestSuiteReport;
  TC: TTestCaseReport;
  I, J: Integer;
  StatusStr: string;
begin
  SB := TStringBuilder.Create;
  try
    SB.AppendLine('{');
    SB.AppendLine('  "testSuites": [');

    for I := 0 to FTestSuites.Count - 1 do
    begin
      Suite := FTestSuites[I];
      SB.AppendLine('    {');
      SB.AppendFormat('      "name": "%s",', [EscapeJson(Suite.Name)]).AppendLine;
      SB.AppendFormat('      "tests": %d,', [Suite.Tests]).AppendLine;
      SB.AppendFormat('      "failures": %d,', [Suite.Failures]).AppendLine;
      SB.AppendFormat('      "errors": %d,', [Suite.Errors]).AppendLine;
      SB.AppendFormat('      "skipped": %d,', [Suite.Skipped]).AppendLine;
      SB.AppendFormat('      "duration": %.3f,', [Suite.Duration]).AppendLine;
      SB.AppendFormat('      "timestamp": "%s",', [FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Suite.Timestamp)]).AppendLine;
      SB.AppendLine('      "testCases": [');

      for J := 0 to High(Suite.TestCases) do
      begin
        TC := Suite.TestCases[J];
        
        case TC.Status of
          trPassed:  StatusStr := 'passed';
          trFailed:  StatusStr := 'failed';
          trSkipped: StatusStr := 'skipped';
          trTimeout: StatusStr := 'timeout';
          trError:   StatusStr := 'error';
        else
          StatusStr := 'unknown';
        end;

        SB.AppendLine('        {');
        SB.AppendFormat('          "className": "%s",', [EscapeJson(TC.ClassName)]).AppendLine;
        SB.AppendFormat('          "testName": "%s",', [EscapeJson(TC.TestName)]).AppendLine;
        SB.AppendFormat('          "duration": %.3f,', [TC.Duration]).AppendLine;
        SB.AppendFormat('          "status": "%s"', [StatusStr]);
        
        if TC.ErrorMessage <> '' then
        begin
          SB.AppendLine(',');
          SB.AppendFormat('          "errorMessage": "%s"', [EscapeJson(TC.ErrorMessage)]);
        end;
        
        if TC.StackTrace <> '' then
        begin
          SB.AppendLine(',');
          SB.AppendFormat('          "stackTrace": "%s"', [EscapeJson(TC.StackTrace)]);
        end;
        
        SB.AppendLine;
        
        if J < High(Suite.TestCases) then
          SB.AppendLine('        },')
        else
          SB.AppendLine('        }');
      end;

      SB.AppendLine('      ]');
      
      if I < FTestSuites.Count - 1 then
        SB.AppendLine('    },')
      else
        SB.AppendLine('    }');
    end;

    SB.AppendLine('  ]');
    SB.AppendLine('}');

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TJsonReporter.SaveToFile(const FileName: string);
var
  Content: string;
begin
  Content := GenerateJson;
  TFile.WriteAllText(FileName, Content, TEncoding.UTF8);
end;

procedure TJsonReporter.Clear;
begin
  FTestSuites.Clear;
  FCurrentTestCases.Clear;
end;

{ TSonarQubeReporter }

constructor TSonarQubeReporter.Create;
begin
  inherited Create;
  FTestCases := TList<TTestCaseReport>.Create;
end;

destructor TSonarQubeReporter.Destroy;
begin
  FTestCases.Free;
  inherited;
end;

function TSonarQubeReporter.EscapeXml(const S: string): string;
begin
  Result := S;
  Result := Result.Replace('&', '&amp;', [rfReplaceAll]);
  Result := Result.Replace('<', '&lt;', [rfReplaceAll]);
  Result := Result.Replace('>', '&gt;', [rfReplaceAll]);
  Result := Result.Replace('"', '&quot;', [rfReplaceAll]);
end;

procedure TSonarQubeReporter.SetCurrentClassName(const Name: string);
begin
  FCurrentClassName := Name;
end;

procedure TSonarQubeReporter.AddTestCase(const Info: TTestInfo);
var
  TC: TTestCaseReport;
begin
  TC.ClassName := FCurrentClassName;
  if TC.ClassName = '' then
    TC.ClassName := Info.FixtureName;
  TC.TestName := Info.DisplayName;
  TC.Duration := Info.Duration.TotalMilliseconds;
  TC.Status := Info.Result;
  TC.ErrorMessage := Info.ErrorMessage;
  TC.StackTrace := Info.StackTrace;

  FTestCases.Add(TC);
end;

function TSonarQubeReporter.GenerateXml: string;
var
  SB: TStringBuilder;
  TC: TTestCaseReport;
  ResultStr, CurrentFile, TestFile: string;
  FileTests: TList<TTestCaseReport>;
  FileGroups: TDictionary<string, TList<TTestCaseReport>>;
  FilePath: string;
begin
  SB := TStringBuilder.Create;
  FileGroups := TDictionary<string, TList<TTestCaseReport>>.Create;
  try
    // Group tests by class name (which represents the file)
    for TC in FTestCases do
    begin
      TestFile := TC.ClassName;
      if TestFile = '' then
        TestFile := 'UnknownFile';
        
      if not FileGroups.ContainsKey(TestFile) then
        FileGroups.Add(TestFile, TList<TTestCaseReport>.Create);
        
      FileGroups[TestFile].Add(TC);
    end;
    
    SB.AppendLine('<?xml version="1.0" encoding="UTF-8"?>');
    SB.AppendLine('<testExecutions version="1">');

    // Generate XML for each file group
    for FilePath in FileGroups.Keys do
    begin
      FileTests := FileGroups[FilePath];
      
      // SonarQube expects <file path="..."> wrapper
      SB.AppendFormat('  <file path="%s">', [EscapeXml(FilePath)]);
      SB.AppendLine;
      
      for TC in FileTests do
      begin
        case TC.Status of
          trPassed:  ResultStr := 'ok';
          trFailed:  ResultStr := 'failure';
          trSkipped: ResultStr := 'skipped';
          trError:   ResultStr := 'error';
        else
          ResultStr := 'ok';
        end;

        SB.AppendFormat('    <testCase name="%s" duration="%d"',
          [EscapeXml(TC.TestName), Round(TC.Duration)]);

        if TC.Status = trPassed then
          SB.AppendLine('/>')
        else
        begin
          SB.AppendLine('>');
          SB.AppendFormat('      <%s message="%s"></%s>',
            [ResultStr, EscapeXml(TC.ErrorMessage), ResultStr]);
          SB.AppendLine;
          SB.AppendLine('    </testCase>');
        end;
      end;
      
      SB.AppendLine('  </file>');
    end;

    SB.AppendLine('</testExecutions>');

    Result := SB.ToString;
  finally
    // Free all the lists in the dictionary
    for FileTests in FileGroups.Values do
      FileTests.Free;
    FileGroups.Free;
    SB.Free;
  end;
end;

procedure TSonarQubeReporter.SaveToFile(const FileName: string);
var
  Content: string;
begin
  Content := GenerateXml;
  TFile.WriteAllText(FileName, Content, TEncoding.UTF8);
end;

procedure TSonarQubeReporter.Clear;
begin
  FTestCases.Clear;
end;

{ TXUnitReporter }

constructor TXUnitReporter.Create;
begin
  inherited Create;
  FTestSuites := TList<TTestSuiteReport>.Create;
  FCurrentTestCases := TList<TTestCaseReport>.Create;
end;

destructor TXUnitReporter.Destroy;
begin
  FTestSuites.Free;
  FCurrentTestCases.Free;
  inherited;
end;

function TXUnitReporter.EscapeXml(const S: string): string;
begin
  Result := S;
  Result := Result.Replace('&', '&amp;', [rfReplaceAll]);
  Result := Result.Replace('<', '&lt;', [rfReplaceAll]);
  Result := Result.Replace('>', '&gt;', [rfReplaceAll]);
  Result := Result.Replace('"', '&quot;', [rfReplaceAll]);
end;

function TXUnitReporter.FormatDuration(Seconds: Double): string;
begin
  Result := FormatFloat('0.000000', Seconds);
end;

procedure TXUnitReporter.BeginSuite(const Name: string);
begin
  FCurrentSuite := Default(TTestSuiteReport);
  FCurrentSuite.Name := Name;
  FCurrentSuite.Timestamp := Now;
  FCurrentTestCases.Clear;
end;

procedure TXUnitReporter.AddTestCase(const Info: TTestInfo);
var
  TC: TTestCaseReport;
begin
  TC.ClassName := Info.FixtureName;
  TC.TestName := Info.DisplayName;
  TC.Duration := Info.Duration.TotalSeconds;
  TC.Status := Info.Result;
  TC.ErrorMessage := Info.ErrorMessage;
  TC.StackTrace := Info.StackTrace;

  FCurrentTestCases.Add(TC);

  Inc(FCurrentSuite.Tests);
  case Info.Result of
    trFailed: Inc(FCurrentSuite.Failures);
    trError: Inc(FCurrentSuite.Errors);
    trSkipped: Inc(FCurrentSuite.Skipped);
  end;
  FCurrentSuite.Duration := FCurrentSuite.Duration + TC.Duration;
end;

procedure TXUnitReporter.EndSuite;
begin
  FCurrentSuite.TestCases := FCurrentTestCases.ToArray;
  FTestSuites.Add(FCurrentSuite);
end;

function TXUnitReporter.GenerateXml: string;
var
  SB: TStringBuilder;
  Suite: TTestSuiteReport;
  TC: TTestCaseReport;
  ResultStr: string;
begin
  SB := TStringBuilder.Create;
  try
    // XML header
    SB.AppendLine('<?xml version="1.0" encoding="UTF-8"?>');
    
    // xUnit v2 format: <assemblies> root with <assembly> children
    SB.AppendLine('<assemblies>');
    
    for Suite in FTestSuites do
    begin
      SB.AppendFormat('  <assembly name="%s" run-date="%s" run-time="%s" time="%s" total="%d" passed="%d" failed="%d" skipped="%d" errors="%d">',
        [EscapeXml(Suite.Name), 
         FormatDateTime('yyyy-mm-dd', Suite.Timestamp),
         FormatDateTime('hh:nn:ss', Suite.Timestamp),
         FormatDuration(Suite.Duration),
         Suite.Tests,
         Suite.Tests - Suite.Failures - Suite.Errors - Suite.Skipped,
         Suite.Failures,
         Suite.Skipped,
         Suite.Errors]);
      SB.AppendLine;
      
      SB.AppendFormat('    <collection name="%s" time="%s" total="%d" passed="%d" failed="%d" skipped="%d">',
        [EscapeXml(Suite.Name),
         FormatDuration(Suite.Duration),
         Suite.Tests,
         Suite.Tests - Suite.Failures - Suite.Errors - Suite.Skipped,
         Suite.Failures + Suite.Errors,
         Suite.Skipped]);
      SB.AppendLine;

      // Test cases
      for TC in Suite.TestCases do
      begin
        case TC.Status of
          trPassed:  ResultStr := 'Pass';
          trFailed:  ResultStr := 'Fail';
          trSkipped: ResultStr := 'Skip';
          trError:   ResultStr := 'Fail';
        else
          ResultStr := 'Pass';
        end;

        SB.AppendFormat('      <test name="%s" type="%s" method="%s" time="%s" result="%s"',
          [EscapeXml(TC.ClassName + '.' + TC.TestName),
           EscapeXml(TC.ClassName),
           EscapeXml(TC.TestName),
           FormatDuration(TC.Duration),
           ResultStr]);

        if (TC.Status = trPassed) and (TC.ErrorMessage = '') then
          SB.AppendLine(' />')
        else
        begin
          SB.AppendLine('>');
          if TC.Status in [trFailed, trError] then
          begin
            SB.AppendLine('        <failure>');
            SB.AppendFormat('          <message>%s</message>', [EscapeXml(TC.ErrorMessage)]);
            SB.AppendLine;
            if TC.StackTrace <> '' then
            begin
              SB.AppendFormat('          <stack-trace>%s</stack-trace>', [EscapeXml(TC.StackTrace)]);
              SB.AppendLine;
            end;
            SB.AppendLine('        </failure>');
          end
          else if TC.Status = trSkipped then
          begin
            SB.AppendFormat('        <reason>%s</reason>', [EscapeXml(TC.ErrorMessage)]);
            SB.AppendLine;
          end;
          SB.AppendLine('      </test>');
        end;
      end;

      SB.AppendLine('    </collection>');
      SB.AppendLine('  </assembly>');
    end;

    SB.AppendLine('</assemblies>');

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TXUnitReporter.SaveToFile(const FileName: string);
begin
  TFile.WriteAllText(FileName, GenerateXml, TEncoding.UTF8);
end;

procedure TXUnitReporter.Clear;
begin
  FTestSuites.Clear;
  FCurrentTestCases.Clear;
end;

{ TTRXReporter }

constructor TTRXReporter.Create;
begin
  inherited Create;
  FTestSuites := TList<TTestSuiteReport>.Create;
  FCurrentTestCases := TList<TTestCaseReport>.Create;
  FRunId := CreateGuid;
  FStartTime := Now;
  FRunName := 'Dext Test Run';
end;

destructor TTRXReporter.Destroy;
begin
  FTestSuites.Free;
  FCurrentTestCases.Free;
  inherited;
end;

function TTRXReporter.EscapeXml(const S: string): string;
begin
  Result := S;
  Result := Result.Replace('&', '&amp;', [rfReplaceAll]);
  Result := Result.Replace('<', '&lt;', [rfReplaceAll]);
  Result := Result.Replace('>', '&gt;', [rfReplaceAll]);
  Result := Result.Replace('"', '&quot;', [rfReplaceAll]);
end;

function TTRXReporter.FormatDurationTS(Seconds: Double): string;
var
  Hours, Minutes, Secs, MilliSecs: Integer;
begin
  // Format as HH:MM:SS.FFFFFFF (TimeSpan format)
  Hours := Trunc(Seconds) div 3600;
  Minutes := (Trunc(Seconds) mod 3600) div 60;
  Secs := Trunc(Seconds) mod 60;
  MilliSecs := Round(Frac(Seconds) * 10000000);
  Result := Format('%.2d:%.2d:%.2d.%.7d', [Hours, Minutes, Secs, MilliSecs]);
end;

function TTRXReporter.CreateGuid: TGUID;
begin
  System.SysUtils.CreateGUID(Result);
end;

function TTRXReporter.GuidToString(const G: TGUID): string;
begin
  Result := GUIDToString(G);
  // Remove braces for TRX format
  Result := Copy(Result, 2, Length(Result) - 2);
end;

procedure TTRXReporter.BeginRun(const RunName: string);
begin
  FRunName := RunName;
  FRunId := CreateGuid;
  FStartTime := Now;
end;

procedure TTRXReporter.BeginSuite(const Name: string);
begin
  FCurrentSuite := Default(TTestSuiteReport);
  FCurrentSuite.Name := Name;
  FCurrentSuite.Timestamp := Now;
  FCurrentTestCases.Clear;
end;

procedure TTRXReporter.AddTestCase(const Info: TTestInfo);
var
  TC: TTestCaseReport;
begin
  TC.ClassName := Info.FixtureName;
  TC.TestName := Info.DisplayName;
  TC.Duration := Info.Duration.TotalSeconds;
  TC.Status := Info.Result;
  TC.ErrorMessage := Info.ErrorMessage;
  TC.StackTrace := Info.StackTrace;

  FCurrentTestCases.Add(TC);

  Inc(FCurrentSuite.Tests);
  case Info.Result of
    trFailed: Inc(FCurrentSuite.Failures);
    trError: Inc(FCurrentSuite.Errors);
    trSkipped: Inc(FCurrentSuite.Skipped);
  end;
  FCurrentSuite.Duration := FCurrentSuite.Duration + TC.Duration;
end;

procedure TTRXReporter.EndSuite;
begin
  FCurrentSuite.TestCases := FCurrentTestCases.ToArray;
  FTestSuites.Add(FCurrentSuite);
end;

function TTRXReporter.GenerateXml: string;
var
  SB: TStringBuilder;
  Suite: TTestSuiteReport;
  TC: TTestCaseReport;
  TotalTests, TotalPassed, TotalFailed: Integer;
  TotalTime: Double;
  TestId, ExecutionId: TGUID;
  EndTime: TDateTime;
  Outcome: string;
begin
  SB := TStringBuilder.Create;
  try
    // Calculate totals
    TotalTests := 0;
    TotalPassed := 0;
    TotalFailed := 0;
    TotalTime := 0;
    
    for Suite in FTestSuites do
    begin
      Inc(TotalTests, Suite.Tests);
      Inc(TotalFailed, Suite.Failures + Suite.Errors);
      TotalPassed := TotalPassed + Suite.Tests - Suite.Failures - Suite.Errors - Suite.Skipped;
      TotalTime := TotalTime + Suite.Duration;
    end;

    EndTime := FStartTime + (TotalTime / 86400);

    // TRX XML structure
    SB.AppendLine('<?xml version="1.0" encoding="UTF-8"?>');
    SB.AppendFormat('<TestRun id="%s" name="%s" runUser="%s" xmlns="http://microsoft.com/schemas/VisualStudio/TeamTest/2010">',
      [GuidToString(FRunId), EscapeXml(FRunName), 'Dext']);
    SB.AppendLine;

    // Times
    SB.AppendFormat('  <Times creation="%s" queuing="%s" start="%s" finish="%s" />',
      [FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', FStartTime),
       FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', FStartTime),
       FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', FStartTime),
       FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', EndTime)]);
    SB.AppendLine;

    // Results Summary
    if TotalFailed > 0 then
      Outcome := 'Failed'
    else
      Outcome := 'Passed';
      
    SB.AppendFormat('  <ResultSummary outcome="%s">', [Outcome]);
    SB.AppendLine;
    SB.AppendFormat('    <Counters total="%d" passed="%d" failed="%d" />', 
      [TotalTests, TotalPassed, TotalFailed]);
    SB.AppendLine;
    SB.AppendLine('  </ResultSummary>');

    // Test Definitions
    SB.AppendLine('  <TestDefinitions>');
    for Suite in FTestSuites do
    begin
      for TC in Suite.TestCases do
      begin
        TestId := CreateGuid;
        SB.AppendFormat('    <UnitTest name="%s" id="%s">',
          [EscapeXml(TC.TestName), GuidToString(TestId)]);
        SB.AppendLine;
        SB.AppendFormat('      <TestMethod codeBase="%s" className="%s" name="%s" />',
          [EscapeXml(TC.ClassName), EscapeXml(TC.ClassName), EscapeXml(TC.TestName)]);
        SB.AppendLine;
        SB.AppendLine('    </UnitTest>');
      end;
    end;
    SB.AppendLine('  </TestDefinitions>');

    // Results
    SB.AppendLine('  <Results>');
    for Suite in FTestSuites do
    begin
      for TC in Suite.TestCases do
      begin
        ExecutionId := CreateGuid;
        TestId := CreateGuid;
        
        case TC.Status of
          trPassed:  Outcome := 'Passed';
          trFailed:  Outcome := 'Failed';
          trSkipped: Outcome := 'NotExecuted';
          trError:   Outcome := 'Error';
        else
          Outcome := 'Passed';
        end;

        SB.AppendFormat('    <UnitTestResult executionId="%s" testId="%s" testName="%s" duration="%s" outcome="%s"',
          [GuidToString(ExecutionId), GuidToString(TestId), EscapeXml(TC.TestName),
           FormatDurationTS(TC.Duration), Outcome]);

        if (TC.Status = trPassed) and (TC.ErrorMessage = '') then
          SB.AppendLine(' />')
        else
        begin
          SB.AppendLine('>');
          if TC.ErrorMessage <> '' then
          begin
            SB.AppendLine('      <Output>');
            SB.AppendLine('        <ErrorInfo>');
            SB.AppendFormat('          <Message>%s</Message>', [EscapeXml(TC.ErrorMessage)]);
            SB.AppendLine;
            if TC.StackTrace <> '' then
            begin
              SB.AppendFormat('          <StackTrace>%s</StackTrace>', [EscapeXml(TC.StackTrace)]);
              SB.AppendLine;
            end;
            SB.AppendLine('        </ErrorInfo>');
            SB.AppendLine('      </Output>');
          end;
          SB.AppendLine('    </UnitTestResult>');
        end;
      end;
    end;
    SB.AppendLine('  </Results>');

    SB.AppendLine('</TestRun>');

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure TTRXReporter.SaveToFile(const FileName: string);
begin
  TFile.WriteAllText(FileName, GenerateXml, TEncoding.UTF8);
end;

procedure TTRXReporter.Clear;
begin
  FTestSuites.Clear;
  FCurrentTestCases.Clear;
end;

{ THTMLReporter }

constructor THTMLReporter.Create;
begin
  inherited Create;
  FTestSuites := TList<TTestSuiteReport>.Create;
  FCurrentTestCases := TList<TTestCaseReport>.Create;
  FReportTitle := 'Dext Test Report';
end;

destructor THTMLReporter.Destroy;
begin
  FTestSuites.Free;
  FCurrentTestCases.Free;
  inherited;
end;

function THTMLReporter.EscapeHtml(const S: string): string;
begin
  Result := S;
  Result := Result.Replace('&', '&amp;', [rfReplaceAll]);
  Result := Result.Replace('<', '&lt;', [rfReplaceAll]);
  Result := Result.Replace('>', '&gt;', [rfReplaceAll]);
  Result := Result.Replace('"', '&quot;', [rfReplaceAll]);
  Result := Result.Replace(#13#10, '<br>', [rfReplaceAll]);
  Result := Result.Replace(#10, '<br>', [rfReplaceAll]);
end;

function THTMLReporter.GetStatusClass(Status: TTestResult): string;
begin
  case Status of
    trPassed:  Result := 'passed';
    trFailed:  Result := 'failed';
    trSkipped: Result := 'skipped';
    trTimeout: Result := 'timeout';
    trError:   Result := 'error';
  else
    Result := 'unknown';
  end;
end;

function THTMLReporter.GetStatusIcon(Status: TTestResult): string;
begin
  case Status of
    trPassed:  Result := '&#10004;'; // Check mark
    trFailed:  Result := '&#10008;'; // X mark
    trSkipped: Result := '&#9888;';  // Warning
    trTimeout: Result := '&#9201;';  // Timer
    trError:   Result := '&#9760;';  // Skull
  else
    Result := '&#63;';
  end;
end;

procedure THTMLReporter.SetTitle(const Title: string);
begin
  FReportTitle := Title;
end;

procedure THTMLReporter.BeginSuite(const Name: string);
begin
  FCurrentSuite := Default(TTestSuiteReport);
  FCurrentSuite.Name := Name;
  FCurrentSuite.Timestamp := Now;
  FCurrentTestCases.Clear;
end;

procedure THTMLReporter.AddTestCase(const Info: TTestInfo);
var
  TC: TTestCaseReport;
begin
  TC.ClassName := Info.FixtureName;
  TC.TestName := Info.DisplayName;
  TC.Duration := Info.Duration.TotalSeconds;
  TC.Status := Info.Result;
  TC.ErrorMessage := Info.ErrorMessage;
  TC.StackTrace := Info.StackTrace;

  FCurrentTestCases.Add(TC);

  Inc(FCurrentSuite.Tests);
  case Info.Result of
    trFailed: Inc(FCurrentSuite.Failures);
    trError: Inc(FCurrentSuite.Errors);
    trSkipped: Inc(FCurrentSuite.Skipped);
  end;
  FCurrentSuite.Duration := FCurrentSuite.Duration + TC.Duration;
end;

procedure THTMLReporter.EndSuite;
begin
  FCurrentSuite.TestCases := FCurrentTestCases.ToArray;
  FTestSuites.Add(FCurrentSuite);
end;

function THTMLReporter.GenerateHtml: string;
var
  SB: TStringBuilder;
  Suite: TTestSuiteReport;
  TC: TTestCaseReport;
  TotalTests, TotalPassed, TotalFailed, TotalSkipped: Integer;
  TotalTime: Double;
  PassRate: Double;
begin
  SB := TStringBuilder.Create;
  try
    // Calculate totals
    TotalTests := 0;
    TotalPassed := 0;
    TotalFailed := 0;
    TotalSkipped := 0;
    TotalTime := 0;
    
    for Suite in FTestSuites do
    begin
      Inc(TotalTests, Suite.Tests);
      Inc(TotalFailed, Suite.Failures + Suite.Errors);
      Inc(TotalSkipped, Suite.Skipped);
      TotalPassed := TotalPassed + Suite.Tests - Suite.Failures - Suite.Errors - Suite.Skipped;
      TotalTime := TotalTime + Suite.Duration;
    end;
    
    if TotalTests > 0 then
      PassRate := (TotalPassed / TotalTests) * 100
    else
      PassRate := 100;

    // HTML Header with embedded CSS
    SB.AppendLine('<!DOCTYPE html>');
    SB.AppendLine('<html lang="en">');
    SB.AppendLine('<head>');
    SB.AppendLine('  <meta charset="UTF-8">');
    SB.AppendLine('  <meta name="viewport" content="width=device-width, initial-scale=1.0">');
    SB.AppendFormat('  <title>%s</title>', [EscapeHtml(FReportTitle)]);
    SB.AppendLine;
    SB.AppendLine('  <style>');
    SB.AppendLine('    :root { --bg: #1a1a2e; --card: #16213e; --accent: #0f3460; --success: #00d26a; --fail: #ff6b6b; --warn: #feca57; --text: #eee; }');
    SB.AppendLine('    * { box-sizing: border-box; margin: 0; padding: 0; }');
    SB.AppendLine('    body { font-family: "Segoe UI", system-ui, sans-serif; background: var(--bg); color: var(--text); line-height: 1.6; padding: 2rem; }');
    SB.AppendLine('    .container { max-width: 1200px; margin: 0 auto; }');
    SB.AppendLine('    h1 { font-size: 2.5rem; margin-bottom: 1rem; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); -webkit-background-clip: text; -webkit-text-fill-color: transparent; }');
    SB.AppendLine('    .stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 1rem; margin-bottom: 2rem; }');
    SB.AppendLine('    .stat-card { background: var(--card); border-radius: 12px; padding: 1.5rem; text-align: center; border-left: 4px solid var(--accent); }');
    SB.AppendLine('    .stat-card.passed { border-left-color: var(--success); }');
    SB.AppendLine('    .stat-card.failed { border-left-color: var(--fail); }');
    SB.AppendLine('    .stat-card.skipped { border-left-color: var(--warn); }');
    SB.AppendLine('    .stat-value { font-size: 2.5rem; font-weight: bold; }');
    SB.AppendLine('    .stat-label { color: #888; text-transform: uppercase; font-size: 0.8rem; letter-spacing: 1px; }');
    SB.AppendLine('    .progress-bar { height: 8px; background: #333; border-radius: 4px; margin: 1rem 0 2rem; overflow: hidden; }');
    SB.AppendLine('    .progress-fill { height: 100%; background: linear-gradient(90deg, var(--success), #00b894); transition: width 0.5s; }');
    SB.AppendLine('    .suite { background: var(--card); border-radius: 12px; margin-bottom: 1.5rem; overflow: hidden; }');
    SB.AppendLine('    .suite-header { background: var(--accent); padding: 1rem 1.5rem; display: flex; justify-content: space-between; align-items: center; cursor: pointer; }');
    SB.AppendLine('    .suite-header h2 { font-size: 1.2rem; font-weight: 500; }');
    SB.AppendLine('    .suite-meta { color: #888; font-size: 0.9rem; }');
    SB.AppendLine('    .test-list { padding: 0; }');
    SB.AppendLine('    .test { padding: 1rem 1.5rem; border-bottom: 1px solid #2a2a4a; display: flex; align-items: center; gap: 1rem; }');
    SB.AppendLine('    .test:last-child { border-bottom: none; }');
    SB.AppendLine('    .test-icon { font-size: 1.2rem; width: 30px; text-align: center; }');
    SB.AppendLine('    .test-icon.passed { color: var(--success); }');
    SB.AppendLine('    .test-icon.failed, .test-icon.error { color: var(--fail); }');
    SB.AppendLine('    .test-icon.skipped { color: var(--warn); }');
    SB.AppendLine('    .test-name { flex: 1; }');
    SB.AppendLine('    .test-duration { color: #888; font-size: 0.9rem; }');
    SB.AppendLine('    .error-msg { background: rgba(255,107,107,0.1); border-left: 3px solid var(--fail); padding: 0.75rem 1rem; margin-top: 0.5rem; font-family: monospace; font-size: 0.85rem; white-space: pre-wrap; }');
    SB.AppendLine('    .timestamp { color: #666; font-size: 0.8rem; margin-top: 2rem; text-align: center; }');
    SB.AppendLine('  </style>');
    SB.AppendLine('</head>');
    SB.AppendLine('<body>');
    SB.AppendLine('  <div class="container">');
    
    // Header
    SB.AppendFormat('    <h1>%s</h1>', [EscapeHtml(FReportTitle)]);
    SB.AppendLine;
    
    // Stats cards
    SB.AppendLine('    <div class="stats">');
    SB.AppendFormat('      <div class="stat-card"><div class="stat-value">%d</div><div class="stat-label">Total Tests</div></div>', [TotalTests]);
    SB.AppendLine;
    SB.AppendFormat('      <div class="stat-card passed"><div class="stat-value" style="color:var(--success)">%d</div><div class="stat-label">Passed</div></div>', [TotalPassed]);
    SB.AppendLine;
    SB.AppendFormat('      <div class="stat-card failed"><div class="stat-value" style="color:var(--fail)">%d</div><div class="stat-label">Failed</div></div>', [TotalFailed]);
    SB.AppendLine;
    SB.AppendFormat('      <div class="stat-card skipped"><div class="stat-value" style="color:var(--warn)">%d</div><div class="stat-label">Skipped</div></div>', [TotalSkipped]);
    SB.AppendLine;
    SB.AppendFormat('      <div class="stat-card"><div class="stat-value">%.2fs</div><div class="stat-label">Duration</div></div>', [TotalTime]);
    SB.AppendLine;
    SB.AppendLine('    </div>');
    
    // Progress bar
    SB.AppendLine('    <div class="progress-bar">');
    SB.AppendFormat('      <div class="progress-fill" style="width: %.1f%%"></div>', [PassRate]);
    SB.AppendLine;
    SB.AppendLine('    </div>');
    
    // Test suites
    for Suite in FTestSuites do
    begin
      SB.AppendLine('    <div class="suite">');
      SB.AppendLine('      <div class="suite-header">');
      SB.AppendFormat('        <h2>%s</h2>', [EscapeHtml(Suite.Name)]);
      SB.AppendLine;
      SB.AppendFormat('        <span class="suite-meta">%d tests &bull; %.3fs</span>', [Suite.Tests, Suite.Duration]);
      SB.AppendLine;
      SB.AppendLine('      </div>');
      SB.AppendLine('      <div class="test-list">');
      
      for TC in Suite.TestCases do
      begin
        SB.AppendLine('        <div class="test">');
        SB.AppendFormat('          <span class="test-icon %s">%s</span>', [GetStatusClass(TC.Status), GetStatusIcon(TC.Status)]);
        SB.AppendLine;
        SB.AppendLine('          <div class="test-name">');
        SB.AppendFormat('            %s', [EscapeHtml(TC.TestName)]);
        if TC.ErrorMessage <> '' then
        begin
          SB.AppendLine;
          SB.AppendFormat('            <div class="error-msg">%s</div>', [EscapeHtml(TC.ErrorMessage)]);
        end;
        SB.AppendLine;
        SB.AppendLine('          </div>');
        SB.AppendFormat('          <span class="test-duration">%.3fs</span>', [TC.Duration]);
        SB.AppendLine;
        SB.AppendLine('        </div>');
      end;
      
      SB.AppendLine('      </div>');
      SB.AppendLine('    </div>');
    end;
    
    // Timestamp
    SB.AppendFormat('    <div class="timestamp">Generated by Dext Testing Framework &bull; %s</div>', 
      [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)]);
    SB.AppendLine;
    
    SB.AppendLine('  </div>');
    SB.AppendLine('</body>');
    SB.AppendLine('</html>');

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

procedure THTMLReporter.SaveToFile(const FileName: string);
begin
  TFile.WriteAllText(FileName, GenerateHtml, TEncoding.UTF8);
end;

procedure THTMLReporter.Clear;
begin
  FTestSuites.Clear;
  FCurrentTestCases.Clear;
end;

end.
