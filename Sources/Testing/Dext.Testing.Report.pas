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
  TReportFormat = (rfJUnit, rfXUnit, rfJSON, rfSonarQube);

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
  ResultStr: string;
begin
  SB := TStringBuilder.Create;
  try
    SB.AppendLine('<?xml version="1.0" encoding="UTF-8"?>');
    SB.AppendLine('<testExecutions version="1">');

    for TC in FTestCases do
    begin
      case TC.Status of
        trPassed:  ResultStr := 'ok';
        trFailed:  ResultStr := 'failure';
        trSkipped: ResultStr := 'skipped';
        trError:   ResultStr := 'error';
      else
        ResultStr := 'ok';
      end;

      SB.AppendFormat('  <testCase name="%s" duration="%d"',
        [EscapeXml(TC.TestName), Round(TC.Duration)]);

      if TC.Status = trPassed then
        SB.AppendLine('/>')
      else
      begin
        SB.AppendLine('>');
        SB.AppendFormat('    <%s message="%s">%s</%s>',
          [ResultStr, EscapeXml(TC.ErrorMessage), EscapeXml(TC.StackTrace), ResultStr]);
        SB.AppendLine;
        SB.AppendLine('  </testCase>');
      end;
    end;

    SB.AppendLine('</testExecutions>');

    Result := SB.ToString;
  finally
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

end.
