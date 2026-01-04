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
{  Dext.Testing.Fluent - Fluent API for Test Configuration                  }
{                                                                           }
{  Provides an intuitive, chainable API for configuring and running tests:  }
{                                                                           }
{    TTest                                                                  }
{      .Configure                                                           }
{        .Verbose                                                           }
{        .ExportToJUnit('results.xml')                                      }
{        .ExportToJson('results.json')                                      }
{        .FilterByCategory('Unit')                                          }
{      .RegisterFixture(TMyTests)                                           }
{      .Run;                                                                }
{                                                                           }
{***************************************************************************}

unit Dext.Testing.Fluent;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Dext.Testing.Runner;

type
  TTestConfigurator = record
  private
    FVerbose: Boolean;
    FDebugDiscovery: Boolean;
    FJUnitFile: string;
    FJsonFile: string;
    FSonarQubeFile: string;
    FCategories: TArray<string>;
    FTestPattern: string;
    FFixturePattern: string;
    FIncludeExplicit: Boolean;
    FFixtureClasses: TArray<TClass>;
  public
    /// <summary>
    ///   Enables verbose output with detailed test information.
    /// </summary>
    function Verbose: TTestConfigurator;

    /// <summary>
    ///   Disables verbose output (compact dot notation).
    /// </summary>
    function Compact: TTestConfigurator;

    /// <summary>
    ///   Enables debug discovery logging.
    /// </summary>
    function DebugDiscovery: TTestConfigurator;

    /// <summary>
    ///   Configures JUnit XML export.
    /// </summary>
    function ExportToJUnit(const FileName: string): TTestConfigurator;

    /// <summary>
    ///   Configures JSON report export.
    /// </summary>
    function ExportToJson(const FileName: string): TTestConfigurator;

    /// <summary>
    ///   Configures SonarQube report export.
    /// </summary>
    function ExportToSonarQube(const FileName: string): TTestConfigurator;

    /// <summary>
    ///   Filters tests by category.
    /// </summary>
    function FilterByCategory(const Category: string): TTestConfigurator;

    /// <summary>
    ///   Filters tests by multiple categories.
    /// </summary>
    function FilterByCategories(const Categories: array of string): TTestConfigurator;

    /// <summary>
    ///   Filters tests by name pattern.
    /// </summary>
    function FilterByName(const Pattern: string): TTestConfigurator;

    /// <summary>
    ///   Filters fixtures by name pattern.
    /// </summary>
    function FilterByFixture(const Pattern: string): TTestConfigurator;

    /// <summary>
    ///   Includes explicit tests in the run.
    /// </summary>
    function IncludeExplicitTests: TTestConfigurator;

    /// <summary>
    ///   Registers a fixture class (for classes in .dpr files).
    /// </summary>
    function RegisterFixture(AClass: TClass): TTestConfigurator;

    /// <summary>
    ///   Registers multiple fixture classes.
    /// </summary>
    function RegisterFixtures(const Classes: array of TClass): TTestConfigurator;

    /// <summary>
    ///   Runs all registered/discovered tests with the configured options.
    ///   Returns True if all tests passed.
    /// </summary>
    function Run: Boolean;

    /// <summary>
    ///   Returns the test summary after running.
    /// </summary>
    function GetSummary: TTestSummary;
  end;

  /// <summary>
  ///   Main entry point for fluent test configuration.
  /// </summary>
  TTest = record
  public
    /// <summary>
    ///   Starts the fluent configuration.
    /// </summary>
    class function Configure: TTestConfigurator; static;

    /// <summary>
    ///   Quick run with defaults - discover and execute all tests.
    /// </summary>
    class function RunAll: Boolean; static;

    /// <summary>
    ///   Quick run with verbose output.
    /// </summary>
    class function RunAllVerbose: Boolean; static;

    /// <summary>
    ///   Quick run a specific fixture.
    /// </summary>
    class function RunFixture(AClass: TClass): Boolean; static;

    /// <summary>
    ///   Quick run tests in a category.
    /// </summary>
    class function RunCategory(const Category: string): Boolean; static;
  end;

implementation

uses
  Dext.Testing.Report;

{ TTestConfigurator }

function TTestConfigurator.Verbose: TTestConfigurator;
begin
  FVerbose := True;
  Result := Self;
end;

function TTestConfigurator.Compact: TTestConfigurator;
begin
  FVerbose := False;
  Result := Self;
end;

function TTestConfigurator.DebugDiscovery: TTestConfigurator;
begin
  FDebugDiscovery := True;
  Result := Self;
end;

function TTestConfigurator.ExportToJUnit(const FileName: string): TTestConfigurator;
begin
  FJUnitFile := FileName;
  Result := Self;
end;

function TTestConfigurator.ExportToJson(const FileName: string): TTestConfigurator;
begin
  FJsonFile := FileName;
  Result := Self;
end;

function TTestConfigurator.ExportToSonarQube(const FileName: string): TTestConfigurator;
begin
  FSonarQubeFile := FileName;
  Result := Self;
end;

function TTestConfigurator.FilterByCategory(const Category: string): TTestConfigurator;
begin
  SetLength(FCategories, Length(FCategories) + 1);
  FCategories[High(FCategories)] := Category;
  Result := Self;
end;

function TTestConfigurator.FilterByCategories(const Categories: array of string): TTestConfigurator;
var
  I, Start: Integer;
begin
  Start := Length(FCategories);
  SetLength(FCategories, Start + Length(Categories));
  for I := 0 to High(Categories) do
    FCategories[Start + I] := Categories[I];
  Result := Self;
end;

function TTestConfigurator.FilterByName(const Pattern: string): TTestConfigurator;
begin
  FTestPattern := Pattern;
  Result := Self;
end;

function TTestConfigurator.FilterByFixture(const Pattern: string): TTestConfigurator;
begin
  FFixturePattern := Pattern;
  Result := Self;
end;

function TTestConfigurator.IncludeExplicitTests: TTestConfigurator;
begin
  FIncludeExplicit := True;
  Result := Self;
end;

function TTestConfigurator.RegisterFixture(AClass: TClass): TTestConfigurator;
begin
  SetLength(FFixtureClasses, Length(FFixtureClasses) + 1);
  FFixtureClasses[High(FFixtureClasses)] := AClass;
  Result := Self;
end;

function TTestConfigurator.RegisterFixtures(const Classes: array of TClass): TTestConfigurator;
var
  I, Start: Integer;
begin
  Start := Length(FFixtureClasses);
  SetLength(FFixtureClasses, Start + Length(Classes));
  for I := 0 to High(Classes) do
    FFixtureClasses[Start + I] := Classes[I];
  Result := Self;
end;

function TTestConfigurator.Run: Boolean;
var
  Filter: TTestFilter;
  Cls: TClass;
begin
  // Apply configuration
  TTestRunner.SetVerbose(FVerbose);
  TTestRunner.SetDebugDiscovery(FDebugDiscovery);

  // Register fixtures
  for Cls in FFixtureClasses do
    TTestRunner.RegisterFixture(Cls);

  // If no fixtures registered, discover automatically
  if Length(FFixtureClasses) = 0 then
    TTestRunner.Discover;

  // Build filter
  Filter := Default(TTestFilter);
  Filter.Categories := FCategories;
  Filter.TestNamePattern := FTestPattern;
  Filter.FixtureNamePattern := FFixturePattern;
  Filter.IncludeExplicit := FIncludeExplicit;

  // Run tests
  if (Length(FCategories) > 0) or (FTestPattern <> '') or 
     (FFixturePattern <> '') or FIncludeExplicit then
    TTestRunner.RunFiltered(Filter)
  else
    TTestRunner.RunAll;

  // Export reports
  if FJUnitFile <> '' then
    TTestRunner.SaveJUnitReport(FJUnitFile);
  if FJsonFile <> '' then
    TTestRunner.SaveJsonReport(FJsonFile);

  // Return success status
  Result := TTestRunner.Summary.Failed = 0;
end;

function TTestConfigurator.GetSummary: TTestSummary;
begin
  Result := TTestRunner.Summary;
end;

{ TTest }

class function TTest.Configure: TTestConfigurator;
begin
  Result := Default(TTestConfigurator);
  Result.FVerbose := False;  // Default to compact mode
end;

class function TTest.RunAll: Boolean;
begin
  TTestRunner.Discover;
  TTestRunner.RunAll;
  Result := TTestRunner.Summary.Failed = 0;
end;

class function TTest.RunAllVerbose: Boolean;
begin
  Result := TTest.Configure
    .Verbose
    .Run;
end;

class function TTest.RunFixture(AClass: TClass): Boolean;
begin
  Result := TTest.Configure
    .RegisterFixture(AClass)
    .Run;
end;

class function TTest.RunCategory(const Category: string): Boolean;
begin
  Result := TTest.Configure
    .FilterByCategory(Category)
    .Run;
end;

initialization

finalization
  TTestRunner.Clear;
  
end.
