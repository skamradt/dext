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
{  Created: 2026-01-07                                                      }
{                                                                           }
{  Dext.Testing - Wildcard unit for all testing features.                    }
{***************************************************************************}
unit Dext.Testing;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  Dext,
  // {BEGIN_DEXT_USES}
  // Generated Uses
  Dext.Assertions,
  Dext.Interception.ClassProxy,
  Dext.Interception,
  Dext.Interception.Proxy,
  Dext.Mocks.Auto,
  Dext.Mocks.Interceptor,
  Dext.Mocks.Matching,
  Dext.Mocks,
  Dext.Testing.Attributes,
  Dext.Testing.Console,
  Dext.Testing.Dashboard,
  Dext.Testing.DI,
  Dext.Testing.Fluent,
  Dext.Testing.History,
  Dext.Testing.Report,
  Dext.Testing.Runner
  // {END_DEXT_USES}
  ;

type
  // {BEGIN_DEXT_ALIASES}
  // Generated Aliases

  // Dext.Assertions
  EAssertionFailed = Dext.Assertions.EAssertionFailed;
  Assert = Dext.Assertions.Assert;
  ShouldDateTime = Dext.Assertions.ShouldDateTime;
  ShouldString = Dext.Assertions.ShouldString;
  ShouldInteger = Dext.Assertions.ShouldInteger;
  ShouldBoolean = Dext.Assertions.ShouldBoolean;
  ShouldAction = Dext.Assertions.ShouldAction;
  ShouldDouble = Dext.Assertions.ShouldDouble;
  ShouldInt64 = Dext.Assertions.ShouldInt64;
  ShouldGuid = Dext.Assertions.ShouldGuid;
  ShouldUUID = Dext.Assertions.ShouldUUID;
  ShouldVariant = Dext.Assertions.ShouldVariant;
  ShouldObject = Dext.Assertions.ShouldObject;
  ShouldProperty = Dext.Assertions.ShouldProperty;
  ShouldObjectHelper = Dext.Assertions.ShouldObjectHelper;
  ShouldInterface = Dext.Assertions.ShouldInterface;
  ShouldHelper = Dext.Assertions.ShouldHelper;
  // ShouldList<T> is used via TShould or directly from Dext.Assertions

  // Dext.Interception
  EInterceptionException = Dext.Interception.EInterceptionException;
  IInvocation = Dext.Interception.IInvocation;
  IInterceptor = Dext.Interception.IInterceptor;
  IProxyTargetAccessor = Dext.Interception.IProxyTargetAccessor;
  TProxy = Dext.Interception.TProxy;

  // Dext.Interception.ClassProxy
  TClassProxy = Dext.Interception.ClassProxy.TClassProxy;

  // Dext.Interception.Proxy
  TInvocation = Dext.Interception.Proxy.TInvocation;
  TInterfaceProxy = Dext.Interception.Proxy.TInterfaceProxy;

  // Dext.Mocks
  EMockException = Dext.Mocks.EMockException;
  TMockBehavior = Dext.Mocks.TMockBehavior;
  Times = Dext.Mocks.Times;
  IMock = Dext.Mocks.IMock;
  // ISetup<T> = Dext.Mocks.ISetup<T>;
  // IWhen<T> = Dext.Mocks.IWhen<T>;
  // IMock<T> = Dext.Mocks.IMock<T>;
  // ISetup<T> = Dext.Mocks.ISetup<T>;
  // IWhen<T> = Dext.Mocks.IWhen<T>;
  // Mock<T> = Dext.Mocks.Mock<T>;

  // Dext.Mocks.Auto
  TAutoMocker = Dext.Mocks.Auto.TAutoMocker;

  // Dext.Mocks.Interceptor
  TMockState = Dext.Mocks.Interceptor.TMockState;
  TMethodSetup = Dext.Mocks.Interceptor.TMethodSetup;
  TMethodCall = Dext.Mocks.Interceptor.TMethodCall;
  TMockInterceptor = Dext.Mocks.Interceptor.TMockInterceptor;
  // TSetup<T> = Dext.Mocks.Interceptor.TSetup<T>;
  // TWhen<T> = Dext.Mocks.Interceptor.TWhen<T>;
  // TMock<T> = Dext.Mocks.Interceptor.TMock<T>;

  // Dext.Mocks.Matching
  TMatcherFactory = Dext.Mocks.Matching.TMatcherFactory;
  Arg = Dext.Mocks.Matching.Arg;

  // Dext.Testing.Attributes
  TestFixtureAttribute = Dext.Testing.Attributes.TestFixtureAttribute;
  TestClassAttribute = Dext.Testing.Attributes.TestClassAttribute;
  TestAttribute = Dext.Testing.Attributes.TestAttribute;
  FactAttribute = Dext.Testing.Attributes.FactAttribute;
  SetupAttribute = Dext.Testing.Attributes.SetupAttribute;
  TearDownAttribute = Dext.Testing.Attributes.TearDownAttribute;
  BeforeAllAttribute = Dext.Testing.Attributes.BeforeAllAttribute;
  ClassInitializeAttribute = Dext.Testing.Attributes.ClassInitializeAttribute;
  AfterAllAttribute = Dext.Testing.Attributes.AfterAllAttribute;
  ClassCleanupAttribute = Dext.Testing.Attributes.ClassCleanupAttribute;
  AssemblyInitializeAttribute = Dext.Testing.Attributes.AssemblyInitializeAttribute;
  OneTimeSetUpAttribute = Dext.Testing.Attributes.OneTimeSetUpAttribute;
  AssemblyCleanupAttribute = Dext.Testing.Attributes.AssemblyCleanupAttribute;
  OneTimeTearDownAttribute = Dext.Testing.Attributes.OneTimeTearDownAttribute;
  TestCaseAttribute = Dext.Testing.Attributes.TestCaseAttribute;
  TestCaseSourceAttribute = Dext.Testing.Attributes.TestCaseSourceAttribute;
  IgnoreAttribute = Dext.Testing.Attributes.IgnoreAttribute;
  SkipAttribute = Dext.Testing.Attributes.SkipAttribute;
  TimeoutAttribute = Dext.Testing.Attributes.TimeoutAttribute;
  RepeatAttribute = Dext.Testing.Attributes.RepeatAttribute;
  MaxTimeAttribute = Dext.Testing.Attributes.MaxTimeAttribute;
  ExplicitAttribute = Dext.Testing.Attributes.ExplicitAttribute;
  CategoryAttribute = Dext.Testing.Attributes.CategoryAttribute;
  TraitAttribute = Dext.Testing.Attributes.TraitAttribute;
  DescriptionAttribute = Dext.Testing.Attributes.DescriptionAttribute;
  PriorityAttribute = Dext.Testing.Attributes.PriorityAttribute;
  PlatformAttribute = Dext.Testing.Attributes.PlatformAttribute;
  ValuesAttribute = Dext.Testing.Attributes.ValuesAttribute;
  RangeAttribute = Dext.Testing.Attributes.RangeAttribute;
  RandomAttribute = Dext.Testing.Attributes.RandomAttribute;
  CombinatorialAttribute = Dext.Testing.Attributes.CombinatorialAttribute;

  // Dext.Testing.Console
  TTestRunner = Dext.Testing.Console.TTestRunner;

  // Dext.Testing.Dashboard
  TDashboardListener = Dext.Testing.Dashboard.TDashboardListener;

  // Dext.Testing.DI
  TTestServiceProvider = Dext.Testing.DI.TTestServiceProvider;

  // Dext.Testing.Fluent
  TTestConfigurator = Dext.Testing.Fluent.TTestConfigurator;
  TTest = Dext.Testing.Fluent.TTest;

  // Dext.Testing.History
  TTestHistoryManager = Dext.Testing.History.TTestHistoryManager;

  // Dext.Testing.Report
  TReportFormat = Dext.Testing.Report.TReportFormat;
  TTestCaseReport = Dext.Testing.Report.TTestCaseReport;
  TTestSuiteReport = Dext.Testing.Report.TTestSuiteReport;
  TJUnitReporter = Dext.Testing.Report.TJUnitReporter;
  TJsonReporter = Dext.Testing.Report.TJsonReporter;
  TSonarQubeReporter = Dext.Testing.Report.TSonarQubeReporter;
  TXUnitReporter = Dext.Testing.Report.TXUnitReporter;
  TTRXReporter = Dext.Testing.Report.TTRXReporter;
  THTMLReporter = Dext.Testing.Report.THTMLReporter;

  // Dext.Testing.Runner
  TTestResult = Dext.Testing.Runner.TTestResult;
  TTestInfo = Dext.Testing.Runner.TTestInfo;
  TTestSummary = Dext.Testing.Runner.TTestSummary;
  TTestFilter = Dext.Testing.Runner.TTestFilter;
  TTestStartEvent = Dext.Testing.Runner.TTestStartEvent;
  TTestCompleteEvent = Dext.Testing.Runner.TTestCompleteEvent;
  TFixtureStartEvent = Dext.Testing.Runner.TFixtureStartEvent;
  TFixtureCompleteEvent = Dext.Testing.Runner.TFixtureCompleteEvent;
  ITestListener = Dext.Testing.Runner.ITestListener;
  TOutputFormat = Dext.Testing.Runner.TOutputFormat;
  TTestFixtureInfo = Dext.Testing.Runner.TTestFixtureInfo;
  ITestContext = Dext.Testing.Runner.ITestContext;
  TTestContext = Dext.Testing.Runner.TTestContext;
  TTestConsole = Dext.Testing.Runner.TTestConsole;

const
  // Dext.Mocks
  Loose = Dext.Mocks.Loose;
  Strict = Dext.Mocks.Strict;
  // Dext.Mocks.Interceptor
  Acting = Dext.Mocks.Interceptor.Acting;
  Arranging = Dext.Mocks.Interceptor.Arranging;
  Asserting = Dext.Mocks.Interceptor.Asserting;
  // Dext.Testing.Report
  rfJUnit = Dext.Testing.Report.rfJUnit;
  rfXUnit = Dext.Testing.Report.rfXUnit;
  rfJSON = Dext.Testing.Report.rfJSON;
  rfSonarQube = Dext.Testing.Report.rfSonarQube;
  rfTRX = Dext.Testing.Report.rfTRX;
  // Dext.Testing.Runner
  trPassed = Dext.Testing.Runner.trPassed;
  trFailed = Dext.Testing.Runner.trFailed;
  trSkipped = Dext.Testing.Runner.trSkipped;
  trTimeout = Dext.Testing.Runner.trTimeout;
  trError = Dext.Testing.Runner.trError;
  ofConsole = Dext.Testing.Runner.ofConsole;
  ofXUnit = Dext.Testing.Runner.ofXUnit;
  ofJUnit = Dext.Testing.Runner.ofJUnit;
  // {END_DEXT_ALIASES}

// Global helper functions for cleaner syntax
function Should(const Value: string): ShouldString; overload;
function Should(Value: Integer): ShouldInteger; overload;
function Should(Value: Int64): ShouldInt64; overload;
function Should(Value: Boolean): ShouldBoolean; overload;
function Should(Value: Double): ShouldDouble; overload;
function Should(const Action: TProc): ShouldAction; overload;
function Should(const Value: TObject): ShouldObject; overload;
function Should(const Value: IInterface): ShouldInterface; overload;
function Should(const Value: TGUID): ShouldGuid; overload;
function Should(const Value: TUUID): ShouldUUID; overload;
function Should(const Value: Variant): ShouldVariant; overload;
function ShouldDate(Value: TDateTime): ShouldDateTime; overload;
function Should: ShouldHelper; overload;

implementation

function Should(const Value: string): ShouldString; begin Result := Dext.Assertions.Should(Value); end;
function Should(Value: Integer): ShouldInteger; begin Result := Dext.Assertions.Should(Value); end;
function Should(Value: Int64): ShouldInt64; begin Result := Dext.Assertions.Should(Value); end;
function Should(Value: Boolean): ShouldBoolean; begin Result := Dext.Assertions.Should(Value); end;
function Should(Value: Double): ShouldDouble; begin Result := Dext.Assertions.Should(Value); end;
function Should(const Action: TProc): ShouldAction; begin Result := Dext.Assertions.Should(Action); end;
function Should(const Value: TObject): ShouldObject; begin Result := Dext.Assertions.Should(Value); end;
function Should(const Value: IInterface): ShouldInterface; begin Result := Dext.Assertions.Should(Value); end;
function Should(const Value: TGUID): ShouldGuid; begin Result := Dext.Assertions.Should(Value); end;
function Should(const Value: TUUID): ShouldUUID; begin Result := Dext.Assertions.Should(Value); end;
function Should(const Value: Variant): ShouldVariant; begin Result := Dext.Assertions.Should(Value); end;
function ShouldDate(Value: TDateTime): ShouldDateTime; begin Result := Dext.Assertions.ShouldDate(Value); end;
function Should: ShouldHelper; begin end;
end.
