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
{  Created: 2026-01-03                                                      }
{                                                                           }
{  Dext.Mocks - Mocking framework inspired by Moq (.NET)                    }
{                                                                           }
{  Usage:                                                                   }
{    var                                                                    }
{      Calculator: Mock<ICalculator>;                                       }
{    begin                                                                  }
{      Calculator := Mock<ICalculator>.Create;                              }
{      Calculator.Setup.Returns(42).When.Add(Arg.Any<Integer>, 10);         }
{      WriteLn(Calculator.Instance.Add(5, 10)); // 42                       }
{      Calculator.Received(Times.Once).Add(5, 10);                          }
{    end;                                                                   }
{                                                                           }
{***************************************************************************}
unit Dext.Mocks;

interface

uses
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections,
  Dext.Interception;

type
  /// <summary>
  ///   Exception raised for mock-related errors.
  /// </summary>
  EMockException = class(Exception);

  /// <summary>
  ///   Mock behavior mode.
  /// </summary>
  TMockBehavior = (
    /// <summary>Never throws for unexpected calls, returns default values.</summary>
    Loose,
    /// <summary>Throws for any call that wasn't explicitly set up.</summary>
    Strict
  );

  /// <summary>
  ///   Specifies the number of times a method should be called.
  /// </summary>
  Times = record
  private
    FMin: Integer;
    FMax: Integer;
    FDescription: string;
  public
    class function Never: Times; static;
    class function Once: Times; static;
    class function AtLeastOnce: Times; static;
    class function AtLeast(Count: Integer): Times; static;
    class function AtMost(Count: Integer): Times; static;
    class function Exactly(Count: Integer): Times; static;
    class function Between(Min, Max: Integer): Times; static;

    function Matches(Count: Integer): Boolean;
    function ToString(ActualCount: Integer): string;
  end;

  // Forward declarations
  // Forward declarations
  IMock<T> = interface;
  ISetup<T> = interface;
  IWhen<T> = interface;

  /// <summary>
  ///   Internal mock interface.
  /// </summary>
  IMock<T> = interface
    ['{D7E8F9A0-1B2C-3D4E-5F6A-7B8C9D0E1F2A}']
    function GetInstance: T;
    function GetBehavior: TMockBehavior;
    procedure SetBehavior(Value: TMockBehavior);
    function Setup: ISetup<T>;
    function Received: T; overload;
    function Received(const ATimes: Times): T; overload;
    function DidNotReceive: T;
    procedure Reset;
    procedure Verify;
    procedure VerifyNoOtherCalls;
    procedure SetCallBase(Value: Boolean);

    property Instance: T read GetInstance;
    property Behavior: TMockBehavior read GetBehavior write SetBehavior;
  end;

  /// <summary>
  ///   Setup interface for configuring mock behavior.
  /// </summary>
  ISetup<T> = interface
    ['{E8F9A0B1-2C3D-4E5F-6A7B-8C9D0E1F2A3B}']
    /// <summary>Configure the mock to return a specific value.</summary>
    function Returns(const Value: TValue): IWhen<T>; overload;
    /// <summary>Configure the mock to return multiple values in sequence.</summary>
    function ReturnsInSequence(const Values: TArray<TValue>): IWhen<T>; overload;
    function ReturnsInSequence(const Values: TArray<Integer>): IWhen<T>; overload;
    function ReturnsInSequence(const Values: TArray<string>): IWhen<T>; overload;
    function ReturnsInSequence(const Values: TArray<Boolean>): IWhen<T>; overload;
    
    // Typed overloads for common types (avoid TValue.From<T>)
    /// <summary>Configure the mock to return an integer value.</summary>
    function Returns(Value: Integer): IWhen<T>; overload;
    /// <summary>Configure the mock to return a string value.</summary>
    function Returns(const Value: string): IWhen<T>; overload;
    /// <summary>Configure the mock to return a boolean value.</summary>
    function Returns(Value: Boolean): IWhen<T>; overload;
    /// <summary>Configure the mock to return a double value.</summary>
    function Returns(Value: Double): IWhen<T>; overload;
    /// <summary>Configure the mock to return an Int64 value.</summary>
    function Returns(Value: Int64): IWhen<T>; overload;
    
    /// <summary>Configure the mock to throw an exception.</summary>
    function Throws(ExceptionClass: ExceptClass; const Msg: string = ''): IWhen<T>;
    /// <summary>Configure the mock to execute a custom action with full invocation context.</summary>
    function Executes(const Action: TProc<IInvocation>): IWhen<T>;
    /// <summary>Configure the mock to execute a custom callback with arguments.</summary>
    function Callback(const Action: TProc<TArray<TValue>>): IWhen<T>;
  end;

  /// <summary>
  ///   When interface - chain to specify which method call triggers the behavior.
  /// </summary>
  IWhen<T> = interface
    ['{F9A0B1C2-3D4E-5F6A-7B8C-9D0E1F2A3B4C}']
    /// <summary>Returns the mock for calling the method to set up.</summary>
    function When: T;
  end;

  /// <summary>
  ///   Main mock record with fluent API inspired by Moq.
  /// </summary>
  Mock<T> = record
  private
    FMock: IMock<T>;
    procedure EnsureCreated;
    function GetInstance: T;
  public
    /// <summary>Creates a new mock with the specified behavior.</summary>
    class function Create(Behavior: TMockBehavior = TMockBehavior.Loose): Mock<T>; overload; static;
    class function Create(Interceptor: TObject): Mock<T>; overload; static;

    /// <summary>The mock instance - use this with your SUT.</summary>
    property Instance: T read GetInstance;

    /// <summary>Configure mock behavior.</summary>
    function Setup: ISetup<T>;

    /// <summary>Verify the method was called (default: at least once).</summary>
    function Received: T; overload;
    /// <summary>Verify the method was called a specific number of times.</summary>
    function Received(const ATimes: Times): T; overload;
    /// <summary>Verify the method was never called.</summary>
    function DidNotReceive: T;

    /// <summary>Reset all setups and recorded calls.</summary>
    procedure Reset;

    /// <summary>Verify all expectations were met.</summary>
    procedure Verify;
    
    /// <summary>Verify no other calls were made besides those already verified.</summary>
    procedure VerifyNoOtherCalls;
    
    /// <summary>Enable calling the base implementation for methods without setups (class mocks only).</summary>
    function CallsBaseForUnconfiguredMembers: Mock<T>;

    /// <summary>Allows using mock directly where T is expected.</summary>
    class operator Implicit(const AMock: Mock<T>): T;

    /// <summary>Access to the mock object for verification (Moq-style .Object).</summary>
    property Object_: T read GetInstance;
  end;

implementation

uses
  Dext.Mocks.Interceptor;

{ Times }

class function Times.Never: Times;
begin
  Result.FMin := 0;
  Result.FMax := 0;
  Result.FDescription := 'never';
end;

class function Times.Once: Times;
begin
  Result.FMin := 1;
  Result.FMax := 1;
  Result.FDescription := 'once';
end;

class function Times.AtLeastOnce: Times;
begin
  Result.FMin := 1;
  Result.FMax := MaxInt;
  Result.FDescription := 'at least once';
end;

class function Times.AtLeast(Count: Integer): Times;
begin
  Result.FMin := Count;
  Result.FMax := MaxInt;
  Result.FDescription := Format('at least %d times', [Count]);
end;

class function Times.AtMost(Count: Integer): Times;
begin
  Result.FMin := 0;
  Result.FMax := Count;
  Result.FDescription := Format('at most %d times', [Count]);
end;

class function Times.Exactly(Count: Integer): Times;
begin
  Result.FMin := Count;
  Result.FMax := Count;
  Result.FDescription := Format('exactly %d times', [Count]);
end;

class function Times.Between(Min, Max: Integer): Times;
begin
  Result.FMin := Min;
  Result.FMax := Max;
  Result.FDescription := Format('between %d and %d times', [Min, Max]);
end;

function Times.Matches(Count: Integer): Boolean;
begin
  Result := (Count >= FMin) and (Count <= FMax);
end;

function Times.ToString(ActualCount: Integer): string;
begin
  Result := Format('expected %s but was called %d times', [FDescription, ActualCount]);
end;

{ Mock<T> }

class function Mock<T>.Create(Behavior: TMockBehavior): Mock<T>;
begin
  Result.FMock := TMock<T>.Create(Behavior);
end;

class function Mock<T>.Create(Interceptor: TObject): Mock<T>;
begin
  Result.FMock := TMock<T>.Create(TMockInterceptor(Interceptor));
end;

procedure Mock<T>.EnsureCreated;
begin
  if FMock = nil then
    FMock := TMock<T>.Create(TMockBehavior.Loose);
end;

function Mock<T>.GetInstance: T;
begin
  EnsureCreated;
  Result := FMock.Instance;
end;

function Mock<T>.Setup: ISetup<T>;
begin
  EnsureCreated;
  Result := FMock.Setup;
end;

function Mock<T>.Received: T;
begin
  EnsureCreated;
  Result := FMock.Received;
end;

function Mock<T>.Received(const ATimes: Times): T;
begin
  EnsureCreated;
  Result := FMock.Received(ATimes);
end;

function Mock<T>.DidNotReceive: T;
begin
  EnsureCreated;
  Result := FMock.DidNotReceive;
end;

procedure Mock<T>.Reset;
begin
  if FMock <> nil then
    FMock.Reset;
end;

procedure Mock<T>.Verify;
begin
  EnsureCreated;
  FMock.Verify;
end;

procedure Mock<T>.VerifyNoOtherCalls;
begin
  EnsureCreated;
  FMock.VerifyNoOtherCalls;
end;

function Mock<T>.CallsBaseForUnconfiguredMembers: Mock<T>;
begin
  EnsureCreated;
  FMock.SetCallBase(True);
  Result := Self;
end;

class operator Mock<T>.Implicit(const AMock: Mock<T>): T;
begin
  Result := AMock.Instance;
end;

end.
