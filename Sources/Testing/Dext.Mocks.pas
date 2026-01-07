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

unit Dext.Mocks;

interface

uses
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections,
  Dext.Interception;

type
  EMockException = class(Exception);

  TMockBehavior = (Loose, Strict);

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

  ISetup<T> = interface;
  IWhen<T> = interface;

  /// <summary>
  ///   Base non-generic mock interface.
  /// </summary>
  IMock = interface
    ['{C6D7E8F9-0A1B-2C3D-4E5F-6A7B8C9D0E1F}']
    function GetInstanceValue: TValue;
    procedure Verify;
    procedure VerifyNoOtherCalls;
    procedure Reset;
  end;

  /// <summary>
  ///   Generic mock interface.
  /// </summary>
  IMock<T> = interface(IMock)
    ['{D7E8F9A0-1B2C-3D4E-5F6A-7B8C9D0E1F2A}']
    function GetInstance: T;
    function GetBehavior: TMockBehavior;
    procedure SetBehavior(Value: TMockBehavior);
    function Setup: ISetup<T>;
    function Received: T; overload;
    function Received(const ATimes: Times): T; overload;
    function DidNotReceive: T;
    procedure SetCallBase(Value: Boolean);

    property Instance: T read GetInstance;
    property Behavior: TMockBehavior read GetBehavior write SetBehavior;
  end;

  ISetup<T> = interface
    ['{E8F9A0B1-2C3D-4E5F-6A7B-8C9D0E1F2A3B}']
    function Returns(const Value: TValue): IWhen<T>; overload;
    function ReturnsInSequence(const Values: TArray<TValue>): IWhen<T>; overload;
    function ReturnsInSequence(const Values: TArray<Integer>): IWhen<T>; overload;
    function ReturnsInSequence(const Values: TArray<string>): IWhen<T>; overload;
    function ReturnsInSequence(const Values: TArray<Boolean>): IWhen<T>; overload;
    function Returns(Value: Integer): IWhen<T>; overload;
    function Returns(const Value: string): IWhen<T>; overload;
    function Returns(Value: Boolean): IWhen<T>; overload;
    function Returns(Value: Double): IWhen<T>; overload;
    function Returns(Value: Int64): IWhen<T>; overload;
    function Throws(ExceptionClass: ExceptClass; const Msg: string = ''): IWhen<T>;
    function Executes(const Action: TProc<IInvocation>): IWhen<T>;
    function Callback(const Action: TProc<TArray<TValue>>): IWhen<T>;
  end;

  IWhen<T> = interface
    ['{F9A0B1C2-3D4E-5F6A-7B8C-9D0E1F2A3B4C}']
    function When: T;
  end;

  Mock<T> = record
  private
    FMock: IMock<T>;
    procedure EnsureCreated;
    function GetInstance: T;
  public
    class function Create(Behavior: TMockBehavior = TMockBehavior.Loose): Mock<T>; overload; static;
    class function Create(Interceptor: TObject): Mock<T>; overload; static;
    class function FromInterface(const Intf: IMock<T>): Mock<T>; static;

    property Instance: T read GetInstance;
    function Setup: ISetup<T>;
    function Received: T; overload;
    function Received(const ATimes: Times): T; overload;
    function DidNotReceive: T;
    procedure Reset;
    procedure Verify; overload;
    function Verify(const ATimes: Times): T; overload;
    procedure VerifyNoOtherCalls;
    function CallsBaseForUnconfiguredMembers: Mock<T>;

    class operator Implicit(const AMock: Mock<T>): T;
    property Object_: T read GetInstance;
    function ProxyInterface: IMock<T>;
  end;

implementation

uses
  Dext.Mocks.Interceptor;

{ Times }

class function Times.Never: Times;
begin
  Result.FMin := 0; Result.FMax := 0; Result.FDescription := 'never';
end;

class function Times.Once: Times;
begin
  Result.FMin := 1; Result.FMax := 1; Result.FDescription := 'once';
end;

class function Times.AtLeastOnce: Times;
begin
  Result.FMin := 1; Result.FMax := MaxInt; Result.FDescription := 'at least once';
end;

class function Times.AtLeast(Count: Integer): Times;
begin
  Result.FMin := Count; Result.FMax := MaxInt; Result.FDescription := Format('at least %d times', [Count]);
end;

class function Times.AtMost(Count: Integer): Times;
begin
  Result.FMin := 0; Result.FMax := Count; Result.FDescription := Format('at most %d times', [Count]);
end;

class function Times.Exactly(Count: Integer): Times;
begin
  Result.FMin := Count; Result.FMax := Count; Result.FDescription := Format('exactly %d times', [Count]);
end;

class function Times.Between(Min, Max: Integer): Times;
begin
  Result.FMin := Min; Result.FMax := Max; Result.FDescription := Format('between %d and %d times', [Min, Max]);
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

class function Mock<T>.FromInterface(const Intf: IMock<T>): Mock<T>;
begin
  Result.FMock := Intf;
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
  if FMock <> nil then FMock.Reset;
end;

procedure Mock<T>.Verify;
begin
  EnsureCreated; FMock.Verify;
end;

function Mock<T>.Verify(const ATimes: Times): T;
begin
  EnsureCreated; Result := FMock.Received(ATimes);
end;

procedure Mock<T>.VerifyNoOtherCalls;
begin
  EnsureCreated; FMock.VerifyNoOtherCalls;
end;

function Mock<T>.CallsBaseForUnconfiguredMembers: Mock<T>;
begin
  EnsureCreated; FMock.SetCallBase(True); Result := Self;
end;

class operator Mock<T>.Implicit(const AMock: Mock<T>): T;
begin
  Result := AMock.Instance;
end;

function Mock<T>.ProxyInterface: IMock<T>;
begin
  EnsureCreated; Result := FMock;
end;

end.
