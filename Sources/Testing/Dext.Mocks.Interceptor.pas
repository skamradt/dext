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

unit Dext.Mocks.Interceptor;

interface

uses
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections,
  Dext.Interception,
  Dext.Interception.ClassProxy,
  Dext.Mocks;

type
  TMockState = (Acting, Arranging, Asserting);

  TMethodSetup = class
  private
    FName: string;
    FReturnValues: TArray<TValue>;
    FCurrentValueIndex: Integer;
    FExceptionClass: ExceptClass;
    FExceptionMessage: string;
    FAction: TProc<IInvocation>;
    FArgumentMatchers: TArray<TPredicate<TValue>>;
  public
    constructor Create(const AName: string);
    function MatchesArguments(const Args: TArray<TValue>): Boolean;
    function GetNextReturnValue: TValue;
    property Name: string read FName;
    property ReturnValues: TArray<TValue> read FReturnValues write FReturnValues;
    property ExceptionClass: ExceptClass read FExceptionClass write FExceptionClass;
    property ExceptionMessage: string read FExceptionMessage write FExceptionMessage;
    property Action: TProc<IInvocation> read FAction write FAction;
    property ArgumentMatchers: TArray<TPredicate<TValue>> read FArgumentMatchers write FArgumentMatchers;
  end;

  TMethodCall = class
  private
    FName: string;
    FArguments: TArray<TValue>;
    FVerified: Boolean;
  public
    constructor Create(const AName: string; const AArguments: TArray<TValue>);
    property Name: string read FName;
    property Arguments: TArray<TValue> read FArguments;
    property Verified: Boolean read FVerified write FVerified;
  end;

  TMockInterceptor = class(TInterfacedObject, IInterceptor)
  private
    FBehavior: TMockBehavior;
    FState: TMockState;
    FSetups: TObjectList<TMethodSetup>;
    FReceivedCalls: TObjectList<TMethodCall>;
    FPendingSetup: TMethodSetup;
    FVerifyTimes: Times;
    FCallBase: Boolean;
  public
    constructor Create(ABehavior: TMockBehavior);
    destructor Destroy; override;
    procedure ClearInterceptors;
    procedure Intercept(const Invocation: IInvocation);
    procedure BeginSetup(ASetup: TMethodSetup);
    procedure BeginVerify(const ATimes: Times);
    procedure VerifyNoOtherCalls;
    procedure Reset;
    property Behavior: TMockBehavior read FBehavior write FBehavior;
    property State: TMockState read FState;
    property Setups: TObjectList<TMethodSetup> read FSetups;
    property ReceivedCalls: TObjectList<TMethodCall> read FReceivedCalls;
    property CallBase: Boolean read FCallBase write FCallBase;
  end;

  TSetup<T> = class(TInterfacedObject, ISetup<T>)
  private
    FMock: IMock<T>;
    FInterceptor: TMockInterceptor;
    FProxy: T;
  public
    constructor Create(const AMock: IMock<T>; AInterceptor: TMockInterceptor; const AProxy: T);
    function Returns(const Value: TValue): IWhen<T>; overload;
    function Returns(const Values: TArray<TValue>): IWhen<T>; overload;
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

  TWhen<T> = class(TInterfacedObject, IWhen<T>)
  private
    FMock: IMock<T>;
    FProxyRef: IInterface;
    FProxy: T;
  public
    constructor Create(const AMock: IMock<T>; const AProxy: T);
    function When: T;
  end;

  TMock<T> = class(TInterfacedObject, IMock, IMock<T>)
  private
    FInterceptorObj: TMockInterceptor;
    FInterceptor: IInterceptor;
    FInstance: T;
    FClassProxy: TClassProxy;
    FOwnsProxy: Boolean;
  public
    constructor Create(ABehavior: TMockBehavior); overload;
    constructor Create(AInterceptor: TMockInterceptor); overload;
    constructor Create(AInterceptor: TMockInterceptor; AClassProxy: TClassProxy; AOwnsProxy: Boolean); overload;
    destructor Destroy; override;

    // IMock
    function GetInstanceValue: TValue;
    procedure Verify;
    procedure VerifyNoOtherCalls;
    procedure Reset;

    // IMock<T>
    function GetInstance: T;
    function GetBehavior: TMockBehavior;
    procedure SetBehavior(Value: TMockBehavior);
    function Setup: ISetup<T>;
    function Received: T; overload;
    function Received(const ATimes: Times): T; overload;
    function DidNotReceive: T;
    procedure SetCallBase(Value: Boolean);

    property Instance: T read GetInstance;
  end;

implementation

uses
  Dext.Mocks.Matching,
  Dext.Interception.Proxy;

{ TMethodSetup }

constructor TMethodSetup.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
  FCurrentValueIndex := 0;
end;

function TMethodSetup.MatchesArguments(const Args: TArray<TValue>): Boolean;
var
  I: Integer;
begin
  if Length(FArgumentMatchers) = 0 then Exit(True);
  if Length(Args) <> Length(FArgumentMatchers) then Exit(False);
  for I := 0 to High(Args) do
    if Assigned(FArgumentMatchers[I]) and not FArgumentMatchers[I](Args[I]) then Exit(False);
  Result := True;
end;

function TMethodSetup.GetNextReturnValue: TValue;
begin
  if Length(FReturnValues) = 0 then Exit(TValue.Empty);
  Result := FReturnValues[FCurrentValueIndex];
  if FCurrentValueIndex < High(FReturnValues) then Inc(FCurrentValueIndex);
end;

{ TMethodCall }

constructor TMethodCall.Create(const AName: string; const AArguments: TArray<TValue>);
begin
  inherited Create;
  FName := AName;
  FArguments := AArguments;
  FVerified := False;
end;

{ TMockInterceptor }

constructor TMockInterceptor.Create(ABehavior: TMockBehavior);
begin
  inherited Create;
  FBehavior := ABehavior;
  FState := TMockState.Acting;
  FSetups := TObjectList<TMethodSetup>.Create(True);
  FReceivedCalls := TObjectList<TMethodCall>.Create(True);
end;

destructor TMockInterceptor.Destroy;
begin
  FSetups.Free;
  FReceivedCalls.Free;
  inherited;
end;

procedure TMockInterceptor.Intercept(const Invocation: IInvocation);
var
  MethodName: string;
  Setup: TMethodSetup;
  I: Integer;
  MatchingCall: TMethodCall;
  CallCount: Integer;
  Matchers: TArray<TPredicate<TValue>>;
  AllMatch: Boolean;
begin
  MethodName := Invocation.Method.Name;
  case FState of
    TMockState.Arranging:
    begin
      if Assigned(FPendingSetup) then
      begin
        FPendingSetup.FName := MethodName;
        Matchers := TMatcherFactory.GetMatchers;
        if Length(Matchers) > 0 then FPendingSetup.FArgumentMatchers := Matchers;
        FSetups.Add(FPendingSetup);
        FPendingSetup := nil;
      end;
      FState := TMockState.Acting;
    end;
    TMockState.Acting:
    begin
      FReceivedCalls.Add(TMethodCall.Create(MethodName, Copy(Invocation.Arguments)));
      for I := FSetups.Count - 1 downto 0 do
      begin
        Setup := FSetups[I];
        if SameText(Setup.Name, MethodName) and Setup.MatchesArguments(Invocation.Arguments) then
        begin
          if Assigned(Setup.Action) then Setup.Action(Invocation);
          if Setup.ExceptionClass <> nil then raise Setup.ExceptionClass.Create(Setup.ExceptionMessage);
          Invocation.Result := Setup.GetNextReturnValue;
          Exit;
        end;
      end;
      if FBehavior = TMockBehavior.Strict then raise EMockException.CreateFmt('Unexpected call to %s', [MethodName]);
    end;
    TMockState.Asserting:
    begin
      CallCount := 0;
      Matchers := TMatcherFactory.GetMatchers;
      for MatchingCall in FReceivedCalls do
      begin
        if SameText(MatchingCall.Name, MethodName) then
        begin
          if Length(Matchers) = 0 then
          begin
            Inc(CallCount);
            MatchingCall.Verified := True;
          end
          else
          begin
            AllMatch := True;
            for I := 0 to High(Matchers) do
            begin
              if (I < Length(MatchingCall.Arguments)) and Assigned(Matchers[I]) and not Matchers[I](MatchingCall.Arguments[I]) then
              begin
                AllMatch := False;
                Break;
              end;
            end;
            if AllMatch then
            begin
              Inc(CallCount);
              MatchingCall.Verified := True;
            end;
          end;
        end;
      end;
      FState := TMockState.Acting;
      if not FVerifyTimes.Matches(CallCount) then
        raise EMockException.Create(FVerifyTimes.ToString(CallCount) + ' for ' + MethodName);
    end;
  end;
end;

procedure TMockInterceptor.BeginSetup(ASetup: TMethodSetup);
begin
  FPendingSetup := ASetup;
  FState := TMockState.Arranging;
end;

procedure TMockInterceptor.BeginVerify(const ATimes: Times);
begin
  FVerifyTimes := ATimes;
  FState := TMockState.Asserting;
end;

procedure TMockInterceptor.VerifyNoOtherCalls;
var
  Call: TMethodCall;
begin
  for Call in FReceivedCalls do
    if not Call.Verified then raise EMockException.CreateFmt('Unverified call to %s', [Call.Name]);
end;

procedure TMockInterceptor.Reset;
begin
  FSetups.Clear;
  FReceivedCalls.Clear;
  FPendingSetup := nil;
  FState := TMockState.Acting;
end;

procedure TMockInterceptor.ClearInterceptors;
begin
  FSetups.Clear;
  FReceivedCalls.Clear;
  FPendingSetup := nil;
end;

{ TSetup<T> }

constructor TSetup<T>.Create(const AMock: IMock<T>; AInterceptor: TMockInterceptor; const AProxy: T);
begin
  inherited Create;
  FMock := AMock;
  FInterceptor := AInterceptor;
  FProxy := AProxy;
end;

function TSetup<T>.Returns(const Value: TValue): IWhen<T>;
var
  S: TMethodSetup;
begin
  S := TMethodSetup.Create('');
  S.ReturnValues := [Value];
  FInterceptor.BeginSetup(S);
  Result := TWhen<T>.Create(FMock, FProxy);
end;

function TSetup<T>.Returns(const Values: TArray<TValue>): IWhen<T>;
var
  S: TMethodSetup;
begin
  S := TMethodSetup.Create('');
  S.ReturnValues := Values;
  FInterceptor.BeginSetup(S);
  Result := TWhen<T>.Create(FMock, FProxy);
end;

function TSetup<T>.Returns(Value: Integer): IWhen<T>;
begin
  Result := Returns(TValue.From<Integer>(Value));
end;

function TSetup<T>.Returns(const Value: string): IWhen<T>;
begin
  Result := Returns(TValue.From<string>(Value));
end;

function TSetup<T>.Returns(Value: Boolean): IWhen<T>;
begin
  Result := Returns(TValue.From<Boolean>(Value));
end;

function TSetup<T>.Returns(Value: Double): IWhen<T>;
begin
  Result := Returns(TValue.From<Double>(Value));
end;

function TSetup<T>.Returns(Value: Int64): IWhen<T>;
begin
  Result := Returns(TValue.From<Int64>(Value));
end;

function TSetup<T>.Throws(ExceptionClass: ExceptClass; const Msg: string): IWhen<T>;
var
  S: TMethodSetup;
begin
  S := TMethodSetup.Create('');
  S.ExceptionClass := ExceptionClass;
  S.ExceptionMessage := Msg;
  FInterceptor.BeginSetup(S);
  Result := TWhen<T>.Create(FMock, FProxy);
end;

function TSetup<T>.Executes(const Action: TProc<IInvocation>): IWhen<T>;
var
  S: TMethodSetup;
begin
  S := TMethodSetup.Create('');
  S.Action := Action;
  FInterceptor.BeginSetup(S);
  Result := TWhen<T>.Create(FMock, FProxy);
end;

function TSetup<T>.ReturnsInSequence(const Values: TArray<TValue>): IWhen<T>; begin Result := Returns(Values); end;

function TSetup<T>.ReturnsInSequence(const Values: TArray<Integer>): IWhen<T>;
var
  V: TArray<TValue>;
  I: Integer;
begin
  SetLength(V, Length(Values));
  for I := 0 to High(Values) do V[I] := TValue.From<Integer>(Values[I]);
  Result := Returns(V);
end;

function TSetup<T>.ReturnsInSequence(const Values: TArray<string>): IWhen<T>;
var
  V: TArray<TValue>;
  I: Integer;
begin
  SetLength(V, Length(Values));
  for I := 0 to High(Values) do V[I] := TValue.From<string>(Values[I]);
  Result := Returns(V);
end;

function TSetup<T>.ReturnsInSequence(const Values: TArray<Boolean>): IWhen<T>;
var
  V: TArray<TValue>;
  I: Integer;
begin
  SetLength(V, Length(Values));
  for I := 0 to High(Values) do V[I] := TValue.From<Boolean>(Values[I]);
  Result := Returns(V);
end;

function TSetup<T>.Callback(const Action: TProc<TArray<TValue>>): IWhen<T>;
begin
  Result := Executes(procedure(Inv: IInvocation)
    begin
      if Assigned(Action) then Action(Inv.Arguments);
    end);
end;

{ TWhen<T> }

constructor TWhen<T>.Create(const AMock: IMock<T>; const AProxy: T);
begin
  inherited Create;
  FMock := AMock;
  FProxy := AProxy;
  if GetTypeKind(T) = tkInterface then FProxyRef := IInterface(PPointer(@AProxy)^) else FProxyRef := nil;
end;

function TWhen<T>.When: T; begin Result := FProxy; end;

{ TMock<T> }

constructor TMock<T>.Create(ABehavior: TMockBehavior);
var
  Info: PTypeInfo;
  Proxy: TClassProxy;
begin
  inherited Create;
  FInterceptorObj := TMockInterceptor.Create(ABehavior);
  FInterceptor := FInterceptorObj;
  Info := TypeInfo(T);
  FOwnsProxy := True;
  if Info.Kind = tkInterface then
    FInstance := TProxy.CreateInterface<T>(FInterceptor)
  else if Info.Kind = tkClass then
  begin
    Proxy := TClassProxy.Create(Info.TypeData.ClassType, [FInterceptor]);
    FClassProxy := Proxy;
    FInstance := TValue.From(Proxy.Instance).AsType<T>;
  end
  else
    raise Exception.Create('Mock<T> only supports interfaces and classes');
end;

constructor TMock<T>.Create(AInterceptor: TMockInterceptor);
var
  Info: PTypeInfo;
  Proxy: TClassProxy;
begin
  inherited Create;
  FInterceptorObj := AInterceptor;
  FInterceptor := FInterceptorObj;
  Info := TypeInfo(T);
  FOwnsProxy := True;
  if Info.Kind = tkInterface then
    FInstance := TProxy.CreateInterface<T>(FInterceptor)
  else if Info.Kind = tkClass then
  begin
    Proxy := TClassProxy.Create(Info.TypeData.ClassType, [FInterceptor]);
    FClassProxy := Proxy;
    FInstance := TValue.From(Proxy.Instance).AsType<T>;
  end;
end;

constructor TMock<T>.Create(AInterceptor: TMockInterceptor; AClassProxy: TClassProxy; AOwnsProxy: Boolean);
begin
  inherited Create;
  FInterceptorObj := AInterceptor;
  FInterceptor := FInterceptorObj;
  FClassProxy := AClassProxy;
  FOwnsProxy := AOwnsProxy;
  FInstance := TValue.From(FClassProxy.Instance).AsType<T>;
end;

destructor TMock<T>.Destroy;
begin
  if FOwnsProxy then 
    FreeAndNil(FClassProxy);
  inherited;
end;


function TMock<T>.GetInstanceValue: TValue;
begin
  Result := TValue.From<T>(FInstance);
end;

procedure TMock<T>.Verify;
begin
end;

procedure TMock<T>.VerifyNoOtherCalls;
begin
  FInterceptorObj.VerifyNoOtherCalls;
end;

procedure TMock<T>.Reset;
begin
  FInterceptorObj.Reset;
end;

function TMock<T>.GetInstance: T;
begin
  Result := FInstance;
end;

function TMock<T>.GetBehavior: TMockBehavior;
begin
  Result := FInterceptorObj.Behavior;
end;

procedure TMock<T>.SetBehavior(Value: TMockBehavior);
begin
  FInterceptorObj.Behavior := Value;
end;

function TMock<T>.Setup: ISetup<T>;
begin
  Result := TSetup<T>.Create(Self, FInterceptorObj, FInstance);
end;

function TMock<T>.Received: T;
begin
  Result := Received(Times.AtLeastOnce);
end;

function TMock<T>.Received(const ATimes: Times): T;
begin
  FInterceptorObj.BeginVerify(ATimes);
  Result := FInstance;
end;

function TMock<T>.DidNotReceive: T;
begin
  Result := Received(Times.Never);
end;

procedure TMock<T>.SetCallBase(Value: Boolean);
begin
  FInterceptorObj.CallBase := Value;
end;

end.
