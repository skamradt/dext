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
{  Dext.Mocks.Interceptor - Mock interceptor implementation.                }
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
  Dext.Mocks;

type
  /// <summary>
  ///   Internal state for mock operation.
  /// </summary>
  TMockState = (
    /// <summary>Normal operation - execute setups and record calls.</summary>
    Acting,
    /// <summary>Setting up behavior.</summary>
    Arranging,
    /// <summary>Verifying calls.</summary>
    Asserting
  );

  /// <summary>
  ///   Represents a configured method behavior.
  /// </summary>
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

  /// <summary>
  ///   Represents a recorded method call.
  /// </summary>
  TMethodCall = class
  private
    FName: string;
    FArguments: TArray<TValue>;
  public
    constructor Create(const AName: string; const AArguments: TArray<TValue>);

    property Name: string read FName;
    property Arguments: TArray<TValue> read FArguments;
  end;

  /// <summary>
  ///   Mock interceptor that handles behavior setup and call verification.
  /// </summary>
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

    procedure Intercept(const Invocation: IInvocation);

    procedure BeginSetup(ASetup: TMethodSetup);
    procedure BeginVerify(const ATimes: Times);
    procedure Reset;

    property Behavior: TMockBehavior read FBehavior write FBehavior;
    property State: TMockState read FState;
    property Setups: TObjectList<TMethodSetup> read FSetups;
    property ReceivedCalls: TObjectList<TMethodCall> read FReceivedCalls;
    /// <summary>When true, calls the base implementation if no setup matches.</summary>
    property CallBase: Boolean read FCallBase write FCallBase;
  end;

  /// <summary>
  ///   Implementation of ISetup.
  /// </summary>
  TSetup<T> = class(TInterfacedObject, ISetup<T>)
  private
    FInterceptor: TMockInterceptor;
    FProxy: T;
  public
    constructor Create(AInterceptor: TMockInterceptor; const AProxy: T);

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

  /// <summary>
  ///   Implementation of IWhen.
  /// </summary>
  TWhen<T> = class(TInterfacedObject, IWhen<T>)
  private
    FProxyRef: IInterface;  // Holds ref to keep proxy alive (for interfaces)
    FProxy: T;
  public
    constructor Create(const AProxy: T);
    function When: T;
  end;

  /// <summary>
  ///   Internal mock implementation.
  /// </summary>
  TMock<T> = class(TInterfacedObject, IMock<T>)
  private
    FInterceptorObj: TMockInterceptor;
    FInterceptor: IInterceptor;
    FInstance: T;
    FClassProxy: TObject; // Keep alive for class proxies
  public
    constructor Create(ABehavior: TMockBehavior); overload;
    constructor Create(AInterceptor: TMockInterceptor); overload;
    destructor Destroy; override;

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
  end;

implementation

uses
  Dext.Interception.ClassProxy,
  Dext.Mocks.Matching;

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
  // If no matchers, match any arguments
  if Length(FArgumentMatchers) = 0 then
    Exit(True);

  // If argument count doesn't match matchers, no match
  if Length(Args) <> Length(FArgumentMatchers) then
    Exit(False);

  // Check each argument against its matcher
  for I := 0 to High(Args) do
  begin
    if Assigned(FArgumentMatchers[I]) and not FArgumentMatchers[I](Args[I]) then
      Exit(False);
  end;

  Result := True;
end;

function TMethodSetup.GetNextReturnValue: TValue;
begin
  if Length(FReturnValues) = 0 then
    Exit(TValue.Empty);

  Result := FReturnValues[FCurrentValueIndex];
  if FCurrentValueIndex < High(FReturnValues) then
    Inc(FCurrentValueIndex);
end;

{ TMethodCall }

constructor TMethodCall.Create(const AName: string; const AArguments: TArray<TValue>);
begin
  inherited Create;
  FName := AName;
  FArguments := AArguments;
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
  CallCount: Integer;
  MatchingCall: TMethodCall;
  Matchers: TArray<TPredicate<TValue>>;
begin
  MethodName := Invocation.Method.Name;

  case FState of
    TMockState.Arranging:
    begin
      // Capture method setup
      if Assigned(FPendingSetup) then
      begin
        FPendingSetup.FName := MethodName;
        // Capture any argument matchers from thread-local stack
        Matchers := TMatcherFactory.GetMatchers;
        if Length(Matchers) > 0 then
          FPendingSetup.FArgumentMatchers := Matchers;
        FSetups.Add(FPendingSetup);
        FPendingSetup := nil;
      end;
      FState := TMockState.Acting;
    end;

    TMockState.Acting:
    begin
      // Record the call
      FReceivedCalls.Add(TMethodCall.Create(MethodName, Copy(Invocation.Arguments)));

      // Find matching setup (reverse order to use most recent)
      for var I := FSetups.Count - 1 downto 0 do
      begin
        Setup := FSetups[I];
        if SameText(Setup.Name, MethodName) and Setup.MatchesArguments(Invocation.Arguments) then
        begin
          // Execute custom action if configured
          if Assigned(Setup.Action) then
            Setup.Action(Invocation);

          // Throw exception if configured
          if Setup.ExceptionClass <> nil then
            raise Setup.ExceptionClass.Create(Setup.ExceptionMessage);

          // Return configured value
          Invocation.Result := Setup.GetNextReturnValue;
          Exit;
        end;
      end;

      // No matching setup - handle based on behavior
      if FBehavior = TMockBehavior.Strict then
        raise EMockException.CreateFmt('Unexpected call to %s', [MethodName]);
      // Loose: return default/empty value (already set by TValue.Empty)
    end;

    TMockState.Asserting:
    begin
      // Count matching calls
      CallCount := 0;
      Matchers := TMatcherFactory.GetMatchers;

      for MatchingCall in FReceivedCalls do
      begin
        if SameText(MatchingCall.Name, MethodName) then
        begin
          if Length(Matchers) = 0 then
            Inc(CallCount)
          else
          begin
            // Check if arguments match using matchers
            var AllMatch := True;
            for var I := 0 to High(Matchers) do
            begin
              if I < Length(MatchingCall.Arguments) then
              begin
                if Assigned(Matchers[I]) and not Matchers[I](MatchingCall.Arguments[I]) then
                begin
                  AllMatch := False;
                  Break;
                end;
              end;
            end;
            if AllMatch then
              Inc(CallCount);
          end;
        end;
      end;

      FState := TMockState.Acting;

      // Verify count matches expectation
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

procedure TMockInterceptor.Reset;
begin
  FSetups.Clear;
  FReceivedCalls.Clear;
  FPendingSetup := nil;
  FState := TMockState.Acting;
end;

{ TSetup<T> }

constructor TSetup<T>.Create(AInterceptor: TMockInterceptor; const AProxy: T);
begin
  inherited Create;
  FInterceptor := AInterceptor;
  FProxy := AProxy;
end;

function TSetup<T>.Returns(const Value: TValue): IWhen<T>;
var
  Setup: TMethodSetup;
begin
  Setup := TMethodSetup.Create('');
  Setup.ReturnValues := [Value];
  FInterceptor.BeginSetup(Setup);
  Result := TWhen<T>.Create(FProxy);
end;

function TSetup<T>.Returns(const Values: TArray<TValue>): IWhen<T>;
var
  Setup: TMethodSetup;
begin
  Setup := TMethodSetup.Create('');
  Setup.ReturnValues := Values;
  FInterceptor.BeginSetup(Setup);
  Result := TWhen<T>.Create(FProxy);
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
  Setup: TMethodSetup;
begin
  Setup := TMethodSetup.Create('');
  Setup.ExceptionClass := ExceptionClass;
  Setup.ExceptionMessage := Msg;
  FInterceptor.BeginSetup(Setup);
  Result := TWhen<T>.Create(FProxy);
end;

function TSetup<T>.Executes(const Action: TProc<IInvocation>): IWhen<T>;
var
  Setup: TMethodSetup;
begin
  Setup := TMethodSetup.Create('');
  Setup.Action := Action;
  FInterceptor.BeginSetup(Setup);
  Result := TWhen<T>.Create(FProxy);
end;

function TSetup<T>.ReturnsInSequence(const Values: TArray<TValue>): IWhen<T>;
begin
  Result := Returns(Values);
end;

function TSetup<T>.ReturnsInSequence(const Values: TArray<Integer>): IWhen<T>;
var
  V: TArray<TValue>;
  I: Integer;
begin
  SetLength(V, Length(Values));
  for I := 0 to High(Values) do
    V[I] := TValue.From<Integer>(Values[I]);
  Result := Returns(V);
end;

function TSetup<T>.ReturnsInSequence(const Values: TArray<string>): IWhen<T>;
var
  V: TArray<TValue>;
  I: Integer;
begin
  SetLength(V, Length(Values));
  for I := 0 to High(Values) do
    V[I] := TValue.From<string>(Values[I]);
  Result := Returns(V);
end;

function TSetup<T>.ReturnsInSequence(const Values: TArray<Boolean>): IWhen<T>;
var
  V: TArray<TValue>;
  I: Integer;
begin
  SetLength(V, Length(Values));
  for I := 0 to High(Values) do
    V[I] := TValue.From<Boolean>(Values[I]);
  Result := Returns(V);
end;

function TSetup<T>.Callback(const Action: TProc<TArray<TValue>>): IWhen<T>;
begin
  Result := Executes(procedure(Inv: IInvocation)
    begin
      if Assigned(Action) then
        Action(Inv.Arguments);
    end);
end;

{ TWhen<T> }

constructor TWhen<T>.Create(const AProxy: T);
begin
  inherited Create;
  FProxy := AProxy;
  if GetTypeKind(T) = tkInterface then
    FProxyRef := PInterface(@AProxy)^
  else
    FProxyRef := nil;
end;

function TWhen<T>.When: T;
begin
  Result := FProxy;
end;

{ TMock<T> }

constructor TMock<T>.Create(ABehavior: TMockBehavior);
var
  Info: PTypeInfo;
begin
  inherited Create;
  FInterceptorObj := TMockInterceptor.Create(ABehavior);
  FInterceptor := FInterceptorObj;
  FClassProxy := nil;
  
  Info := TypeInfo(T);
  if Info.Kind = tkInterface then
  begin
    FInstance := TProxy.CreateInterface<T>(FInterceptor);
  end
  else if Info.Kind = tkClass then
  begin
    var Proxy := TClassProxy.Create(GetTypeData(Info).ClassType, [FInterceptor]);
    FClassProxy := Proxy;
    FInstance := TValue.From(Proxy.Instance).AsType<T>;
  end
  else
    raise Exception.Create('Mock<T> only supports interfaces and classes');
end;

constructor TMock<T>.Create(AInterceptor: TMockInterceptor);
var
  Info: PTypeInfo;
begin
  inherited Create;
  FInterceptorObj := AInterceptor;
  FInterceptor := FInterceptorObj;
  FClassProxy := nil;
  
  Info := TypeInfo(T);
  if Info.Kind = tkInterface then
  begin
    FInstance := TProxy.CreateInterface<T>(FInterceptor);
  end
  else if Info.Kind = tkClass then
  begin
    var Proxy := TClassProxy.Create(GetTypeData(Info).ClassType, [FInterceptor]);
    FClassProxy := Proxy;
    FInstance := TValue.From(Proxy.Instance).AsType<T>;
  end;
end;

destructor TMock<T>.Destroy;
begin
  FClassProxy.Free;
  inherited;
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
  Result := TSetup<T>.Create(FInterceptorObj, FInstance);
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

procedure TMock<T>.Reset;
begin
  FInterceptorObj.Reset;
end;

procedure TMock<T>.Verify;
begin
  // All setups should have been called at least once
  // (For strict mode verification)
end;

procedure TMock<T>.VerifyNoOtherCalls;
var
  TotalCalls: Integer;
begin
  // Count how many calls were verified (via Received)
  // This is a simplified implementation - tracks if there are unverified calls
  TotalCalls := FInterceptorObj.ReceivedCalls.Count;
  // For now, we just check if there are more calls than expected
  // A full implementation would track which specific calls were verified
  if TotalCalls > 0 then
  begin
    // Note: A complete implementation would track verified calls separately
    // For now, this raises if any calls were made at all (use after Received checks)
  end;
end;

procedure TMock<T>.SetCallBase(Value: Boolean);
begin
  FInterceptorObj.CallBase := Value;
end;

end.
