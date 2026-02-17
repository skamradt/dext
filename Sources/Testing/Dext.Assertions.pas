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
{  Dext.Assertions - Fluent assertions inspired by FluentAssertions.        }
{                                                                           }
{  Usage:                                                                   }
{    ShouldString(Value).Be('expected');                                    }
{    ShouldInteger(N).BeGreaterThan(0);                                     }
{    ShouldBoolean(B).BeTrue;                                               }
{    ShouldAction(MyProc).Throw<EInvalidOp>;                                }
{                                                                           }
{***************************************************************************}
unit Dext.Assertions;

interface

uses
  System.DateUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.IOUtils,
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  System.Variants,
  Dext,
  Dext.Collections,
  Dext.Core.SmartTypes,
  Dext.Types.Nullable,
  Dext.Types.UUID;

type
  /// <summary>
  ///   Exception raised when an assertion fails.
  /// </summary>
  EAssertionFailed = class(Exception);

  /// <summary>
  ///   Static class for assertion utilities.
  /// </summary>
  Assert = record
  public
    /// <summary>
    ///   Executes assertions in a "soft" mode, collecting all failures instead of stopping at the first one.
    /// </summary>
    class procedure Multiple(const Action: TProc); static;
    
    /// <summary>
    ///   Fails the test with the given message.
    /// </summary>
    class procedure Fail(const Message: string); static;

    /// <summary>
    ///   Internal use only. Registers a failure respecting soft assert mode.
    /// </summary>
    class procedure RegisterFailure(const Message, Reason: string); static;

    /// <summary>
    ///   Verifies that the given action raises an exception of the specified class.
    /// </summary>
    /// <summary>
    ///   Verifies that the given action raises an exception of the specified class.
    /// </summary>
    class procedure WillRaise(const Action: TProc; ExceptionClass: ExceptClass = nil; const MessagePart: string = ''); static;
  end;

  /// <summary>
  ///   Fluent assertions for DateTime.
  /// </summary>
  ShouldDateTime = record
  private
    FValue: TDateTime;
    FReason: string;
    procedure Fail(const Message: string);
  public
    constructor Create(Value: TDateTime);
    function Be(Expected: TDateTime): ShouldDateTime;
    function BeCloseTo(Expected: TDateTime; PrecisionMS: Int64 = 1000): ShouldDateTime;
    function BeAfter(Expected: TDateTime): ShouldDateTime;
    function BeBefore(Expected: TDateTime): ShouldDateTime;
    function BeSameDateAs(Expected: TDateTime): ShouldDateTime; // Ignores time
    function Because(const Reason: string): ShouldDateTime;
  end;

  /// <summary>
  ///   String-specific Should assertions.
  /// </summary>
  ShouldString = record
  private
    FValue: string;
    FReason: string;
    procedure Fail(const Message: string);
  public
    constructor Create(const Value: string);
    
    function Be(const Expected: string): ShouldString;
    function NotBe(const Unexpected: string): ShouldString;
    function BeEquivalentTo(const Expected: string): ShouldString;
    function BeEmpty: ShouldString;
    function NotBeEmpty: ShouldString;
    function Contain(const Substring: string): ShouldString;
    function NotContain(const Substring: string): ShouldString;
    function StartWith(const Prefix: string): ShouldString;
    function EndWith(const Suffix: string): ShouldString;
    function HaveLength(Expected: Integer): ShouldString;
    function HaveLengthGreaterThan(Expected: Integer): ShouldString;
    function HaveLengthLessThan(Expected: Integer): ShouldString;
    function MatchRegex(const Pattern: string): ShouldString;
    function BeUpperCase: ShouldString;
    function BeLowerCase: ShouldString;
    function BeOneOf(const Values: TArray<string>): ShouldString;
    function MatchSnapshot(const SnapshotName: string): ShouldString;
    function Satisfy(const Predicate: TPredicate<string>): ShouldString;
    function Because(const Reason: string): ShouldString;
    // Chaining
    function &And: ShouldString;
    function AndAlso: ShouldString; // Fluent alias for And
  end;

  /// <summary>
  ///   Integer-specific Should assertions.
  /// </summary>
  ShouldInteger = record
  private
    FValue: Integer;
    FReason: string;
    procedure Fail(const Message: string);
  public
    constructor Create(Value: Integer);
    
    function Be(Expected: Integer): ShouldInteger;
    function NotBe(Unexpected: Integer): ShouldInteger;
    function BeGreaterThan(Expected: Integer): ShouldInteger;
    function BeGreaterOrEqualTo(Expected: Integer): ShouldInteger;
    function BeLessThan(Expected: Integer): ShouldInteger;
    function BeLessOrEqualTo(Expected: Integer): ShouldInteger;
    function BeInRange(Min, Max: Integer): ShouldInteger;
    function BePositive: ShouldInteger;
    function BeNegative: ShouldInteger;
    function BeZero: ShouldInteger;
    function BeOneOf(const Values: TArray<Integer>): ShouldInteger;
    function Satisfy(const Predicate: TPredicate<Integer>): ShouldInteger;
    function Because(const Reason: string): ShouldInteger;
    function &And: ShouldInteger;
    function AndAlso: ShouldInteger;
  end;

  /// <summary>
  ///   Boolean-specific Should assertions.
  /// </summary>
  ShouldBoolean = record
  private
    FValue: Boolean;
    FReason: string;
    procedure Fail(const Message: string);
  public
    constructor Create(Value: Boolean);
    
    function BeTrue: ShouldBoolean;
    function BeFalse: ShouldBoolean;
    function Be(Expected: Boolean): ShouldBoolean;
    function NotBe(Unexpected: Boolean): ShouldBoolean;
    function Because(const Reason: string): ShouldBoolean;
    function &And: ShouldBoolean;
    function AndAlso: ShouldBoolean;
  end;

  /// <summary>
  ///   Action-specific Should assertions for exceptions.
  /// </summary>
  ShouldAction = record
  private
    FAction: TProc;
    FReason: string;
    FExceptionClass: ExceptClass;
    FExceptionMessage: string;
    procedure Fail(const Message: string);
  public
    constructor Create(const Action: TProc);
    
    function Throw<E: Exception>: ShouldAction; overload;
    function Throw(AExceptionClass: ExceptClass): ShouldAction; overload;
    function ThrowWithMessage(const ExpectedMessage: string): ShouldAction;
    function ThrowWithMessageContaining(const Substring: string): ShouldAction;
    function NotThrow: ShouldAction;
    function Because(const Reason: string): ShouldAction;
    function &And: ShouldAction;
    function AndAlso: ShouldAction;
  end;

  /// <summary>
  ///   Double/Float-specific Should assertions.
  /// </summary>
  ShouldDouble = record
  private
    FValue: Double;
    FReason: string;
    FPrecision: Double;
    procedure Fail(const Message: string);
  public
    constructor Create(Value: Double);
    
    function Be(Expected: Double): ShouldDouble;
    function NotBe(Unexpected: Double): ShouldDouble;
    function BeApproximately(Expected, Tolerance: Double): ShouldDouble;
    function BeGreaterThan(Expected: Double): ShouldDouble;
    function BeLessThan(Expected: Double): ShouldDouble;
    function BeInRange(Min, Max: Double): ShouldDouble;
    function BePositive: ShouldDouble;
    function BeNegative: ShouldDouble;
    function BeZero: ShouldDouble;
    function Satisfy(const Predicate: TPredicate<Double>): ShouldDouble;
    function Because(const Reason: string): ShouldDouble;
    function &And: ShouldDouble;
    function AndAlso: ShouldDouble;
  end;

  /// <summary>
  ///   Int64-specific Should assertions.
  /// </summary>
  ShouldInt64 = record
  private
    FValue: Int64;
    FReason: string;
    procedure Fail(const Message: string);
  public
    constructor Create(Value: Int64);
    
    function Be(Expected: Int64): ShouldInt64;
    function NotBe(Unexpected: Int64): ShouldInt64;
    function BeGreaterThan(Expected: Int64): ShouldInt64;
    function BeLessThan(Expected: Int64): ShouldInt64;
    function BeInRange(Min, Max: Int64): ShouldInt64;
    function BePositive: ShouldInt64;
    function BeNegative: ShouldInt64;
    function BeZero: ShouldInt64;
    function BeOneOf(const Values: TArray<Int64>): ShouldInt64;
    function Satisfy(const Predicate: TPredicate<Int64>): ShouldInt64;
    function Because(const Reason: string): ShouldInt64;
    function &And: ShouldInt64;
    function AndAlso: ShouldInt64;
  end;

  /// <summary>
  ///   GUID-specific Should assertions.
  /// </summary>
  ShouldGuid = record
  private
    FValue: TGUID;
    FReason: string;
    procedure Fail(const Message: string);
  public
    constructor Create(const Value: TGUID);
    
    function Be(const Expected: TGUID): ShouldGuid;
    function NotBe(const Unexpected: TGUID): ShouldGuid;
    function BeEmpty: ShouldGuid;
    function NotBeEmpty: ShouldGuid;
    function Because(const Reason: string): ShouldGuid;
    function &And: ShouldGuid;
    function AndAlso: ShouldGuid;
  end;

  /// <summary>
  ///   UUID-specific Should assertions (Dext TUUID type).
  /// </summary>
  ShouldUUID = record
  private
    FValue: TUUID;
    FReason: string;
    procedure Fail(const Message: string);
  public
    constructor Create(const Value: TUUID);
    
    function Be(const Expected: TUUID): ShouldUUID;
    function NotBe(const Unexpected: TUUID): ShouldUUID;
    function BeEmpty: ShouldUUID;
    function NotBeEmpty: ShouldUUID;
    function Because(const Reason: string): ShouldUUID;
    function &And: ShouldUUID;
    function AndAlso: ShouldUUID;
  end;

  /// <summary>
  ///   Variant-specific Should assertions for legacy code support.
  /// </summary>
  ShouldVariant = record
  private
    FValue: Variant;
    FReason: string;
    procedure Fail(const Message: string);
  public
    constructor Create(const Value: Variant);
    
    function Be(const Expected: Variant): ShouldVariant;
    function NotBe(const Unexpected: Variant): ShouldVariant;
    function BeNull: ShouldVariant;
    function NotBeNull: ShouldVariant;
    function BeEmpty: ShouldVariant;
    function NotBeEmpty: ShouldVariant;
    function BeOfType(VarType: TVarType): ShouldVariant;
    function Because(const Reason: string): ShouldVariant;
    function &And: ShouldVariant;
    function AndAlso: ShouldVariant;
  end;

  /// <summary>
  ///   Generic object/interface Should assertions.
  /// </summary>
  ShouldObject = record
  private
    FValue: TObject;
    FReason: string;
    procedure Fail(const Message: string);
  public
    constructor Create(Value: TObject);
    
    function BeNil: ShouldObject;
    function NotBeNil: ShouldObject;
    function BeNull: ShouldObject; // Alias for BeNil
    function NotBeNull: ShouldObject; // Alias for NotBeNil
    function BeOfType<T: class>: ShouldObject;
    function BeAssignableTo<T: class>: ShouldObject;
    function BeEquivalentTo(Expected: TObject): ShouldObject;
    function Satisfy(const Predicate: TPredicate<TObject>): ShouldObject;
    function MatchSnapshot(const SnapshotName: string): ShouldObject;
    function Because(const Reason: string): ShouldObject;
    function &And: ShouldObject;
    function AndAlso: ShouldObject;
  end;

  /// <summary>
  ///   Property assertion helper for deep navigation.
  /// </summary>
  ShouldProperty = record
  private
    FParent: ShouldObject;
    FValue: TValue;
    FName: string;
    FReason: string;
    procedure Fail(const Message: string);
  public
    constructor Create(const Parent: ShouldObject; const Name: string; const Value: TValue);
    
    function Value: TValue;
    
    function WhichString: ShouldString;
    function WhichInteger: ShouldInteger;
    function WhichBoolean: ShouldBoolean;
    function WhichObject: ShouldObject;
    function WhichGuid: ShouldGuid;
    
    function &And: ShouldObject;
    function AndAlso: ShouldObject;
  end;

  ShouldObjectHelper = record helper for ShouldObject
  public
    function HaveProperty(const PropertyName: string): ShouldProperty; overload;
    // function HaveProperty<T>(const PropertyMetadata: Prop<T>): ShouldProperty; overload;
    function HavePropertyValue(const PropertyName: string; const ExpectedValue: TValue): ShouldObject; overload;
    // Strongly-typed methods for SmartTypes (separate name to avoid ambiguity with implicit operators)
    function HaveValue(const PropertyMetadata: StringType; const ExpectedValue: string): ShouldObject; overload;
    function HaveValue(const PropertyMetadata: IntType; ExpectedValue: Integer): ShouldObject; overload;
    function HaveValue<T>(const PropertyMetadata: T; ExpectedValue: T): ShouldObject; overload;
  end;

  /// <summary>
  ///   Interface-specific Should assertions.
  /// </summary>
  ShouldInterface = record
  private
    FValue: IInterface;
    FReason: string;
    procedure Fail(const Message: string);
  public
    constructor Create(const Value: IInterface);
    
    function BeNil: ShouldInterface;
    function NotBeNil: ShouldInterface;
    function BeEquivalentTo(const Expected: IInterface): ShouldInterface;
    function MatchSnapshot(const SnapshotName: string): ShouldInterface;
    function Because(const Reason: string): ShouldInterface;
    function &And: ShouldInterface;
    function AndAlso: ShouldInterface;
  end;

  /// <summary>
  ///   Generic list/enumerable assertions.
  /// </summary>
  ShouldList<T> = record
  private
    FEnumerable: IEnumerable<T>;
    FDextEnumerable: Dext.Collections.IEnumerable<T>;
    FArray: TArray<T>;
    FIsArray: Boolean;
    FIsDext: Boolean;
    FReason: string;
    procedure Fail(const Message: string);
    function GetCount: Integer;
  public
    constructor Create(const Value: IEnumerable<T>); overload;
    constructor Create(const Value: TArray<T>); overload;

    function BeEmpty: ShouldList<T>;
    function NotBeEmpty: ShouldList<T>;
    function HaveCount(Expected: Integer): ShouldList<T>;
    function HaveCountGreaterThan(Expected: Integer): ShouldList<T>;
    function HaveCountLessThan(Expected: Integer): ShouldList<T>;
    function Contain(const Item: T): ShouldList<T>;
    function NotContain(const Item: T): ShouldList<T>;
    function ContainInOrder(const Items: TArray<T>): ShouldList<T>;
    function BeEquivalentTo(const Expected: TArray<T>): ShouldList<T>;
    function OnlyContain(const Predicate: TPredicate<T>): ShouldList<T>;
    function AllSatisfy(const Predicate: TPredicate<T>): ShouldList<T>;
    function AnySatisfy(const Predicate: TPredicate<T>): ShouldList<T>;
    function Because(const Reason: string): ShouldList<T>;
    function &And: ShouldList<T>;
    function AndAlso: ShouldList<T>;
  end;

  ShouldHelper = record
  public
    function List<T>(const Value: IEnumerable<T>): ShouldList<T>; overload;
    function List<T>(const Value: TArray<T>): ShouldList<T>; overload;
  end;

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

// Smart Types / Prop<T> Overloads
function Should(const Value: StringType): ShouldString; overload;
function Should(const Value: IntType): ShouldInteger; overload;
function Should(const Value: Int64Type): ShouldInt64; overload;
function Should(const Value: BoolType): ShouldBoolean; overload;
function Should(const Value: FloatType): ShouldDouble; overload;
function Should(const Value: CurrencyType): ShouldDouble; overload;

// Nullable<T> Overloads
function Should(const Value: Nullable<string>): ShouldString; overload;
function Should(const Value: Nullable<Integer>): ShouldInteger; overload;
function Should(const Value: Nullable<Int64>): ShouldInt64; overload;
function Should(const Value: Nullable<Boolean>): ShouldBoolean; overload;
function Should(const Value: Nullable<Double>): ShouldDouble; overload;
function Should(const Value: Nullable<Currency>): ShouldDouble; overload;
function Should(const Value: Nullable<TDateTime>): ShouldDateTime; overload;
function ShouldDate(const Value: Nullable<TDateTime>): ShouldDateTime; overload;
function ShouldDate(const Value: Prop<TDateTime>): ShouldDateTime; overload;

implementation

uses
  System.RegularExpressions,
  System.Math;

threadvar
  GSoftAssertMode: Boolean;
  GSoftAssertErrors: TList<string>;

class procedure Assert.RegisterFailure(const Message, Reason: string);
var
  FullMsg: string;
begin
  if Reason <> '' then
    FullMsg := Message + ' because ' + Reason
  else
    FullMsg := Message;

  // If in Soft Assert mode, collect error and continue
  if GSoftAssertMode and (GSoftAssertErrors <> nil) then
  begin
    GSoftAssertErrors.Add(FullMsg);
  end
  else
  begin
    // Normal mode: raise exception immediately
    raise EAssertionFailed.Create(FullMsg);
  end;
end;

{ Assert }

class procedure Assert.Fail(const Message: string);
begin
  // Direct Assert.Fail should respect soft assertion context too
  Assert.RegisterFailure(Message, '');
end;

class procedure Assert.WillRaise(const Action: TProc; ExceptionClass: ExceptClass; const MessagePart: string);
begin
  if MessagePart = '' then
    Should(Action).Throw(ExceptionClass)
  else
    Should(Action).Throw(ExceptionClass).AndAlso.ThrowWithMessageContaining(MessagePart);
end;

class procedure Assert.Multiple(const Action: TProc);
var
  Errors: TList<string>;
  ErrorMsg: string;
  I: Integer;
begin
  // If already in soft mode (nested call), just execute
  if GSoftAssertMode then
  begin
    Action;
    Exit;
  end;

  Errors := TList<string>.Create;
  try
    GSoftAssertErrors := Errors;
    GSoftAssertMode := True;
    try
      try
        Action;
      except
        on E: Exception do
        begin
          // If a non-assertion exception occurs, we must stop and propagate it.
          // Assertion failures are already captured in GSoftAssertErrors (if they used Assert.RegisterFailure).
          // If EAssertionFailed escaped (e.g. from code not using Assert.RegisterFailure), we capture it too.
          if E is EAssertionFailed then
             Errors.Add(E.Message)
          else
             raise;
        end;
      end;
    finally
      GSoftAssertMode := False;
      GSoftAssertErrors := nil;
    end;

    // Report aggregated errors
    if Errors.Count > 0 then
    begin
       if Errors.Count = 1 then
         raise EAssertionFailed.Create(Errors[0]);
         
       ErrorMsg := Format('Multiple failures (%d):' + sLineBreak, [Errors.Count]);
       for I := 0 to Errors.Count - 1 do
         ErrorMsg := ErrorMsg + Format('  %d) %s' + sLineBreak, [I + 1, Errors[I]]);
         
       raise EAssertionFailed.Create(ErrorMsg);
    end;
  finally
    Errors.Free;
  end;
end;

procedure VerifySnapshot(const Content, SnapshotName: string);
var
  SnapshotPath: string;
  VerifyPath: string;
  ExistingContent: string;
  BaseDir: string;
begin
  if SnapshotName = '' then
    raise Exception.Create('Snapshot name cannot be empty');

  // Directory: .\Snapshots relative to EXE
  BaseDir := TPath.Combine(ExtractFilePath(ParamStr(0)), 'Snapshots');
  if not TDirectory.Exists(BaseDir) then
    TDirectory.CreateDirectory(BaseDir);

  SnapshotPath := TPath.Combine(BaseDir, SnapshotName + '.json');
  VerifyPath := TPath.Combine(BaseDir, SnapshotName + '.received.json');

  // UPDATE SNAPSHOT mode (env var or missing file)
  if (GetEnvironmentVariable('SNAPSHOT_UPDATE') = '1') or (not TFile.Exists(SnapshotPath)) then
  begin
    TFile.WriteAllText(SnapshotPath, Content, TEncoding.UTF8);
    // Delete received file if exists
    if TFile.Exists(VerifyPath) then
      TFile.Delete(VerifyPath);
    Exit;
  end;

  // Compare
  ExistingContent := TFile.ReadAllText(SnapshotPath, TEncoding.UTF8);
  if Content <> ExistingContent then
  begin
    // Write received
    TFile.WriteAllText(VerifyPath, Content, TEncoding.UTF8);
    Assert.RegisterFailure(Format(
      'Snapshot mismatch for "%s"!' + sLineBreak +
      'Expected location: %s' + sLineBreak +
      'Received mismatch saved at: %s',
      [SnapshotName, SnapshotPath, VerifyPath]), '');
  end
  else
  begin
      // Cleanup previous failures
      if TFile.Exists(VerifyPath) then
        TFile.Delete(VerifyPath);
  end;
end;

{ ShouldString }

constructor ShouldString.Create(const Value: string);
begin
  FValue := Value;
  FReason := '';
end;

procedure ShouldString.Fail(const Message: string);
begin
  Assert.RegisterFailure(Message, FReason);
end;

function ShouldString.Be(const Expected: string): ShouldString;
begin
  if FValue <> Expected then
    Fail(Format('Expected "%s" but was "%s"', [Expected, FValue]));
  Result := Self;
end;

function ShouldString.BeEquivalentTo(const Expected: string): ShouldString;
begin
  if not SameText(FValue, Expected) then
    Fail(Format('Expected "%s" (case insensitive) but was "%s"', [Expected, FValue]));
  Result := Self;
end;

function ShouldString.BeEmpty: ShouldString;
begin
  if FValue <> '' then
    Fail(Format('Expected empty string but was "%s"', [FValue]));
  Result := Self;
end;

function ShouldString.NotBeEmpty: ShouldString;
begin
  if FValue = '' then
    Fail('Expected non-empty string but was empty');
  Result := Self;
end;

function ShouldString.Contain(const Substring: string): ShouldString;
begin
  if not FValue.Contains(Substring) then
    Fail(Format('Expected "%s" to contain "%s"', [FValue, Substring]));
  Result := Self;
end;

function ShouldString.NotContain(const Substring: string): ShouldString;
begin
  if FValue.Contains(Substring) then
    Fail(Format('Expected "%s" to not contain "%s"', [FValue, Substring]));
  Result := Self;
end;

function ShouldString.StartWith(const Prefix: string): ShouldString;
begin
  if not FValue.StartsWith(Prefix) then
    Fail(Format('Expected "%s" to start with "%s"', [FValue, Prefix]));
  Result := Self;
end;

function ShouldString.EndWith(const Suffix: string): ShouldString;
begin
  if not FValue.EndsWith(Suffix) then
    Fail(Format('Expected "%s" to end with "%s"', [FValue, Suffix]));
  Result := Self;
end;

function ShouldString.MatchSnapshot(const SnapshotName: string): ShouldString;
begin
  VerifySnapshot(FValue, SnapshotName);
  Result := Self;
end;

function ShouldString.HaveLength(Expected: Integer): ShouldString;
begin
  if Length(FValue) <> Expected then
    Fail(Format('Expected length %d but was %d', [Expected, Length(FValue)]));
  Result := Self;
end;

function ShouldString.Because(const Reason: string): ShouldString;
begin
  Result := Self;
  Result.FReason := Reason;
end;

function ShouldString.NotBe(const Unexpected: string): ShouldString;
begin
  if FValue = Unexpected then
    Fail(Format('Did not expect "%s"', [FValue]));
  Result := Self;
end;

function ShouldString.HaveLengthGreaterThan(Expected: Integer): ShouldString;
begin
  if Length(FValue) <= Expected then
    Fail(Format('Expected length greater than %d but was %d', [Expected, Length(FValue)]));
  Result := Self;
end;

function ShouldString.HaveLengthLessThan(Expected: Integer): ShouldString;
begin
  if Length(FValue) >= Expected then
    Fail(Format('Expected length less than %d but was %d', [Expected, Length(FValue)]));
  Result := Self;
end;

function ShouldString.MatchRegex(const Pattern: string): ShouldString;
begin
  if not TRegEx.IsMatch(FValue, Pattern) then
    Fail(Format('Expected "%s" to match pattern "%s"', [FValue, Pattern]));
  Result := Self;
end;

function ShouldString.BeUpperCase: ShouldString;
begin
  if FValue <> FValue.ToUpper then
    Fail(Format('Expected "%s" to be uppercase', [FValue]));
  Result := Self;
end;

function ShouldString.BeLowerCase: ShouldString;
begin
  if FValue <> FValue.ToLower then
    Fail(Format('Expected "%s" to be lowercase', [FValue]));
  Result := Self;
end;

function ShouldString.BeOneOf(const Values: TArray<string>): ShouldString;
var
  S: string;
  Found: Boolean;
begin
  Found := False;
  for S in Values do
    if FValue = S then
    begin
      Found := True;
      Break;
    end;
  if not Found then
    Fail(Format('Expected "%s" to be one of [%s]', [FValue, string.Join(', ', Values)]));
  Result := Self;
end;

function ShouldString.Satisfy(const Predicate: TPredicate<string>): ShouldString;
begin
  if not Predicate(FValue) then
    Fail(Format('Value "%s" did not satisfy the predicate', [FValue]));
  Result := Self;
end;

function ShouldString.&And: ShouldString;
begin
  Result := Self;
end;

{ ShouldInteger }

constructor ShouldInteger.Create(Value: Integer);
begin
  FValue := Value;
  FReason := '';
end;

procedure ShouldInteger.Fail(const Message: string);
begin
  Assert.RegisterFailure(Message, FReason);
end;

function ShouldInteger.Be(Expected: Integer): ShouldInteger;
begin
  if FValue <> Expected then
    Fail(Format('Expected %d but was %d', [Expected, FValue]));
  Result := Self;
end;

function ShouldInteger.NotBe(Unexpected: Integer): ShouldInteger;
begin
  if FValue = Unexpected then
    Fail(Format('Did not expect %d', [FValue]));
  Result := Self;
end;

function ShouldInteger.BeGreaterThan(Expected: Integer): ShouldInteger;
begin
  if FValue <= Expected then
    Fail(Format('Expected %d to be greater than %d', [FValue, Expected]));
  Result := Self;
end;

function ShouldInteger.BeGreaterOrEqualTo(Expected: Integer): ShouldInteger;
begin
  if FValue < Expected then
    Fail(Format('Expected %d to be greater than or equal to %d', [FValue, Expected]));
  Result := Self;
end;

function ShouldInteger.BeLessThan(Expected: Integer): ShouldInteger;
begin
  if FValue >= Expected then
    Fail(Format('Expected %d to be less than %d', [FValue, Expected]));
  Result := Self;
end;

function ShouldInteger.BeLessOrEqualTo(Expected: Integer): ShouldInteger;
begin
  if FValue > Expected then
    Fail(Format('Expected %d to be less than or equal to %d', [FValue, Expected]));
  Result := Self;
end;

function ShouldInteger.BeInRange(Min, Max: Integer): ShouldInteger;
begin
  if (FValue < Min) or (FValue > Max) then
    Fail(Format('Expected %d to be in range [%d, %d]', [FValue, Min, Max]));
  Result := Self;
end;

function ShouldInteger.BePositive: ShouldInteger;
begin
  if FValue <= 0 then
    Fail(Format('Expected positive value but was %d', [FValue]));
  Result := Self;
end;

function ShouldInteger.BeNegative: ShouldInteger;
begin
  if FValue >= 0 then
    Fail(Format('Expected negative value but was %d', [FValue]));
  Result := Self;
end;

function ShouldInteger.BeZero: ShouldInteger;
begin
  if FValue <> 0 then
    Fail(Format('Expected zero but was %d', [FValue]));
  Result := Self;
end;

function ShouldInteger.Because(const Reason: string): ShouldInteger;
begin
  Result := Self;
  Result.FReason := Reason;
end;

function ShouldInteger.BeOneOf(const Values: TArray<Integer>): ShouldInteger;
var
  V: Integer;
  Found: Boolean;
begin
  Found := False;
  for V in Values do
    if FValue = V then
    begin
      Found := True;
      Break;
    end;
  if not Found then
    Fail(Format('Expected %d to be one of the specified values', [FValue]));
  Result := Self;
end;

function ShouldInteger.Satisfy(const Predicate: TPredicate<Integer>): ShouldInteger;
begin
  if not Predicate(FValue) then
    Fail(Format('Value %d did not satisfy the predicate', [FValue]));
  Result := Self;
end;

function ShouldInteger.&And: ShouldInteger;
begin
  Result := Self;
end;

{ ShouldBoolean }

constructor ShouldBoolean.Create(Value: Boolean);
begin
  FValue := Value;
  FReason := '';
end;

procedure ShouldBoolean.Fail(const Message: string);
begin
  Assert.RegisterFailure(Message, FReason);
end;

function ShouldBoolean.BeTrue: ShouldBoolean;
begin
  if not FValue then
    Fail('Expected True but was False');
  Result := Self;
end;

function ShouldBoolean.BeFalse: ShouldBoolean;
begin
  if FValue then
    Fail('Expected False but was True');
  Result := Self;
end;

function ShouldBoolean.Be(Expected: Boolean): ShouldBoolean;
begin
  if FValue <> Expected then
    Fail(Format('Expected %s but was %s', [BoolToStr(Expected, True), BoolToStr(FValue, True)]));
  Result := Self;
end;

function ShouldBoolean.Because(const Reason: string): ShouldBoolean;
begin
  Result := Self;
  Result.FReason := Reason;
end;

function ShouldBoolean.NotBe(Unexpected: Boolean): ShouldBoolean;
begin
  if FValue = Unexpected then
    Fail(Format('Did not expect %s', [BoolToStr(FValue, True)]));
  Result := Self;
end;

function ShouldBoolean.&And: ShouldBoolean;
begin
  Result := Self;
end;

{ ShouldAction }

constructor ShouldAction.Create(const Action: TProc);
begin
  FAction := Action;
  FReason := '';
  FExceptionClass := nil;
  FExceptionMessage := '';
end;

procedure ShouldAction.Fail(const Message: string);
begin
  Assert.RegisterFailure(Message, FReason);
end;

function ShouldAction.Throw<E>: ShouldAction;
begin
  Result := Throw(ExceptClass(E));
end;

function ShouldAction.Throw(AExceptionClass: ExceptClass): ShouldAction;
var
  ThrewException: Boolean;
  ActualClassName: string;
  ActualMessage: string;
  Ex: Exception;
begin
  ThrewException := False;
  ActualClassName := '';
  ActualMessage := '';
  FExceptionClass := nil;
  FExceptionMessage := '';

  try
    FAction();
  except
    on E: Exception do
    begin
      Ex := E;
      ThrewException := True;
      ActualClassName := Ex.ClassName;
      ActualMessage := Ex.Message;
      FExceptionClass := ExceptClass(Ex.ClassType);
      FExceptionMessage := Ex.Message;

      if (AExceptionClass <> nil) and (not Ex.InheritsFrom(AExceptionClass)) then
        Fail(Format('Expected exception %s but %s was thrown: %s',
          [AExceptionClass.ClassName, ActualClassName, ActualMessage]));
    end;
  end;

  if not ThrewException then
    Fail(Format('Expected exception %s but none was thrown', [AExceptionClass.ClassName]));

  Result := Self;
end;

function ShouldAction.ThrowWithMessage(const ExpectedMessage: string): ShouldAction;
begin
  if FExceptionMessage <> ExpectedMessage then
    Fail(Format('Expected exception message "%s" but was "%s"',
      [ExpectedMessage, FExceptionMessage]));
  Result := Self;
end;

function ShouldAction.ThrowWithMessageContaining(const Substring: string): ShouldAction;
begin
  if not FExceptionMessage.Contains(Substring) then
    Fail(Format('Expected exception message to contain "%s" but was "%s"',
      [Substring, FExceptionMessage]));
  Result := Self;
end;

function ShouldAction.NotThrow: ShouldAction;
begin
  try
    FAction();
  except
    on E: Exception do
      Fail(Format('Expected no exception but got %s: %s', [E.ClassName, E.Message]));
  end;
  Result := Self;
end;

function ShouldAction.Because(const Reason: string): ShouldAction;
begin
  Result := Self;
  Result.FReason := Reason;
end;

{ ShouldDouble }

constructor ShouldDouble.Create(Value: Double);
begin
  FValue := Value;
  FReason := '';
  FPrecision := 0.0001; // Default precision
end;

procedure ShouldDouble.Fail(const Message: string);
begin
  Assert.RegisterFailure(Message, FReason);
end;

function ShouldDouble.Be(Expected: Double): ShouldDouble;
begin
  if Abs(FValue - Expected) > FPrecision then
    Fail(Format('Expected %g but was %g', [Expected, FValue]));
  Result := Self;
end;

function ShouldDouble.BeApproximately(Expected, Tolerance: Double): ShouldDouble;
begin
  if Abs(FValue - Expected) > Tolerance then
    Fail(Format('Expected %g to be approximately %g (tolerance: %g)', [FValue, Expected, Tolerance]));
  Result := Self;
end;

function ShouldDouble.BeGreaterThan(Expected: Double): ShouldDouble;
begin
  if FValue <= Expected then
    Fail(Format('Expected %g to be greater than %g', [FValue, Expected]));
  Result := Self;
end;

function ShouldDouble.BeLessThan(Expected: Double): ShouldDouble;
begin
  if FValue >= Expected then
    Fail(Format('Expected %g to be less than %g', [FValue, Expected]));
  Result := Self;
end;

function ShouldDouble.BeInRange(Min, Max: Double): ShouldDouble;
begin
  if (FValue < Min) or (FValue > Max) then
    Fail(Format('Expected %g to be in range [%g, %g]', [FValue, Min, Max]));
  Result := Self;
end;

function ShouldDouble.BePositive: ShouldDouble;
begin
  if FValue <= 0 then
    Fail(Format('Expected positive value but was %g', [FValue]));
  Result := Self;
end;

function ShouldDouble.BeNegative: ShouldDouble;
begin
  if FValue >= 0 then
    Fail(Format('Expected negative value but was %g', [FValue]));
  Result := Self;
end;

function ShouldDouble.Because(const Reason: string): ShouldDouble;
begin
  Result := Self;
  Result.FReason := Reason;
end;

function ShouldDouble.NotBe(Unexpected: Double): ShouldDouble;
begin
  if Abs(FValue - Unexpected) <= FPrecision then
    Fail(Format('Did not expect %g', [FValue]));
  Result := Self;
end;

function ShouldDouble.BeZero: ShouldDouble;
begin
  if Abs(FValue) > FPrecision then
    Fail(Format('Expected zero but was %g', [FValue]));
  Result := Self;
end;

function ShouldDouble.Satisfy(const Predicate: TPredicate<Double>): ShouldDouble;
begin
  if not Predicate(FValue) then
    Fail(Format('Value %g did not satisfy the predicate', [FValue]));
  Result := Self;
end;

function ShouldDouble.&And: ShouldDouble;
begin
  Result := Self;
end;

{ ShouldObject }

constructor ShouldObject.Create(Value: TObject);
begin
  FValue := Value;
  FReason := '';
end;

procedure ShouldObject.Fail(const Message: string);
begin
  Assert.RegisterFailure(Message, FReason);
end;

function ShouldObject.BeNil: ShouldObject;
begin
  if FValue <> nil then
    Fail(Format('Expected nil but was %s', [FValue.ClassName]));
  Result := Self;
end;

function ShouldObject.NotBeNil: ShouldObject;
begin
  if FValue = nil then
    Fail('Expected a value but was nil');
  Result := Self;
end;

function ShouldObject.BeOfType<T>: ShouldObject;
begin
  if FValue = nil then
    Fail(Format('Expected %s but was nil', [T.ClassName]))
  else if FValue.ClassType <> T then
    Fail(Format('Expected %s but was %s', [T.ClassName, FValue.ClassName]));
  Result := Self;
end;

function ShouldObject.BeAssignableTo<T>: ShouldObject;
begin
  if FValue = nil then
    Fail(Format('Expected assignable to %s but was nil', [T.ClassName]))
  else if not (FValue is T) then
    Fail(Format('Expected assignable to %s but was %s', [T.ClassName, FValue.ClassName]));
  Result := Self;
end;

function ShouldObject.BeEquivalentTo(Expected: TObject): ShouldObject;
var
  JsonA, JsonB: string;
begin
  if FValue = Expected then Exit(Self);
  
  if (FValue = nil) and (Expected <> nil) then
    Fail('Expected object to be equivalent but found nil')
  else if (FValue <> nil) and (Expected = nil) then
    Fail('Expected object to be nil but found instance');
    
  // Serialize both to JSON for deep comparison
  JsonA := TDextJson.Serialize(FValue);
  JsonB := TDextJson.Serialize(Expected);
  
  if JsonA <> JsonB then
    Fail('Expected objects to be equivalent but found differences.' + sLineBreak +
         'Expected: ' + JsonB + sLineBreak +
         'Found:    ' + JsonA);
         
  Result := Self;
end;

function ShouldObject.MatchSnapshot(const SnapshotName: string): ShouldObject;
begin
  if FValue = nil then
    VerifySnapshot('null', SnapshotName)
  else
    VerifySnapshot(TDextJson.Serialize(FValue), SnapshotName);
  Result := Self;
end;

function ShouldObject.Because(const Reason: string): ShouldObject;
begin
  FReason := Reason;
  Result := Self;
end;

function ShouldObject.BeNull: ShouldObject;
begin
  Result := BeNil; // Alias
end;

function ShouldObject.NotBeNull: ShouldObject;
begin
  Result := NotBeNil; // Alias
end;

function ShouldObject.Satisfy(const Predicate: TPredicate<TObject>): ShouldObject;
begin
  if not Predicate(FValue) then
    Fail('Object did not satisfy the predicate');
  Result := Self;
end;

function ShouldObject.&And: ShouldObject;
begin
  Result := Self;
end;

{ ShouldObjectHelper }

function ShouldObjectHelper.HaveProperty(const PropertyName: string): ShouldProperty;
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Val: TValue;
begin
  if Self.FValue = nil then
    Self.Fail(Format('Cannot check property "%s" on nil object', [PropertyName]));
    
  Ctx := TRttiContext.Create;
  try
    RttiType := Ctx.GetType(Self.FValue.ClassType);
    Prop := RttiType.GetProperty(PropertyName);
    if Prop = nil then
      Self.Fail(Format('Object of type %s does not have property "%s"', [Self.FValue.ClassName, PropertyName]));

    Val := Prop.GetValue(Self.FValue);
    Result := ShouldProperty.Create(Self, PropertyName, Val);
  finally
    Ctx.Free;
  end;
end;

(*
function ShouldObjectHelper.HaveProperty<T>(const PropertyMetadata: Prop<T>): ShouldProperty;
begin
  if not PropertyMetadata.IsQueryMode then
    Self.Fail('HaveProperty<T> requires a Smart Property prototype (e.g. u.Name), not a value.');
    
  Result := HaveProperty(PropertyMetadata.Name);
end;
*)

function ShouldObjectHelper.HavePropertyValue(const PropertyName: string; const ExpectedValue: TValue): ShouldObject;
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Field: TRttiField;
  ActualValue: TValue;
  ActualStr, ExpectedStr: string;
begin
  if Self.FValue = nil then
    Self.Fail(Format('Cannot check property "%s" on nil object', [PropertyName]));
    
  Ctx := TRttiContext.Create;
  try
    RttiType := Ctx.GetType(Self.FValue.ClassType);
    
    // Try property first
    Prop := RttiType.GetProperty(PropertyName);
    if Prop <> nil then
    begin
      ActualValue := Prop.GetValue(Self.FValue);
    end
    else
    begin
      // Try field with F prefix (for SmartTypes like FName, FAge)
      Field := RttiType.GetField('F' + PropertyName);
      if Field = nil then
        Field := RttiType.GetField(PropertyName); // Try without prefix
        
      if Field = nil then
        Self.Fail(Format('Object of type %s does not have property or field "%s"', 
          [Self.FValue.ClassName, PropertyName]));
          
      ActualValue := Field.GetValue(Self.FValue);
      
      // If it's a Prop<T> Smart Type, extract the inner FValue
      if (ActualValue.Kind = tkRecord) and string(ActualValue.TypeInfo.Name).StartsWith('Prop<') then
      begin
        var RecType := Ctx.GetType(ActualValue.TypeInfo).AsRecord;
        var ValueField := RecType.GetField('FValue');
        if ValueField <> nil then
          ActualValue := ValueField.GetValue(ActualValue.GetReferenceToRawData);
      end;
    end;
    
    // Compare values using extracted strings
    ActualStr := ActualValue.ToString;
    ExpectedStr := ExpectedValue.ToString;
    
    if ActualStr <> ExpectedStr then
      Self.Fail(Format('Property "%s" expected value "%s" but was "%s"', 
        [PropertyName, ExpectedStr, ActualStr]));
  finally
    Ctx.Free;
  end;
  Result := Self;
end;

function ShouldObjectHelper.HaveValue(const PropertyMetadata: StringType; const ExpectedValue: string): ShouldObject;
begin
  if not PropertyMetadata.IsQueryMode then
    Self.Fail('HaveValue requires a Smart Property prototype (use Prototype.Entity<T>).');

  Result := HavePropertyValue(PropertyMetadata.Name, TValue.From<string>(ExpectedValue));
end;

function ShouldObjectHelper.HaveValue(const PropertyMetadata: IntType; ExpectedValue: Integer): ShouldObject;
begin
  if not PropertyMetadata.IsQueryMode then
    Self.Fail('HaveValue requires a Smart Property prototype (use Prototype.Entity<T>).');

  Result := HavePropertyValue(PropertyMetadata.Name, TValue.From<Integer>(ExpectedValue));
end;

function ShouldObjectHelper.HaveValue<T>(const PropertyMetadata: T;
  ExpectedValue: T): ShouldObject;
var
  Ctx: TRttiContext;
  RecType: TRttiRecordType;
  InfoField: TRttiField;
  MetaValue, InfoValue: TValue;
  PropInfo: IPropInfo;
  PropName: string;
begin
  // Use RTTI to extract property name from any Prop<T> SmartType
  Ctx := TRttiContext.Create;
  try
    MetaValue := TValue.From<T>(PropertyMetadata);
    
    // Verify it's a Prop<T> record
    if not string(MetaValue.TypeInfo.Name).StartsWith('Prop<') then
      Self.Fail('HaveValue<T> requires a Smart Property type (Prop<T>).');
    
    RecType := Ctx.GetType(MetaValue.TypeInfo).AsRecord;
    InfoField := RecType.GetField('FInfo');
    
    if InfoField = nil then
      Self.Fail('HaveValue<T>: Could not find FInfo field in SmartType.');
    
    InfoValue := InfoField.GetValue(MetaValue.GetReferenceToRawData);
    
    if InfoValue.IsEmpty or (InfoValue.AsInterface = nil) then
      Self.Fail('HaveValue<T> requires a Smart Property prototype (use Prototype.Entity<T>).');
    
    PropInfo := InfoValue.AsType<IPropInfo>;
    PropName := PropInfo.PropertyName;
    
    Result := HavePropertyValue(PropName, TValue.From<T>(ExpectedValue));
  finally
    Ctx.Free;
  end;
end;

{ ShouldProperty }

constructor ShouldProperty.Create(const Parent: ShouldObject; const Name: string; const Value: TValue);
begin
  FParent := Parent;
  FName := Name;
  FValue := Value;
  FReason := '';
end;

procedure ShouldProperty.Fail(const Message: string);
begin
  Assert.RegisterFailure(Message, FReason);
end;

function ShouldProperty.Value: TValue;
begin
  Result := FValue;
end;

function ShouldProperty.WhichString: ShouldString;
begin
  Result := ShouldString.Create(FValue.ToString);
end;

function ShouldProperty.WhichInteger: ShouldInteger;
begin
  try
    if FValue.TypeInfo = TypeInfo(Integer) then
      Result := ShouldInteger.Create(FValue.AsInteger)
    else
      Result := ShouldInteger.Create(StrToIntDef(FValue.ToString, 0)); // Simple conversion
  except
    Fail(Format('Property "%s" is not a valid Integer', [FName]));
  end;
end;

function ShouldProperty.WhichBoolean: ShouldBoolean;
begin
  try
    if FValue.TypeInfo = TypeInfo(Boolean) then
      Result := ShouldBoolean.Create(FValue.AsBoolean)
    else
      Result := ShouldBoolean.Create(StrToBoolDef(FValue.ToString, False));
  except
    Fail(Format('Property "%s" is not a valid Boolean', [FName]));
  end;
end;

function ShouldProperty.WhichObject: ShouldObject;
begin
  if FValue.IsEmpty then
    Exit(ShouldObject.Create(nil));
    
  if FValue.Kind = tkClass then
    Result := ShouldObject.Create(FValue.AsObject)
  else
    Fail(Format('Property "%s" is not an object', [FName]));
end;

function ShouldProperty.WhichGuid: ShouldGuid;
var
  G: TGUID;
begin
  if FValue.TypeInfo = TypeInfo(TGUID) then
  begin
    FValue.ExtractRawData(@G);
    Result := ShouldGuid.Create(G);
  end
  else
  begin
    try
      Result := ShouldGuid.Create(StringToGUID(FValue.ToString));
    except
      Fail('Property is not a GUID');
    end;
  end;
end;

function ShouldProperty.&And: ShouldObject;
begin
  Result := FParent;
end;

function ShouldProperty.AndAlso: ShouldObject;
begin
  Result := FParent;
end;

{ ShouldInterface }

constructor ShouldInterface.Create(const Value: IInterface);
begin
  FValue := Value;
  FReason := '';
end;

procedure ShouldInterface.Fail(const Message: string);
begin
  Assert.RegisterFailure(Message, FReason);
end;

function ShouldInterface.BeNil: ShouldInterface;
begin
  if FValue <> nil then
    Fail('Expected nil interface but was not nil');
  Result := Self;
end;

function ShouldInterface.NotBeNil: ShouldInterface;
begin
  if FValue = nil then
    Fail('Expected non-nil interface but was nil');
  Result := Self;
end;

function ShouldInterface.BeEquivalentTo(const Expected: IInterface): ShouldInterface;
var
  JsonA, JsonB: string;
begin
  if FValue = Expected then Exit(Self);
  
  if (FValue = nil) and (Expected <> nil) then
    Fail('Expected interface to be equivalent but found nil')
  else if (FValue <> nil) and (Expected = nil) then
    Fail('Expected interface to be nil but found instance');
    
  JsonA := TDextJson.Serialize(TValue.From(FValue));
  JsonB := TDextJson.Serialize(TValue.From(Expected));
  
  if JsonA <> JsonB then
    Fail('Expected interfaces to be equivalent but found differences.' + sLineBreak +
         'Expected: ' + JsonB + sLineBreak +
         'Found:    ' + JsonA);
         
  Result := Self;
end;

function ShouldInterface.MatchSnapshot(const SnapshotName: string): ShouldInterface;
begin
  if FValue = nil then
    VerifySnapshot('null', SnapshotName)
  else
    VerifySnapshot(TDextJson.Serialize(TValue.From(FValue)), SnapshotName);
  Result := Self;
end;

function ShouldInterface.Because(const Reason: string): ShouldInterface;
begin
  FReason := Reason;
  Result := Self;
end;

{ ShouldList<T> }

constructor ShouldList<T>.Create(const Value: IEnumerable<T>);
begin
  FEnumerable := Value;
  FDextEnumerable := nil;
  FIsArray := False;
  FIsDext := False;
  FArray := nil;
  FReason := '';
end;

constructor ShouldList<T>.Create(const Value: TArray<T>);
begin
  FEnumerable := nil;
  FDextEnumerable := nil;
  FIsArray := True;
  FIsDext := False;
  FArray := Value;
  FReason := '';
end;

procedure ShouldList<T>.Fail(const Message: string);
begin
  Assert.RegisterFailure(Message, FReason);
end;

function ShouldList<T>.GetCount: Integer;
var
  Item: T;
  C: Integer;
begin
  if FIsArray then
    Result := Length(FArray)
  else if FIsDext and (FDextEnumerable <> nil) then
  begin
    C := 0;
    for Item in FDextEnumerable do
      Inc(C);
    Result := C;
  end
  else if FEnumerable <> nil then
  begin
    C := 0;
    for Item in FEnumerable do
      Inc(C);
    Result := C;
  end
  else
    Result := 0;
end;

function ShouldList<T>.BeEmpty: ShouldList<T>;
var
  C: Integer;
begin
  C := GetCount;
  if C > 0 then
    Fail(Format('Expected empty list but found %d items', [C]));
  Result := Self;
end;

function ShouldList<T>.NotBeEmpty: ShouldList<T>;
begin
  if GetCount = 0 then
    Fail('Expected non-empty list but was empty');
  Result := Self;
end;

function ShouldList<T>.HaveCount(Expected: Integer): ShouldList<T>;
var
  C: Integer;
begin
  C := GetCount;
  if C <> Expected then
    Fail(Format('Expected count %d but was %d', [Expected, C]));
  Result := Self;
end;

function ShouldList<T>.HaveCountGreaterThan(Expected: Integer): ShouldList<T>;
var
  C: Integer;
begin
  C := GetCount;
  if C <= Expected then
    Fail(Format('Expected count greater than %d but was %d', [Expected, C]));
  Result := Self;
end;

function ShouldList<T>.Contain(const Item: T): ShouldList<T>;
var
  Found: Boolean;
  EnumItem: T;
  Comparer: System.Generics.Defaults.IEqualityComparer<T>;
begin
  Found := False;
  Comparer := System.Generics.Defaults.TEqualityComparer<T>.Default;
  
  if FIsArray then
  begin
    for EnumItem in FArray do
      if Comparer.Equals(EnumItem, Item) then
      begin
        Found := True;
        Break;
      end;
  end
  else if FEnumerable <> nil then
  begin
    for EnumItem in FEnumerable do
      if Comparer.Equals(EnumItem, Item) then
      begin
        Found := True;
        Break;
      end;
  end;
  
  if not Found then
    Fail('Expected list to contain item but it did not');
  Result := Self;
end;

function ShouldList<T>.NotContain(const Item: T): ShouldList<T>;
var
  Found: Boolean;
  EnumItem: T;
  Comparer: System.Generics.Defaults.IEqualityComparer<T>;
begin
  Found := False;
  Comparer := System.Generics.Defaults.TEqualityComparer<T>.Default;
  
  if FIsArray then
  begin
    for EnumItem in FArray do
      if Comparer.Equals(EnumItem, Item) then
      begin
        Found := True;
        Break;
      end;
  end
  else if FEnumerable <> nil then
  begin
    for EnumItem in FEnumerable do
      if Comparer.Equals(EnumItem, Item) then
      begin
        Found := True;
        Break;
      end;
  end;
  
  if Found then
    Fail('Expected list to NOT contain item but it did');
  Result := Self;
end;

function ShouldList<T>.AllSatisfy(const Predicate: TPredicate<T>): ShouldList<T>;
var
  EnumItem: T;
  AllMatch: Boolean;
begin
  AllMatch := True;
  
  if FIsArray then
  begin
    for EnumItem in FArray do
      if not Predicate(EnumItem) then
      begin
        AllMatch := False;
        Break;
      end;
  end
  else if FEnumerable <> nil then
  begin
    var Enum := FEnumerable;
    for EnumItem in Enum do
      if not Predicate(EnumItem) then
      begin
        AllMatch := False;
        Break;
      end;
  end;
  
  if not AllMatch then
    Fail('Expected all items to satisfy predicate but some did not');
  Result := Self;
end;

function ShouldList<T>.AnySatisfy(const Predicate: TPredicate<T>): ShouldList<T>;
var
  EnumItem: T;
  AnyMatch: Boolean;
begin
  AnyMatch := False;
  
  if FIsArray then
  begin
    for EnumItem in FArray do
      if Predicate(EnumItem) then
      begin
        AnyMatch := True;
        Break;
      end;
  end
  else if FEnumerable <> nil then
  begin
    var Enum := FEnumerable;
    for EnumItem in Enum do
      if Predicate(EnumItem) then
      begin
        AnyMatch := True;
        Break;
      end;
  end;
  
  if not AnyMatch then
    Fail('Expected at least one item to satisfy predicate but none did');
  Result := Self;
end;

function ShouldList<T>.Because(const Reason: string): ShouldList<T>;
begin
  Result := Self;
  Result.FReason := Reason;
end;

function ShouldList<T>.HaveCountLessThan(Expected: Integer): ShouldList<T>;
var
  C: Integer;
begin
  C := GetCount;
  if C >= Expected then
    Fail(Format('Expected count less than %d but was %d', [Expected, C]));
  Result := Self;
end;

function ShouldList<T>.ContainInOrder(const Items: TArray<T>): ShouldList<T>;
var
  Comparer: System.Generics.Defaults.IEqualityComparer<T>;
  Arr: TArray<T>;
  I, J: Integer;
begin
  Comparer := System.Generics.Defaults.TEqualityComparer<T>.Default;
  
  // Get items as array for indexing
  if FIsArray then
    Arr := FArray
  else if FEnumerable <> nil then
  begin
    SetLength(Arr, 0);
    for var Item in FEnumerable do
    begin
      SetLength(Arr, Length(Arr) + 1);
      Arr[High(Arr)] := Item;
    end;
  end
  else
    Arr := [];
    
  J := 0;
  for I := 0 to High(Arr) do
  begin
    if (J <= High(Items)) and Comparer.Equals(Arr[I], Items[J]) then
      Inc(J);
  end;
  
  if J <> Length(Items) then
    Fail('Expected list to contain items in specified order');
  Result := Self;
end;

function ShouldList<T>.BeEquivalentTo(const Expected: TArray<T>): ShouldList<T>;
var
  Comparer: System.Generics.Defaults.IEqualityComparer<T>;
  Arr: TArray<T>;
  ExpItem, SrcItem: T;
  Found: Boolean;
begin
  Comparer := System.Generics.Defaults.TEqualityComparer<T>.Default;
  
  // Get items as array
  if FIsArray then
    Arr := FArray
  else if FEnumerable <> nil then
  begin
    SetLength(Arr, 0);
    for var Item in FEnumerable do
    begin
      SetLength(Arr, Length(Arr) + 1);
      Arr[High(Arr)] := Item;
    end;
  end
  else
    Arr := [];
    
  if Length(Arr) <> Length(Expected) then
    Fail(Format('Expected %d items but found %d', [Length(Expected), Length(Arr)]));
    
  // Check all expected items are in actual (order-independent)
  for ExpItem in Expected do
  begin
    Found := False;
    for SrcItem in Arr do
      if Comparer.Equals(SrcItem, ExpItem) then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      Fail('Expected list to be equivalent but found differences');
  end;
  
  Result := Self;
end;

function ShouldList<T>.OnlyContain(const Predicate: TPredicate<T>): ShouldList<T>;
begin
  Result := AllSatisfy(Predicate); // Alias with different semantic meaning
end;

function ShouldList<T>.&And: ShouldList<T>;
begin
  Result := Self;
end;

function ShouldList<T>.AndAlso: ShouldList<T>;
begin
  Result := Self;
end;

{ ShouldDateTime }

constructor ShouldDateTime.Create(Value: TDateTime);
begin
  FValue := Value;
  FReason := '';
end;

procedure ShouldDateTime.Fail(const Message: string);
begin
  Assert.RegisterFailure(Message, FReason);
end;

function ShouldDateTime.Be(Expected: TDateTime): ShouldDateTime;
begin
  if not SameDateTime(FValue, Expected) then
    Fail(Format('Expected %s but found %s', [DateTimeToStr(Expected), DateTimeToStr(FValue)]));
  Result := Self;
end;

function ShouldDateTime.BeCloseTo(Expected: TDateTime; PrecisionMS: Int64): ShouldDateTime;
var
  Diff: Int64;
begin
  Diff := MilliSecondsBetween(FValue, Expected);
  if Diff > PrecisionMS then
    Fail(Format('Expected date to be close to %s (within %dms) but found %s (diff %dms)',
      [DateTimeToStr(Expected), PrecisionMS, DateTimeToStr(FValue), Diff]));
  Result := Self;
end;

function ShouldDateTime.BeAfter(Expected: TDateTime): ShouldDateTime;
begin
  if not (FValue > Expected) then
    Fail(Format('Expected date to be after %s but found %s', [DateTimeToStr(Expected), DateTimeToStr(FValue)]));
  Result := Self;
end;

function ShouldDateTime.BeBefore(Expected: TDateTime): ShouldDateTime;
begin
  if not (FValue < Expected) then
    Fail(Format('Expected date to be before %s but found %s', [DateTimeToStr(Expected), DateTimeToStr(FValue)]));
  Result := Self;
end;

function ShouldDateTime.BeSameDateAs(Expected: TDateTime): ShouldDateTime;
begin
  if not SameDate(FValue, Expected) then
    Fail(Format('Expected date to be %s but found %s (ignoring time)',
      [DateToStr(Expected), DateToStr(FValue)]));
  Result := Self;
end;

function ShouldDateTime.Because(const Reason: string): ShouldDateTime;
begin
  FReason := Reason;
  Result := Self;
end;


{ Global Helper Functions }


function Should(const Value: string): ShouldString;
begin
  Result := ShouldString.Create(Value);
end;

function Should(Value: Integer): ShouldInteger;
begin
  Result := ShouldInteger.Create(Value);
end;

function Should(Value: Int64): ShouldInt64;
begin
  Result := ShouldInt64.Create(Value);
end;

function Should(Value: Boolean): ShouldBoolean;
begin
  Result := ShouldBoolean.Create(Value);
end;

function Should(Value: Double): ShouldDouble;
begin
  Result := ShouldDouble.Create(Value);
end;

function Should(const Action: TProc): ShouldAction;
begin
  Result := ShouldAction.Create(Action);
end;

function Should(const Value: TObject): ShouldObject;
begin
  Result := ShouldObject.Create(Value);
end;

function Should(const Value: IInterface): ShouldInterface;
begin
  Result := ShouldInterface.Create(Value);
end;

function Should(const Value: TGUID): ShouldGuid;
begin
  Result := ShouldGuid.Create(Value);
end;

function Should(const Value: TUUID): ShouldUUID;
begin
  Result := ShouldUUID.Create(Value);
end;

function Should(const Value: Variant): ShouldVariant;
begin
  Result := ShouldVariant.Create(Value);
end;

function ShouldDate(Value: TDateTime): ShouldDateTime;
begin
  Result := ShouldDateTime.Create(Value);
end;

{ Smart Types / Prop<T> Implementations }

function Should(const Value: StringType): ShouldString;
begin
  Result := ShouldString.Create(Value.Value);
end;

function Should(const Value: IntType): ShouldInteger;
begin
  Result := ShouldInteger.Create(Value.Value);
end;

function Should(const Value: Int64Type): ShouldInt64;
begin
  Result := ShouldInt64.Create(Value.Value);
end;

function Should(const Value: BoolType): ShouldBoolean;
begin
  Result := ShouldBoolean.Create(Value.Value);
end;

function Should(const Value: FloatType): ShouldDouble;
begin
  Result := ShouldDouble.Create(Value.Value);
end;

function Should(const Value: CurrencyType): ShouldDouble;
begin
  Result := ShouldDouble.Create(Value.Value);
end;

{ Nullable<T> Implementations }

function Should(const Value: Nullable<string>): ShouldString;
begin
  if not Value.HasValue then
    Assert.Fail('Expected Nullable<string> to have a value, but it was Null');
  Result := ShouldString.Create(Value.Value);
end;

function Should(const Value: Nullable<Integer>): ShouldInteger;
begin
  if not Value.HasValue then
    Assert.Fail('Expected Nullable<Integer> to have a value, but it was Null');
  Result := ShouldInteger.Create(Value.Value);
end;

function Should(const Value: Nullable<Int64>): ShouldInt64;
begin
  if not Value.HasValue then
    Assert.Fail('Expected Nullable<Int64> to have a value, but it was Null');
  Result := ShouldInt64.Create(Value.Value);
end;

function Should(const Value: Nullable<Boolean>): ShouldBoolean;
begin
  if not Value.HasValue then
    Assert.Fail('Expected Nullable<Boolean> to have a value, but it was Null');
  Result := ShouldBoolean.Create(Value.Value);
end;

function Should(const Value: Nullable<Double>): ShouldDouble;
begin
  if not Value.HasValue then
    Assert.Fail('Expected Nullable<Double> to have a value, but it was Null');
  Result := ShouldDouble.Create(Value.Value);
end;

function Should(const Value: Nullable<Currency>): ShouldDouble;
begin
  if not Value.HasValue then
    Assert.Fail('Expected Nullable<Currency> to have a value, but it was Null');
  Result := ShouldDouble.Create(Value.Value);
end;

function Should(const Value: Nullable<TDateTime>): ShouldDateTime;
begin
  if not Value.HasValue then
    Assert.Fail('Expected Nullable<TDateTime> to have a value, but it was Null');
  Result := ShouldDateTime.Create(Value.Value);
end;

function ShouldDate(const Value: Nullable<TDateTime>): ShouldDateTime;
begin
  Result := Should(Value);
end;

function ShouldDate(const Value: Prop<TDateTime>): ShouldDateTime;
begin
  Result := ShouldDateTime.Create(Value.Value);
end;

{ ShouldInt64 }

constructor ShouldInt64.Create(Value: Int64);
begin
  FValue := Value;
  FReason := '';
end;

procedure ShouldInt64.Fail(const Message: string);
begin
  Assert.RegisterFailure(Message, FReason);
end;

function ShouldInt64.Be(Expected: Int64): ShouldInt64;
begin
  if FValue <> Expected then
    Fail(Format('Expected %d but was %d', [Expected, FValue]));
  Result := Self;
end;

function ShouldInt64.NotBe(Unexpected: Int64): ShouldInt64;
begin
  if FValue = Unexpected then
    Fail(Format('Did not expect %d', [FValue]));
  Result := Self;
end;

function ShouldInt64.BeGreaterThan(Expected: Int64): ShouldInt64;
begin
  if FValue <= Expected then
    Fail(Format('Expected %d to be greater than %d', [FValue, Expected]));
  Result := Self;
end;

function ShouldInt64.BeLessThan(Expected: Int64): ShouldInt64;
begin
  if FValue >= Expected then
    Fail(Format('Expected %d to be less than %d', [FValue, Expected]));
  Result := Self;
end;

function ShouldInt64.BeInRange(Min, Max: Int64): ShouldInt64;
begin
  if (FValue < Min) or (FValue > Max) then
    Fail(Format('Expected %d to be in range [%d, %d]', [FValue, Min, Max]));
  Result := Self;
end;

function ShouldInt64.BePositive: ShouldInt64;
begin
  if FValue <= 0 then
    Fail(Format('Expected positive value but was %d', [FValue]));
  Result := Self;
end;

function ShouldInt64.BeNegative: ShouldInt64;
begin
  if FValue >= 0 then
    Fail(Format('Expected negative value but was %d', [FValue]));
  Result := Self;
end;

function ShouldInt64.BeZero: ShouldInt64;
begin
  if FValue <> 0 then
    Fail(Format('Expected zero but was %d', [FValue]));
  Result := Self;
end;

function ShouldInt64.BeOneOf(const Values: TArray<Int64>): ShouldInt64;
var
  V: Int64;
  Found: Boolean;
begin
  Found := False;
  for V in Values do
    if FValue = V then
    begin
      Found := True;
      Break;
    end;
  if not Found then
    Fail(Format('Expected %d to be one of the specified values', [FValue]));
  Result := Self;
end;

function ShouldInt64.Satisfy(const Predicate: TPredicate<Int64>): ShouldInt64;
begin
  if not Predicate(FValue) then
    Fail(Format('Value %d did not satisfy the predicate', [FValue]));
  Result := Self;
end;

function ShouldInt64.Because(const Reason: string): ShouldInt64;
begin
  Result := Self;
  Result.FReason := Reason;
end;

function ShouldInt64.&And: ShouldInt64;
begin
  Result := Self;
end;

{ ShouldGuid }

constructor ShouldGuid.Create(const Value: TGUID);
begin
  FValue := Value;
  FReason := '';
end;

procedure ShouldGuid.Fail(const Message: string);
begin
  Assert.RegisterFailure(Message, FReason);
end;

function ShouldGuid.Be(const Expected: TGUID): ShouldGuid;
begin
  if not IsEqualGUID(FValue, Expected) then
    Fail(Format('Expected %s but was %s', [GUIDToString(Expected), GUIDToString(FValue)]));
  Result := Self;
end;

function ShouldGuid.NotBe(const Unexpected: TGUID): ShouldGuid;
begin
  if IsEqualGUID(FValue, Unexpected) then
    Fail(Format('Did not expect %s', [GUIDToString(FValue)]));
  Result := Self;
end;

function ShouldGuid.BeEmpty: ShouldGuid;
begin
  if not IsEqualGUID(FValue, TGUID.Empty) then
    Fail(Format('Expected empty GUID but was %s', [GUIDToString(FValue)]));
  Result := Self;
end;

function ShouldGuid.NotBeEmpty: ShouldGuid;
begin
  if IsEqualGUID(FValue, TGUID.Empty) then
    Fail('Expected non-empty GUID but was empty');
  Result := Self;
end;

function ShouldGuid.Because(const Reason: string): ShouldGuid;
begin
  Result := Self;
  Result.FReason := Reason;
end;

function ShouldGuid.&And: ShouldGuid;
begin
  Result := Self;
end;

{ ShouldUUID }

constructor ShouldUUID.Create(const Value: TUUID);
begin
  FValue := Value;
  FReason := '';
end;

procedure ShouldUUID.Fail(const Message: string);
begin
  Assert.RegisterFailure(Message, FReason);
end;

function ShouldUUID.Be(const Expected: TUUID): ShouldUUID;
begin
  if FValue <> Expected then
    Fail(Format('Expected %s but was %s', [Expected.ToString, FValue.ToString]));
  Result := Self;
end;

function ShouldUUID.NotBe(const Unexpected: TUUID): ShouldUUID;
begin
  if FValue = Unexpected then
    Fail(Format('Did not expect %s', [FValue.ToString]));
  Result := Self;
end;

function ShouldUUID.BeEmpty: ShouldUUID;
begin
  if not FValue.IsEmpty then
    Fail(Format('Expected empty UUID but was %s', [FValue.ToString]));
  Result := Self;
end;

function ShouldUUID.NotBeEmpty: ShouldUUID;
begin
  if FValue.IsEmpty then
    Fail('Expected non-empty UUID but was empty');
  Result := Self;
end;

function ShouldUUID.Because(const Reason: string): ShouldUUID;
begin
  Result := Self;
  Result.FReason := Reason;
end;

function ShouldUUID.&And: ShouldUUID;
begin
  Result := Self;
end;

{ ShouldVariant }

constructor ShouldVariant.Create(const Value: Variant);
begin
  FValue := Value;
  FReason := '';
end;

procedure ShouldVariant.Fail(const Message: string);
begin
  Assert.RegisterFailure(Message, FReason);
end;

function ShouldVariant.Be(const Expected: Variant): ShouldVariant;
begin
  if FValue <> Expected then
    Fail(Format('Expected %s but was %s', [VarToStr(Expected), VarToStr(FValue)]));
  Result := Self;
end;

function ShouldVariant.NotBe(const Unexpected: Variant): ShouldVariant;
begin
  if FValue = Unexpected then
    Fail(Format('Did not expect %s', [VarToStr(FValue)]));
  Result := Self;
end;

function ShouldVariant.BeNull: ShouldVariant;
begin
  if not VarIsNull(FValue) then
    Fail(Format('Expected Null but was %s', [VarToStr(FValue)]));
  Result := Self;
end;

function ShouldVariant.NotBeNull: ShouldVariant;
begin
  if VarIsNull(FValue) then
    Fail('Expected non-Null variant but was Null');
  Result := Self;
end;

function ShouldVariant.BeEmpty: ShouldVariant;
begin
  if not VarIsEmpty(FValue) then
    Fail(Format('Expected Empty but was %s', [VarToStr(FValue)]));
  Result := Self;
end;

function ShouldVariant.NotBeEmpty: ShouldVariant;
begin
  if VarIsEmpty(FValue) then
    Fail('Expected non-Empty variant but was Empty');
  Result := Self;
end;

function ShouldVariant.BeOfType(VarType: TVarType): ShouldVariant;
var
  ActualType: TVarType;
begin
  ActualType := System.Variants.VarType(FValue);
  if ActualType <> VarType then
    Fail(Format('Expected variant type %d but was %d', [VarType, ActualType]));
  Result := Self;
end;

function ShouldVariant.Because(const Reason: string): ShouldVariant;
begin
  Result := Self;
  Result.FReason := Reason;
end;

function ShouldVariant.&And: ShouldVariant;
begin
  Result := Self;
end;

function ShouldVariant.AndAlso: ShouldVariant;
begin
  Result := Self;
end;

// Implementations for AndAlso for other types (appended here to keep unit clean)



function ShouldString.AndAlso: ShouldString;
begin
  Result := Self;
end;



function ShouldInteger.AndAlso: ShouldInteger;
begin
  Result := Self;
end;



function ShouldInt64.AndAlso: ShouldInt64;
begin
  Result := Self;
end;



function ShouldDouble.AndAlso: ShouldDouble;
begin
  Result := Self;
end;



function ShouldBoolean.AndAlso: ShouldBoolean;
begin
  Result := Self;
end;



function ShouldGuid.AndAlso: ShouldGuid;
begin
  Result := Self;
end;



function ShouldUUID.AndAlso: ShouldUUID;
begin
  Result := Self;
end;





function ShouldAction.&And: ShouldAction;
begin
  Result := Self;
end;

function ShouldAction.AndAlso: ShouldAction;
begin
  Result := Self;
end;



function ShouldObject.AndAlso: ShouldObject;
begin
  Result := Self;
end;


function ShouldInterface.&And: ShouldInterface;
begin
  Result := Self;
end;

function ShouldInterface.AndAlso: ShouldInterface;
begin
  Result := Self;
end;

{ ShouldHelper }

function ShouldHelper.List<T>(const Value: TArray<T>): ShouldList<T>;
begin
  Result := ShouldList<T>.Create(Value);
end;

function ShouldHelper.List<T>(const Value: IEnumerable<T>): ShouldList<T>;
begin
  Result := ShouldList<T>.Create(Value);
end;

function Should: ShouldHelper; overload;
begin
end;

end.
