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
{  Created: 2025-12-27                                                      }
{                                                                           }
{  Smart Properties - Type-safe query expressions without magic strings.   }
{                                                                           }
{  Usage:                                                                   }
{    // In entity definition                                                }
{    TUser = class                                                          }
{      FAge: IntType;                                                       }
{      FName: StringType;                                                   }
{    end;                                                                   }
{                                                                           }
{    // In query (magic happens!)                                           }
{    Users.Where(function(U: TUser): BooleanExpression                           }
{    begin                                                                  }
{      Result := (U.Age > 18) and (U.Name.StartsWith('Ce'));                }
{    end);                                                                  }
{                                                                           }
{***************************************************************************}
unit Dext.Core.SmartTypes;

interface

uses
  System.SysUtils,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Rtti,
  System.Variants,
  Dext.Specifications.Interfaces,
  Dext.Specifications.Types;

type
  /// <summary>
  ///   Interface that carries column metadata for a property.
  ///   When present, the Prop<T> operates in Query Mode (generates expressions).
  ///   When nil, operates in Runtime Mode (normal value comparison).
  /// </summary>
  IPropInfo = interface
    ['{6DCBC43A-0D70-40BA-ADEE-BC450A69F296}']
    function GetColumnName: string;
    property ColumnName: string read GetColumnName;
  end;

  /// <summary>
  ///   Hybrid record that can hold a runtime Boolean OR an IExpression node.
  ///   Enables seamless transition between query building and runtime evaluation.
  /// </summary>
  BooleanExpression = record
  private
    FRuntimeValue: Boolean;
    FExpression: IExpression;
    class function InternalAnd(const Left, Right: BooleanExpression): BooleanExpression; static;
    class function InternalOr(const Left, Right: BooleanExpression): BooleanExpression; static;
  public
    class function FromQuery(const AExpr: IExpression): BooleanExpression; static;
    class function FromRuntime(const AValue: Boolean): BooleanExpression; static;

    class operator Implicit(const Value: BooleanExpression): Boolean;
    class operator Implicit(const Value: BooleanExpression): TFluentExpression;

    class operator LogicalAnd(const Left, Right: BooleanExpression): BooleanExpression;
    class operator BitwiseAnd(const Left, Right: BooleanExpression): BooleanExpression;
    class operator BitwiseOr(const Left, Right: BooleanExpression): BooleanExpression;
    class operator LogicalOr(const Left, Right: BooleanExpression): BooleanExpression;
    class operator LogicalNot(const Value: BooleanExpression): BooleanExpression;

    property Expression: IExpression read FExpression;
    property RuntimeValue: Boolean read FRuntimeValue;
  end;

  /// <summary>
  ///   Generic property wrapper that enables operator overloading for queries.
  ///   When FInfo is assigned (via TPrototype), operators generate Expression Trees.
  ///   When FInfo is nil (normal usage), operators perform runtime comparisons.
  /// </summary>
  Prop<T> = record
  private
    FValue: T;
    FInfo: IPropInfo;  // MUST be in instance section, not class var!
    function GetColumnName: string;
    function IsQueryMode: Boolean;
  public
    class operator Implicit(const Value: T): Prop<T>;
    class operator Implicit(const Value: Prop<T>): T;

    // Comparison operators
    class operator Equal(const LHS: Prop<T>; const RHS: T): BooleanExpression;
    class operator NotEqual(const LHS: Prop<T>; const RHS: T): BooleanExpression;
    class operator GreaterThan(const LHS: Prop<T>; const RHS: T): BooleanExpression;
    class operator GreaterThanOrEqual(const LHS: Prop<T>; const RHS: T): BooleanExpression;
    class operator LessThan(const LHS: Prop<T>; const RHS: T): BooleanExpression;
    class operator LessThanOrEqual(const LHS: Prop<T>; const RHS: T): BooleanExpression;

    // Prop<T> vs Prop<T> comparison
    class operator GreaterThan(const LHS: Prop<T>; const RHS: Prop<T>): BooleanExpression;

    // String-specific methods
    function Like(const Pattern: string): BooleanExpression;
    function StartsWith(const Value: string): BooleanExpression;
    function EndsWith(const Value: string): BooleanExpression;
    function Contains(const Value: string): BooleanExpression;

    // Collection methods
    function &In(const Values: TArray<T>): BooleanExpression;
    function NotIn(const Values: TArray<T>): BooleanExpression;

    // Null handling
    function IsNull: BooleanExpression;
    function IsNotNull: BooleanExpression;

    // Range
    function Between(const Lower, Upper: T): BooleanExpression;

    property Value: T read FValue write FValue;
  end;

  /// <summary>
  ///   Implementation of IPropInfo that holds column name.
  ///   Created by TPrototype and injected into Prop<T>.FInfo.
  /// </summary>
  TPropInfo = class(TInterfacedObject, IPropInfo)
  private
    FColumnName: string;
  public
    constructor Create(const AColumnName: string);
    function GetColumnName: string;
  end;

  // ---------------------------------------------------------------------------
  // Type Aliases for cleaner entity definitions
  // ---------------------------------------------------------------------------

  StringType = Prop<string>;
  IntType = Prop<Integer>;
  Int64Type = Prop<Int64>;
  BoolType = Prop<Boolean>;
  FloatType = Prop<Double>;
  CurrencyType = Prop<Currency>;
  DateTimeType = Prop<TDateTime>;
  DateType = Prop<TDate>;
  TimeType = Prop<TTime>;

  /// <summary>
  ///   Short alias for BooleanExpression.
  /// </summary>
  BoolExpr = BooleanExpression;

  /// <summary>
  ///   Delegate for Smart Property queries.
  /// </summary>
  TQueryPredicate<T> = reference to function(Arg: T): BooleanExpression;

/// <summary>
///   Extracts the inner value from a Smart Type (Prop<T>) record.
///   If the value is not a Smart Type, returns the original TValue.ToString.
///   Use this for identity map keys and other scenarios requiring raw values.
/// </summary>
function GetSmartValue(const AValue: TValue; const ATypeName: string): string;

implementation

{ TPropInfo }

constructor TPropInfo.Create(const AColumnName: string);
begin
  FColumnName := AColumnName;
end;

function TPropInfo.GetColumnName: string;
begin
  Result := FColumnName;
end;

{ GetSmartValue }

function GetSmartValue(const AValue: TValue; const ATypeName: string): string;
var
  Ctx: TRttiContext;
  RecType: TRttiRecordType;
  ValueField: TRttiField;
  InnerVal: TValue;
begin
  // Check if it's a record type that might be Prop<T>
  if (AValue.Kind = tkRecord) and ATypeName.StartsWith('Prop<') then
  begin
    // It's a Smart Type - extract FValue field
    Ctx := TRttiContext.Create;
    try
      RecType := Ctx.GetType(AValue.TypeInfo).AsRecord;
      ValueField := RecType.GetField('FValue');
      if ValueField <> nil then
      begin
        InnerVal := ValueField.GetValue(AValue.GetReferenceToRawData);
        Result := InnerVal.ToString;
        Exit;
      end;
    finally
      Ctx.Free;
    end;
  end;
  // Default: use TValue.ToString
  Result := AValue.ToString;
end;

{ BooleanExpression }

class function BooleanExpression.FromQuery(const AExpr: IExpression): BooleanExpression;
begin
  Result.FExpression := AExpr;
  Result.FRuntimeValue := False;
end;

class function BooleanExpression.FromRuntime(const AValue: Boolean): BooleanExpression;
begin
  Result.FRuntimeValue := AValue;
  Result.FExpression := nil;
end;

class operator BooleanExpression.Implicit(const Value: BooleanExpression): Boolean;
begin
  // In query mode, return False (expression is not a boolean)
  if Value.FExpression <> nil then
    Exit(False);
  Result := Value.FRuntimeValue;
end;

class operator BooleanExpression.Implicit(const Value: BooleanExpression): TFluentExpression;
begin
  if Value.FExpression <> nil then
    Result := TFluentExpression.From(Value.FExpression)
  else
    Result := TFluentExpression.From(TConstantExpression.Create(Value.FRuntimeValue));
end;

class function BooleanExpression.InternalAnd(const Left, Right: BooleanExpression): BooleanExpression;
begin
  // If either side has an expression, build AST
  if (Left.FExpression <> nil) or (Right.FExpression <> nil) then
  begin
    var LExpr := TFluentExpression(Left).Expression;
    var RExpr := TFluentExpression(Right).Expression;
    Result := BooleanExpression.FromQuery(TLogicalExpression.Create(LExpr, RExpr, loAnd));
  end
  else
    Result := BooleanExpression.FromRuntime(Left.FRuntimeValue and Right.FRuntimeValue);
end;

class function BooleanExpression.InternalOr(const Left, Right: BooleanExpression): BooleanExpression;
begin
  if (Left.FExpression <> nil) or (Right.FExpression <> nil) then
  begin
    var LExpr := TFluentExpression(Left).Expression;
    var RExpr := TFluentExpression(Right).Expression;
    Result := BooleanExpression.FromQuery(TLogicalExpression.Create(LExpr, RExpr, loOr));
  end
  else
    Result := BooleanExpression.FromRuntime(Left.FRuntimeValue or Right.FRuntimeValue);
end;

class operator BooleanExpression.LogicalAnd(const Left, Right: BooleanExpression): BooleanExpression;
begin
  Result := InternalAnd(Left, Right);
end;

class operator BooleanExpression.BitwiseAnd(const Left, Right: BooleanExpression): BooleanExpression;
begin
  Result := InternalAnd(Left, Right);
end;

class operator BooleanExpression.BitwiseOr(const Left, Right: BooleanExpression): BooleanExpression;
begin
  Result := InternalOr(Left, Right);
end;

class operator BooleanExpression.LogicalOr(const Left, Right: BooleanExpression): BooleanExpression;
begin
  Result := InternalOr(Left, Right);
end;

class operator BooleanExpression.LogicalNot(const Value: BooleanExpression): BooleanExpression;
begin
  if Value.FExpression <> nil then
    Result := BooleanExpression.FromQuery(TUnaryExpression.Create(Value.FExpression))
  else
    Result := BooleanExpression.FromRuntime(not Value.FRuntimeValue);
end;

{ Prop<T> }

function Prop<T>.GetColumnName: string;
begin
  if FInfo <> nil then
    Result := FInfo.ColumnName
  else
    Result := '';
end;

function Prop<T>.IsQueryMode: Boolean;
begin
  Result := FInfo <> nil;
end;

class operator Prop<T>.Implicit(const Value: T): Prop<T>;
begin
  Result.FValue := Value;
  Result.FInfo := nil;  // Real instances don't have metadata
end;

class operator Prop<T>.Implicit(const Value: Prop<T>): T;
begin
  Result := Value.FValue;
end;

class operator Prop<T>.Equal(const LHS: Prop<T>; const RHS: T): BooleanExpression;
begin
  if LHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(
      TBinaryExpression.Create(LHS.GetColumnName, boEqual, TValue.From<T>(RHS)))
  else
    Result := BooleanExpression.FromRuntime(
      TComparer<T>.Default.Compare(LHS.FValue, RHS) = 0);
end;

class operator Prop<T>.NotEqual(const LHS: Prop<T>; const RHS: T): BooleanExpression;
begin
  if LHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(
      TBinaryExpression.Create(LHS.GetColumnName, boNotEqual, TValue.From<T>(RHS)))
  else
    Result := BooleanExpression.FromRuntime(
      TComparer<T>.Default.Compare(LHS.FValue, RHS) <> 0);
end;

class operator Prop<T>.GreaterThan(const LHS: Prop<T>; const RHS: T): BooleanExpression;
begin
  if LHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(
      TBinaryExpression.Create(LHS.GetColumnName, boGreaterThan, TValue.From<T>(RHS)))
  else
    Result := BooleanExpression.FromRuntime(
      TComparer<T>.Default.Compare(LHS.FValue, RHS) > 0);
end;

class operator Prop<T>.GreaterThanOrEqual(const LHS: Prop<T>; const RHS: T): BooleanExpression;
begin
  if LHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(
      TBinaryExpression.Create(LHS.GetColumnName, boGreaterThanOrEqual, TValue.From<T>(RHS)))
  else
    Result := BooleanExpression.FromRuntime(
      TComparer<T>.Default.Compare(LHS.FValue, RHS) >= 0);
end;

class operator Prop<T>.LessThan(const LHS: Prop<T>; const RHS: T): BooleanExpression;
begin
  if LHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(
      TBinaryExpression.Create(LHS.GetColumnName, boLessThan, TValue.From<T>(RHS)))
  else
    Result := BooleanExpression.FromRuntime(
      TComparer<T>.Default.Compare(LHS.FValue, RHS) < 0);
end;

class operator Prop<T>.LessThanOrEqual(const LHS: Prop<T>; const RHS: T): BooleanExpression;
begin
  if LHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(
      TBinaryExpression.Create(LHS.GetColumnName, boLessThanOrEqual, TValue.From<T>(RHS)))
  else
    Result := BooleanExpression.FromRuntime(
      TComparer<T>.Default.Compare(LHS.FValue, RHS) <= 0);
end;

class operator Prop<T>.GreaterThan(const LHS: Prop<T>; const RHS: Prop<T>): BooleanExpression;
begin
  if LHS.IsQueryMode or RHS.IsQueryMode then
  begin
    var LName := LHS.GetColumnName;
    var RName := RHS.GetColumnName;
    Result := BooleanExpression.FromQuery(
      TBinaryExpression.Create(LName, boGreaterThan, TValue.From(RName)));
  end
  else
    Result := BooleanExpression.FromRuntime(
      TComparer<T>.Default.Compare(LHS.FValue, RHS.FValue) > 0);
end;

function Prop<T>.Like(const Pattern: string): BooleanExpression;
begin
  if IsQueryMode then
    Result := BooleanExpression.FromQuery(
      TBinaryExpression.Create(GetColumnName, boLike, Pattern))
  else
  begin
    var StrVal := TValue.From<T>(FValue).ToString;
    Result := BooleanExpression.FromRuntime(StrVal.Contains(Pattern.Replace('%', '')));
  end;
end;

function Prop<T>.StartsWith(const Value: string): BooleanExpression;
begin
  Result := Like(Value + '%');
end;

function Prop<T>.EndsWith(const Value: string): BooleanExpression;
begin
  Result := Like('%' + Value);
end;

function Prop<T>.Contains(const Value: string): BooleanExpression;
begin
  Result := Like('%' + Value + '%');
end;

function Prop<T>.&In(const Values: TArray<T>): BooleanExpression;
begin
  if IsQueryMode then
  begin
    var V := TValue.From<TArray<T>>(Values);
    Result := BooleanExpression.FromQuery(
      TBinaryExpression.Create(GetColumnName, boIn, V));
  end
  else
  begin
    var Comp := TEqualityComparer<T>.Default;
    for var Item in Values do
      if Comp.Equals(FValue, Item) then
        Exit(BooleanExpression.FromRuntime(True));
    Result := BooleanExpression.FromRuntime(False);
  end;
end;

function Prop<T>.NotIn(const Values: TArray<T>): BooleanExpression;
begin
  var Check := &In(Values);
  if Check.FExpression <> nil then
    Result := BooleanExpression.FromQuery(
      TBinaryExpression.Create(GetColumnName, boNotIn, TValue.From<TArray<T>>(Values)))
  else
    Result := BooleanExpression.FromRuntime(not Check.FRuntimeValue);
end;

function Prop<T>.IsNull: BooleanExpression;
begin
  if IsQueryMode then
    Result := BooleanExpression.FromQuery(
      TUnaryExpression.Create(GetColumnName, uoIsNull))
  else
    Result := BooleanExpression.FromRuntime(False);
end;

function Prop<T>.IsNotNull: BooleanExpression;
begin
  if IsQueryMode then
    Result := BooleanExpression.FromQuery(
      TUnaryExpression.Create(GetColumnName, uoIsNotNull))
  else
    Result := BooleanExpression.FromRuntime(True);
end;

function Prop<T>.Between(const Lower, Upper: T): BooleanExpression;
begin
  // Compose using existing operators
  var LowerCheck := (Self >= Lower);
  var UpperCheck := (Self <= Upper);
  Result := LowerCheck and UpperCheck;
end;

end.
