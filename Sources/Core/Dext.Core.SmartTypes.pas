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

{$RTTI EXPLICIT FIELDS([vcPrivate..vcPublic]) PROPERTIES([vcPrivate..vcPublic]) METHODS([vcPrivate..vcPublic])}

interface

uses
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  System.Variants,
  Dext.Types.Nullable,
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
    function GetPropertyName: string;
    property ColumnName: string read GetColumnName;
    property PropertyName: string read GetPropertyName;
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

    class operator Implicit(const Value: Boolean): BooleanExpression;
    class operator Implicit(const Value: BooleanExpression): Boolean;
    class operator Implicit(const Value: BooleanExpression): TFluentExpression;
    class operator Implicit(const Value: TFluentExpression): BooleanExpression;
    class operator Implicit(const Value: BooleanExpression): IExpression;

    class operator LogicalAnd(const Left, Right: BooleanExpression): BooleanExpression;
    class operator BitwiseAnd(const Left, Right: BooleanExpression): BooleanExpression;
    class operator LogicalOr(const Left, Right: BooleanExpression): BooleanExpression;
    class operator BitwiseOr(const Left, Right: BooleanExpression): BooleanExpression;
    class operator LogicalXor(const Left, Right: BooleanExpression): BooleanExpression;
    class operator BitwiseXor(const Left, Right: BooleanExpression): BooleanExpression;
    class operator LogicalNot(const Value: BooleanExpression): BooleanExpression;

    class operator LogicalAnd(const Left: BooleanExpression; const Right: Boolean): BooleanExpression;
    class operator LogicalOr(const Left: BooleanExpression; const Right: Boolean): BooleanExpression;

    property Expression: IExpression read FExpression;
    property RuntimeValue: Boolean read FRuntimeValue;
  end;

  /// <summary>
  ///   Generic property wrapper that enables operator overloading for queries.
  ///   When FInfo is assigned (via TPrototype), operators generate Expression Trees.
  ///   When FInfo is nil (normal usage), operators perform runtime comparisons.
  /// </summary>
  {$RTTI EXPLICIT FIELDS([vcPrivate..vcPublished])}
  Prop<T> = record
  private
    FValue: T;
    FInfo: IPropInfo;
    FExpression: IExpression;
    function GetColumnName: string;
    function GetPropertyName: string;
    function GetExpression: IExpression;
  public
    function IsQueryMode: Boolean;
    class operator Implicit(const Value: T): Prop<T>;
    class operator Implicit(const Value: Prop<T>): T;
    class operator Implicit(const Value: Prop<T>): BooleanExpression;
    class operator Implicit(const Value: Prop<T>): IExpression;

    // Nullable<T> interop
    class operator Implicit(const Value: Prop<T>): Nullable<T>;
    class operator Implicit(const Value: Nullable<T>): Prop<T>;

    // Variant interop
    class operator Implicit(const Value: Variant): Prop<T>;
    class operator Implicit(const Value: Prop<T>): Variant;
    
    // Explicit operators for safe casting (avoiding binary hardcasts)
    class operator Explicit(const Value: Prop<T>): string;
    class operator Explicit(const Value: Prop<T>): Integer;
    class operator Explicit(const Value: Prop<T>): Int64;
    class operator Explicit(const Value: Prop<T>): Double;
    class operator Explicit(const Value: Prop<T>): Currency;
    class operator Explicit(const Value: Prop<T>): Boolean;
    class operator Explicit(const Value: Prop<T>): TDateTime;

    // Fluent conversion methods
    function AsString: string;
    function AsInteger: Integer;
    function AsInt64: Int64;
    function AsDouble: Double;
    function AsCurrency: Currency;
    function AsBoolean: Boolean;
    function AsDateTime: TDateTime;
    // Factory for calculated properties
    class function FromExpression(const AExpr: IExpression): Prop<T>; static;

    // Comparison operators
    class operator Equal(const LHS: Prop<T>; const RHS: T): BooleanExpression;
    class operator Equal(const LHS: T; const RHS: Prop<T>): BooleanExpression;
    class operator NotEqual(const LHS: Prop<T>; const RHS: T): BooleanExpression;
    class operator NotEqual(const LHS: T; const RHS: Prop<T>): BooleanExpression;
    class operator GreaterThan(const LHS: Prop<T>; const RHS: T): BooleanExpression;
    class operator GreaterThan(const LHS: T; const RHS: Prop<T>): BooleanExpression;
    class operator GreaterThanOrEqual(const LHS: Prop<T>; const RHS: T): BooleanExpression;
    class operator GreaterThanOrEqual(const LHS: T; const RHS: Prop<T>): BooleanExpression;
    class operator LessThan(const LHS: Prop<T>; const RHS: T): BooleanExpression;
    class operator LessThan(const LHS: T; const RHS: Prop<T>): BooleanExpression;
    class operator LessThanOrEqual(const LHS: Prop<T>; const RHS: T): BooleanExpression;
    class operator LessThanOrEqual(const LHS: T; const RHS: Prop<T>): BooleanExpression;

    // Prop<T> vs Prop<T> comparison
    class operator Equal(const LHS, RHS: Prop<T>): BooleanExpression;
    class operator NotEqual(const LHS, RHS: Prop<T>): BooleanExpression;
    class operator GreaterThan(const LHS, RHS: Prop<T>): BooleanExpression;
    class operator GreaterThanOrEqual(const LHS, RHS: Prop<T>): BooleanExpression;
    class operator LessThan(const LHS, RHS: Prop<T>): BooleanExpression;
    class operator LessThanOrEqual(const LHS, RHS: Prop<T>): BooleanExpression;

    // Logical Operators (primarily for BoolType)
    class operator LogicalNot(const Value: Prop<T>): BooleanExpression;
    class operator LogicalAnd(const LHS: Prop<T>; const RHS: Boolean): BooleanExpression;
    class operator LogicalOr(const LHS: Prop<T>; const RHS: Boolean): BooleanExpression;

    // Unary Operators
    class operator Negative(const Value: Prop<T>): Prop<T>;
    class operator Positive(const Value: Prop<T>): Prop<T>;

    // Arithmetic Operators
    class operator Add(const LHS: Prop<T>; const RHS: T): Prop<T>;
    class operator Add(const LHS: T; const RHS: Prop<T>): Prop<T>;
    class operator Add(const LHS, RHS: Prop<T>): Prop<T>;
    class operator Subtract(const LHS: Prop<T>; const RHS: T): Prop<T>;
    class operator Subtract(const LHS: T; const RHS: Prop<T>): Prop<T>;
    class operator Subtract(const LHS, RHS: Prop<T>): Prop<T>;
    class operator Multiply(const LHS: Prop<T>; const RHS: T): Prop<T>;
    class operator Multiply(const LHS: T; const RHS: Prop<T>): Prop<T>;
    class operator Multiply(const LHS, RHS: Prop<T>): Prop<T>;
    class operator Divide(const LHS: Prop<T>; const RHS: T): Prop<T>;
    class operator Divide(const LHS: T; const RHS: Prop<T>): Prop<T>;
    class operator Divide(const LHS, RHS: Prop<T>): Prop<T>;

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

    // Order By Support
    function Asc: IOrderBy;
    function Desc: IOrderBy;

    property Name: string read GetPropertyName;
    property Value: T read FValue write FValue;
    property Expression: IExpression read GetExpression;
  end;

  /// <summary>
  ///   Implementation of IPropInfo that holds column name.
  ///   Created by TPrototype and injected into Prop<T>.FInfo.
  /// </summary>
  TPropInfo = class(TInterfacedObject, IPropInfo)
  private
    FColumnName: string;
    FPropertyName: string;
  public
    constructor Create(const AColumnName: string; const APropertyName: string = '');
    function GetColumnName: string;
    function GetPropertyName: string;
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

uses
  Dext.Specifications.OrderBy;

{ TPropInfo }

constructor TPropInfo.Create(const AColumnName: string; const APropertyName: string = '');
begin
  FColumnName := AColumnName;
  if APropertyName <> '' then
    FPropertyName := APropertyName
  else
    FPropertyName := AColumnName;
end;

function TPropInfo.GetColumnName: string;
begin
  Result := FColumnName;
end;

function TPropInfo.GetPropertyName: string;
begin
  Result := FPropertyName;
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

class operator BooleanExpression.Implicit(const Value: BooleanExpression): IExpression;
begin
  if Value.FExpression <> nil then
    Result := Value.FExpression
  else
    Result := TConstantExpression.Create(Value.FRuntimeValue);
end;

class operator BooleanExpression.Implicit(const Value: BooleanExpression): TFluentExpression;
begin
  if Value.FExpression <> nil then
    Result := TFluentExpression.From(Value.FExpression)
  else
    Result := TFluentExpression.From(TConstantExpression.Create(Value.FRuntimeValue));
end;

class operator BooleanExpression.Implicit(const Value: TFluentExpression): BooleanExpression;
begin
  Result := BooleanExpression.FromQuery(Value.Expression);
end;

class operator BooleanExpression.Implicit(const Value: Boolean): BooleanExpression;
begin
  Result := BooleanExpression.FromRuntime(Value)
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

class operator BooleanExpression.LogicalAnd(const Left, Right: BooleanExpression): BooleanExpression;
begin
  Result := InternalAnd(Left, Right);
end;

class operator BooleanExpression.BitwiseAnd(const Left, Right: BooleanExpression): BooleanExpression;
begin
  Result := InternalAnd(Left, Right);
end;

class operator BooleanExpression.LogicalOr(const Left, Right: BooleanExpression): BooleanExpression;
begin
  Result := InternalOr(Left, Right);
end;

class operator BooleanExpression.BitwiseOr(const Left, Right: BooleanExpression): BooleanExpression;
begin
  Result := InternalOr(Left, Right);
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

class operator BooleanExpression.LogicalXor(const Left, Right: BooleanExpression): BooleanExpression;
begin
  if (Left.FExpression <> nil) or (Right.FExpression <> nil) then
    raise Exception.Create('Logical XOR not yet supported in queries')
  else
    Result := BooleanExpression.FromRuntime(Left.FRuntimeValue xor Right.FRuntimeValue);
end;

class operator BooleanExpression.BitwiseXor(const Left, Right: BooleanExpression): BooleanExpression;
begin
  Result := Left xor Right;
end;

class operator BooleanExpression.LogicalNot(const Value: BooleanExpression): BooleanExpression;
begin
  if Value.FExpression <> nil then
    Result := BooleanExpression.FromQuery(TUnaryExpression.Create(Value.FExpression))
  else
    Result := BooleanExpression.FromRuntime(not Value.FRuntimeValue);
end;

class operator BooleanExpression.LogicalAnd(const Left: BooleanExpression; const Right: Boolean): BooleanExpression;
begin
  if Left.FExpression <> nil then
    Result := BooleanExpression.FromQuery(TLogicalExpression.Create(Left.FExpression, TConstantExpression.Create(Right), loAnd))
  else
    Result := BooleanExpression.FromRuntime(Left.FRuntimeValue and Right);
end;

class operator BooleanExpression.LogicalOr(const Left: BooleanExpression; const Right: Boolean): BooleanExpression;
begin
  if Left.FExpression <> nil then
    Result := BooleanExpression.FromQuery(TLogicalExpression.Create(Left.FExpression, TConstantExpression.Create(Right), loOr))
  else
    Result := BooleanExpression.FromRuntime(Left.FRuntimeValue or Right);
end;

{ Prop<T> }

function Prop<T>.GetPropertyName: string;
begin
  if FInfo <> nil then
    Result := FInfo.PropertyName
  else
    Result := '';
end;

function Prop<T>.GetColumnName: string;
begin
  if FInfo <> nil then
    Result := FInfo.ColumnName
  else
    Result := '';
end;

function Prop<T>.GetExpression: IExpression;
begin
  if FExpression <> nil then
    Result := FExpression
  else if FInfo <> nil then
    Result := TPropertyExpression.Create(FInfo.ColumnName)
  else
    Result := TLiteralExpression.Create(TValue.From<T>(FValue));
end;

class function Prop<T>.FromExpression(const AExpr: IExpression): Prop<T>;
begin
  Result.FExpression := AExpr;
  Result.FInfo := nil;
  Result.FValue := Default(T);
end;

function Prop<T>.IsQueryMode: Boolean;
begin
  Result := (FInfo <> nil) or (FExpression <> nil);
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

class operator Prop<T>.Implicit(const Value: Prop<T>): BooleanExpression;
begin
  if Value.IsQueryMode then
    Result := BooleanExpression.FromQuery(Value.GetExpression)
  else
  begin
    var B: Boolean;
    if TValue.From<T>(Value.FValue).TryAsType<Boolean>(B) then
      Result := BooleanExpression.FromRuntime(B)
    else
      Result := BooleanExpression.FromRuntime(False);
  end;
end;

class operator Prop<T>.Implicit(const Value: Prop<T>): IExpression;
begin
  Result := Value.GetExpression;
end;


class operator Prop<T>.Implicit(const Value: Prop<T>): Nullable<T>;
begin
  Result := Nullable<T>.Create(Value.FValue);
end;

class operator Prop<T>.Implicit(const Value: Nullable<T>): Prop<T>;
begin
  if Value.HasValue then
    Result.FValue := Value.Value
  else
    Result.FValue := Default(T);
  Result.FInfo := nil;
end;

class operator Prop<T>.Implicit(const Value: Variant): Prop<T>;
begin
  Result.FValue := TValue.FromVariant(Value).AsType<T>;
  Result.FInfo := nil;
end;

class operator Prop<T>.Implicit(const Value: Prop<T>): Variant;
begin
  Result := TValue.From<T>(Value.FValue).AsVariant;
end;

class operator Prop<T>.Explicit(const Value: Prop<T>): string;
begin
  if TypeInfo(T) = TypeInfo(string) then
    Result := TValue.From<T>(Value.FValue).AsString
  else
    Result := TValue.From<T>(Value.FValue).ToString;
end;

class operator Prop<T>.Explicit(const Value: Prop<T>): Integer;
begin
  if TypeInfo(T) = TypeInfo(Integer) then
    Result := TValue.From<T>(Value.FValue).AsInteger
  else
    Result := TValue.From<T>(Value.FValue).Cast(TypeInfo(Integer)).AsInteger;
end;

class operator Prop<T>.Explicit(const Value: Prop<T>): Int64;
begin
  if TypeInfo(T) = TypeInfo(Int64) then
    Result := TValue.From<T>(Value.FValue).AsInt64
  else
    Result := TValue.From<T>(Value.FValue).Cast(TypeInfo(Int64)).AsInt64;
end;

class operator Prop<T>.Explicit(const Value: Prop<T>): Double;
begin
  if (TypeInfo(T) = TypeInfo(Double)) or (TypeInfo(T) = TypeInfo(TDateTime)) then
    Result := TValue.From<T>(Value.FValue).AsExtended
  else
    Result := TValue.From<T>(Value.FValue).Cast(TypeInfo(Double)).AsExtended;
end;

class operator Prop<T>.Explicit(const Value: Prop<T>): Currency;
begin
  if TypeInfo(T) = TypeInfo(Currency) then
    Result := TValue.From<T>(Value.FValue).AsCurrency
  else
    Result := TValue.From<T>(Value.FValue).Cast(TypeInfo(Currency)).AsCurrency;
end;

class operator Prop<T>.Explicit(const Value: Prop<T>): Boolean;
begin
  if TypeInfo(T) = TypeInfo(Boolean) then
    Result := TValue.From<T>(Value.FValue).AsBoolean
  else
    Result := TValue.From<T>(Value.FValue).Cast(TypeInfo(Boolean)).AsBoolean;
end;

class operator Prop<T>.Explicit(const Value: Prop<T>): TDateTime;
begin
  if (TypeInfo(T) = TypeInfo(TDateTime)) or (TypeInfo(T) = TypeInfo(Double)) then
    Result := TValue.From<T>(Value.FValue).AsType<TDateTime>
  else
    Result := TValue.From<T>(Value.FValue).Cast(TypeInfo(TDateTime)).AsType<TDateTime>;
end;

function Prop<T>.AsString: string;
begin
  Result := string(Self);
end;

function Prop<T>.AsInteger: Integer;
begin
  Result := Integer(Self);
end;

function Prop<T>.AsInt64: Int64;
begin
  Result := Int64(Self);
end;

function Prop<T>.AsDouble: Double;
begin
  Result := Double(Self);
end;

function Prop<T>.AsCurrency: Currency;
begin
  Result := Currency(Self);
end;

function Prop<T>.AsBoolean: Boolean;
begin
  Result := Boolean(Self);
end;

function Prop<T>.AsDateTime: TDateTime;
begin
  Result := TDateTime(Self);
end;

class operator Prop<T>.Equal(const LHS: Prop<T>; const RHS: T): BooleanExpression;
begin
  if LHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(TBinaryExpression.Create(LHS.GetExpression, TLiteralExpression.Create(TValue.From<T>(RHS)), boEqual))
  else
    Result := BooleanExpression.FromRuntime(TComparer<T>.Default.Compare(LHS.FValue, RHS) = 0);
end;

class operator Prop<T>.Equal(const LHS: T; const RHS: Prop<T>): BooleanExpression;
begin
  Result := RHS = LHS;
end;

class operator Prop<T>.NotEqual(const LHS: Prop<T>; const RHS: T): BooleanExpression;
begin
  if LHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(TBinaryExpression.Create(LHS.GetExpression, TLiteralExpression.Create(TValue.From<T>(RHS)), boNotEqual))
  else
    Result := BooleanExpression.FromRuntime(TComparer<T>.Default.Compare(LHS.FValue, RHS) <> 0);
end;

class operator Prop<T>.NotEqual(const LHS: T; const RHS: Prop<T>): BooleanExpression;
begin
  Result := RHS <> LHS;
end;

class operator Prop<T>.GreaterThan(const LHS: Prop<T>; const RHS: T): BooleanExpression;
begin
  if LHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(TBinaryExpression.Create(LHS.GetExpression, TLiteralExpression.Create(TValue.From<T>(RHS)), boGreaterThan))
  else
    Result := BooleanExpression.FromRuntime(TComparer<T>.Default.Compare(LHS.FValue, RHS) > 0);
end;

class operator Prop<T>.GreaterThan(const LHS: T; const RHS: Prop<T>): BooleanExpression;
begin
  Result := (RHS < LHS);
end;

class operator Prop<T>.GreaterThanOrEqual(const LHS: Prop<T>; const RHS: T): BooleanExpression;
begin
  if LHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(TBinaryExpression.Create(LHS.GetExpression, TLiteralExpression.Create(TValue.From<T>(RHS)), boGreaterThanOrEqual))
  else
    Result := BooleanExpression.FromRuntime(TComparer<T>.Default.Compare(LHS.FValue, RHS) >= 0);
end;

class operator Prop<T>.GreaterThanOrEqual(const LHS: T; const RHS: Prop<T>): BooleanExpression;
begin
  Result := (RHS <= LHS);
end;

class operator Prop<T>.LessThan(const LHS: Prop<T>; const RHS: T): BooleanExpression;
begin
  if LHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(TBinaryExpression.Create(LHS.GetExpression, TLiteralExpression.Create(TValue.From<T>(RHS)), boLessThan))
  else
    Result := BooleanExpression.FromRuntime(TComparer<T>.Default.Compare(LHS.FValue, RHS) < 0);
end;

class operator Prop<T>.LessThan(const LHS: T; const RHS: Prop<T>): BooleanExpression;
begin
  Result := (RHS > LHS);
end;

class operator Prop<T>.LessThanOrEqual(const LHS: Prop<T>; const RHS: T): BooleanExpression;
begin
  if LHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(TBinaryExpression.Create(LHS.GetExpression, TLiteralExpression.Create(TValue.From<T>(RHS)), boLessThanOrEqual))
  else
    Result := BooleanExpression.FromRuntime(TComparer<T>.Default.Compare(LHS.FValue, RHS) <= 0);
end;

class operator Prop<T>.LessThanOrEqual(const LHS: T; const RHS: Prop<T>): BooleanExpression;
begin
  Result := (RHS >= LHS);
end;

class operator Prop<T>.Equal(const LHS, RHS: Prop<T>): BooleanExpression;
begin
  if LHS.IsQueryMode or RHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(TBinaryExpression.Create(LHS.GetExpression, RHS.GetExpression, boEqual))
  else
    Result := BooleanExpression.FromRuntime(TComparer<T>.Default.Compare(LHS.FValue, RHS.FValue) = 0);
end;

class operator Prop<T>.NotEqual(const LHS, RHS: Prop<T>): BooleanExpression;
begin
  if LHS.IsQueryMode or RHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(TBinaryExpression.Create(LHS.GetExpression, RHS.GetExpression, boNotEqual))
  else
    Result := BooleanExpression.FromRuntime(TComparer<T>.Default.Compare(LHS.FValue, RHS.FValue) <> 0);
end;

class operator Prop<T>.GreaterThan(const LHS: Prop<T>; const RHS: Prop<T>): BooleanExpression;
begin
  if LHS.IsQueryMode or RHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(TBinaryExpression.Create(LHS.GetExpression, RHS.GetExpression, boGreaterThan))
  else
    Result := BooleanExpression.FromRuntime(TComparer<T>.Default.Compare(LHS.FValue, RHS.FValue) > 0);
end;

class operator Prop<T>.GreaterThanOrEqual(const LHS, RHS: Prop<T>): BooleanExpression;
begin
  if LHS.IsQueryMode or RHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(TBinaryExpression.Create(LHS.GetExpression, RHS.GetExpression, boGreaterThanOrEqual))
  else
    Result := BooleanExpression.FromRuntime(TComparer<T>.Default.Compare(LHS.FValue, RHS.FValue) >= 0);
end;

class operator Prop<T>.LessThan(const LHS, RHS: Prop<T>): BooleanExpression;
begin
  if LHS.IsQueryMode or RHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(TBinaryExpression.Create(LHS.GetExpression, RHS.GetExpression, boLessThan))
  else
    Result := BooleanExpression.FromRuntime(TComparer<T>.Default.Compare(LHS.FValue, RHS.FValue) < 0);
end;

class operator Prop<T>.LessThanOrEqual(const LHS, RHS: Prop<T>): BooleanExpression;
begin
  if LHS.IsQueryMode or RHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(TBinaryExpression.Create(LHS.GetExpression, RHS.GetExpression, boLessThanOrEqual))
  else
    Result := BooleanExpression.FromRuntime(TComparer<T>.Default.Compare(LHS.FValue, RHS.FValue) <= 0);
end;

class operator Prop<T>.LogicalNot(const Value: Prop<T>): BooleanExpression;
begin
  if Value.IsQueryMode then
    Result := BooleanExpression.FromQuery(TBinaryExpression.Create(Value.GetExpression, TLiteralExpression.Create(TValue.From<Boolean>(False)), boEqual))
  else
  begin
    var B: Boolean;
    if TValue.From<T>(Value.FValue).TryAsType<Boolean>(B) then
      Result := BooleanExpression.FromRuntime(not B)
    else
      raise Exception.Create('Logical NOT operator is only supported for Boolean properties');
  end;
end;

class operator Prop<T>.LogicalAnd(const LHS: Prop<T>; const RHS: Boolean): BooleanExpression;
begin
  if LHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(TLogicalExpression.Create(LHS.GetExpression, TConstantExpression.Create(RHS), loAnd))
  else
  begin
    var B: Boolean;
    if TValue.From<T>(LHS.FValue).TryAsType<Boolean>(B) then
      Result := BooleanExpression.FromRuntime(B and RHS)
    else
      Result := BooleanExpression.FromRuntime(False);
  end;
end;

class operator Prop<T>.LogicalOr(const LHS: Prop<T>; const RHS: Boolean): BooleanExpression;
begin
  if LHS.IsQueryMode then
    Result := BooleanExpression.FromQuery(TLogicalExpression.Create(LHS.GetExpression, TConstantExpression.Create(RHS), loOr))
  else
  begin
    var B: Boolean;
    if TValue.From<T>(LHS.FValue).TryAsType<Boolean>(B) then
      Result := BooleanExpression.FromRuntime(B or RHS)
    else
      Result := BooleanExpression.FromRuntime(True); // False or True is True? No, this branch is if conversion fails.
  end;
end;

class operator Prop<T>.Negative(const Value: Prop<T>): Prop<T>;
begin
  if Value.IsQueryMode then
    // Unary minus would need TUnaryExpression to support aoNegative? 
    // For now, multiply by -1
    Result := Value * TValue.From<Integer>(-1).AsType<T>
  else
  begin
    // Runtime math using variants
    var V: Variant := TValue.From<T>(Value.FValue).AsVariant;
    Result.FValue := TValue.FromVariant(-V).AsType<T>;
    Result.FInfo := nil;
  end;
end;

class operator Prop<T>.Positive(const Value: Prop<T>): Prop<T>;
begin
  Result := Value;
end;

class operator Prop<T>.Add(const LHS: Prop<T>; const RHS: T): Prop<T>;
begin
  if LHS.IsQueryMode then
    Result := Prop<T>.FromExpression(TArithmeticExpression.Create(LHS.GetExpression, TLiteralExpression.Create(TValue.From<T>(RHS)), aoAdd))
  else
  begin
    var V1: Variant := TValue.From<T>(LHS.FValue).AsVariant;
    var V2: Variant := TValue.From<T>(RHS).AsVariant;
    Result.FValue := TValue.FromVariant(V1 + V2).AsType<T>;
    Result.FInfo := nil;
  end;
end;

class operator Prop<T>.Add(const LHS, RHS: Prop<T>): Prop<T>;
begin
  if LHS.IsQueryMode or RHS.IsQueryMode then
    Result := Prop<T>.FromExpression(TArithmeticExpression.Create(LHS.GetExpression, RHS.GetExpression, aoAdd))
  else
  begin
    var V1: Variant := TValue.From<T>(LHS.FValue).AsVariant;
    var V2: Variant := TValue.From<T>(RHS.FValue).AsVariant;
    Result.FValue := TValue.FromVariant(V1 + V2).AsType<T>;
    Result.FInfo := nil;
  end;
end;

class operator Prop<T>.Add(const LHS: T; const RHS: Prop<T>): Prop<T>;
begin
  Result := RHS + LHS;
end;

class operator Prop<T>.Subtract(const LHS: Prop<T>; const RHS: T): Prop<T>;
begin
  if LHS.IsQueryMode then
    Result := Prop<T>.FromExpression(TArithmeticExpression.Create(LHS.GetExpression, TLiteralExpression.Create(TValue.From<T>(RHS)), aoSubtract))
  else
  begin
    var V1: Variant := TValue.From<T>(LHS.FValue).AsVariant;
    var V2: Variant := TValue.From<T>(RHS).AsVariant;
    Result.FValue := TValue.FromVariant(V1 - V2).AsType<T>;
    Result.FInfo := nil;
  end;
end;

class operator Prop<T>.Subtract(const LHS, RHS: Prop<T>): Prop<T>;
begin
  if LHS.IsQueryMode or RHS.IsQueryMode then
    Result := Prop<T>.FromExpression(TArithmeticExpression.Create(LHS.GetExpression, RHS.GetExpression, aoSubtract))
  else
  begin
    var V1: Variant := TValue.From<T>(LHS.FValue).AsVariant;
    var V2: Variant := TValue.From<T>(RHS.FValue).AsVariant;
    Result.FValue := TValue.FromVariant(V1 - V2).AsType<T>;
    Result.FInfo := nil;
  end;
end;

class operator Prop<T>.Subtract(const LHS: T; const RHS: Prop<T>): Prop<T>;
begin
  if RHS.IsQueryMode then
    Result := Prop<T>.FromExpression(TArithmeticExpression.Create(TLiteralExpression.Create(TValue.From<T>(LHS)), RHS.GetExpression, aoSubtract))
  else
  begin
    var V1: Variant := TValue.From<T>(LHS).AsVariant;
    var V2: Variant := TValue.From<T>(RHS.FValue).AsVariant;
    Result.FValue := TValue.FromVariant(V1 - V2).AsType<T>;
    Result.FInfo := nil;
  end;
end;

class operator Prop<T>.Multiply(const LHS: Prop<T>; const RHS: T): Prop<T>;
begin
  if LHS.IsQueryMode then
    Result := Prop<T>.FromExpression(TArithmeticExpression.Create(LHS.GetExpression, TLiteralExpression.Create(TValue.From<T>(RHS)), aoMultiply))
  else
  begin
    var V1: Variant := TValue.From<T>(LHS.FValue).AsVariant;
    var V2: Variant := TValue.From<T>(RHS).AsVariant;
    Result.FValue := TValue.FromVariant(V1 * V2).AsType<T>;
    Result.FInfo := nil;
  end;
end;

class operator Prop<T>.Multiply(const LHS, RHS: Prop<T>): Prop<T>;
begin
  if LHS.IsQueryMode or RHS.IsQueryMode then
    Result := Prop<T>.FromExpression(TArithmeticExpression.Create(LHS.GetExpression, RHS.GetExpression, aoMultiply))
  else
  begin
    var V1: Variant := TValue.From<T>(LHS.FValue).AsVariant;
    var V2: Variant := TValue.From<T>(RHS.FValue).AsVariant;
    Result.FValue := TValue.FromVariant(V1 * V2).AsType<T>;
    Result.FInfo := nil;
  end;
end;

class operator Prop<T>.Multiply(const LHS: T; const RHS: Prop<T>): Prop<T>;
begin
  Result := RHS * LHS;
end;

class operator Prop<T>.Divide(const LHS: Prop<T>; const RHS: T): Prop<T>;
begin
  if LHS.IsQueryMode then
    Result := Prop<T>.FromExpression(TArithmeticExpression.Create(LHS.GetExpression, TLiteralExpression.Create(TValue.From<T>(RHS)), aoDivide))
  else
  begin
    var V1: Variant := TValue.From<T>(LHS.FValue).AsVariant;
    var V2: Variant := TValue.From<T>(RHS).AsVariant;
    Result.FValue := TValue.FromVariant(V1 / V2).AsType<T>;
    Result.FInfo := nil;
  end;
end;

class operator Prop<T>.Divide(const LHS, RHS: Prop<T>): Prop<T>;
begin
  if LHS.IsQueryMode or RHS.IsQueryMode then
    Result := Prop<T>.FromExpression(TArithmeticExpression.Create(LHS.GetExpression, RHS.GetExpression, aoDivide))
  else
  begin
    var V1: Variant := TValue.From<T>(LHS.FValue).AsVariant;
    var V2: Variant := TValue.From<T>(RHS.FValue).AsVariant;
    Result.FValue := TValue.FromVariant(V1 / V2).AsType<T>;
    Result.FInfo := nil;
  end;
end;

class operator Prop<T>.Divide(const LHS: T; const RHS: Prop<T>): Prop<T>;
begin
  if RHS.IsQueryMode then
    Result := Prop<T>.FromExpression(TArithmeticExpression.Create(TLiteralExpression.Create(TValue.From<T>(LHS)), RHS.GetExpression, aoDivide))
  else
  begin
    var V1: Variant := TValue.From<T>(LHS).AsVariant;
    var V2: Variant := TValue.From<T>(RHS.FValue).AsVariant;
    Result.FValue := TValue.FromVariant(V1 / V2).AsType<T>;
    Result.FInfo := nil;
  end;
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

function Prop<T>.Asc: IOrderBy;
begin
  if IsQueryMode then
    Result := TOrderBy.Create(GetColumnName, True)
  else
    Result := nil; // Runtime sorting not supported via this method yet
end;

function Prop<T>.Desc: IOrderBy;
begin
  if IsQueryMode then
    Result := TOrderBy.Create(GetColumnName, False)
  else
    Result := nil;
end;

end.
