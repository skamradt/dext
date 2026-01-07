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
{  Created: 2025-12-08                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.Specifications.Evaluator;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Variants,
  Dext.Specifications.Interfaces,
  Dext.Specifications.Types;

type
  /// <summary>
  ///   Evaluates an expression tree against an object instance in memory using RTTI.
  /// </summary>
  TExpressionEvaluator = class
  public
    class function Evaluate(const AExpression: IExpression; const AObject: TObject): Boolean;
  end;

implementation

type
  TEvaluatorVisitor = class(TInterfacedObject, IExpressionVisitor)
  private
    FObject: TObject;
    FResult: Boolean;
    FCtx: TRttiContext;
    
    function GetPropertyValue(const APropertyName: string): TValue;
    function ResolveValue(const AExpression: IExpression): TValue;
    function Compare(const Left, Right: TValue; Op: TBinaryOperator): Boolean;
    function Calculate(const Left, Right: TValue; Op: TArithmeticOperator): TValue;
  public
    constructor Create(AObject: TObject);
    procedure Visit(const AExpression: IExpression);
    property Result: Boolean read FResult;
  end;

{ TExpressionEvaluator }

class function TExpressionEvaluator.Evaluate(const AExpression: IExpression; const AObject: TObject): Boolean;
var
  Visitor: TEvaluatorVisitor;
begin
  if AExpression = nil then Exit(True);
  
  Visitor := TEvaluatorVisitor.Create(AObject);
  try
    Visitor.Visit(AExpression);
    Result := Visitor.Result;
  finally
    Visitor.Free;
  end;
end;

{ TEvaluatorVisitor }

constructor TEvaluatorVisitor.Create(AObject: TObject);
begin
  FObject := AObject;
  FCtx := TRttiContext.Create;
end;

function TEvaluatorVisitor.GetPropertyValue(const APropertyName: string): TValue;
var
  Typ: TRttiType;
  Prop: TRttiProperty;
  Val: TValue;
begin
  Typ := FCtx.GetType(FObject.ClassType);
  Prop := Typ.GetProperty(APropertyName);
  if Prop = nil then
    raise Exception.CreateFmt('Property "%s" not found on class "%s"', [APropertyName, FObject.ClassName]);
  
  Val := Prop.GetValue(FObject);
  
  // Unwrap Smart Types (Prop<T>)
  if (Val.Kind = tkRecord) and string(Val.TypeInfo.Name).StartsWith('Prop<') then
  begin
    var ValueField := FCtx.GetType(Val.TypeInfo).GetField('FValue');
    if ValueField <> nil then
      Val := ValueField.GetValue(Val.GetReferenceToRawData);
  end;
  
  Result := Val;
end;

function TEvaluatorVisitor.ResolveValue(const AExpression: IExpression): TValue;
begin
  if AExpression = nil then Exit(TValue.Empty);

  if AExpression is TPropertyExpression then
    Result := GetPropertyValue(TPropertyExpression(AExpression).PropertyName)
  else if AExpression is TLiteralExpression then
    Result := TLiteralExpression(AExpression).Value
  else if AExpression is TArithmeticExpression then
  begin
    var Math := TArithmeticExpression(AExpression);
    Result := Calculate(ResolveValue(Math.Left), ResolveValue(Math.Right), Math.ArithmeticOperator);
  end
  else
    Result := TValue.Empty;
end;

function TEvaluatorVisitor.Compare(const Left, Right: TValue; Op: TBinaryOperator): Boolean;
var
  L, R: Variant;
begin
  if Left.IsEmpty or Right.IsEmpty then
  begin
    // Handle nulls
    case Op of
      boEqual: Result := Left.IsEmpty and Right.IsEmpty;
      boNotEqual: Result := not (Left.IsEmpty and Right.IsEmpty);
      else Result := False;
    end;
    Exit;
  end;

  L := Left.AsVariant;
  R := Right.AsVariant;

  case Op of
    boEqual: Result := L = R;
    boNotEqual: Result := L <> R;
    boGreaterThan: Result := L > R;
    boGreaterThanOrEqual: Result := L >= R;
    boLessThan: Result := L < R;
    boLessThanOrEqual: Result := L <= R;
    boLike: 
      begin
        // Simple LIKE implementation (case-insensitive)
        // Supports % at start/end
        var S := VarToStr(L).ToLower;
        var P := VarToStr(R).ToLower;
        if P.StartsWith('%') and P.EndsWith('%') then
          Result := S.Contains(P.Substring(1, P.Length - 2))
        else if P.StartsWith('%') then
          Result := S.EndsWith(P.Substring(1))
        else if P.EndsWith('%') then
          Result := S.StartsWith(P.Substring(0, P.Length - 1))
        else
          Result := S = P;
      end;
    boNotLike: Result := not Compare(Left, Right, boLike);
    boIn: 
      begin
        Result := False;
        if Right.IsArray then
        begin
          for var I := 0 to Right.GetArrayLength - 1 do
          begin
            var Elem := Right.GetArrayElement(I);
            if Compare(Left, Elem, boEqual) then
            begin
              Result := True;
              Break;
            end;
          end;
        end;
      end;
    boNotIn: Result := not Compare(Left, Right, boIn);
    boBitwiseAnd: Result := (Integer(L) and Integer(R)) <> 0;
    boBitwiseOr: Result := (Integer(L) or Integer(R)) <> 0;
    boBitwiseXor: Result := (Integer(L) xor Integer(R)) <> 0;
    else Result := False;
  end;
end;

function TEvaluatorVisitor.Calculate(const Left, Right: TValue; Op: TArithmeticOperator): TValue;
var
  L, R: Variant;
begin
  L := Left.AsVariant;
  R := Right.AsVariant;
  case Op of
    aoAdd: Result := TValue.FromVariant(L + R);
    aoSubtract: Result := TValue.FromVariant(L - R);
    aoMultiply: Result := TValue.FromVariant(L * R);
    aoDivide: Result := TValue.FromVariant(L / R);
    aoModulus: Result := TValue.FromVariant(L mod R);
    aoIntDivide: Result := TValue.FromVariant(L div R);
    else Result := TValue.Empty;
  end;
end;

procedure TEvaluatorVisitor.Visit(const AExpression: IExpression);
begin
  if AExpression is TBinaryExpression then
  begin
    var Bin := TBinaryExpression(AExpression);
    var L := ResolveValue(Bin.Left);
    var R := ResolveValue(Bin.Right);
    FResult := Compare(L, R, Bin.BinaryOperator);
  end
  else if AExpression is TLogicalExpression then
  begin
    var Log := TLogicalExpression(AExpression);
    
    // Visit Left
    Visit(Log.Left);
    var LeftRes := FResult;
    
    // Short-circuit
    if (Log.LogicalOperator = loAnd) and (not LeftRes) then
    begin
      FResult := False;
      Exit;
    end;
    if (Log.LogicalOperator = loOr) and (LeftRes) then
    begin
      FResult := True;
      Exit;
    end;
    
    // Visit Right
    Visit(Log.Right);
    var RightRes := FResult;
    
    if Log.LogicalOperator = loAnd then
      FResult := LeftRes and RightRes
    else
      FResult := LeftRes or RightRes;
  end
  else if AExpression is TUnaryExpression then
  begin
    var Un := TUnaryExpression(AExpression);
    if Un.UnaryOperator = uoNot then
    begin
      Visit(Un.Expression);
      FResult := not FResult;
    end
    else if Un.UnaryOperator = uoIsNull then
    begin
      var Val := GetPropertyValue(Un.PropertyName);
      FResult := Val.IsEmpty or (Val.Kind = tkClass) and (Val.AsObject = nil) or (Val.Kind = tkInterface) and (Val.AsInterface = nil);
    end
    else if Un.UnaryOperator = uoIsNotNull then
    begin
      var Val := GetPropertyValue(Un.PropertyName);
      FResult := not (Val.IsEmpty or (Val.Kind = tkClass) and (Val.AsObject = nil) or (Val.Kind = tkInterface) and (Val.AsInterface = nil));
    end;
  end
  else if AExpression is TConstantExpression then
  begin
    FResult := TConstantExpression(AExpression).Value;
  end;
end;

end.

