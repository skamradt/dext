unit Dext.Specifications.Types;

interface

uses
  System.SysUtils,
  System.Rtti,
  Dext.Specifications.Interfaces;

type
  TAbstractCriterion = class(TInterfacedObject, ICriterion)
  public
    function ToString: string; override;
  end;

  /// <summary>
  ///   Represents a binary operator (Left Op Right).
  ///   e.g., Age > 18, Name = 'John'
  /// </summary>
  TBinaryOperator = (boEqual, boNotEqual, boGreaterThan, boGreaterThanOrEqual, 
    boLessThan, boLessThanOrEqual, boLike, boNotLike, boIn, boNotIn);

  TBinaryCriterion = class(TAbstractCriterion)
  private
    FPropertyName: string;
    FValue: TValue;
    FOperator: TBinaryOperator;
  public
    constructor Create(const APropertyName: string; AOperator: TBinaryOperator; const AValue: TValue);
    property PropertyName: string read FPropertyName;
    property Value: TValue read FValue;
    property Operator: TBinaryOperator read FOperator;
    function ToString: string; override;
  end;

  /// <summary>
  ///   Represents a logical operation (AND, OR).
  /// </summary>
  TLogicalOperator = (loAnd, loOr);

  TLogicalCriterion = class(TAbstractCriterion)
  private
    FLeft: ICriterion;
    FRight: ICriterion;
    FOperator: TLogicalOperator;
  public
    constructor Create(const ALeft, ARight: ICriterion; AOperator: TLogicalOperator);
    property Left: ICriterion read FLeft;
    property Right: ICriterion read FRight;
    property Operator: TLogicalOperator read FOperator;
    function ToString: string; override;
  end;

  /// <summary>
  ///   Represents a unary operation (NOT, IsNull, IsNotNull).
  /// </summary>
  TUnaryOperator = (uoNot, uoIsNull, uoIsNotNull);

  TUnaryCriterion = class(TAbstractCriterion)
  private
    FCriterion: ICriterion; // For NOT
    FPropertyName: string;  // For IsNull/IsNotNull
    FOperator: TUnaryOperator;
  public
    constructor Create(const ACriterion: ICriterion); overload; // For NOT
    constructor Create(const APropertyName: string; AOperator: TUnaryOperator); overload; // For IsNull
    
    property Criterion: ICriterion read FCriterion;
    property PropertyName: string read FPropertyName;
    property Operator: TUnaryOperator read FOperator;
    function ToString: string; override;
  end;

  /// <summary>
  ///   Represents a constant value (True/False) for always matching/not matching.
  /// </summary>
  TConstantCriterion = class(TAbstractCriterion)
  private
    FValue: Boolean;
  public
    constructor Create(AValue: Boolean);
    property Value: Boolean read FValue;
    function ToString: string; override;
  end;

  /// <summary>
  ///   Helper record to build criteria expressions fluently.
  ///   Usage: Prop('Age') > 18
  /// </summary>
  TProp = record
  public
    type
      /// <summary>
      ///   Represents an intermediate expression in the criteria tree.
      ///   Has implicit conversion to ICriterion.
      /// </summary>
      TExpr = record
      private
        FCriterion: ICriterion;
      public
        class operator Implicit(const Value: ICriterion): TExpr;
        class operator Implicit(const Value: TExpr): ICriterion;
        
        // Logical Operators (AND, OR, NOT) - return ICriterion like Spring4D
        class operator LogicalAnd(const Left, Right: TExpr): ICriterion;
        class operator LogicalOr(const Left, Right: TExpr): ICriterion;
        class operator LogicalNot(const Value: TExpr): ICriterion;
      end;
      
    var
      FName: string;
      
  public
    constructor Create(const AName: string);
    
    // Comparison Operators
    class operator Equal(const Left: TProp; const Right: TValue): TExpr;
    class operator NotEqual(const Left: TProp; const Right: TValue): TExpr;
    class operator GreaterThan(const Left: TProp; const Right: TValue): TExpr;
    class operator GreaterThanOrEqual(const Left: TProp; const Right: TValue): TExpr;
    class operator LessThan(const Left: TProp; const Right: TValue): TExpr;
    class operator LessThanOrEqual(const Left: TProp; const Right: TValue): TExpr;
    
    // Special Methods (Like, In, etc)
    function Like(const Pattern: string): ICriterion;
    function NotLike(const Pattern: string): ICriterion;
    function StartsWith(const Value: string): ICriterion;
    function EndsWith(const Value: string): ICriterion;
    function Contains(const Value: string): ICriterion;
    
    function &In(const Values: TArray<string>): ICriterion; overload;
    function &In(const Values: TArray<Integer>): ICriterion; overload;
    function NotIn(const Values: TArray<string>): ICriterion; overload;
    function NotIn(const Values: TArray<Integer>): ICriterion; overload;
    
    function IsNull: ICriterion;
    function IsNotNull: ICriterion;
    
    // Between as a method (not operator) like Spring4D
    function Between(const Lower, Upper: Variant): ICriterion;
  end;

implementation

{ TAbstractCriterion }

function TAbstractCriterion.ToString: string;
begin
  Result := ClassName;
end;

{ TBinaryCriterion }

constructor TBinaryCriterion.Create(const APropertyName: string;
  AOperator: TBinaryOperator; const AValue: TValue);
begin
  inherited Create;
  FPropertyName := APropertyName;
  FOperator := AOperator;
  FValue := AValue;
end;

function TBinaryCriterion.ToString: string;
begin
  Result := Format('(%s %d %s)', [FPropertyName, Ord(FOperator), FValue.ToString]);
end;

{ TLogicalCriterion }

constructor TLogicalCriterion.Create(const ALeft, ARight: ICriterion;
  AOperator: TLogicalOperator);
begin
  inherited Create;
  FLeft := ALeft;
  FRight := ARight;
  FOperator := AOperator;
end;

function TLogicalCriterion.ToString: string;
var
  OpStr: string;
begin
  if FOperator = loAnd then OpStr := 'AND' else OpStr := 'OR';
  Result := Format('(%s %s %s)', [FLeft.ToString, OpStr, FRight.ToString]);
end;

{ TUnaryCriterion }

constructor TUnaryCriterion.Create(const ACriterion: ICriterion);
begin
  inherited Create;
  FOperator := uoNot;
  FCriterion := ACriterion;
end;

constructor TUnaryCriterion.Create(const APropertyName: string;
  AOperator: TUnaryOperator);
begin
  inherited Create;
  FPropertyName := APropertyName;
  FOperator := AOperator;
end;

function TUnaryCriterion.ToString: string;
begin
  if FOperator = uoNot then
    Result := Format('(NOT %s)', [FCriterion.ToString])
  else if FOperator = uoIsNull then
    Result := Format('(%s IS NULL)', [FPropertyName])
  else
    Result := Format('(%s IS NOT NULL)', [FPropertyName]);
end;

{ TConstantCriterion }

constructor TConstantCriterion.Create(AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;

function TConstantCriterion.ToString: string;
begin
  Result := BoolToStr(FValue, True);
end;

{ TProp.TExpr }

class operator TProp.TExpr.Implicit(const Value: ICriterion): TExpr;
begin
  Result.FCriterion := Value;
end;

class operator TProp.TExpr.Implicit(const Value: TExpr): ICriterion;
begin
  Result := Value.FCriterion;
end;

class operator TProp.TExpr.LogicalAnd(const Left, Right: TExpr): ICriterion;
begin
  Result := TLogicalCriterion.Create(Left.FCriterion, Right.FCriterion, loAnd);
end;

class operator TProp.TExpr.LogicalOr(const Left, Right: TExpr): ICriterion;
begin
  Result := TLogicalCriterion.Create(Left.FCriterion, Right.FCriterion, loOr);
end;

class operator TProp.TExpr.LogicalNot(const Value: TExpr): ICriterion;
begin
  Result := TUnaryCriterion.Create(Value.FCriterion);
end;

{ TProp }

constructor TProp.Create(const AName: string);
begin
  FName := AName;
end;

class operator TProp.Equal(const Left: TProp; const Right: TValue): TExpr;
begin
  Result.FCriterion := TBinaryCriterion.Create(Left.FName, boEqual, Right);
end;

class operator TProp.NotEqual(const Left: TProp; const Right: TValue): TExpr;
begin
  Result.FCriterion := TBinaryCriterion.Create(Left.FName, boNotEqual, Right);
end;

class operator TProp.GreaterThan(const Left: TProp; const Right: TValue): TExpr;
begin
  Result.FCriterion := TBinaryCriterion.Create(Left.FName, boGreaterThan, Right);
end;

class operator TProp.GreaterThanOrEqual(const Left: TProp; const Right: TValue): TExpr;
begin
  Result.FCriterion := TBinaryCriterion.Create(Left.FName, boGreaterThanOrEqual, Right);
end;

class operator TProp.LessThan(const Left: TProp; const Right: TValue): TExpr;
begin
  Result.FCriterion := TBinaryCriterion.Create(Left.FName, boLessThan, Right);
end;

class operator TProp.LessThanOrEqual(const Left: TProp; const Right: TValue): TExpr;
begin
  Result.FCriterion := TBinaryCriterion.Create(Left.FName, boLessThanOrEqual, Right);
end;

function TProp.Like(const Pattern: string): ICriterion;
begin
  Result := TBinaryCriterion.Create(FName, boLike, Pattern);
end;

function TProp.NotLike(const Pattern: string): ICriterion;
begin
  Result := TBinaryCriterion.Create(FName, boNotLike, Pattern);
end;

function TProp.StartsWith(const Value: string): ICriterion;
begin
  Result := Like(Value + '%');
end;

function TProp.EndsWith(const Value: string): ICriterion;
begin
  Result := Like('%' + Value);
end;

function TProp.Contains(const Value: string): ICriterion;
begin
  Result := Like('%' + Value + '%');
end;

function TProp.&In(const Values: TArray<string>): ICriterion;
var
  Val: TValue;
begin
  Val := TValue.From<TArray<string>>(Values);
  Result := TBinaryCriterion.Create(FName, boIn, Val);
end;

function TProp.&In(const Values: TArray<Integer>): ICriterion;
var
  Val: TValue;
begin
  Val := TValue.From<TArray<Integer>>(Values);
  Result := TBinaryCriterion.Create(FName, boIn, Val);
end;

function TProp.NotIn(const Values: TArray<string>): ICriterion;
var
  Val: TValue;
begin
  Val := TValue.From<TArray<string>>(Values);
  Result := TBinaryCriterion.Create(FName, boNotIn, Val);
end;

function TProp.NotIn(const Values: TArray<Integer>): ICriterion;
var
  Val: TValue;
begin
  Val := TValue.From<TArray<Integer>>(Values);
  Result := TBinaryCriterion.Create(FName, boNotIn, Val);
end;

function TProp.IsNull: ICriterion;
begin
  Result := TUnaryCriterion.Create(FName, uoIsNull);
end;

function TProp.IsNotNull: ICriterion;
begin
  Result := TUnaryCriterion.Create(FName, uoIsNotNull);
end;

function TProp.Between(const Lower, Upper: Variant): ICriterion;
begin
  // (Prop >= Lower) AND (Prop <= Upper)
  // Create criteria manually to avoid operator ambiguity
  var LowerCrit: ICriterion := TBinaryCriterion.Create(FName, boGreaterThanOrEqual, TValue.FromVariant(Lower));
  var UpperCrit: ICriterion := TBinaryCriterion.Create(FName, boLessThanOrEqual, TValue.FromVariant(Upper));
  Result := TLogicalCriterion.Create(LowerCrit, UpperCrit, loAnd);
end;

end.
