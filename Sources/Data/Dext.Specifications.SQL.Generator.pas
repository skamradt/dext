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
unit Dext.Specifications.SQL.Generator;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  System.Variants,
  Data.DB,
  Dext.Specifications.Interfaces,
  Dext.Specifications.Types,
  Dext.Entity.Dialects,
  Dext.Entity.Attributes,
  Dext.Entity.Mapping,
  Dext.Entity.TypeConverters,
  Dext.Types.Nullable,
  Dext.Types.UUID,
  Dext.Entity.Cache;

type
  ISQLColumnMapper = interface
    ['{6C3E8F9A-1B2C-4D5E-9F0A-1B2C3D4E5F6A}']
    function MapColumn(const AName: string): string;
  end;

  /// <summary>
  ///   Translates a Expression Tree into a SQL WHERE clause and Parameters.
  /// </summary>
  TSQLWhereGenerator = class
  private
    FSQL: TStringBuilder;
    FParams: TDictionary<string, TValue>;
    FParamCount: Integer;
    FDialect: ISQLDialect;
    FColumnMapper: ISQLColumnMapper;
    
    procedure ProcessBinary(const C: TBinaryExpression);
    procedure ProcessArithmetic(const C: TArithmeticExpression);
    procedure ProcessLogical(const C: TLogicalExpression);
    procedure ProcessUnary(const C: TUnaryExpression);
    procedure ProcessConstant(const C: TConstantExpression);
    procedure ProcessProperty(const C: TPropertyExpression);
    procedure ProcessLiteral(const C: TLiteralExpression);

    procedure ResolveSQL(const AExpression: IExpression);
    
    function GetNextParamName: string;
    function GetBinaryOpSQL(Op: TBinaryOperator): string;
    function GetArithmeticOpSQL(Op: TArithmeticOperator): string;
    function GetLogicalOpSQL(Op: TLogicalOperator): string;
    function GetUnaryOpSQL(Op: TUnaryOperator): string;

    function MapColumn(const AName: string): string;
    function QuoteColumnOrAlias(const AName: string): string;
  public
    constructor Create(ADialect: ISQLDialect; AMapper: ISQLColumnMapper = nil);
    destructor Destroy; override;
    
    function Generate(const AExpression: IExpression): string;
    
    property Params: TDictionary<string, TValue> read FParams;
  end;

  TSQLColumnMapper<T: class> = class(TInterfacedObject, ISQLColumnMapper)
  public
    function MapColumn(const AName: string): string;
  end;

  TSQLGeneratorHelper = class
  public
    class function GetCascadeSQL(AAction: TCascadeAction): string;
    class function GetColumnNameForProperty(ATyp: TRttiType; const APropName: string): string;
    class function GetRelatedTableAndPK(ACtx: TRttiContext; AClass: TClass; out ATable, APK: string): Boolean;
  end;

  /// <summary>
  ///   Generates SQL for CRUD operations (Insert, Update, Delete).
  /// </summary>
  TSQLGenerator<T: class> = class
  private
    FDialect: ISQLDialect;
    FParams: TDictionary<string, TValue>;
    FParamCount: Integer;
    FMap: TEntityMap;
    // Properties to control filtering
    FIgnoreQueryFilters: Boolean;
    FOnlyDeleted: Boolean;
    FSchema: string;

    function GetNextParamName: string;
    function GetTableName: string;
    function GetSoftDeleteFilter: string;
    function GetDiscriminatorFilter: string;
    function GetDiscriminatorValueSQL: string;

    function GetDialectEnum: TDatabaseDialect;
    function GetJoinTypeSQL(AType: TJoinType): string;
    function GenerateJoins(const AJoins: TArray<IJoin>): string;
    function GenerateGroupBy(const AGroupBy: TArray<string>): string;
    function QuoteColumnOrAlias(const AName: string): string;

  public
    constructor Create(ADialect: ISQLDialect; AMap: TEntityMap = nil);
    destructor Destroy; override;
    
    property IgnoreQueryFilters: Boolean read FIgnoreQueryFilters write FIgnoreQueryFilters;
    property OnlyDeleted: Boolean read FOnlyDeleted write FOnlyDeleted;
    property Schema: string read FSchema write FSchema;
    
    function GenerateInsert(const AEntity: T): string;
    function GenerateInsertTemplate(out AProps: TList<TPair<TRttiProperty, string>>): string;
    function GenerateUpdate(const AEntity: T): string;
    function GenerateDelete(const AEntity: T): string;
    
    function GenerateSelect(const ASpec: ISpecification<T>): string; overload;
    function GenerateSelect: string; overload;
    function GenerateCount(const ASpec: ISpecification<T>): string; overload;
    function GenerateCount: string; overload;
    function GenerateCreateTable(const ATableName: string): string;
    
    property Params: TDictionary<string, TValue> read FParams;
  end;

  TSQLParamCollector = class
  private
    FParams: TDictionary<string, TValue>;
    FParamCount: Integer;

    function GetNextParamName: string;
    procedure Resolve(const Ex: IExpression);
    procedure ProcessBinary(const C: TBinaryExpression);
    procedure ProcessArithmetic(const C: TArithmeticExpression);
    procedure ProcessLogical(const C: TLogicalExpression);
    procedure ProcessUnary(const C: TUnaryExpression);
    procedure ProcessLiteral(const C: TLiteralExpression);
  public
    constructor Create(AParams: TDictionary<string, TValue>);
    procedure Collect(const AExpression: IExpression);
  end;

implementation

{ TSQLParamCollector }

constructor TSQLParamCollector.Create(AParams: TDictionary<string, TValue>);
begin
  FParams := AParams;
  FParamCount := 0; 
end;

function TSQLParamCollector.GetNextParamName: string;
begin
  Inc(FParamCount);
  Result := 'p' + IntToStr(FParamCount);
end;

procedure TSQLParamCollector.Collect(const AExpression: IExpression);
begin
  if AExpression <> nil then
    Resolve(AExpression);
end;

procedure TSQLParamCollector.Resolve(const Ex: IExpression);
begin
  if Ex is TBinaryExpression then
    ProcessBinary(TBinaryExpression(Ex))
  else if Ex is TLogicalExpression then
    ProcessLogical(TLogicalExpression(Ex))
  else if Ex is TUnaryExpression then
    ProcessUnary(TUnaryExpression(Ex))
  else if Ex is TArithmeticExpression then
    ProcessArithmetic(TArithmeticExpression(Ex))
  else if Ex is TConstantExpression then
    // No params
  else if Ex is TPropertyExpression then
    // No params
  else if Ex is TLiteralExpression then
    ProcessLiteral(TLiteralExpression(Ex))
  else
    raise Exception.Create('Unknown expression type in ParamCollector: ' + Ex.ToString);
end;

procedure TSQLParamCollector.ProcessBinary(const C: TBinaryExpression);
var
  I: Integer;
  ArrayValue: TValue;
begin
  // Standard traversal order must match TSQLWhereGenerator exactly:
  // 1. IN/NOT IN with Array -> Loop elements
  // 2. Others -> Resolve Left -> Resolve Right (or Literal)
  
  if (C.BinaryOperator = boIn) or (C.BinaryOperator = boNotIn) then
  begin
    if (C.Right is TLiteralExpression) and TLiteralExpression(C.Right).Value.IsArray then
    begin
       ArrayValue := TLiteralExpression(C.Right).Value;
       for I := 0 to ArrayValue.GetArrayLength - 1 do
       begin
         FParams.Add(GetNextParamName, ArrayValue.GetArrayElement(I));
       end;
       Exit;
    end;
  end;
  
  Resolve(C.Left);
  
  if C.Right is TLiteralExpression then
  begin
    FParams.Add(GetNextParamName, TLiteralExpression(C.Right).Value);
  end
  else
    Resolve(C.Right);
end;

procedure TSQLParamCollector.ProcessArithmetic(const C: TArithmeticExpression);
begin
  Resolve(C.Left);
  Resolve(C.Right);
end;

procedure TSQLParamCollector.ProcessLogical(const C: TLogicalExpression);
begin
  Resolve(C.Left);
  Resolve(C.Right);
end;

procedure TSQLParamCollector.ProcessUnary(const C: TUnaryExpression);
begin
  if C.UnaryOperator = uoNot then
    Resolve(C.Expression)
  else
    // IsNull/IsNotNull has no params (uses PropertyName)
    ;
end;

procedure TSQLParamCollector.ProcessLiteral(const C: TLiteralExpression);
begin
  FParams.Add(GetNextParamName, C.Value);
end;

{ TSQLGeneratorHelper }

class function TSQLGeneratorHelper.GetCascadeSQL(AAction: TCascadeAction): string;
begin
  case AAction of
    caCascade: Result := 'CASCADE';
    caSetNull: Result := 'SET NULL';
    caRestrict: Result := 'RESTRICT';
    else Result := 'NO ACTION';
  end;
end;

class function TSQLGeneratorHelper.GetColumnNameForProperty(ATyp: TRttiType; const APropName: string): string;
var
  P: TRttiProperty;
  A: TCustomAttribute;
begin
  Result := APropName; // Default
  P := ATyp.GetProperty(APropName);
  if P <> nil then
  begin
    for A in P.GetAttributes do
    begin
      if A is ColumnAttribute then Exit(ColumnAttribute(A).Name);
    end;
  end;
end;

class function TSQLGeneratorHelper.GetRelatedTableAndPK(ACtx: TRttiContext; AClass: TClass; out ATable, APK: string): Boolean;
var
  RTyp: TRttiType;
  RProp: TRttiProperty;
  RAttr, SubAttr: TCustomAttribute;
begin
  Result := False;
  RTyp := ACtx.GetType(AClass);
  if RTyp = nil then Exit;
  
  // Table Name
  ATable := RTyp.Name;
  for RAttr in RTyp.GetAttributes do
    if RAttr is TableAttribute then
      ATable := TableAttribute(RAttr).Name;
      
  // PK
  for RProp in RTyp.GetProperties do
  begin
    for RAttr in RProp.GetAttributes do
    begin
      if RAttr is PKAttribute then
      begin
        APK := RProp.Name;
        // Check for Column Attribute on PK
        for SubAttr in RProp.GetAttributes do
          if SubAttr is ColumnAttribute then
            APK := ColumnAttribute(SubAttr).Name;
        Exit(True);
      end;
    end;
  end;
  
  // Fallback to 'Id'
  RProp := RTyp.GetProperty('Id');
  if RProp <> nil then
  begin
    APK := 'Id';
    for RAttr in RProp.GetAttributes do
      if RAttr is ColumnAttribute then
        APK := ColumnAttribute(RAttr).Name;
    Exit(True);
  end;
end;

{ TSQLWhereGenerator }

constructor TSQLWhereGenerator.Create(ADialect: ISQLDialect; AMapper: ISQLColumnMapper = nil);
begin
  FSQL := TStringBuilder.Create;
  FParams := TDictionary<string, TValue>.Create;
  FParamCount := 0;
  FDialect := ADialect;
  FColumnMapper := AMapper;
end;

destructor TSQLWhereGenerator.Destroy;
begin
  FSQL.Free;
  FParams.Free;
  inherited;
end;

function TSQLWhereGenerator.MapColumn(const AName: string): string;
begin
  if FColumnMapper <> nil then
    Result := FColumnMapper.MapColumn(AName)
  else
    Result := AName;
end;

function TSQLWhereGenerator.QuoteColumnOrAlias(const AName: string): string;
var
  Parts: TArray<string>;
  i: Integer;
begin
  if AName.Contains('.') then
  begin
    Parts := AName.Split(['.']);
    Result := '';
    for i := 0 to High(Parts) do
    begin
      if i > 0 then Result := Result + '.';
      Result := Result + FDialect.QuoteIdentifier(Parts[i]);
    end;
  end
  else
    Result := FDialect.QuoteIdentifier(AName);
end;

function TSQLWhereGenerator.Generate(const AExpression: IExpression): string;
begin
  FSQL.Clear;
  FParams.Clear;
  FParamCount := 0;
  
  if AExpression = nil then
    Exit('');
    
  ResolveSQL(AExpression);
  Result := FSQL.ToString;
end;

function TSQLWhereGenerator.GetNextParamName: string;
begin
  Inc(FParamCount);
  Result := 'p' + IntToStr(FParamCount);
end;

procedure TSQLWhereGenerator.ResolveSQL(const AExpression: IExpression);
begin
  if AExpression is TBinaryExpression then
    ProcessBinary(TBinaryExpression(AExpression))
  else if AExpression is TArithmeticExpression then
    ProcessArithmetic(TArithmeticExpression(AExpression))
  else if AExpression is TLogicalExpression then
    ProcessLogical(TLogicalExpression(AExpression))
  else if AExpression is TUnaryExpression then
    ProcessUnary(TUnaryExpression(AExpression))
  else if AExpression is TConstantExpression then
    ProcessConstant(TConstantExpression(AExpression))
  else if AExpression is TPropertyExpression then
    ProcessProperty(TPropertyExpression(AExpression))
  else if AExpression is TLiteralExpression then
    ProcessLiteral(TLiteralExpression(AExpression))
  else
    raise Exception.Create('Unknown expression type: ' + AExpression.ToString);
end;

procedure TSQLWhereGenerator.ProcessBinary(const C: TBinaryExpression);
var
  ParamName: string;
  ArrayValue: TValue;
  I: Integer;
  ParamNames: TStringBuilder;
  SQLCast: string;
  Converter: ITypeConverter;
  DialectEnum: TDatabaseDialect;
  Quoted: string;
begin
  // Special handling for IN and NOT IN operators
  if (C.BinaryOperator = boIn) or (C.BinaryOperator = boNotIn) then
  begin
    if (C.Right is TLiteralExpression) and TLiteralExpression(C.Right).Value.IsArray then
    begin
      ArrayValue := TLiteralExpression(C.Right).Value;
      ParamNames := TStringBuilder.Create;
      try
        for I := 0 to ArrayValue.GetArrayLength - 1 do
        begin
          ParamName := GetNextParamName;
          FParams.Add(ParamName, ArrayValue.GetArrayElement(I));
          if I > 0 then ParamNames.Append(', ');
          ParamNames.Append(':').Append(ParamName);
        end;
        
        FSQL.Append('(');
        ResolveSQL(C.Left);
        FSQL.Append(' ')
            .Append(GetBinaryOpSQL(C.BinaryOperator))
            .Append(' (')
            .Append(ParamNames.ToString)
            .Append('))');
      finally
        ParamNames.Free;
      end;
      Exit;
    end;
  end;

  FSQL.Append('(');
  ResolveSQL(C.Left);
  FSQL.Append(' ')
      .Append(GetBinaryOpSQL(C.BinaryOperator))
      .Append(' ');

  if C.Right is TLiteralExpression then
  begin
    var Lit := TLiteralExpression(C.Right);
    ParamName := GetNextParamName;
    FParams.Add(ParamName, Lit.Value);
    
    // Type converter support for SQL casting in WHERE clause
    Converter := TTypeConverterRegistry.Instance.GetConverter(Lit.Value.TypeInfo);
    
    // Determine Dialect Enum
    DialectEnum := ddUnknown;
    Quoted := FDialect.QuoteIdentifier('t');
    if Quoted.StartsWith('[') then DialectEnum := ddSQLServer
    else if Quoted.StartsWith('`') then DialectEnum := ddMySQL
    else if Quoted.StartsWith('"') then
    begin
       if SameText(FDialect.BooleanToSQL(True), 'TRUE') then DialectEnum := ddPostgreSQL
       else DialectEnum := ddSQLite;
    end;
    
    if (DialectEnum = ddPostgreSQL) and 
       ((Lit.Value.TypeInfo = TypeInfo(TGUID)) or 
        (Lit.Value.TypeInfo = TypeInfo(TUUID)) or
        ((Lit.Value.Kind in [tkString, tkUString, tkWString]) and
         ((Length(Lit.Value.AsString) = 36) or (Length(Lit.Value.AsString) = 38)) and
         (Lit.Value.AsString.IndexOf('-') > 0))) then
    begin
       SQLCast := ':' + ParamName + '::uuid';
    end
    else if Converter <> nil then
    begin
      SQLCast := Converter.GetSQLCast(':' + ParamName, DialectEnum);
    end
    else
      SQLCast := ':' + ParamName;

    FSQL.Append(SQLCast);
  end
  else
    ResolveSQL(C.Right);

  FSQL.Append(')');
end;

procedure TSQLWhereGenerator.ProcessArithmetic(const C: TArithmeticExpression);
begin
  FSQL.Append('(');
  ResolveSQL(C.Left);
  FSQL.Append(' ')
      .Append(GetArithmeticOpSQL(C.ArithmeticOperator))
      .Append(' ');
  ResolveSQL(C.Right);
  FSQL.Append(')');
end;

procedure TSQLWhereGenerator.ProcessLogical(const C: TLogicalExpression);
begin
  FSQL.Append('(');
  ResolveSQL(C.Left);
  FSQL.Append(' ')
      .Append(GetLogicalOpSQL(C.LogicalOperator))
      .Append(' ');
  ResolveSQL(C.Right);
  FSQL.Append(')');
end;

procedure TSQLWhereGenerator.ProcessUnary(const C: TUnaryExpression);
begin
  if C.UnaryOperator = uoNot then
  begin
    FSQL.Append('(NOT ');
    ResolveSQL(C.Expression);
    FSQL.Append(')');
  end
  else
  begin
    // IsNull / IsNotNull
    FSQL.Append('(')
        .Append(QuoteColumnOrAlias(MapColumn(C.PropertyName)))
        .Append(' ')
        .Append(GetUnaryOpSQL(C.UnaryOperator))
        .Append(')');
  end;
end;

procedure TSQLWhereGenerator.ProcessProperty(const C: TPropertyExpression);
begin
  FSQL.Append(QuoteColumnOrAlias(MapColumn(C.PropertyName)));
end;

procedure TSQLWhereGenerator.ProcessLiteral(const C: TLiteralExpression);
var
  ParamName: string;
begin
  ParamName := GetNextParamName;
  FParams.Add(ParamName, C.Value);
  FSQL.Append(':').Append(ParamName);
end;

procedure TSQLWhereGenerator.ProcessConstant(const C: TConstantExpression);
begin
  if C.Value then
    FSQL.Append('(1=1)')
  else
    FSQL.Append('(1=0)');
end;

function TSQLWhereGenerator.GetBinaryOpSQL(Op: TBinaryOperator): string;
begin
  case Op of
    boEqual: Result := '=';
    boNotEqual: Result := '<>';
    boGreaterThan: Result := '>';
    boGreaterThanOrEqual: Result := '>=';
    boLessThan: Result := '<';
    boLessThanOrEqual: Result := '<=';
    boLike: Result := 'LIKE';
    boNotLike: Result := 'NOT LIKE';
    boIn: Result := 'IN';
    boNotIn: Result := 'NOT IN';
    boBitwiseAnd: Result := '&';
    boBitwiseOr: Result := '|';
    boBitwiseXor: Result := '#';
  else
    Result := '=';
  end;
end;

function TSQLWhereGenerator.GetArithmeticOpSQL(Op: TArithmeticOperator): string;
begin
  case Op of
    aoAdd: Result := '+';
    aoSubtract: Result := '-';
    aoMultiply: Result := '*';
    aoDivide: Result := '/';
    aoModulus: Result := '%';
    aoIntDivide: Result := '/';
  else
    Result := '+';
  end;
end;

function TSQLWhereGenerator.GetLogicalOpSQL(Op: TLogicalOperator): string;
begin
  case Op of
    loAnd: Result := 'AND';
    loOr: Result := 'OR';
  else
    Result := 'AND';
  end;
end;

function TSQLWhereGenerator.GetUnaryOpSQL(Op: TUnaryOperator): string;
begin
  case Op of
    uoIsNull: Result := 'IS NULL';
    uoIsNotNull: Result := 'IS NOT NULL';
  else
    Result := '';
  end;
end;



{ TSQLColumnMapper<T> }

function TSQLColumnMapper<T>.MapColumn(const AName: string): string;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  Attr: TCustomAttribute;
begin
  Result := AName;
  Ctx := TRttiContext.Create;
  Typ := Ctx.GetType(T);
  Prop := Typ.GetProperty(AName);
  if Prop <> nil then
  begin
    for Attr in Prop.GetAttributes do
    begin
      if Attr is ColumnAttribute then Exit(ColumnAttribute(Attr).Name);
      if Attr is ForeignKeyAttribute then Exit(ForeignKeyAttribute(Attr).ColumnName);
    end;
  end;
end;

{ TSQLGenerator<T> }

constructor TSQLGenerator<T>.Create(ADialect: ISQLDialect; AMap: TEntityMap = nil);
begin
  FDialect := ADialect;
  FMap := AMap;
  if FMap = nil then
    FMap := TModelBuilder.Instance.GetMap(TypeInfo(T));
  FParams := TDictionary<string, TValue>.Create;
  FParamCount := 0;
end;

destructor TSQLGenerator<T>.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TSQLGenerator<T>.GetNextParamName: string;
begin
  Inc(FParamCount);
  Result := 'p' + IntToStr(FParamCount);
end;

function TSQLGenerator<T>.GetTableName: string;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Attr: TCustomAttribute;
begin
  if (FMap <> nil) and (FMap.TableName <> '') then
    Result := FMap.TableName
  else
  begin
    // Fallback should not happen if map is always created, 
    // but keep logic just in case or simpler:
    Result := FMap.TableName;
    if Result = '' then
    begin
       Ctx := TRttiContext.Create;
       Typ := Ctx.GetType(T);
       Result := Typ.Name;
       for Attr in Typ.GetAttributes do
          if Attr is TableAttribute then
            Result := TableAttribute(Attr).Name;
    end;
  end;

  Result := FDialect.QuoteIdentifier(Result);
  
  if (FSchema <> '') and FDialect.UseSchemaPrefix then
    Result := FDialect.QuoteIdentifier(FSchema) + '.' + Result;
end;

function TSQLGenerator<T>.GetSoftDeleteFilter: string;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Attr: TCustomAttribute;
  SoftDeleteAttr: SoftDeleteAttribute;
  Prop: TRttiProperty;
  ColumnName: string;
  PropName: string;
  DeletedVal, NotDeletedVal: Variant;
  IsSoftDelete: Boolean;
  PropMap: TPropertyMap;
  TargetPropType: PTypeInfo;
begin
  Result := '';
  IsSoftDelete := False;
  PropName := '';
  DeletedVal := True;
  NotDeletedVal := False;
  TargetPropType := nil;
  
  Ctx := TRttiContext.Create;
  Typ := Ctx.GetType(T);
  if Typ = nil then Exit;
  
  // 1. Check Fluent Mapping
  if (FMap <> nil) and FMap.IsSoftDelete then
  begin
    IsSoftDelete := True;
    PropName := FMap.SoftDeleteProp;
    DeletedVal := FMap.SoftDeleteDeletedValue;
    NotDeletedVal := FMap.SoftDeleteNotDeletedValue;
  end
   // 2. Check Attribute
  else
  begin
    for Attr in Typ.GetAttributes do
    begin
      if Attr is SoftDeleteAttribute then
      begin
        SoftDeleteAttr := SoftDeleteAttribute(Attr);
        IsSoftDelete := True;
        PropName := SoftDeleteAttr.ColumnName; // This is actually the COLUMN NAME in the attribute
        DeletedVal := SoftDeleteAttr.DeletedValue;
        NotDeletedVal := SoftDeleteAttr.NotDeletedValue;
        Break;
      end;
    end;
  end;

  if not IsSoftDelete then Exit;

  // Find actual column name and Property Type
  // Note: PropName from attribute is the COLUMN NAME usually, but let's try to match property first
  ColumnName := PropName;

  // Searching for the property that maps to this column (or has this name)
  for Prop in Typ.GetProperties do
  begin
    var PropColumnName: string := Prop.Name;

    // Check Prop Map
    if (FMap <> nil) and FMap.Properties.TryGetValue(Prop.Name, PropMap) then
    begin
       if PropMap.ColumnName <> '' then PropColumnName := PropMap.ColumnName;
    end
    else
    begin
      // Check Column Attribute
      for Attr in Prop.GetAttributes do
      begin
        if Attr is ColumnAttribute then
        begin
          PropColumnName := ColumnAttribute(Attr).Name;
          Break;
        end;
      end;
    end;

    // Match found if Property Name OR Column Name matches
    if SameText(Prop.Name, PropName) or SameText(PropColumnName, PropName) then
    begin
      ColumnName := PropColumnName;
      TargetPropType := Prop.PropertyType.Handle;
      Break;
    end;
  end;

  if FIgnoreQueryFilters then Exit;

  // Generate filter (Start)
  // NOTE: We use Literals instead of Parameters (pSoftDelete) to ensure compatibility with SQL Caching.
  // Cached queries re-hydrate parameters from the Specification, which does not contain System Filters.
  // Since SoftDelete values are metadata-driven (static constants), using literals is safe.

  if FOnlyDeleted then
  begin
     // ---------------------------------------------------------
     // Case 1: Show ONLY Deleted items
     // ---------------------------------------------------------

     // Determine Literal Value for "Deleted" state
     var LiteralVal: string;

     var IsTargetBool := (TargetPropType <> nil) and
       ((TargetPropType = TypeInfo(Boolean)) or (SameText(string(TargetPropType.Name), 'Boolean')));

     if IsTargetBool then
     begin
       var BoolVal: Boolean;
       if VarIsType(DeletedVal, varBoolean) then
         BoolVal := DeletedVal
       else if VarIsNumeric(DeletedVal) then
         BoolVal := (Integer(DeletedVal) <> 0)
       else
         BoolVal := StrToBoolDef(VarToStr(DeletedVal), True);
       
       LiteralVal := FDialect.BooleanToSQL(BoolVal);
     end
     else if VarIsType(DeletedVal, varBoolean) then
     begin
        LiteralVal := FDialect.BooleanToSQL(DeletedVal);
     end
     else if VarIsNull(DeletedVal) then
     begin
        LiteralVal := 'NULL';
     end
     else
     begin
       LiteralVal := VarToStr(DeletedVal);
       if VarIsType(DeletedVal, varString) or VarIsType(DeletedVal, varUString) then
         LiteralVal := QuotedStr(LiteralVal);
     end;

     if LiteralVal = 'NULL' then
       Result := Format('%s IS NULL', [FDialect.QuoteIdentifier(ColumnName)])
     else
       Result := Format('%s = %s', [FDialect.QuoteIdentifier(ColumnName), LiteralVal]);
  end
  else
  begin
     // ---------------------------------------------------------
     // Case 2: Show ONLY Active (Not Deleted) items (Default)
     // ---------------------------------------------------------
     
     var LiteralVal: string;
     var IsTargetBool := (TargetPropType <> nil) and
       ((TargetPropType = TypeInfo(Boolean)) or (SameText(string(TargetPropType.Name), 'Boolean')));

     if IsTargetBool then
     begin
       var BoolVal: Boolean;
       if VarIsType(NotDeletedVal, varBoolean) then
         BoolVal := NotDeletedVal
       else if VarIsNumeric(NotDeletedVal) then
         BoolVal := (Integer(NotDeletedVal) <> 0)
       else
         BoolVal := StrToBoolDef(VarToStr(NotDeletedVal), False);
       
       LiteralVal := FDialect.BooleanToSQL(BoolVal);
     end
     else if VarIsType(NotDeletedVal, varBoolean) then
     begin
       LiteralVal := FDialect.BooleanToSQL(NotDeletedVal);
     end
     else if VarIsNull(NotDeletedVal) then
     begin
        LiteralVal := 'NULL';
     end
     else
     begin
       LiteralVal := VarToStr(NotDeletedVal);
       if VarIsType(NotDeletedVal, varString) or VarIsType(NotDeletedVal, varUString) then
         LiteralVal := QuotedStr(LiteralVal);
     end;
   
     // For PostgreSQL/Boolean, COALESCE requires consistent types
     // But since we are using literals, strict typing is handled by the dialect's SQL syntax
     if LiteralVal = 'NULL' then
        Result := Format('%s IS NULL', [FDialect.QuoteIdentifier(ColumnName)])
     else
        Result := Format('COALESCE(%s, %s) = %s',
          [FDialect.QuoteIdentifier(ColumnName), LiteralVal, LiteralVal]);
  end;
end;

function TSQLGenerator<T>.GetDiscriminatorValueSQL: string;
begin
  if (FMap <> nil) and (FMap.DiscriminatorValue <> Null) then
  begin
    if VarIsType(FMap.DiscriminatorValue, varString) or VarIsType(FMap.DiscriminatorValue, varUString) then
      Result := QuotedStr(VarToStr(FMap.DiscriminatorValue))
    else
      Result := VarToStr(FMap.DiscriminatorValue);
  end
  else
    Result := 'NULL';
end;

function TSQLGenerator<T>.GetDiscriminatorFilter: string;
begin
  Result := '';
  if (FMap <> nil) and (FMap.InheritanceStrategy = TInheritanceStrategy.TablePerHierarchy) and 
     (FMap.DiscriminatorColumn <> '') and (GetDiscriminatorValueSQL <> 'NULL') then
  begin
    Result := FDialect.QuoteIdentifier(FMap.DiscriminatorColumn) + ' = ' + GetDiscriminatorValueSQL;
    // Note: DiscriminatorColumn should be quoted if needed, but FMap usually has raw name. 
    // Ideally use FDialect.QuoteIdentifier(FMap.DiscriminatorColumn).
    // Let's fix that below.
  end;
end;

function TSQLGenerator<T>.GetDialectEnum: TDatabaseDialect;
begin
  if FDialect <> nil then
    Result := FDialect.GetDialect
  else
    Result := ddUnknown;
end;

function TSQLGenerator<T>.GenerateInsert(const AEntity: T): string;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  Attr: TCustomAttribute;
  ColName, ParamName: string;
  SBCols, SBVals: TStringBuilder;
  IsAutoInc, IsMapped, First: Boolean;
  Val: TValue;
  SQLCast: string;
  Converter: ITypeConverter;
  NullableHelper: TNullableHelper;
  PropMap: TPropertyMap;
begin
  FParams.Clear;
  FParamCount := 0;
  
  Ctx := TRttiContext.Create;
  Typ := Ctx.GetType(T);
  
  SBCols := TStringBuilder.Create;
  SBVals := TStringBuilder.Create;
  try
    First := True;
    
    for Prop in Typ.GetProperties do
    begin
      IsMapped := True;
      IsAutoInc := False;
      ColName := Prop.Name;
      
      PropMap := nil;
      if FMap <> nil then
        FMap.Properties.TryGetValue(Prop.Name, PropMap);
        
      if PropMap <> nil then
      begin
        if PropMap.IsIgnored then IsMapped := False;
        if PropMap.IsAutoInc then IsAutoInc := True;
        if PropMap.ColumnName <> '' then ColName := PropMap.ColumnName;
      end;

      for Attr in Prop.GetAttributes do
      begin
        if Attr is NotMappedAttribute then IsMapped := False;
        
        if (PropMap = nil) or not PropMap.IsAutoInc then
          if Attr is AutoIncAttribute then IsAutoInc := True;
          
        if (PropMap = nil) or (PropMap.ColumnName = '') then
        begin
          if Attr is ColumnAttribute then ColName := ColumnAttribute(Attr).Name;
          if Attr is ForeignKeyAttribute then ColName := ForeignKeyAttribute(Attr).ColumnName;
        end;
      end;
      
      if not IsMapped or IsAutoInc then Continue;
      
      if not First then
      begin
        SBCols.Append(', ');
        SBVals.Append(', ');
      end;
      First := False;
      
      SBCols.Append(FDialect.QuoteIdentifier(ColName));
      
      
      Val := Prop.GetValue(Pointer(AEntity));
      
      // Check for Nullable<T>
      if IsNullable(Val.TypeInfo) then
      begin
        NullableHelper := TNullableHelper.Create(Val.TypeInfo);
        if not NullableHelper.HasValue(Val.GetReferenceToRawData) then
        begin
          // It is NULL.
          SBVals.Append('NULL');
          Continue; // Skip adding parameter
        end
        else
        begin
          // It has value. Extract it.
          Val := NullableHelper.GetValue(Val.GetReferenceToRawData);
        end;
      end;

      ParamName := GetNextParamName;
      
      // Check if there's a type converter that needs SQL casting
      if PropMap <> nil then
        Converter := PropMap.Converter
      else
        Converter := TTypeConverterRegistry.Instance.GetConverter(Val.TypeInfo);
      if Converter <> nil then
      begin
        SQLCast := Converter.GetSQLCast(':' + ParamName, GetDialectEnum);
        SBVals.Append(SQLCast);
      end
      else
        SBVals.Append(':').Append(ParamName);
        
      FParams.Add(ParamName, Val);
    end;
    
    // Add Discriminator
    if (FMap <> nil) and (FMap.InheritanceStrategy = TInheritanceStrategy.TablePerHierarchy) and 
       (FMap.DiscriminatorColumn <> '') then
    begin
       // Check if already added? (Optimization: explicit loop check or just trust map setup usually implies shadow)
       // For now, simpler: assume shadow property if configured via HasDiscriminator
       // We should ideally check if ColName/Prop above handled it.
       // But HasDiscriminator implies it's metadata driven.
       // If a property maps to it, IsMapped would be true.
       // We can check if SBCols contains the column name... but string search is flaky.
       // Let's rely on standard practice: If users map it properly, they shouldn't use HasDiscriminator with const value?
       // Actually HasDiscriminator SETS the value.
       
       // Safe Add:
       if not SBCols.ToString.Contains(FDialect.QuoteIdentifier(FMap.DiscriminatorColumn)) then
       begin
         if SBCols.Length > 0 then
         begin
           SBCols.Append(', ');
           SBVals.Append(', ');
         end;
         SBCols.Append(FDialect.QuoteIdentifier(FMap.DiscriminatorColumn));
         SBVals.Append(GetDiscriminatorValueSQL);
       end;
    end;

    Result := Format('INSERT INTO %s (%s) VALUES (%s)', 
      [GetTableName, SBCols.ToString, SBVals.ToString]);
      
  finally
    SBCols.Free;
    SBVals.Free;
  end;
end;

function TSQLGenerator<T>.GenerateInsertTemplate(out AProps: TList<TPair<TRttiProperty, string>>): string;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  Attr: TCustomAttribute;
  ColName: string;
  SBCols, SBVals: TStringBuilder;
  IsAutoInc, IsMapped, First: Boolean;
  PropMap: TPropertyMap;
begin
  Ctx := TRttiContext.Create;
  Typ := Ctx.GetType(T);
  
  SBCols := TStringBuilder.Create;
  SBVals := TStringBuilder.Create;
  AProps := TList<TPair<TRttiProperty, string>>.Create;
  
  try
    First := True;

    for Prop in Typ.GetProperties do
    begin
      IsMapped := True;
      IsAutoInc := False;
      ColName := Prop.Name;
      
      PropMap := nil;
      if FMap <> nil then
        FMap.Properties.TryGetValue(Prop.Name, PropMap);
        
      if PropMap <> nil then
      begin
        if PropMap.IsIgnored then IsMapped := False;
        if PropMap.IsAutoInc then IsAutoInc := True;
        if PropMap.ColumnName <> '' then ColName := PropMap.ColumnName;
      end;

      for Attr in Prop.GetAttributes do
      begin
        if Attr is NotMappedAttribute then IsMapped := False;
        
        if (PropMap = nil) or not PropMap.IsAutoInc then
          if Attr is AutoIncAttribute then IsAutoInc := True;
          
        if (PropMap = nil) or (PropMap.ColumnName = '') then
        begin
          if Attr is ColumnAttribute then ColName := ColumnAttribute(Attr).Name;
          if Attr is ForeignKeyAttribute then ColName := ForeignKeyAttribute(Attr).ColumnName;
        end;
      end;
      
      if not IsMapped or IsAutoInc then Continue;
      
      AProps.Add(TPair<TRttiProperty, string>.Create(Prop, ColName));
      
      if not First then
      begin
        SBCols.Append(', ');
        SBVals.Append(', ');
      end;
      First := False;
      
      SBCols.Append(FDialect.QuoteIdentifier(ColName));
      // Use Column Name as Parameter Name for Array DML
      SBVals.Append(':').Append(ColName); 
    end;
    
    Result := Format('INSERT INTO %s (%s) VALUES (%s)', 
      [GetTableName, SBCols.ToString, SBVals.ToString]);
      
  finally
    SBCols.Free;
    SBVals.Free;
    // AProps is returned, caller must free
  end;
end;

function TSQLGenerator<T>.GenerateUpdate(const AEntity: T): string;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  Attr: TCustomAttribute;
  ColName, ParamName, ParamNameNew: string;
  SBSet, SBWhere: TStringBuilder;
  IsPK, IsMapped, IsVersion: Boolean;
  Val: TValue;
  NewVersionVal: Integer;
  FirstSet, FirstWhere: Boolean;
  PropMap: TPropertyMap;
  SQLCastStr: string;
  Converter: ITypeConverter;
begin
  FParams.Clear;
  FParamCount := 0;
  
  Ctx := TRttiContext.Create;
  Typ := Ctx.GetType(T);
  
  SBSet := TStringBuilder.Create;
  SBWhere := TStringBuilder.Create;
  try
    FirstSet := True;
    FirstWhere := True;
    
    for Prop in Typ.GetProperties do
    begin
      IsMapped := True;
      IsPK := False;
      IsVersion := False;
      ColName := Prop.Name;
      
      PropMap := nil;
      if FMap <> nil then
        FMap.Properties.TryGetValue(Prop.Name, PropMap);
        
      if PropMap <> nil then
      begin
        if PropMap.IsIgnored then IsMapped := False;
        if PropMap.IsPK then IsPK := True;
        // Version not yet supported in Fluent Mapping explicitly? Assuming no for now or check map.
        if PropMap.ColumnName <> '' then ColName := PropMap.ColumnName;
      end;

      for Attr in Prop.GetAttributes do
      begin
        if Attr is NotMappedAttribute then IsMapped := False;
        
        if (PropMap = nil) or not PropMap.IsPK then
          if Attr is PKAttribute then IsPK := True;
          
        if Attr is VersionAttribute then IsVersion := True; // Version attribute still respected
        
        if (PropMap = nil) or (PropMap.ColumnName = '') then
        begin
          if Attr is ColumnAttribute then ColName := ColumnAttribute(Attr).Name;
          if Attr is ForeignKeyAttribute then ColName := ForeignKeyAttribute(Attr).ColumnName;
        end;
      end;
      
      if not IsMapped then Continue;
      
      Val := Prop.GetValue(Pointer(AEntity));
      
      if IsVersion then
      begin
        // Optimistic Concurrency Logic
        
        // 1. Add to WHERE clause: Version = :OldVersion
        ParamName := GetNextParamName;
        FParams.Add(ParamName, Val);
        
        if not FirstWhere then SBWhere.Append(' AND ');
        FirstWhere := False;
        SBWhere.Append(FDialect.QuoteIdentifier(ColName)).Append(' = :').Append(ParamName);
        
        // 2. Add to SET clause: Version = :NewVersion (OldVersion + 1)
        ParamNameNew := GetNextParamName;
        if Val.IsEmpty then NewVersionVal := 1 else NewVersionVal := Val.AsInteger + 1;
        FParams.Add(ParamNameNew, NewVersionVal);
        
        if not FirstSet then SBSet.Append(', ');
        FirstSet := False;
        
        SQLCastStr := '';
        Converter := TTypeConverterRegistry.Instance.GetConverter(Prop.PropertyType.Handle);
        if Converter <> nil then
           SQLCastStr := Converter.GetSQLCast(':' + ParamNameNew, GetDialectEnum)
        else
           SQLCastStr := ':' + ParamNameNew;

        SBSet.Append(FDialect.QuoteIdentifier(ColName)).Append(' = ').Append(SQLCastStr);
      end
      else if IsPK then
      begin
        // Primary Key -> WHERE clause
        ParamName := GetNextParamName;
        FParams.Add(ParamName, Val);
        
        if not FirstWhere then SBWhere.Append(' AND ');
        FirstWhere := False;
        
        // Apply ::uuid cast for PostgreSQL if value is GUID/UUID or GUID-like string
        SQLCastStr := ':' + ParamName;
        if (GetDialectEnum = ddPostgreSQL) and
           ((Val.TypeInfo = TypeInfo(TGUID)) or 
            (Val.TypeInfo = TypeInfo(TUUID)) or
            ((Val.Kind in [tkString, tkUString, tkWString]) and
             ((Length(Val.AsString) = 36) or (Length(Val.AsString) = 38)) and
             (Val.AsString.IndexOf('-') > 0))) then
          SQLCastStr := ':' + ParamName + '::uuid';
        
        SBWhere.Append(FDialect.QuoteIdentifier(ColName)).Append(' = ').Append(SQLCastStr);
      end
      else
      begin
        // Standard Column -> SET clause
        ParamName := GetNextParamName;
        FParams.Add(ParamName, Val);
        
        if not FirstSet then SBSet.Append(', ');
        FirstSet := False;
        
        SQLCastStr := '';
        if PropMap <> nil then
           Converter := PropMap.Converter
        else
           Converter := TTypeConverterRegistry.Instance.GetConverter(Prop.PropertyType.Handle);
        if Converter <> nil then
           SQLCastStr := Converter.GetSQLCast(':' + ParamName, GetDialectEnum)
        else
           SQLCastStr := ':' + ParamName;

        SBSet.Append(FDialect.QuoteIdentifier(ColName)).Append(' = ').Append(SQLCastStr);
      end;
    end;
    
    if SBWhere.Length = 0 then
      raise Exception.Create('Cannot generate UPDATE: No Primary Key defined.');
      
    Result := Format('UPDATE %s SET %s WHERE %s', 
      [GetTableName, SBSet.ToString, SBWhere.ToString]);
      
  finally
    SBSet.Free;
    SBWhere.Free;
  end;
end;

function TSQLGenerator<T>.GenerateDelete(const AEntity: T): string;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  Attr: TCustomAttribute;
  ColName, ParamName: string;
  SBWhere: TStringBuilder;
  IsPK, FirstWhere: Boolean;
  Val: TValue;
  PropMap: TPropertyMap;
begin
  FParams.Clear;
  FParamCount := 0;
  
  Ctx := TRttiContext.Create;
  Typ := Ctx.GetType(T);
  
  SBWhere := TStringBuilder.Create;
  try
    FirstWhere := True;
    
    for Prop in Typ.GetProperties do
    begin
      IsPK := False;
      ColName := Prop.Name;
      
      PropMap := nil;
      if FMap <> nil then
        FMap.Properties.TryGetValue(Prop.Name, PropMap);
        
      if PropMap <> nil then
      begin
        if PropMap.IsPK then IsPK := True;
        if PropMap.ColumnName <> '' then ColName := PropMap.ColumnName;
      end;

      for Attr in Prop.GetAttributes do
      begin
        if (PropMap = nil) or not PropMap.IsPK then
          if Attr is PKAttribute then IsPK := True;
          
        if (PropMap = nil) or (PropMap.ColumnName = '') then
          if Attr is ColumnAttribute then ColName := ColumnAttribute(Attr).Name;
      end;
      
      if not IsPK then Continue;
      
      Val := Prop.GetValue(Pointer(AEntity));
      ParamName := GetNextParamName;
      FParams.Add(ParamName, Val);
      
      if not FirstWhere then SBWhere.Append(' AND ');
      FirstWhere := False;
      
      // Apply ::uuid cast for PostgreSQL if value is GUID/UUID or GUID-like string
      var SQLCastStr := ':' + ParamName;
      if (GetDialectEnum = ddPostgreSQL) and
         ((Val.TypeInfo = TypeInfo(TGUID)) or 
          (Val.TypeInfo = TypeInfo(TUUID)) or
          ((Val.Kind in [tkString, tkUString, tkWString]) and
           ((Length(Val.AsString) = 36) or (Length(Val.AsString) = 38)) and
           (Val.AsString.IndexOf('-') > 0))) then
        SQLCastStr := ':' + ParamName + '::uuid';
      
      SBWhere.Append(FDialect.QuoteIdentifier(ColName)).Append(' = ').Append(SQLCastStr);
    end;
    
    if SBWhere.Length = 0 then
      raise Exception.Create('Cannot generate DELETE: No Primary Key defined.');
      
    Result := Format('DELETE FROM %s WHERE %s', 
      [GetTableName, SBWhere.ToString]);
      
  finally
    SBWhere.Free;
  end;
end;

function TSQLGenerator<T>.GenerateSelect(const ASpec: ISpecification<T>): string;
var
  WhereGen: TSQLWhereGenerator;
  WhereSQL: string;
  SB: TStringBuilder;
  Prop: TRttiProperty;
  ColName: string;
  Attr: TCustomAttribute;
  Ctx: TRttiContext;
  Typ: TRttiType;
  First: Boolean;
  SelectedCols: TArray<string>;
  OrderBy: TArray<IOrderBy>;
  Skip, Take, i: Integer;
  Pair: TPair<string, TValue>;
  IsMapped: Boolean;
  PropMap: TPropertyMap;
  SoftDeleteFilter, DiscriminatorFilter: string;
  SortCol: string;
  P: TRttiProperty;
  // Caching
  Sig: string;
  CachedSQL: string;
  Collector: TSQLParamCollector;
begin
  FParams.Clear;
  FParamCount := 0;
  
  // 1. Check Cache
  if ASpec <> nil then
  begin
    Sig := Format('%s:%s:%s:Ign:%d:Del:%d', 
      [(FDialect as TObject).ClassName, GetTableName, ASpec.GetSignature, 
       Ord(FIgnoreQueryFilters), Ord(FOnlyDeleted)]);
    if TSQLCache.Instance.TryGetSQL(Sig, CachedSQL) then
    begin
      // Re-hydrate parameters using Collector (fast traversal)
      Collector := TSQLParamCollector.Create(FParams);
      try
        Collector.Collect(ASpec.GetExpression);
      finally
        Collector.Free;
      end;
      Result := CachedSQL;
      Exit;
    end;
  end;
  
  WhereGen := TSQLWhereGenerator.Create(FDialect, TSQLColumnMapper<T>.Create);
    
  try
    WhereSQL := WhereGen.Generate(ASpec.GetExpression);
    
    // Copy params
    for Pair in WhereGen.Params do
    begin
      FParams.Add(Pair.Key, Pair.Value);
    end;
  finally
    WhereGen.Free;
  end;
  
  SB := TStringBuilder.Create;
  try
    SB.Append('SELECT ');
    
    SelectedCols := ASpec.GetSelectedColumns;
    if Length(SelectedCols) > 0 then
    begin
      // Custom projection - translate property names to column names
      Ctx := TRttiContext.Create;
      Typ := Ctx.GetType(T);
      
      for i := 0 to High(SelectedCols) do
      begin
        if i > 0 then SB.Append(', ');
        
        ColName := SelectedCols[i]; // Default to property name
        
        // Try to find the property and get its mapped column name
        Prop := Typ.GetProperty(SelectedCols[i]);
        if Prop <> nil then
        begin
          // Check Fluent mapping first
          PropMap := nil;
          if FMap <> nil then
            FMap.Properties.TryGetValue(Prop.Name, PropMap);
            
          if (PropMap <> nil) and (PropMap.ColumnName <> '') then
            ColName := PropMap.ColumnName
          else
          begin
            // Check attributes
            for Attr in Prop.GetAttributes do
            begin
              if Attr is ColumnAttribute then
              begin
                ColName := ColumnAttribute(Attr).Name;
                Break;
              end;
            end;
          end;
        end;
        
        SB.Append(FDialect.QuoteIdentifier(ColName));
      end;
    end
    else
    begin
      // Select all mapped columns
      Ctx := TRttiContext.Create;
      Typ := Ctx.GetType(T);
      First := True;
      
      for Prop in Typ.GetProperties do
      begin
        ColName := Prop.Name;
        IsMapped := True;
        
        PropMap := nil;
        if FMap <> nil then
          FMap.Properties.TryGetValue(Prop.Name, PropMap);
          
        if PropMap <> nil then
        begin
          if PropMap.IsIgnored then IsMapped := False;
          if PropMap.ColumnName <> '' then ColName := PropMap.ColumnName;
        end;

        for Attr in Prop.GetAttributes do
        begin
          if Attr is NotMappedAttribute then IsMapped := False;
          
          if (PropMap = nil) or (PropMap.ColumnName = '') then
          begin
            if Attr is ColumnAttribute then ColName := ColumnAttribute(Attr).Name;
            if Attr is ForeignKeyAttribute then ColName := ForeignKeyAttribute(Attr).ColumnName;
          end;
        end;
        
        if not IsMapped then Continue;
        
        if not First then SB.Append(', ');
        First := False;
        
        SB.Append(FDialect.QuoteIdentifier(ColName));
      end;
    end;
    
    SB.Append(' FROM ').Append(GetTableName);
    
    // Add soft delete filter
    SoftDeleteFilter := GetSoftDeleteFilter;
    DiscriminatorFilter := GetDiscriminatorFilter;
    
    // Append Joins
    SB.Append(GenerateJoins(ASpec.GetJoins));
    
    // Combine filters
    if DiscriminatorFilter <> '' then
    begin
       if SoftDeleteFilter <> '' then
         SoftDeleteFilter := SoftDeleteFilter + ' AND ' + DiscriminatorFilter
       else
         SoftDeleteFilter := DiscriminatorFilter;
    end;
    
    if WhereSQL <> '' then
    begin
      SB.Append(' WHERE ').Append(WhereSQL);
      if SoftDeleteFilter <> '' then
        SB.Append(' AND ').Append(SoftDeleteFilter);
    end
    else if SoftDeleteFilter <> '' then
      SB.Append(' WHERE ').Append(SoftDeleteFilter);
      
    // Group By
    SB.Append(GenerateGroupBy(ASpec.GetGroupBy));

    // Order By
    OrderBy := ASpec.GetOrderBy;
    if Length(OrderBy) > 0 then
    begin
      SB.Append(' ORDER BY ');
      for i := 0 to High(OrderBy) do
      begin
        if i > 0 then SB.Append(', ');
        
        SortCol := OrderBy[i].GetPropertyName;
        // Lookup column name (simplified)
        Ctx := TRttiContext.Create;
        Typ := Ctx.GetType(T);
        P := Typ.GetProperty(SortCol);
        if P <> nil then
        begin
           for Attr in P.GetAttributes do
           begin
             if Attr is ColumnAttribute then SortCol := ColumnAttribute(Attr).Name;
             if Attr is ForeignKeyAttribute then SortCol := ForeignKeyAttribute(Attr).ColumnName;
           end;
        end;
        
        SB.Append(FDialect.QuoteIdentifier(SortCol));
        
        if not OrderBy[i].GetAscending then
          SB.Append(' DESC');
      end;
    end;
    
    // Paging
    if ASpec.IsPagingEnabled then
    begin
      // SQL Server requires ORDER BY when using OFFSET/FETCH
      // If no ORDER BY was specified, add a default one ONLY if dialect requires it
      if (Length(OrderBy) = 0) and FDialect.RequiresOrderByForPaging then
      begin
        SB.Append(' ORDER BY ');
        // Use first column or (SELECT NULL) as fallback
        if Length(SelectedCols) > 0 then
          SB.Append(FDialect.QuoteIdentifier(SelectedCols[0]))
        else
          SB.Append('(SELECT NULL)');
      end
      else if (Length(OrderBy) > 0) and FDialect.RequiresOrderByForPaging then
      begin
         // OrderBy already appended? NO! It was appended before GroupBy in previous logic?
         // Let's check logic flow.
         // Order By was appended at line 1324.
         // Group By must come BEFORE Order By.
      end;
      
      Skip := ASpec.GetSkip;
      Take := ASpec.GetTake;
      
      // Paging syntax generation
      Result := SB.ToString + ' ' + FDialect.GeneratePaging(Skip, Take);
    end
    else
    begin
      Result := SB.ToString;
    end;
    
    // Add to cache
    if (ASpec <> nil) and (Result <> '') then
      TSQLCache.Instance.AddSQL(Sig, Result);
      
  finally
    SB.Free;
  end;
end;

function TSQLGenerator<T>.GenerateSelect: string;
var
  SB: TStringBuilder;
  Prop: TRttiProperty;
  ColName: string;
  Attr: TCustomAttribute;
  Ctx: TRttiContext;
  Typ: TRttiType;
  First, IsMapped: Boolean;
  PropMap: TPropertyMap;
  SoftDeleteFilter, DiscriminatorFilter: string;
begin
  FParams.Clear;
  FParamCount := 0;

  SB := TStringBuilder.Create;
  try
    SB.Append('SELECT ');

    // Select all mapped columns
    Ctx := TRttiContext.Create;
    Typ := Ctx.GetType(T);
    First := True;

    for Prop in Typ.GetProperties do
    begin
      ColName := Prop.Name;
      IsMapped := True;

      PropMap := nil;
      if FMap <> nil then
        FMap.Properties.TryGetValue(Prop.Name, PropMap);

      if PropMap <> nil then
      begin
        if PropMap.IsIgnored then IsMapped := False;
        if PropMap.ColumnName <> '' then ColName := PropMap.ColumnName;
      end;

      for Attr in Prop.GetAttributes do
      begin
        if Attr is NotMappedAttribute then IsMapped := False;

        if (PropMap = nil) or (PropMap.ColumnName = '') then
        begin
          if Attr is ColumnAttribute then ColName := ColumnAttribute(Attr).Name;
          if Attr is ForeignKeyAttribute then ColName := ForeignKeyAttribute(Attr).ColumnName;
        end;
      end;

      if not IsMapped then Continue;

      if not First then SB.Append(', ');
      First := False;

      SB.Append(FDialect.QuoteIdentifier(ColName));
    end;

    SB.Append(' FROM ').Append(GetTableName);

    // Add soft delete filter
    SoftDeleteFilter := GetSoftDeleteFilter;
    DiscriminatorFilter := GetDiscriminatorFilter;
    
    if DiscriminatorFilter <> '' then
    begin
       if SoftDeleteFilter <> '' then
         SoftDeleteFilter := ' WHERE ' + SoftDeleteFilter + ' AND ' + DiscriminatorFilter
       else
         SoftDeleteFilter := ' WHERE ' + DiscriminatorFilter;
    end
    else if SoftDeleteFilter <> '' then
       SoftDeleteFilter := ' WHERE ' + SoftDeleteFilter;
       
    SB.Append(SoftDeleteFilter);

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TSQLGenerator<T>.GenerateCount(const ASpec: ISpecification<T>): string;
var
  WhereGen: TSQLWhereGenerator;
  WhereSQL: string;
  SB: TStringBuilder;
  SoftDeleteFilter, DiscriminatorFilter: string;
  Pair: TPair<string, TValue>;
begin
  FParams.Clear;
  FParamCount := 0;
  
  WhereGen := TSQLWhereGenerator.Create(FDialect, TSQLColumnMapper<T>.Create);
    
  try
    WhereSQL := WhereGen.Generate(ASpec.GetExpression);
    
    // Copy params
    for Pair in WhereGen.Params do
    begin
      FParams.Add(Pair.Key, Pair.Value);
    end;
  finally
    WhereGen.Free;
  end;
  
  SB := TStringBuilder.Create;
  try
    SB.Append('SELECT COUNT(*) FROM ').Append(GetTableName);
    
    // Append Joins
    SB.Append(GenerateJoins(ASpec.GetJoins));

    // Add soft delete filter
    SoftDeleteFilter := GetSoftDeleteFilter;
    DiscriminatorFilter := GetDiscriminatorFilter;
    
    if DiscriminatorFilter <> '' then
    begin
       if SoftDeleteFilter <> '' then
         SoftDeleteFilter := SoftDeleteFilter + ' AND ' + DiscriminatorFilter
       else
         SoftDeleteFilter := DiscriminatorFilter;
    end;
    
    if WhereSQL <> '' then
    begin
      SB.Append(' WHERE ').Append(WhereSQL);
      if SoftDeleteFilter <> '' then
        SB.Append(' AND ').Append(SoftDeleteFilter);
    end
    else if SoftDeleteFilter <> '' then
      SB.Append(' WHERE ').Append(SoftDeleteFilter);
      
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TSQLGenerator<T>.GenerateCount: string;
var
  SoftDeleteFilter, DiscriminatorFilter: string;
begin
  FParams.Clear;
  FParamCount := 0;
  
  Result := 'SELECT COUNT(*) FROM ' + GetTableName;
  
  // Add soft delete filter
  SoftDeleteFilter := GetSoftDeleteFilter;
  DiscriminatorFilter := GetDiscriminatorFilter;
  
  if DiscriminatorFilter <> '' then
  begin
     if SoftDeleteFilter <> '' then
       Result := Result + ' WHERE ' + SoftDeleteFilter + ' AND ' + DiscriminatorFilter
     else
       Result := Result + ' WHERE ' + DiscriminatorFilter;
  end
  else if SoftDeleteFilter <> '' then
    Result := Result + ' WHERE ' + SoftDeleteFilter;
end;

function TSQLGenerator<T>.GenerateCreateTable(const ATableName: string): string;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  Attr: TCustomAttribute;
  ColName, ColType, Body: string;
  SB: TStringBuilder;
  First, IsPK, IsAutoInc, IsMapped, HasAutoInc: Boolean;
  PKCols: TList<string>;
  FKConstraints: TList<string>;
  Constraint: string;
  TypeAttr: TCustomAttribute;
  PropMap: TPropertyMap;
  i: Integer;
  FK: ForeignKeyAttribute;
  FKPropName: string;
  FKColName: string;
  RelatedTable, RelatedPK: string;
  LConstraint: string;
  PropTypeHandle: PTypeInfo;
  Underlying: PTypeInfo;
  IsSoftDeleteColumn: Boolean;
  SoftDeleteDefaultValue: Variant;
  SoftDelAttr: SoftDeleteAttribute;
begin
  SB := TStringBuilder.Create;
  PKCols := TList<string>.Create;
  FKConstraints := TList<string>.Create;
  try
    Ctx := TRttiContext.Create;
    Typ := Ctx.GetType(T);
    First := True;
    HasAutoInc := False;
    
    for Prop in Typ.GetProperties do
    begin
      IsMapped := True;
      IsPK := False;
      IsAutoInc := False;
      ColName := Prop.Name;
      
      PropMap := nil;
      if FMap <> nil then
        FMap.Properties.TryGetValue(Prop.Name, PropMap);
        
      if PropMap <> nil then
      begin
        if PropMap.IsIgnored then IsMapped := False;
        if PropMap.IsPK then IsPK := True;
        if PropMap.IsAutoInc then IsAutoInc := True;
        if PropMap.ColumnName <> '' then ColName := PropMap.ColumnName;
      end;
      
      for Attr in Prop.GetAttributes do
      begin
        if Attr is NotMappedAttribute then IsMapped := False;
        
        if (PropMap = nil) or not PropMap.IsPK then
          if Attr is PKAttribute then IsPK := True;
          
        if (PropMap = nil) or not PropMap.IsAutoInc then
           if Attr is AutoIncAttribute then IsAutoInc := True;
           
        if Attr is ForeignKeyAttribute then
        begin
             // Use FK Attribute to define column name if not explicitly mapped
             FK := ForeignKeyAttribute(Attr);
             if (PropMap = nil) or (PropMap.ColumnName = '') then
                ColName := FK.ColumnName;
                
             // Add FK Constraint logic
             FKPropName := FK.ColumnName;
             FKColName := TSQLGeneratorHelper.GetColumnNameForProperty(Typ, FKPropName);
          
             if (Prop.PropertyType.TypeKind = tkClass) and 
                TSQLGeneratorHelper.GetRelatedTableAndPK(Ctx, Prop.PropertyType.AsInstance.MetaclassType, RelatedTable, RelatedPK) then
             begin
                 LConstraint := Format('FOREIGN KEY (%s) REFERENCES %s (%s)', 
                   [FDialect.QuoteIdentifier(FKColName), 
                    FDialect.QuoteIdentifier(RelatedTable), 
                    FDialect.QuoteIdentifier(RelatedPK)]);
                    
                 if FK.OnDelete <> caNoAction then
                   LConstraint := LConstraint + ' ON DELETE ' + TSQLGeneratorHelper.GetCascadeSQL(FK.OnDelete);
                   
                 if FK.OnUpdate <> caNoAction then
                   LConstraint := LConstraint + ' ON UPDATE ' + TSQLGeneratorHelper.GetCascadeSQL(FK.OnUpdate);
                   
                 FKConstraints.Add(LConstraint);
             end;
        end;
        
        if (PropMap = nil) or (PropMap.ColumnName = '') then
        begin
          if Attr is ColumnAttribute then ColName := ColumnAttribute(Attr).Name;
        end;
      end;
      
      if not IsMapped then Continue;
      
      if not First then SB.Append(', ');
      First := False;
      
      SB.Append(FDialect.QuoteIdentifier(ColName));
      SB.Append(' ');
      
      PropTypeHandle := Prop.PropertyType.Handle;
      
      // Handle Explicit DbType (Attributes or Fluent)
      if (PropMap <> nil) and (PropMap.DataType <> ftUnknown) then
      begin
        ColType := FDialect.GetColumnTypeForField(PropMap.DataType, IsAutoInc);
      end
      else
      begin
        // Handle Nullable<T>
        if IsNullable(Prop.PropertyType.Handle) then
        begin
          Underlying := GetUnderlyingType(Prop.PropertyType.Handle);
          if Underlying <> nil then
            PropTypeHandle := Underlying;
        end
        // Handle Prop<T> (Smart Types)
        else if (Prop.PropertyType.TypeKind = tkRecord) then
        begin
           var FieldFValue := Prop.PropertyType.GetField('FValue');
           if (FieldFValue <> nil) and 
              (Prop.PropertyType.Name.Contains('Prop<') or (Prop.PropertyType.GetProperty('Value') <> nil)) then
           begin
              PropTypeHandle := FieldFValue.FieldType.Handle;
           end;
        end;
        
        ColType := FDialect.GetColumnType(PropTypeHandle, IsAutoInc);
      end;
      
      // Apply MaxLength for string columns - check from PropMap or Attribute
      var MaxLen: Integer := 0;
      if (PropMap <> nil) and (PropMap.MaxLength > 0) then
        MaxLen := PropMap.MaxLength
      else
      begin
        // Check for MaxLengthAttribute
        for Attr in Prop.GetAttributes do
        begin
          if Attr is MaxLengthAttribute then
          begin
            MaxLen := MaxLengthAttribute(Attr).Length;
            Break;
          end;
        end;
      end;
      
      // For string types with MaxLength, replace generic types with sized VARCHAR

      if (MaxLen > 0) and (PropTypeHandle.Kind in [tkString, tkUString, tkWString, tkChar, tkWChar]) then
      begin
        if GetDialectEnum = ddSQLServer then
          ColType := Format('NVARCHAR(%d)', [MaxLen])
        else if GetDialectEnum = ddOracle then
          ColType := Format('VARCHAR2(%d)', [MaxLen])
        else
          ColType := Format('VARCHAR(%d)', [MaxLen]);
      end;

      SB.Append(ColType);
      
      // Check if this is a soft delete column and add DEFAULT
      IsSoftDeleteColumn := False;
      for TypeAttr in Typ.GetAttributes do
      begin
        if TypeAttr is SoftDeleteAttribute then
        begin
          SoftDelAttr := SoftDeleteAttribute(TypeAttr);
          if SameText(ColName, SoftDelAttr.ColumnName) then
          begin
            IsSoftDeleteColumn := True;
            SoftDeleteDefaultValue := SoftDelAttr.NotDeletedValue;
            Break;
          end;
        end;
      end;
      
      if IsPK then
      begin
        PKCols.Add(FDialect.QuoteIdentifier(ColName));
        if IsAutoInc then
        begin
            SB.Append(' PRIMARY KEY'); 
            HasAutoInc := True;
        end
        else
           SB.Append(' NOT NULL');
      end
      else if IsSoftDeleteColumn then
      begin
        SB.Append(' DEFAULT ').Append(VarToStr(SoftDeleteDefaultValue));
      end;
    end; // End of Properties Loop
    
    // Add Composite PK constraint or Single PK constraint (if not AutoInc)
    if (PKCols.Count > 0) and not HasAutoInc then
    begin
      SB.Append(', PRIMARY KEY (');
      for i := 0 to PKCols.Count - 1 do
      begin
        if i > 0 then SB.Append(', ');
        SB.Append(PKCols[i]);
      end;
      SB.Append(')');
    end;
    
    // Add FK Constraints
    for Constraint in FKConstraints do
    begin
      SB.Append(', ').Append(Constraint);
    end;
    
    Body := SB.ToString;
    Result := FDialect.GetCreateTableSQL(ATableName, Body);
  finally
    FKConstraints.Free;
    PKCols.Free;
    SB.Free;
  end;
end;




function TSQLGenerator<T>.GetJoinTypeSQL(AType: TJoinType): string;
begin
  case AType of
    jtInner: Result := 'INNER JOIN';
    jtLeft: Result := 'LEFT JOIN';
    jtRight: Result := 'RIGHT JOIN';
    jtFull: Result := 'FULL JOIN';
  else
    Result := 'INNER JOIN';
  end;
end;

function TSQLGenerator<T>.GenerateJoins(const AJoins: TArray<IJoin>): string;
var
  SB: TStringBuilder;
  JoinObj: IJoin;
  WhereGen: TSQLWhereGenerator;
  Pair: TPair<string, TValue>;
begin
  if Length(AJoins) = 0 then Exit('');
  
  SB := TStringBuilder.Create;
  try
    for JoinObj in AJoins do
    begin
       WhereGen := TSQLWhereGenerator.Create(FDialect, nil); // No mapper, uses raw aliased columns
       try
         SB.Append(' ')
           .Append(GetJoinTypeSQL(JoinObj.GetJoinType))
           .Append(' ')
           .Append(FDialect.QuoteIdentifier(JoinObj.GetTableName));
           
         if JoinObj.GetAlias <> '' then
           SB.Append(' ').Append(FDialect.QuoteIdentifier(JoinObj.GetAlias));
           
         SB.Append(' ON ')
           .Append(WhereGen.Generate(JoinObj.GetCondition));
           
         // Merge Params
         for Pair in WhereGen.Params do
           FParams.AddOrSetValue(Pair.Key, Pair.Value);
           
       finally
         WhereGen.Free;
       end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TSQLGenerator<T>.GenerateGroupBy(const AGroupBy: TArray<string>): string;
var
  SB: TStringBuilder;
  i: Integer;
begin
  if Length(AGroupBy) = 0 then Exit('');
  
  SB := TStringBuilder.Create;
  try
    SB.Append(' GROUP BY ');
    for i := 0 to High(AGroupBy) do
    begin
      if i > 0 then SB.Append(', ');
      SB.Append(QuoteColumnOrAlias(AGroupBy[i]));
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TSQLGenerator<T>.QuoteColumnOrAlias(const AName: string): string;
var
  Parts: TArray<string>;
  i: Integer;
begin
  if AName.Contains('.') then
  begin
    Parts := AName.Split(['.']);
    Result := '';
    for i := 0 to High(Parts) do
    begin
      if i > 0 then Result := Result + '.';
      Result := Result + FDialect.QuoteIdentifier(Parts[i]);
    end;
  end
  else
    Result := FDialect.QuoteIdentifier(AName);
end;

end.

