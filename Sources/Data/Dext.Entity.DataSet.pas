unit Dext.Entity.DataSet;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.Rtti,
  System.TypInfo,
  System.Math,
  Data.DB,
  Dext.Utils,
  Dext.Collections,
  Dext.Collections.Vector,
  Dext.Collections.Dict,
  Dext.Core.Span,
  Dext.Entity.Mapping,
  Dext.Json.Utf8;

type
  /// <summary>
  ///   Data Structure of a Record Buffer for TEntityDataSet.
  ///   Stores fully updated bytes and modification trackers.
  /// </summary>
  PEntityRecordHeader = ^TEntityRecordHeader;
  TEntityRecordHeader = record
    BookmarkFlag: TBookmarkFlag;
    BookmarkIndex: Integer;
    RowState: TDataSetState;
    DirtyMask: UInt64; // Mask indicating which fields were modified in the Grid
  end;

  /// <summary>
  ///   Custom TDataSet for high-performance reading and writing to direct objects/lists.
  /// </summary>
  TEntityDataSet = class(TDataSet)
  private
    FEntityMap: TEntityMap;
    FEntityClass: TClass;
    
    // Virtual Buffers (Offsets Index)
    // Physical Objects Reference
    FItems: IList<TObject>;            // Real reference to the object list
    FOwnsItems: Boolean;               // Whether the dataset owns the list and should clear it
    FVirtualIndex: TVector<Integer>;   // Ordered/filtered view over FItems (contains indices to FItems)
    
    FRecordSize: Integer;
    FHeaderSize: Integer;
    
    // Internal Settings
    FReadOnly: Boolean;
    FIncludeShadowProperties: Boolean;
    FIndexFieldNames: string;
    FCurrentRec: Integer; // Dataset native cursor control
    FIsCursorOpen: Boolean;
    FInsertObj: TObject; // Temporary object for uncommitted dsInsert
    FIsAppending: Boolean;
    FPositionBeforeAction: Integer;
    
    procedure SetItems(const Value: IList<TObject>);
    procedure SetIndexFieldNames(const Value: string);
    procedure ApplyFilterAndSort; overload;
    procedure ApplyFilterAndSort(AFiltered: Boolean); overload;
    function CompareObjectsInternal(A, B: TObject; const APropNames: TArray<string>; RttiType: TRttiType): Integer;
    procedure BuildFieldDefs;
    
    /// <summary>
    ///   Core internal method that reads a field value from an entity object.
    ///   Used by both GetFieldData overloads.
    ///   Returns the value as Variant (Unassigned if not found).
    /// </summary>
    function ReadFieldValue(Field: TField; out Value: Variant): Boolean;
  protected
    // TDataSet overrides for filtering and sorting
    procedure InternalHandleException; override;
    function IsCursorOpen: Boolean; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoSearch: Boolean): TGetResult; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterText(const Value: string); override;

    // Mandatory TDataSet overrides
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalInitFieldDefs; override;

    // Buffer Alocations
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    
    // Bookmark and Navigation
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    procedure InternalGotoBookmark(Bookmark: TBookmark); override;
    
    function GetRecordSize: Word; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;

    // DML and Editing
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); override;
    procedure InternalAddRecord(Buffer: TRecordBuffer; Append: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure InternalCancel; override;
    procedure InternalEdit; override;
    procedure InternalInsert; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure DoBeforeScroll; override;
    procedure DoAfterScroll; override;
    procedure DoBeforeInsert; override;
    procedure DoBeforeDelete; override;

  private
    function CreateNewEntity: TObject;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    ///   GetFieldData override for modern Delphi (XE4+) with TValueBuffer (TArray of Byte).
    ///   This is the override that TField.GetData actually calls in modern Delphi.
    /// </summary>
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions = []): Boolean; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    /// <summary>
    ///  Object data loading
    /// </summary>
    procedure Load(const AItems: IList<TObject>; AClass: TClass; AOwns: Boolean = False); overload;
    procedure Load(const AItems: TArray<TObject>; AClass: TClass); overload;
    
    /// <summary>
    ///  UTF-8 JSON data loading (Zero-Alloc Pipeline)
    /// </summary>
    procedure LoadFromUtf8Json(const ASpan: TByteSpan; AClass: TClass);
    procedure Refresh;

    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetCurrentObject: TObject;
    property Items: IList<TObject> read FItems write SetItems;
  published
    property Active;
    property Filter;
    property Filtered;
    property FilterOptions;
    property IncludeShadowProperties: Boolean read FIncludeShadowProperties write FIncludeShadowProperties default False;
    property IndexFieldNames: string read FIndexFieldNames write SetIndexFieldNames;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;

    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;

    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

implementation

uses
  System.StrUtils,
  Dext.Specifications.Evaluator,
  Dext.Specifications.Interfaces,
  Dext.Specifications.Parser,
  Dext.Core.Reflection,
  Dext.Core.Activator,
  Dext.Core.ValueConverters,
  Dext.Entity;

type
  TEntityBlobStream = class(TMemoryStream)
  private
    FField: TField;
    FDataSet: TEntityDataSet;
    FMode: TBlobStreamMode;
    FModified: Boolean;
    FObj: TObject;
  public
    constructor Create(Field: TField; DataSet: TEntityDataSet; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Write(const Buffer; Count: Integer): Longint; override;
  end;

function TValueBufferToValue(ABuffer: TValueBuffer; ADataType: TFieldType): TValue;
begin
  case ADataType of
    ftString, ftWideString:
      Result := TValue.From<string>(TEncoding.Unicode.GetString(ABuffer));
    ftInteger:
      Result := TValue.From<Integer>(PInteger(ABuffer)^);
    ftLargeint:
      Result := TValue.From<Int64>(PInt64(ABuffer)^);
    ftFloat, ftCurrency:
      Result := TValue.From<Double>(PDouble(ABuffer)^);
    ftBoolean:
      Result := TValue.From<Boolean>(PBoolean(ABuffer)^);
    ftDateTime, ftDate, ftTime:
      Result := TValue.From<TDateTime>(PDouble(ABuffer)^);
  else
    Result := TValue.Empty;
  end;
end;

{ TEntityDataSet }

procedure TEntityDataSet.Refresh;
begin
  if Active then
  begin
    ApplyFilterAndSort(Filtered);
    Resync([]);
  end;
end;

constructor TEntityDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecordSize := SizeOf(TEntityRecordHeader);
  FHeaderSize := SizeOf(TEntityRecordHeader);
  FReadOnly := False;
  BookmarkSize := SizeOf(Integer);
  FPositionBeforeAction := -2;
end;

destructor TEntityDataSet.Destroy;
begin
  if Assigned(FInsertObj) then
    FInsertObj.Free;
    
  if FOwnsItems then
    FItems := nil; // IList cuidará da liberação se for o caso
  FItems := nil;
  
  if Assigned(FEntityMap) then
    FEntityMap.Free;
    
  inherited Destroy;
end;

function TEntityDataSet.GetCurrentObject: TObject;
var
  Header: PEntityRecordHeader;
begin
  Result := nil;
  if not Active then Exit;
  
  Header := PEntityRecordHeader(ActiveBuffer);
  if (Header <> nil) then
  begin
    if (Header.BookmarkIndex = -2) then
      Exit(FInsertObj)
    else if (Header.BookmarkIndex >= 0) and (Header.BookmarkIndex < FVirtualIndex.Count) then
      Exit(FItems[FVirtualIndex[Header.BookmarkIndex]]);
  end;

  if (FCurrentRec >= 0) and (FCurrentRec < FVirtualIndex.Count) then
    Result := FItems[FVirtualIndex[FCurrentRec]];
end;

function TEntityDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TEntityBlobStream.Create(Field, Self, Mode);
end;

{ TEntityBlobStream }

constructor TEntityBlobStream.Create(Field: TField; DataSet: TEntityDataSet; Mode: TBlobStreamMode);
var
  Val: TValue;
  B: TBytes;
  S: string;
begin
  inherited Create;
  FField := Field;
  FDataSet := DataSet;
  FMode := Mode;
  FModified := False;
  FObj := FDataSet.GetCurrentObject;
  
  if (FMode <> bmWrite) and Assigned(FObj) then
  begin
    Val := TReflection.GetValue(FObj, FField.FieldName);
    if not Val.IsEmpty then
    begin
      if FField.DataType in [ftMemo, ftWideMemo] then
      begin
        S := Val.AsString;
        if S <> '' then
        begin
          B := TEncoding.Unicode.GetBytes(S);
          Write(B[0], Length(B));
        end;
      end
      else if FField.DataType = ftBlob then
      begin
        B := Val.AsType<TBytes>;
        if Length(B) > 0 then
          Write(B[0], Length(B));
      end;
      Position := 0;
    end;
  end;
end;

destructor TEntityBlobStream.Destroy;
var
  B: TBytes;
  S: string;
begin
  if FModified and (FMode <> bmRead) and Assigned(FObj) then
  begin
    Position := 0;
    SetLength(B, Size);
    if Size > 0 then
      Read(B[0], Size);

    if FField.DataType in [ftMemo, ftWideMemo] then
    begin
      // Detect and strip Unicode BOM ($FF $FE) if present
      if (Size >= 2) and (B[0] = $FF) and (B[1] = $FE) then
        S := TEncoding.Unicode.GetString(B, 2, Size - 2)
      else
        S := TEncoding.Unicode.GetString(B);
        
      TReflection.SetValueByPath(FObj, FField.FieldName, S);
    end
    else
      TReflection.SetValueByPath(FObj, FField.FieldName, TValue.From<TBytes>(B));
  end;
  inherited Destroy;
end;

function TEntityBlobStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;

procedure TEntityDataSet.Load(const AItems: IList<TObject>; AClass: TClass; AOwns: Boolean = False);
begin
  if FOwnsItems and Assigned(FItems) and (FItems <> AItems) then
    FItems := nil;

  FItems := AItems;
  FEntityClass := AClass;
  FOwnsItems := AOwns;

  if FEntityMap = nil then
  begin
    FEntityMap := TEntityMap.Create(AClass.ClassInfo);
    FEntityMap.DiscoverAttributes;
  end;
  
  Active := True; // Chama Open -> InternalOpen e prepara buffers
end;

procedure TEntityDataSet.Load(const AItems: TArray<TObject>; AClass: TClass);
var
  LList: IList<TObject>;
begin
  LList := TCollections.CreateList<TObject>(False);
  LList.AddRange(AItems);
  Load(LList, AClass, True); // Owns the wrapper list but not the objects
end;

procedure TEntityDataSet.LoadFromUtf8Json(const ASpan: TByteSpan; AClass: TClass);
var
  Context: TRttiContext;
  CurrentObj: TObject;
  PropMap: TPropertyMap;
  PropName: string;
  PValue: Pointer;
  Reader: TUtf8JsonReader;
  RttiProp: TRttiProperty;
  RttiType: TRttiType;
begin
  FEntityClass := AClass;
  if FEntityMap = nil then
  begin
    FEntityMap := TEntityMap.Create(AClass.ClassInfo);
    FEntityMap.DiscoverAttributes;
  end;

  Reader := TUtf8JsonReader.Create(ASpan);

  // Limpar itens anteriores
  if not Assigned(FItems) then
    FItems := TCollections.CreateList<TObject>(True);

  if FOwnsItems then
    FItems.Clear;
  FOwnsItems := True;

  if not Reader.Read then Exit;

  // Preparar RTTI context uma vez para todas as propriedades
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(FEntityClass);

    if Reader.TokenType = TJsonTokenType.StartArray then
    begin
      while Reader.Read and (Reader.TokenType <> TJsonTokenType.EndArray) do
      begin
        if Reader.TokenType = TJsonTokenType.StartObject then
        begin
          CurrentObj := FEntityClass.Create;

          FItems.Add(CurrentObj);

          while Reader.Read and (Reader.TokenType <> TJsonTokenType.EndObject) do
          begin
            if Reader.TokenType = TJsonTokenType.PropertyName then
            begin
              PropName := Reader.GetString;
              Reader.Read; // Avance para o valor

              if FEntityMap.Properties.TryGetValue(PropName, PropMap) then
              begin
                // Se o FieldValueOffset é válido (> 0), escrita direta por offset (fast-path)
                // Caso contrário, fallback via RTTI SetValue
                if PropMap.FieldValueOffset > 0 then
                begin
                  PValue := Pointer(PByte(CurrentObj) + PropMap.FieldValueOffset);
                  case PropMap.DataType of
                    ftString, ftWideString:
                      PString(PValue)^ := Reader.GetString;
                    ftInteger, ftSmallint:
                      PInteger(PValue)^ := Reader.GetInt32;
                    ftLargeint:
                      PInt64(PValue)^ := Reader.GetInt64;
                    ftFloat, ftCurrency:
                      PDouble(PValue)^ := Reader.GetDouble;
                    ftBoolean:
                      PBoolean(PValue)^ := Reader.GetBoolean;
                  end;
                end
                else if RttiType <> nil then
                begin
                  // RTTI fallback para classes que usam campos privados padrão
                  RttiProp := RttiType.GetProperty(PropName);
                  if RttiProp <> nil then
                  begin
                    case Reader.TokenType of
                      TJsonTokenType.StringValue:
                        RttiProp.SetValue(CurrentObj, Reader.GetString);
                      TJsonTokenType.Number:
                      begin
                        if RttiProp.PropertyType.Handle = TypeInfo(Integer) then
                          RttiProp.SetValue(CurrentObj, Reader.GetInt32)
                        else if RttiProp.PropertyType.Handle = TypeInfo(Int64) then
                          RttiProp.SetValue(CurrentObj, Reader.GetInt64)
                        else
                          RttiProp.SetValue(CurrentObj, TValue.From<Double>(Reader.GetDouble));
                      end;
                      TJsonTokenType.TrueValue, TJsonTokenType.FalseValue:
                        RttiProp.SetValue(CurrentObj, Reader.GetBoolean);
                    end;
                  end;
                end;
              end
              else
                Reader.Skip; // propriedade não mapeada, pula valor/objeto
            end;
          end;
        end;
      end;
    end;
  finally
    Context.Free;
  end;

  if Active then Close;
  Open;
end;

procedure TEntityDataSet.SetIndexFieldNames(const Value: string);
begin
  if FIndexFieldNames <> Value then
  begin
    FIndexFieldNames := Value;
    if Active then
    begin
      ApplyFilterAndSort;
      Resync([]);
    end;
  end;
end;

procedure TEntityDataSet.SetFiltered(Value: Boolean);
begin
  if Filtered <> Value then
  begin
    if Active then
      ApplyFilterAndSort(Value); // Atualiza antes do inherited disparar o resync interno!
      
    inherited SetFiltered(Value);
  end;
end;

procedure TEntityDataSet.SetFilterText(const Value: string);
begin
  if Filter <> Value then
  begin
    inherited SetFilterText(Value);
    if Active and Filtered then
    begin
      ApplyFilterAndSort;
      Resync([]);
    end;
  end;
end;

procedure TEntityDataSet.ApplyFilterAndSort;
begin
  ApplyFilterAndSort(Filtered);
end;

procedure TEntityDataSet.ApplyFilterAndSort(AFiltered: Boolean);
var
  Context: TRttiContext;
  CurrentObj: TObject;
  EntityType: TRttiType;
  Expr: IExpression;
  i: Integer;
  Names: TArray<string>;
  Passing: Boolean;
begin
  // Salvar o objeto atual para restaurar FCurrentRec depois (mais seguro que índice físico)
  CurrentObj := nil;
  if (FCurrentRec >= 0) and (FCurrentRec < FVirtualIndex.Count) then
    CurrentObj := FItems[FVirtualIndex[FCurrentRec]];

  FVirtualIndex.Clear;

  Expr := nil;
  if AFiltered and (Filter <> '') then
    Expr := TStringExpressionParser.Parse(Filter);

  if not Assigned(FItems) then Exit;

  for i := 0 to FItems.Count - 1 do
  begin
    Passing := True;

    if AFiltered then
    begin
      if Expr <> nil then
        Passing := TExpressionEvaluator.Evaluate(Expr, FItems[I])
      else if Assigned(OnFilterRecord) then
      begin
        OnFilterRecord(Self, Passing);
      end;
    end;

    if Passing then
      FVirtualIndex.Add(I);
  end;

  if (FIndexFieldNames <> '') and (FVirtualIndex.Count > 1) then
  begin
    Names := FIndexFieldNames.Split([';']);
    Context := TRttiContext.Create;
    try
      EntityType := Context.GetType(FEntityClass);
      FVirtualIndex.Sort(Dext.Collections.Comparers.TComparer<Integer>.Construct(
        function(const A, B: Integer): Integer
        begin
          // A and B are indices in FItems
          Result := CompareObjectsInternal(FItems[A], FItems[B], Names, EntityType);
        end));
    finally
      Context.Free;
    end;
  end;

  // Restaurar a posição do cursor na visão virtual
  if CurrentObj <> nil then
  begin
    var NewPhysicalIdx := FItems.IndexOf(CurrentObj);
    if NewPhysicalIdx >= 0 then
      FCurrentRec := FVirtualIndex.IndexOf(NewPhysicalIdx)
    else
      FCurrentRec := -1;
  end
  else
    FCurrentRec := -1;
end;

function TEntityDataSet.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  Context: TRttiContext;
  CurVal: Variant;
  I: Integer;
  Match: Boolean;
  PropMap: TPropertyMap;
  RttiProp: TRttiProperty;
  RttiType: TRttiType;
  PValue: Pointer;
begin
  Result := False;
  if (KeyFields = '') or (FVirtualIndex.Count = 0) then Exit;

  // Simplificado para 1 Campo por iteração clássica de Locate
  if not FEntityMap.Properties.TryGetValue(KeyFields, PropMap) then Exit;

  // Preparar RTTI se necessário (quando FieldValueOffset não está disponível)
  RttiProp := nil;
  if PropMap.FieldValueOffset <= 0 then
  begin
    Context := TRttiContext.Create;
    RttiType := Context.GetType(FEntityClass);
    if RttiType <> nil then
      RttiProp := RttiType.GetProperty(KeyFields);
  end;

  for I := 0 to FVirtualIndex.Count - 1 do
  begin
    // Ler o valor do campo usando offset direto ou RTTI
    if (PropMap.FieldValueOffset > 0) or (PropMap.FieldOffset > 0) then
    begin
      // Use smart types null flag check only for SmartProps that have a null flag
      if (PropMap.FieldValueOffset > 0) and (PropMap.FieldOffset > 0) and
         not PBoolean(Pointer(PByte(FItems[FVirtualIndex[I]]) + PropMap.FieldOffset))^ then
        CurVal := Null
      else
      begin
        if PropMap.FieldValueOffset > 0 then
          PValue := Pointer(PByte(FItems[FVirtualIndex[I]]) + PropMap.FieldValueOffset)
        else
          PValue := Pointer(PByte(FItems[FVirtualIndex[I]]) + PropMap.FieldOffset);

        case PropMap.DataType of
          ftInteger, ftSmallint, ftAutoInc: CurVal := PInteger(PValue)^;
          ftLargeint: CurVal := PInt64(PValue)^;
          ftString, ftWideString: CurVal := PString(PValue)^;
          ftFloat: CurVal := PDouble(PValue)^;
          ftCurrency: CurVal := PCurrency(PValue)^;
          ftBoolean: CurVal := PBoolean(PValue)^;
        else
          Continue;
        end;
      end;
    end
    else if RttiProp <> nil then
      CurVal := RttiProp.GetValue(FItems[FVirtualIndex[I]]).AsVariant
    else
      Continue;

    if Options = [] then Match := CurVal = KeyValues
    else Match := SameText(VarToStr(CurVal), VarToStr(KeyValues));

    if Match then
    begin
      // Posicionar o cursor diretamente no registro encontrado
      FCurrentRec := I;
      Resync([]);
      Result := True;
      Break;
    end;
  end;
end;

function TEntityDataSet.CompareObjectsInternal(A, B: TObject; const APropNames: TArray<string>; RttiType: TRttiType): Integer;
var
  f: TRttiField;
  i: Integer;
  IdA: Integer;
  IdB: Integer;
  IsDesc: Boolean;
  Matched: Boolean;
  p: TRttiProperty;
  PropMap: TPropertyMap;
  PropName: string;
  RttiField: TRttiField;
  RttiProp: TRttiProperty;
  Token: string;
  ValA, ValB: Variant;
begin
  Result := 0;
  PropName := '';
  for i := 0 to High(APropNames) do
  begin
    Token := APropNames[i].Trim;
    if Token = '' then Continue;
    IsDesc := Token.EndsWith(' DESC', True);
    PropName := Token;
    if IsDesc then
      PropName := Token.Substring(0, Token.Length - 5).Trim;

    ValA := Unassigned;
    ValB := Unassigned;
    Matched := False;

    // 1. Try Fast-Path via Mapper (Value Extraction)
    PropMap := nil;
    if FEntityMap.Properties.TryGetValue(PropName, PropMap) then
    begin
      if PropMap.FieldValueOffset > 0 then
      begin
        case PropMap.DataType of
          ftInteger, ftAutoInc, ftShortint, ftWord:
            begin
              ValA := PInteger(Pointer(PByte(A) + PropMap.FieldValueOffset))^;
              ValB := PInteger(Pointer(PByte(B) + PropMap.FieldValueOffset))^;
              Matched := True;
            end;
          ftBoolean:
            begin
              // Use PByte for Boolean as it's typically 1 byte in memory
              ValA := PByte(Pointer(PByte(A) + PropMap.FieldValueOffset))^;
              ValB := PByte(Pointer(PByte(B) + PropMap.FieldValueOffset))^;
              Matched := True;
            end;
          ftFloat, ftCurrency:
            begin
              ValA := PDouble(Pointer(PByte(A) + PropMap.FieldValueOffset))^;
              ValB := PDouble(Pointer(PByte(B) + PropMap.FieldValueOffset))^;
              Matched := True;
            end;
        end;
      end;
    end;

    // 2. Try RTTI Fallback
    if (not Matched) and (RttiType <> nil) then
    begin
      RttiProp := nil;
      for p in RttiType.GetProperties do
        if SameText(p.Name, PropName) then
        begin
          RttiProp := p;
          Break;
        end;

      if RttiProp <> nil then
      begin
        ValA := RttiProp.GetValue(A).AsVariant;
        ValB := RttiProp.GetValue(B).AsVariant;
      end
      else
      begin
        RttiField := nil;
        for f in RttiType.GetFields do
          if SameText(f.Name, PropName) or SameText(f.Name, 'F' + PropName) then
          begin
            RttiField := f;
            Break;
          end;

        if RttiField <> nil then
        begin
          ValA := RttiField.GetValue(A).AsVariant;
          ValB := RttiField.GetValue(B).AsVariant;
        end;
      end;
    end;

    // Compare Variants obtained either via Fast-Path or RTTI
    if (not VarIsEmpty(ValA)) and (not VarIsEmpty(ValB)) then
    begin
      if ValA < ValB then Result := -1 else if ValA > ValB then Result := 1;
    end;

    if Result <> 0 then
    begin
      if IsDesc then Result := -Result;
      Break;
    end;
  end;

  // Stable sort tie-breaker (important for tests)
  if (Result = 0) and (not SameText(PropName, 'Id')) then
  begin
    PropMap := nil;
    if FEntityMap.Properties.TryGetValue('Id', PropMap) then
    begin
      IdA := PInteger(Pointer(PByte(A) + PropMap.FieldValueOffset))^;
      IdB := PInteger(Pointer(PByte(B) + PropMap.FieldValueOffset))^;
      if IdA < IdB then Result := -1 else if IdA > IdB then Result := 1;
    end
    else if RttiType <> nil then
    begin
      // Last-resort RTTI tie-breaker
      RttiProp := nil;
      for p in RttiType.GetProperties do
        if SameText(p.Name, 'Id') then
        begin
          RttiProp := p;
          Break;
        end;
      if RttiProp <> nil then
      begin
        ValA := RttiProp.GetValue(A).AsVariant;
        ValB := RttiProp.GetValue(B).AsVariant;
        if ValA < ValB then Result := -1 else if ValA > ValB then Result := 1;
      end;
    end;
  end;
end;

procedure TEntityDataSet.SetItems(const Value: IList<TObject>);
begin
  if FItems <> Value then
  begin
    if FOwnsItems then
      FItems := nil;
    
    FItems := Value;
    FOwnsItems := False; // Por padrão não somos donos de uma lista injetada via property

    if Active then
    begin
      ApplyFilterAndSort;
      Resync([]);
    end;
  end;
end;

{ TEntityDataSet }

procedure TEntityDataSet.InternalOpen;
begin
  FIsCursorOpen := True;
  
  if FEntityClass = nil then
    raise Exception.Create('EntityClass must be defined before opening TEntityDataSet.');

  if Active or (State = dsInactive) then
  begin
    if FieldDefs.Count = 0 then
      BuildFieldDefs;
      
    if FieldCount = 0 then
      CreateFields;
  end;

  ApplyFilterAndSort;
  BookmarkSize := SizeOf(Integer);
  // Importante para o VCL alocar buffers com espaço para o Header
  FRecordSize := SizeOf(TEntityRecordHeader);
  // Native cursor reset
  FCurrentRec := -1;
  BindFields(True);
end;

procedure TEntityDataSet.InternalClose;
begin
  if Assigned(FInsertObj) then
  begin
    FInsertObj.Free;
    FInsertObj := nil;
  end;
  FIsCursorOpen := False;
  FVirtualIndex.Clear;
end;

procedure TEntityDataSet.InternalDelete;
var
  ActualRow: Integer;
  TargetIdx: Integer;
begin
  if not Assigned(FItems) then Exit;

  // 1. Usar exclusivamente a posição capturada no DoBeforeDelete (GetRecNo é 1-based)
  TargetIdx := FPositionBeforeAction - 1; 

  if (TargetIdx >= 0) and (TargetIdx < FVirtualIndex.Count) then
  begin
    // 2. Identificar o índice real na lista física
    ActualRow := FVirtualIndex[TargetIdx];
    
    // 3. Remover das listas
    FVirtualIndex.RemoveAt(TargetIdx);
    FItems.Delete(ActualRow);
    
    // 4. Reconstruir a visão virtual (necessário se houver filtros ou sorteio ativos)
    ApplyFilterAndSort;

    // 5. Estratégia de Reposicionamento para o Delphi TDataSet:
    // O TDataSet executa UpdateCursorPos e Resync nativamente após o InternalDelete.
    // Setando FCurrentRec em TargetIdx - 1, o framework fará o Next para TargetIdx.
    // Como os registros subiram para ocupar a vaga deletada, o sucessor agora é o novo TargetIdx.
    if FVirtualIndex.Count = 0 then
      FCurrentRec := -1
    else
    if TargetIdx <= FVirtualIndex.Count then
      FCurrentRec := TargetIdx
    else
      FCurrentRec := FVirtualIndex.Count;
    // NOTE: We do NOT call Resync([]); here. The base TDataSet manages this event
    // immediately after InternalDelete, ensuring the cursor lands on the correct record.
  end;
end;

procedure TEntityDataSet.InternalPost;
var
  NewIdx: Integer;
  TargetIdx: Integer;
  TargetPos: Integer;
begin
  if State = dsInsert then
  begin
    if Assigned(FInsertObj) then
    begin
      // 1. Decide between Add (Append) or Insert at position (Insert)
      if FIsAppending then
      begin
        FItems.Add(FInsertObj);
        NewIdx := FItems.Count -1;
      end
      else
      begin
        // Adjust 1-based RecNo to 0-based index
        TargetPos := FPositionBeforeAction - 1;

        if (TargetPos < 0) or (FVirtualIndex.Count = 0) then
          TargetIdx := 0
        else if (TargetPos >= FVirtualIndex.Count) then
          TargetIdx := FItems.Count
        else
          // In Insert mode, use the physical index pointed by the virtual view at the position stored in DoBeforeInsert
          TargetIdx := FVirtualIndex[TargetPos];

        if TargetIdx >= FItems.Count then
        begin
          FItems.Add(FInsertObj);
          NewIdx := FItems.Count - 1;
        end
        else
        begin
          FItems.Insert(TargetIdx, FInsertObj);
          NewIdx := TargetIdx;
        end;
      end;

      FInsertObj := nil; 
      FIsAppending := False; 
      FPositionBeforeAction := -2;

      // 2. Update Virtual View
      ApplyFilterAndSort;

      // 3. Position cursor on the new item
      FCurrentRec := FVirtualIndex.IndexOf(NewIdx);

      // 4. Notificar mudança
      DataEvent(deDataSetChange, 0);
    end;
  end
  else if State = dsEdit then
  begin
    ApplyFilterAndSort;
  end;
end;

procedure TEntityDataSet.InternalCancel;
begin
  if (State = dsInsert) and (FInsertObj <> nil) then
  begin
    FInsertObj.Free;
    FInsertObj := nil;
  end;
end;

procedure TEntityDataSet.InternalEdit;
begin
  // No-op.
end;

procedure TEntityDataSet.DoBeforeScroll;
begin
  inherited DoBeforeScroll;
end;

procedure TEntityDataSet.DoAfterScroll;
begin
  inherited DoAfterScroll;
end;

procedure TEntityDataSet.DoBeforeInsert;
begin
  FPositionBeforeAction := GetRecNo;
  inherited DoBeforeInsert;
end;

procedure TEntityDataSet.DoBeforeDelete;
begin
  FPositionBeforeAction := GetRecNo;
  inherited DoBeforeDelete;
end;

procedure TEntityDataSet.InternalInsert;
begin
  FIsAppending := (PEntityRecordHeader(ActiveBuffer).BookmarkFlag = bfEOF);
  if FInsertObj <> nil then
  begin
    FInsertObj.Free;
    FInsertObj := nil;
  end;
  FInsertObj := CreateNewEntity;
  
  if FInsertObj = nil then
    raise Exception.Create('Auto-append needs a parameterless constructor for ' + FEntityClass.ClassName);

  if (Pointer(ActiveBuffer) <> nil) then
  begin
    // Set virtual control flags for the new insertion buffer
    PEntityRecordHeader(Pointer(ActiveBuffer)).BookmarkIndex := -2; 
    PEntityRecordHeader(Pointer(ActiveBuffer)).BookmarkFlag := bfInserted;
  end;
end;

procedure TEntityDataSet.InternalAddRecord(Buffer: TRecordBuffer; Append: Boolean);
begin
  FIsAppending := Append;
  InternalPost;
end;

procedure TEntityDataSet.InternalFirst;
begin
  FCurrentRec := -1;
end;

procedure TEntityDataSet.InternalLast;
begin
  FCurrentRec := FVirtualIndex.Count;
end;

procedure TEntityDataSet.InternalInitFieldDefs;

  function MapTypeToFieldType(ATypeInfo: PTypeInfo): TFieldType;
  var
    TypeName: string;
  begin
    if ATypeInfo = nil then Exit(ftUnknown);
    case ATypeInfo.Kind of
      tkInteger, tkEnumeration:
      begin
        TypeName := string(ATypeInfo.Name);
        if (ATypeInfo = TypeInfo(Boolean)) or (TypeName = 'Boolean') or (TypeName = 'WordBool') or (TypeName = 'ByteBool') or (TypeName = 'LongBool') then
          Exit(ftBoolean)
        else
          Exit(ftInteger);
      end;
      tkFloat:
      begin
        if ATypeInfo = TypeInfo(TDateTime) then
          Exit(ftDateTime)
        else if ATypeInfo = TypeInfo(Currency) then
          Exit(ftCurrency)
        else
          Exit(ftFloat);
      end;
      tkString, tkLString, tkWString, tkUString, tkChar, tkWChar:
        Exit(ftWideString);
      tkInt64:
        Exit(ftLargeint);
      tkVariant:
        Exit(ftVariant);
      tkRecord:
      begin
        var InnerInfo := TReflection.GetUnderlyingType(ATypeInfo);
        if InnerInfo <> nil then
          Exit(MapTypeToFieldType(InnerInfo))
        else
          Exit(ftUnknown);
      end;
    else
      Exit(ftUnknown);
    end;
  end;

  function IsTBytesType(ATypeInfo: PTypeInfo): Boolean;
  begin
    Result := (ATypeInfo <> nil) and (ATypeInfo.Kind = tkDynArray) and
              (ATypeInfo = TypeInfo(TBytes));
  end;

var
  Context: TRttiContext;
  FieldDef: TFieldDef;
  NewField: TField;
  Prop: TRttiProperty;
  PropMap: TPropertyMap;
  ResolvedType: TFieldType;
  RttiField: TRttiField;
  RttiType: TRttiType;
begin
  if (FEntityMap = nil) or (FEntityClass = nil) then Exit;

  FieldDefs.Clear;

  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(FEntityClass);

    for PropMap in FEntityMap.Properties.Values do
    begin
      if PropMap.IsIgnored or PropMap.IsNavigation then Continue;

      // Calcular resolved type dinamicamente
      ResolvedType := PropMap.DataType;

      // Se tivermos Shadow mapping, ler do RTTI da Classe estática (Prop ou Field)
      if (ResolvedType = ftUnknown) and (RttiType <> nil) then
      begin
        Prop := RttiType.GetProperty(PropMap.PropertyName);
        if Prop <> nil then
        begin
          if IsTBytesType(Prop.PropertyType.Handle) then
            ResolvedType := ftBlob
          else
            ResolvedType := MapTypeToFieldType(Prop.PropertyType.Handle);
        end
        else
        begin
           RttiField := RttiType.GetField(PropMap.PropertyName);
           if RttiField = nil then RttiField := RttiType.GetField('F' + PropMap.PropertyName);

           if RttiField <> nil then
           begin
              if IsTBytesType(RttiField.FieldType.Handle) then
                ResolvedType := ftBlob
              else
                ResolvedType := MapTypeToFieldType(RttiField.FieldType.Handle);

              // Update FieldOffset if not yet set
              if PropMap.FieldValueOffset <= 0 then
                PropMap.FieldValueOffset := RttiField.Offset;
           end;
        end;
      end;

      // Se ainda for desconhecido e houver PTypeInfo no map
      if (ResolvedType = ftUnknown) and Assigned(PropMap.PropertyType) then
        ResolvedType := MapTypeToFieldType(PropMap.PropertyType);

      // CRITICAL: Persist resolved type back into PropMap
      if (ResolvedType in [ftString, ftWideString]) and (PropMap.MaxLength > 255) then
        ResolvedType := ftMemo;

      if (PropMap.DataType = ftUnknown) and (ResolvedType <> ftUnknown) then
        PropMap.DataType := ResolvedType;

      // 1. Popular FieldDefs para metadados
      FieldDef := FieldDefs.AddFieldDef;
      FieldDef.Name := PropMap.PropertyName;
      FieldDef.DataType := ResolvedType;
      if PropMap.MaxLength > 0 then
        FieldDef.Size := PropMap.MaxLength
      else if ResolvedType in [ftString, ftWideString] then
        FieldDef.Size := 255;

      // 2. Instanciar os TFields dinamicamente
      if Fields.FindField(PropMap.PropertyName) = nil then
      begin
        NewField := nil;
        case ResolvedType of
          ftWideString: NewField := TWideStringField.Create(Self);
          ftString: NewField := TStringField.Create(Self);
          ftInteger, ftSmallint: NewField := TIntegerField.Create(Self);
          ftLargeint: NewField := TLargeintField.Create(Self);
          ftFloat: NewField := TFloatField.Create(Self);
          ftCurrency: NewField := TCurrencyField.Create(Self);
          ftBoolean: NewField := TBooleanField.Create(Self);
          ftDateTime: NewField := TDateTimeField.Create(Self);
          ftDate: NewField := TDateField.Create(Self);
          ftTime: NewField := TTimeField.Create(Self);
          ftBlob: NewField := TBlobField.Create(Self);
          ftMemo: NewField := TMemoField.Create(Self);
        end;

        if NewField <> nil then
        begin
          NewField.FieldName := PropMap.PropertyName;
          if NewField is TStringField then
          begin
            if PropMap.MaxLength > 0 then
              TStringField(NewField).Size := PropMap.MaxLength
            else
              TStringField(NewField).Size := 255;
          end
          else if NewField is TWideStringField then
          begin
            if PropMap.MaxLength > 0 then
              TWideStringField(NewField).Size := PropMap.MaxLength
            else
              TWideStringField(NewField).Size := 255;
          end;

          NewField.Required := PropMap.IsRequired and (not PropMap.IsAutoInc);
          NewField.ReadOnly := PropMap.IsAutoInc;
          NewField.DataSet := Self;
        end;
      end;
    end;
  finally
    Context.Free;
  end;
end;

procedure TEntityDataSet.BuildFieldDefs;
begin
  InternalInitFieldDefs;
end;

function TEntityDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  GetMem(Pointer(Result), FRecordSize);
  InternalInitRecord(Result);
end;

procedure TEntityDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Pointer(Buffer));
end;

function TEntityDataSet.CreateNewEntity: TObject;
var
  Context: TRttiContext;
  RttiMethod: TRttiMethod;
  RttiType: TRttiType;
begin
  Result := nil;
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(FEntityClass);
    if RttiType <> nil then
    begin
      for RttiMethod in RttiType.GetMethods do
      begin
        // Busca constructor sem parâmetros
        if RttiMethod.IsConstructor and (Length(RttiMethod.GetParameters) = 0) then
        begin
          Result := RttiMethod.Invoke(FEntityClass, []).AsObject;
          Break;
        end;
      end;
    end;
  finally
    Context.Free;
  end;
end;

procedure TEntityDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FillChar(Buffer^, FRecordSize, 0);
  PEntityRecordHeader(Buffer).BookmarkIndex := -2;
end;

procedure TEntityDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PInteger(Data)^ := PEntityRecordHeader(Buffer).BookmarkIndex;
end;

procedure TEntityDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PEntityRecordHeader(Buffer).BookmarkIndex := PInteger(Data)^;
end;

procedure TEntityDataSet.InternalGotoBookmark(Bookmark: TBookmark);
begin
  FCurrentRec := PInteger(Pointer(Bookmark))^;
end;

function TEntityDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PEntityRecordHeader(Buffer).BookmarkFlag;
end;

procedure TEntityDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PEntityRecordHeader(Buffer).BookmarkFlag := Value;
end;

procedure TEntityDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
var
  Idx: Integer;
begin
  Idx := PEntityRecordHeader(Buffer).BookmarkIndex;
  if (Idx >= 0) and (Idx < FVirtualIndex.Count) then
    FCurrentRec := Idx;
end;

function TEntityDataSet.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TEntityDataSet.GetRecordCount: Integer;
begin
  Result := FVirtualIndex.Count;
end;

function TEntityDataSet.GetRecNo: Integer;
begin
  CheckActive;
  if Pointer(ActiveBuffer) <> nil then
    Result := PEntityRecordHeader(Pointer(ActiveBuffer)).BookmarkIndex + 1
  else
    Result := FCurrentRec + 1;
end;

procedure TEntityDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  Value := System.Math.Min(System.Math.Max(Value, 1), RecordCount);
  if RecNo <> Value then
  begin
    DoBeforeScroll;
    FCurrentRec := Value - 1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

function TEntityDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
var
  Idx: Integer;
begin
  Result := False;
  if (Pointer(Bookmark) = nil) or (FVirtualIndex.Count = 0) then
    Exit;

  // In virtual datasets, the bookmark stores the logical index.
  Idx := PInteger(Pointer(Bookmark))^;
  Result := (Idx >= 0) and (Idx < FVirtualIndex.Count);
end;

function TEntityDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoSearch: Boolean): TGetResult;
var
  Header: PEntityRecordHeader;
begin
  Header := PEntityRecordHeader(Buffer);

  case GetMode of
    gmNext:
      begin
        if FCurrentRec < FVirtualIndex.Count - 1 then
        begin
          Inc(FCurrentRec);
          Result := grOK;
        end
        else
          Result := grEOF;
      end;
      
    gmPrior:
      begin
        if FCurrentRec >= 0 then
          Dec(FCurrentRec);
        if FCurrentRec < 0 then
          Result := grBOF
        else
          Result := grOK;
      end;
      
    gmCurrent:
      begin
        if (FCurrentRec < 0) or (FCurrentRec >= FVirtualIndex.Count) then
          Result := grError
        else
          Result := grOK;
      end;
  else
    Result := grError;
  end;

  if Result = grOK then
  begin
     Header.BookmarkIndex := FCurrentRec;
     Header.BookmarkFlag := bfCurrent;
  end
  else if Result = grEOF then
  begin
     Header.BookmarkFlag := bfEOF;
  end
  else if Result = grBOF then
  begin
     Header.BookmarkFlag := bfBOF;
  end;
end;

// ---------------------------------------------------------------------------
//  ReadFieldValue - Core universal method: reads a property from the entity
// ---------------------------------------------------------------------------
function TEntityDataSet.ReadFieldValue(Field: TField; out Value: Variant): Boolean;
var
  BlobData: TArray<Byte>;
  CurrentObj: TObject;
  Header: PEntityRecordHeader;
  PropMap: TPropertyMap;
  PValue: Pointer;
begin
  Result := False;
  Value := Unassigned;

  if not Active then Exit;
  Header := PEntityRecordHeader(ActiveBuffer);

  // 1. Identify target object - ABSOLUTE PRIORITY to active buffer painting (Grid)
  CurrentObj := nil;
  
  if (Header <> nil) then
  begin
    if (Header.BookmarkIndex = -2) then
      CurrentObj := FInsertObj
    else if (Header.BookmarkIndex >= 0) and (Header.BookmarkIndex < FVirtualIndex.Count) then
      CurrentObj := FItems[FVirtualIndex[Header.BookmarkIndex]];
  end;

  // 2. Fallback to global cursor (programmatic Field.Value access or navigation outside the painting loop)
  if (CurrentObj = nil) and (FCurrentRec >= 0) and (FCurrentRec < FVirtualIndex.Count) then
  begin
     CurrentObj := FItems[FVirtualIndex[FCurrentRec]];
  end;

  if (CurrentObj = nil) or (FEntityMap = nil) then Exit;

  if not FEntityMap.Properties.TryGetValue(Field.FieldName, PropMap) then
    Exit;

  // 3. RTTI Fallback if field offset is not defined or represents a Smart Type / Nullable Wrapper
  if (PropMap.FieldValueOffset <= 0) or 
     ((PropMap.PropertyType <> nil) and TReflection.IsSmartProp(PropMap.PropertyType)) then
  begin
    var RttiType := TReflection.Context.GetType(FEntityClass);
    if RttiType <> nil then
    begin
      var RttiProp := RttiType.GetProperty(Field.FieldName);
      if RttiProp <> nil then
      begin
        var TempValue := RttiProp.GetValue(CurrentObj);
        var UnwrappedValue: TValue;
        if TReflection.TryUnwrapProp(TempValue, UnwrappedValue) then
        begin
          if UnwrappedValue.IsEmpty then Exit;
          Value := UnwrappedValue.AsVariant;
        end
        else
          Value := TempValue.AsVariant;
        Result := True;
        Exit;
      end;

      var RttiField := RttiType.GetField(Field.FieldName);
      if RttiField <> nil then
      begin
        var TempValue := RttiField.GetValue(CurrentObj);
        var UnwrappedValue: TValue;
        if TReflection.TryUnwrapProp(TempValue, UnwrappedValue) then
        begin
          if UnwrappedValue.IsEmpty then Exit;
          Value := UnwrappedValue.AsVariant;
        end
        else
          Value := TempValue.AsVariant;
        Result := True;
        Exit;
      end;
    end;
    Exit;
  end;

  // Leitura direta por offset (fast-path)
  // Somente checa flag de nulo se for um SmartProp (FieldValueOffset > 0) que possui flag (FieldOffset > 0)
  if (PropMap.FieldValueOffset > 0) and (PropMap.FieldOffset > 0) and
     not PBoolean(Pointer(PByte(CurrentObj) + PropMap.FieldOffset))^ then
    Exit;

  // Determinar o ponteiro para o valor real
  if PropMap.FieldValueOffset > 0 then
    PValue := Pointer(PByte(CurrentObj) + PropMap.FieldValueOffset)
  else
    PValue := Pointer(PByte(CurrentObj) + PropMap.FieldOffset);

  case Field.DataType of
    ftString, ftWideString, ftMemo, ftWideMemo:
      Value := PString(PValue)^;
    ftInteger, ftSmallint:
      Value := PInteger(PValue)^;
    ftLargeint:
      Value := PInt64(PValue)^;
    ftFloat:
      Value := PDouble(PValue)^;
    ftCurrency:
      Value := PCurrency(PValue)^;
    ftBoolean:
      Value := PBoolean(PValue)^;
    ftDateTime, ftDate, ftTime:
      Value := PDateTime(PValue)^;
    ftBlob:
    begin
      // Para Blob, retornar True se houver dados (IsNull check)
      // O conteúdo real é servido por CreateBlobStream
      BlobData := TBytes(PValue^);
      if Length(BlobData) = 0 then
        Exit; // Result fica False = IsNull
      Value := True; // Sinaliza não-nulo
    end;
  else
    Exit; // Tipo não mapeado diretamente
  end;
  
  Result := True;
end;

// ---------------------------------------------------------------------------
//  GetFieldData — Override with TValueBuffer (TArray<Byte>) for modern Delphi
//  This is the method TField.GetData actually calls (XE3+).
// ---------------------------------------------------------------------------
function TEntityDataSet.GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean;
var
  DblVal: Double;
  I64Val: Int64;
  IntVal: Integer;
  S: string;
  TempBuf: TValueBuffer;
  TempBytes: TBytes;
  V: Variant;
  WB: WordBool;
begin
  Result := ReadFieldValue(Field, V);
  if not Result then Exit;
  
  // Se Buffer vazio, apenas check de null (TField chama com buffer vazio para testar IsNull)
  if Length(Buffer) = 0 then
    Exit;

  case Field.DataType of
    ftWideString, ftFixedWideChar:
    begin
      S := VarToStr(V);
      TempBytes := TEncoding.Unicode.GetBytes(S);
      // Adicionar null terminator Unicode (2 bytes)
      SetLength(TempBytes, Length(TempBytes) + SizeOf(Char));
      TempBytes[Length(TempBytes) - 2] := 0;
      TempBytes[Length(TempBytes) - 1] := 0;
      Move(TempBytes[0], Buffer[0], Min(Length(TempBytes), Length(Buffer)));
    end;
    
    ftString, ftFixedChar:
    begin
      S := VarToStr(V);
      TempBytes := TEncoding.Default.GetBytes(S);
      Move(TempBytes[0], Buffer[0], Min(Length(TempBytes), Length(Buffer)));
      // Null terminator ANSI
      if Length(TempBytes) < Length(Buffer) then
        Buffer[Length(TempBytes)] := 0;
    end;

    ftInteger, ftSmallint, ftAutoInc:
    begin
      IntVal := V;
      Move(IntVal, Buffer[0], SizeOf(Integer));
    end;

    ftLargeint:
    begin
      I64Val := V;
      Move(I64Val, Buffer[0], SizeOf(Int64));
    end;

    ftFloat:
    begin
      DblVal := V;
      Move(DblVal, Buffer[0], SizeOf(Double));
    end;

    ftCurrency:
    begin
      DblVal := V;
      Move(DblVal, Buffer[0], SizeOf(Double));
    end;

    ftBoolean:
    begin
      WB := WordBool(Boolean(V));
      Move(WB, Buffer[0], SizeOf(WordBool));
    end;

    ftDate, ftTime, ftDateTime:
    begin
      DblVal := V;
      // DateTime precisa de DataConvert para NativeFormat
      SetLength(TempBuf, SizeOf(Double));
      Move(DblVal, TempBuf[0], SizeOf(Double));
      DataConvert(Field, TempBuf, Buffer, True);
    end;
  else
    Result := False;
  end;
end;

procedure TEntityDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer);
var
  CurrentObj: TObject;
  Header: PEntityRecordHeader;
  P: Pointer;
  PropMap: TPropertyMap;
  PValue: Pointer;
begin
  CurrentObj := nil;
  Header := nil;
  if Pointer(ActiveBuffer) <> nil then
    Header := PEntityRecordHeader(ActiveBuffer);

  // Extract raw pointer from TValueBuffer
  if Length(Buffer) > 0 then
    P := @Buffer[0]
  else
    P := nil;

  // Identifica o objeto de destino (Insert ou registro existente)
  if (Header <> nil) and (Header.BookmarkIndex = -2) then
    CurrentObj := FInsertObj
  else if (Header <> nil) and (Header.BookmarkIndex >= 0) and (Header.BookmarkIndex < FVirtualIndex.Count) then
    CurrentObj := FItems[FVirtualIndex[Header.BookmarkIndex]]
  else if (State = dsInsert) then
    CurrentObj := FInsertObj
  else if (FCurrentRec >= 0) and (FCurrentRec < FVirtualIndex.Count) then
    CurrentObj := FItems[FVirtualIndex[FCurrentRec]];

  if (CurrentObj = nil) or (FEntityMap = nil) then Exit;

  if not FEntityMap.Properties.TryGetValue(Field.FieldName, PropMap) then
    Exit;

  if PropMap.FieldValueOffset > 0 then
    PValue := Pointer(PByte(CurrentObj) + PropMap.FieldValueOffset)
  else
    PValue := Pointer(PByte(CurrentObj) + PropMap.FieldOffset);

  // Atualização da flag de nulo (somente para SmartProps que possuem tal flag)
  if (PropMap.FieldValueOffset > 0) and (PropMap.FieldOffset > 0) then
  begin
    PBoolean(Pointer(PByte(CurrentObj) + PropMap.FieldOffset))^ := (P <> nil);
  end;

  // RTTI fallback path (for properties without direct offset or generic wrappers)
  if (PropMap.FieldValueOffset <= 0) or
     ((PropMap.PropertyType <> nil) and TReflection.IsSmartProp(PropMap.PropertyType)) then
  begin
    var RttiType := TReflection.Context.GetType(FEntityClass);
    if RttiType <> nil then
    begin
      var RttiProp := RttiType.GetProperty(Field.FieldName);
      if RttiProp <> nil then
      begin
        if P = nil then
          TReflection.SetValue(CurrentObj, RttiProp, TValue.Empty)
        else
          TReflection.SetValue(CurrentObj, RttiProp, TValueBufferToValue(Buffer, Field.DataType));
        SetModified(True);
        Exit;
      end;

      var RttiField := RttiType.GetField(Field.FieldName);
      if RttiField <> nil then
      begin
        if P = nil then
          TReflection.SetValue(CurrentObj, RttiField, TValue.Empty)
        else
          TReflection.SetValue(CurrentObj, RttiField, TValueBufferToValue(Buffer, Field.DataType));
        SetModified(True);
        Exit;
      end;
    end;
  end;

  // Direct offset write path (fast-path)
  if P <> nil then
  begin
    case Field.DataType of
      ftString, ftWideString, ftMemo, ftWideMemo:
        PString(PValue)^ := string(PWideChar(P));
      ftInteger, ftSmallint:
        PInteger(PValue)^ := PInteger(P)^;
      ftLargeint:
        PInt64(PValue)^ := PInt64(P)^;
      ftFloat, ftCurrency:
        PDouble(PValue)^ := PDouble(P)^;
      ftBoolean:
        PBoolean(PValue)^ := PWordBool(P)^ <> False;
      ftDateTime, ftDate, ftTime:
        PDouble(PValue)^ := PDouble(P)^;
    else
      Exit;
    end;
  end
  else
  begin
    // Buffer vazio = limpar campo
    case Field.DataType of
      ftString, ftWideString, ftMemo, ftWideMemo:
        PString(PValue)^ := '';
      ftInteger, ftSmallint:
        PInteger(PValue)^ := 0;
      ftLargeint:
        PInt64(PValue)^ := 0;
      ftFloat, ftCurrency:
        PDouble(PValue)^ := 0;
      ftBoolean:
        PBoolean(PValue)^ := False;
      ftDateTime, ftDate, ftTime:
        PDouble(PValue)^ := 0;
    end;
  end;

  SetModified(True);
end;

procedure TEntityDataSet.InternalHandleException;
begin
  // No-op. Exceções em memória não exigem buffer rollback ou handle físico de database.
end;

function TEntityDataSet.IsCursorOpen: Boolean;
begin
  Result := FIsCursorOpen;
end;

end.
