unit Dext.Entity.DataSet;

interface

{$POINTERMATH ON}

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.Rtti,
  System.TypInfo,
  System.Math,
  System.DateUtils,
  Data.DB,
  Dext.Core.DateUtils,
  Dext.Utils,
  Dext.Collections,
  Dext.Collections.Vector,
  Dext.Collections.Dict,
  Dext.Core.Span,
  Dext.Entity.Mapping,
  Dext.Entity.Context,
  Dext.Json.Utf8;

type
  PBytes = ^TBytes;

  /// <summary>
  ///   Data Structure of a Record Buffer for TEntityDataSet.
  ///   Stores fully updated bytes and modification trackers.
  /// </summary>
  PEntityRecordHeader = ^TEntityRecordHeader;
  TEntityRecordHeader = packed record
    BookmarkIndex: Integer;
    BookmarkFlag: TBookmarkFlag;
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
    FDbContext: TDbContext;
    
    // Virtual Buffers (Offsets Index)
    // Physical Objects Reference
    FItems: IList<TObject>;            // Real reference to the object list
    FOwnsItems: Boolean;               // Whether the dataset owns the list and should clear it
    FOwnsEntityMap: Boolean;           // Whether the dataset owns the map
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
    FInsertObjRef: TObject; // Reference to track after post
    FIsAppending: Boolean;
    FPositionBeforeAction: Integer;
    FCalcOffsets: TDictionary<string, Integer>;
    FCalcAreaSize: Integer;
    FInternalCalcStorage: TArray<TBytes>;
    FPropertyCache: TDictionary<string, TRttiProperty>;

    function GetProperty(const APropName: string): TRttiProperty;
    procedure SetItems(const Value: IList<TObject>);
    procedure SetIndexFieldNames(const Value: string);
    procedure ApplyFilterAndSort; overload;
    procedure ApplyFilterAndSort(AFiltered: Boolean); overload;
    procedure ApplyFilterAndSort(AFiltered: Boolean; ATrackObj: TObject); overload;
    function CompareObjectsInternal(A, B: TObject; const APropNames: TArray<string>; RttiType: TRttiType): Integer;
    procedure BuildFieldDefs;
    
    function ReadFieldValue(Field: TField; out Value: Variant): Boolean;
  protected
    // TDataSet overrides for filtering and sorting
    procedure InternalHandleException; override;
    function IsCursorOpen: Boolean; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterText(const Value: string); override;

    // Mandatory TDataSet overrides
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalInitFieldDefs; override;

    // Buffer Alocations
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure InternalInitRecord(Buffer: TRecBuf); override;
    procedure CalculateFields(Buffer: TRecBuf); override;
    
    // Bookmark and Navigation
    procedure GetBookmarkData(Buffer: TRecBuf; Data: TBookmark); override;
    procedure SetBookmarkData(Buffer: TRecBuf; Data: TBookmark); override;
    function GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag); override;
    procedure InternalSetToRecord(Buffer: TRecBuf); override;
    procedure InternalGotoBookmark(Bookmark: TBookmark); override;
    
    function GetRecordSize: Word; override;
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    function GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    procedure SetRecNo(Value: Integer); override;

    procedure InternalAddRecord(Buffer: TRecBuf; Append: Boolean); override;
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

    function GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean; overload; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload; override;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); overload; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); overload; override;
    
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions = []): Boolean; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    /// <summary>
    ///  Object data loading (Non-generic legacy)
    /// </summary>
    procedure Load(const AItems: IList<TObject>; AClass: TClass; AOwns: Boolean = False); overload;
    procedure Load(const AItems: TArray<TObject>; AClass: TClass); overload;
    
    /// <summary>
    ///  Generic Object data loading
    /// </summary>
    procedure Load<T: class>(const AItems: IList<T>; AOwns: Boolean = False); overload;
    procedure Load<T: class>(const AItems: TArray<T>); overload;

    /// <summary>
    ///  Generic Fluent Object matching (Json and other string sources)
    /// </summary>
    procedure LoadFromJson(const AJson: string; AClass: TClass); overload;
    procedure LoadFromJson<T: class>(const AJson: string); overload;
    
    /// <summary>
    ///  Data Export to Json
    /// </summary>
    function AsJsonArray: string;
    function AsJsonObject: string;
    
    /// <summary>
    ///  UTF-8 JSON data loading (Zero-Alloc Pipeline)
    /// </summary>
    procedure LoadFromUtf8Json(const ASpan: TByteSpan; AClass: TClass); overload;
    procedure LoadFromUtf8Json<T: class>(const ASpan: TByteSpan); overload;
    procedure Refresh;

    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetCurrentObject: TObject;
    property Items: IList<TObject> read FItems write SetItems;
    property DbContext: TDbContext read FDbContext write FDbContext;
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
  System.AnsiStrings,
  Dext.Specifications.Evaluator,
  Dext.Specifications.Interfaces,
  Dext.Specifications.Parser,
  Dext.Core.Reflection,
  Dext.Core.Activator,
  Dext.Core.ValueConverters,
  Dext.Entity,
  Dext.Json;

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
var
  S: string;
begin
  case ADataType of
    ftString:
    begin
      S := TEncoding.Default.GetString(ABuffer);
      Result := TValue.From<string>(S.TrimRight([#0]));
    end;
    ftWideString, ftMemo, ftWideMemo:
    begin
      S := TEncoding.Unicode.GetString(ABuffer);
      Result := TValue.From<string>(S.TrimRight([#0]));
    end;
    ftInteger, ftSmallint, ftAutoInc:
      Result := TValue.From<Integer>(PInteger(@ABuffer[0])^);
    ftLargeint:
      Result := TValue.From<Int64>(PInt64(@ABuffer[0])^);
    ftFloat, ftCurrency:
      Result := TValue.From<Double>(PDouble(@ABuffer[0])^);
    ftBoolean:
      Result := TValue.From<Boolean>(PBoolean(@ABuffer[0])^);
    ftDateTime, ftDate, ftTime:
      Result := TValue.From<TDateTime>(PDouble(@ABuffer[0])^);
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

function TEntityDataSet.GetProperty(const APropName: string): TRttiProperty;
begin
  Result := nil;
  if (FEntityClass = nil) then Exit;
  if not FPropertyCache.TryGetValue(APropName, Result) then
  begin
    var LType := TReflection.Context.GetType(FEntityClass);
    if LType <> nil then
      Result := LType.GetProperty(APropName);
    FPropertyCache.Add(APropName, Result);
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
  FCalcOffsets := TDictionary<string, Integer>.Create;
  FPropertyCache := TDictionary<string, TRttiProperty>.Create;
end;

destructor TEntityDataSet.Destroy;
begin
  FCalcOffsets.Free;
  if Assigned(FInsertObj) then
    FInsertObj.Free;
    
  FPropertyCache.Free;
    
  if FOwnsItems then
    FItems := nil; // IList cuidará da liberação se for o caso
  FItems := nil;
  
  if Assigned(FEntityMap) and FOwnsEntityMap then
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
    if Assigned(FDbContext) then
    begin
      FEntityMap := FDbContext.ModelBuilder.GetMap(AClass.ClassInfo);
      FOwnsEntityMap := False;
    end;

    if FEntityMap = nil then
    begin
      FEntityMap := TEntityMap.Create(AClass.ClassInfo);
      FEntityMap.DiscoverAttributes;
      FOwnsEntityMap := True;
    end;
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

function TEntityDataSet.AsJsonArray: string;
begin
  if FItems = nil then
    Exit('[]');
  // Serialize the items using Dext.Json
  Result := TDextJson.Serialize(TValue.From<IList<TObject>>(FItems));
end;

function TEntityDataSet.AsJsonObject: string;
begin
  var Obj := GetCurrentObject;
  if Obj = nil then
    Exit('{}');
  Result := TDextJson.Serialize(Obj);
end;

procedure TEntityDataSet.Load<T>(const AItems: TArray<T>);
var
  i: Integer;
  ObjArray: TArray<TObject>;
begin
  SetLength(ObjArray, Length(AItems));
  for i := 0 to High(AItems) do
    ObjArray[i] := TObject(AItems[i]);
  Load(ObjArray, T);
end;

procedure TEntityDataSet.Load<T>(const AItems: IList<T>; AOwns: Boolean);
var
  i: Integer;
  ListObj: IList<TObject>;
begin
  ListObj := TCollections.CreateList<TObject>(AOwns);
  for i := 0 to AItems.Count - 1 do
    ListObj.Add(TObject(AItems[i]));
  Load(ListObj, T, AOwns);
end;

procedure TEntityDataSet.LoadFromJson(const AJson: string; AClass: TClass);
var
  Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(AJson);
  LoadFromUtf8Json(TByteSpan.FromBytes(Bytes), AClass);
end;

procedure TEntityDataSet.LoadFromJson<T>(const AJson: string);
begin
  LoadFromJson(AJson, T);
end;

procedure TEntityDataSet.LoadFromUtf8Json<T>(const ASpan: TByteSpan);
begin
  LoadFromUtf8Json(ASpan, T);
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
    FOwnsEntityMap := True;
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
                  
                  // Ativar flag de nulo se definido (SmartProp, Nullable, Lazy)
                  if PropMap.FieldOffset > 0 then
                    PBoolean(Pointer(PByte(CurrentObj) + PropMap.FieldOffset))^ := True;

                  case PropMap.DataType of
                    ftString, ftWideString:
                      PString(PValue)^ := Reader.GetString;
                    ftInteger, ftSmallint:
                      PInteger(PValue)^ := Reader.GetInt32;
                    ftLargeint:
                      PInt64(PValue)^ := Reader.GetInt64;
                    ftFloat:
                      PDouble(PValue)^ := Reader.GetDouble;
                    ftCurrency:
                      PCurrency(PValue)^ := Reader.GetDouble;
                    ftBoolean:
                      PBoolean(PValue)^ := Reader.GetBoolean;
                    ftDateTime, ftDate, ftTime:
                    begin
                      var LDate: TDateTime;
                      if TryParseISODateTime(Reader.GetString, LDate) then
                        PDateTime(PValue)^ := LDate;
                    end;
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

  // Ativar o dataset e reconstruir visão virtual
  Load(FItems, AClass, True);
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
begin
  ApplyFilterAndSort(AFiltered, nil);
end;

procedure TEntityDataSet.ApplyFilterAndSort(AFiltered: Boolean; ATrackObj: TObject);
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
  CurrentObj := ATrackObj;
  if (CurrentObj = nil) and (FCurrentRec >= 0) and (FCurrentRec < FVirtualIndex.Count) then
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
          ftFloat:
            begin
              ValA := PDouble(Pointer(PByte(A) + PropMap.FieldValueOffset))^;
              ValB := PDouble(Pointer(PByte(B) + PropMap.FieldValueOffset))^;
              Matched := True;
            end;
          ftCurrency:
            begin
              ValA := PCurrency(Pointer(PByte(A) + PropMap.FieldValueOffset))^;
              ValB := PCurrency(Pointer(PByte(B) + PropMap.FieldValueOffset))^;
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
          if SameText(f.Name, PropName) or SameText(f.Name, TReflection.NormalizeFieldName(PropName)) then
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
      if PropMap.FieldValueOffset > 0 then
      begin
        IdA := PInteger(Pointer(PByte(A) + PropMap.FieldValueOffset))^;
        IdB := PInteger(Pointer(PByte(B) + PropMap.FieldValueOffset))^;
        if IdA < IdB then Result := -1 else if IdA > IdB then Result := 1;
      end;
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
var
  LCalcSize: Integer;
  i: Integer;
  Offset: Integer;
  LDef: TFieldDef;
begin
  FIsCursorOpen := True;
  
  if FEntityClass = nil then
    raise Exception.Create('EntityClass must be defined before opening TEntityDataSet.');

  if Active or (State = dsInactive) then
  begin
    if FieldDefs.Count = 0 then
      BuildFieldDefs;
      
    // CRITICAL: Synchronize FieldDefs with existing persistent Fields
    // (e.g. manual InternalCalc added while closed)
    for i := 0 to Fields.Count - 1 do
    begin
      if FieldDefs.IndexOf(Fields[i].FieldName) < 0 then
      begin
        LDef := FieldDefs.AddFieldDef;
        LDef.Name := Fields[i].FieldName;
        LDef.DataType := Fields[i].DataType;
        LDef.Size := Fields[i].Size;
      end;
    end;

    if FieldCount = 0 then
      CreateFields;
  end;

  ApplyFilterAndSort;
  BookmarkSize := SizeOf(Integer);

  // Calcular tamanho necessário para campos calculados
  LCalcSize := 0;
  FCalcOffsets.Clear;
  for i := 0 to Fields.Count - 1 do
  begin
    if Fields[i].FieldKind in [fkCalculated, fkLookup, fkInternalCalc] then
    begin
       // Spacing for Null Flag (1 byte) + Data
       Offset := SizeOf(TEntityRecordHeader) + LCalcSize + 1;
       FCalcOffsets.Add(Fields[i].FieldName, Offset);
       Inc(LCalcSize, Fields[i].DataSize + 1); 
    end;
  end;

  // Importante para o VCL alocar buffers com espaço para o Header + campos calculados
  FCalcAreaSize := LCalcSize;
  FRecordSize := SizeOf(TEntityRecordHeader) + FCalcAreaSize;
  
  SetLength(FInternalCalcStorage, Max(0, FItems.Count));
  for i := 0 to High(FInternalCalcStorage) do FInternalCalcStorage[i] := nil;
  
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
  SetLength(FInternalCalcStorage, 0);
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
  Header: PEntityRecordHeader;
  PhysicalIdx: Integer;
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
        // Adjust 1-based RecNo to 0-based index or use the current virtual position
        TargetPos := FPositionBeforeAction;

        if (TargetPos < 0) or (FVirtualIndex.Count = 0) then
          TargetIdx := 0
        else if (TargetPos >= FVirtualIndex.Count) then
          TargetIdx := FItems.Count
        else
          // In Insert mode, use the physical index pointed by the virtual view at the position stored in DoBeforeInsert
          TargetIdx := FVirtualIndex[TargetPos];

        if TargetIdx > FItems.Count then
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

      if (FCalcAreaSize > 0) and (NewIdx >= 0) and (NewIdx < FItems.Count) then
      begin
        if NewIdx >= Length(FInternalCalcStorage) then 
          SetLength(FInternalCalcStorage, FItems.Count + 10);
        SetLength(FInternalCalcStorage[NewIdx], FCalcAreaSize);
        Move(PByte(NativeInt(ActiveBuffer) + SizeOf(TEntityRecordHeader))^, FInternalCalcStorage[NewIdx][0], FCalcAreaSize);
      end;

      FInsertObjRef := FInsertObj;
      FInsertObj := nil; 
      FIsAppending := False; 
      FPositionBeforeAction := -2;

      // 2. Update Virtual View and track new object
      ApplyFilterAndSort(Filtered, FInsertObjRef);
      FInsertObjRef := nil;

      // 3. Position cursor on the new item
      // 4. Reset tracking state if context is present
      if FDbContext <> nil then
        FDbContext.ChangeTracker.Track(FInsertObj, esUnchanged);

      // 5. Notificar mudança
      DataEvent(deDataSetChange, 0);
    end;
  end
  else if State = dsEdit then
  begin
    Header := PEntityRecordHeader(ActiveBuffer);
    if (Header <> nil) and (Header.BookmarkIndex >= 0) and (Header.BookmarkIndex < FVirtualIndex.Count) then
    begin
      PhysicalIdx := FVirtualIndex[Header.BookmarkIndex];
      if FCalcAreaSize > 0 then
      begin
        if PhysicalIdx >= Length(FInternalCalcStorage) then 
          SetLength(FInternalCalcStorage, PhysicalIdx + 10);
        SetLength(FInternalCalcStorage[PhysicalIdx], FCalcAreaSize);
        Move(PByte(NativeInt(ActiveBuffer) + SizeOf(TEntityRecordHeader))^, FInternalCalcStorage[PhysicalIdx][0], FCalcAreaSize);
      end;
    end;
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
  FPositionBeforeAction := GetRecNo - 1;
  inherited DoBeforeInsert;
end;

procedure TEntityDataSet.DoBeforeDelete;
begin
  FPositionBeforeAction := GetRecNo;
  inherited DoBeforeDelete;
end;

procedure TEntityDataSet.InternalInsert;
begin
  FIsAppending := (PEntityRecordHeader(Pointer(ActiveBuffer)).BookmarkFlag = bfEOF);
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

procedure TEntityDataSet.InternalAddRecord(Buffer: TRecBuf; Append: Boolean);
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
    InnerInfo: PTypeInfo;
  begin
    if ATypeInfo = nil then Exit(ftUnknown);
    
    // Check for Proxy<T>, Lazy<T>, etc using centralized Reflection
    if TReflection.IsSmartProp(ATypeInfo) then
    begin
      InnerInfo := TReflection.GetUnderlyingType(ATypeInfo);
      if InnerInfo <> nil then
        Exit(MapTypeToFieldType(InnerInfo));
    end;

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
      tkRecord, tkInterface:
      begin
        InnerInfo := TReflection.GetUnderlyingType(ATypeInfo);
        if (InnerInfo <> nil) and (InnerInfo <> ATypeInfo) then
          Exit(MapTypeToFieldType(InnerInfo))
        else
        begin
          var LTypeName := string(ATypeInfo.Name);
          if LTypeName.Contains('Lazy<') or LTypeName.Contains('Prop<') then
          begin
             // Se falhou em resolver via RTTI mas o nome indica Lazy, tenta o fallback manual aqui se necessário
             // ou pelo menos evita ftUnknown se for uma interface conhecida.
             // Neste caso, se InnerInfo é nil, é porque o TReflection.GetUnderlyingType falhou para a INTERFACE.
          end;
          Exit(ftUnknown);
        end;
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
      
      // Shadow property check
      if PropMap.IsShadow and (not FIncludeShadowProperties) then Continue;

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
           // Search for field directly, then with F prefix, then normalized, then normalized with F
           RttiField := RttiType.GetField(PropMap.PropertyName);
           if RttiField = nil then RttiField := RttiType.GetField('F' + PropMap.PropertyName);
           if RttiField = nil then RttiField := RttiType.GetField(TReflection.NormalizeFieldName(PropMap.PropertyName));
           if RttiField = nil then RttiField := RttiType.GetField('F' + TReflection.NormalizeFieldName(PropMap.PropertyName));

           // NEW: Special case for Lazy fields that might not have the F prefix in the map but have it in the class
           if (RttiField = nil) and (not PropMap.PropertyName.StartsWith('F', True)) then
             RttiField := RttiType.GetField('F' + PropMap.PropertyName);

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

      // Ensure shadow property has a type if unknown (default to string)
      if (PropMap.IsShadow) and (ResolvedType = ftUnknown) then
        ResolvedType := ftWideString;

      if ResolvedType = ftUnknown then Continue;

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
          ftFloat:
          begin
            NewField := TFloatField.Create(Self);
            TFloatField(NewField).DisplayFormat := '#,##0.00';
            TFloatField(NewField).Precision := 2;
          end;
          ftCurrency:
          begin
            NewField := TCurrencyField.Create(Self);
            TCurrencyField(NewField).DisplayFormat := '#,##0.00';
          end;
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
          NewField.DataSet := Self;  // Ensure dataset is linked before setting other props
          
          if (NewField is TFloatField) and (not (NewField is TCurrencyField)) then
            TFloatField(NewField).Precision := PropMap.Precision;
          if NewField is TCurrencyField then
          begin
            TCurrencyField(NewField).Precision := 4;
            TCurrencyField(NewField).currency := True; // Explicitly enable currency mode
          end;

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
  Result := TRecordBuffer(AllocMem(FRecordSize));
  InternalInitRecord(TRecBuf(Result));
end;

procedure TEntityDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Pointer(Buffer));
  Buffer := nil;
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

procedure TEntityDataSet.InternalInitRecord(Buffer: TRecBuf);
begin
  FillChar(Pointer(Buffer)^, FRecordSize, 0);
  PEntityRecordHeader(Pointer(Buffer)).BookmarkIndex := -2;
end;

procedure TEntityDataSet.CalculateFields(Buffer: TRecBuf);
var
  SaveState: TDataSetState;
begin
  SaveState := State;
  SetState(dsCalcFields);
  try
    inherited CalculateFields(Buffer);
  finally
    SetState(SaveState);
  end;
end;

procedure TEntityDataSet.GetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  if Length(Data) < SizeOf(Integer) then
    SetLength(Data, SizeOf(Integer));
  PInteger(@Data[0])^ := PEntityRecordHeader(Pointer(Buffer)).BookmarkIndex;
end;

procedure TEntityDataSet.SetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  if Length(Data) >= SizeOf(Integer) then
    PEntityRecordHeader(Pointer(Buffer)).BookmarkIndex := PInteger(@Data[0])^;
end;

procedure TEntityDataSet.InternalGotoBookmark(Bookmark: TBookmark);
begin
  if Length(Bookmark) >= SizeOf(Integer) then
    FCurrentRec := PInteger(@Bookmark[0])^;
end;

function TEntityDataSet.GetBookmarkFlag(Buffer: TRecBuf): TBookmarkFlag;
begin
  Result := PEntityRecordHeader(Pointer(Buffer)).BookmarkFlag;
end;

procedure TEntityDataSet.SetBookmarkFlag(Buffer: TRecBuf; Value: TBookmarkFlag);
begin
  PEntityRecordHeader(Pointer(Buffer)).BookmarkFlag := Value;
end;

procedure TEntityDataSet.InternalSetToRecord(Buffer: TRecBuf);
begin
  FCurrentRec := PEntityRecordHeader(Pointer(Buffer)).BookmarkIndex;
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

function TEntityDataSet.GetRecord(Buffer: TRecBuf; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  Header: PEntityRecordHeader;
  PhysicalIdx: Integer;
begin
  Header := PEntityRecordHeader(Pointer(Buffer));

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

    if (FCalcAreaSize > 0) and (FCurrentRec >= 0) and (FCurrentRec < FVirtualIndex.Count) then
    begin
      PhysicalIdx := FVirtualIndex[FCurrentRec];
      if (PhysicalIdx >= 0) and (PhysicalIdx < Length(FInternalCalcStorage)) and (Length(FInternalCalcStorage[PhysicalIdx]) > 0) then
         Move(FInternalCalcStorage[PhysicalIdx][0], PByte(NativeInt(Buffer) + SizeOf(TEntityRecordHeader))^, FCalcAreaSize);
    end;

    CalculateFields(TRecBuf(Buffer));
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
  LP: PByte;
  LVal: TValue;
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

  // 3. Shadow Property support
  if PropMap.IsShadow and (FDbContext <> nil) then
  begin
    var Entry := FDbContext.Entry(CurrentObj);
    Value := Entry.Member(Field.FieldName).GetCurrentValue.AsVariant;
    Result := True;
    Exit;
  end;

  // 4. RTTI Fallback if field offset is not defined or is Lazy
  if (PropMap.FieldValueOffset <= 0) or PropMap.IsLazy then
  begin
    var LProp := GetProperty(Field.FieldName);
    if LProp <> nil then
    begin
      var LTempValue := LProp.GetValue(CurrentObj);
      var LUnwrapped: TValue;
      if TReflection.TryUnwrapProp(LTempValue, LUnwrapped) then
      begin
        if not LUnwrapped.IsEmpty then
        begin
          if (Field.DataType in [ftDate, ftTime, ftDateTime]) then
             Value := LUnwrapped.AsType<TDateTime>
          else if (Field.DataType = ftCurrency) then
             Value := LUnwrapped.AsCurrency
          else
             Value := LUnwrapped.AsVariant;
        end;
      end
      else
      begin
        if (Field.DataType in [ftDate, ftTime, ftDateTime]) then
           Value := LTempValue.AsType<TDateTime>
        else if (Field.DataType = ftCurrency) then
           Value := LTempValue.AsCurrency
        else
           Value := LTempValue.AsVariant;
      end;
      Result := not VarIsEmpty(Value);
      Exit;
    end;

    // Last resort: RTTI Field
    var ctx := TRttiContext.Create;
    var LRttiType := ctx.GetType(CurrentObj.ClassType);
    if LRttiType <> nil then
    begin
      var RttiField := LRttiType.GetField(Field.FieldName);
      if RttiField <> nil then
      begin
        var LFieldVal := RttiField.GetValue(CurrentObj);
        var LUnwrappedField: TValue;
        if TReflection.TryUnwrapProp(LFieldVal, LUnwrappedField) then
        begin
          if not LUnwrappedField.IsEmpty then
          begin
            if (Field.DataType in [ftDate, ftTime, ftDateTime]) then
               Value := LUnwrappedField.AsType<TDateTime>
            else if (Field.DataType = ftCurrency) then
               Value := LUnwrappedField.AsCurrency
            else
               Value := LUnwrappedField.AsVariant;
          end;
        end
        else
        begin
          if (Field.DataType in [ftDate, ftTime, ftDateTime]) then
             Value := LFieldVal.AsType<TDateTime>
          else if (Field.DataType = ftCurrency) then
             Value := LFieldVal.AsCurrency
          else
             Value := LFieldVal.AsVariant;
        end;
        Result := not VarIsEmpty(Value);
        Exit;
      end;
    end;
  end;

  // 5. Direct value extraction (Fast Path)
  // Checar flag de nulo se definido (SmartProp, Nullable, Lazy)
  if (PropMap.FieldOffset > 0) then
  begin
    LP := PByte(CurrentObj);
    Inc(LP, PropMap.FieldOffset);
    if not PBoolean(LP)^ then
    begin
      Value := Null;
      Exit(True);
    end;
  end;

  // Determinar o ponteiro para o valor real
  // CRITICAL: Ensure we have a valid non-zero offset before direct memory access
  if PropMap.FieldValueOffset > 0 then
  begin
    LP := PByte(CurrentObj);
    Inc(LP, PropMap.FieldValueOffset);
    PValue := LP;
  end
  else if PropMap.FieldOffset > 0 then
  begin
    LP := PByte(CurrentObj);
    Inc(LP, PropMap.FieldOffset);
    PValue := LP;
  end
  else
    Exit(False); // Cannot read without offset or RTTI (handled before)

  if PValue = nil then Exit;

  case PropMap.DataType of
    ftString, ftWideString, ftMemo, ftWideMemo:
      Value := PString(PValue)^;
    ftInteger, ftSmallint, ftWord:
      Value := PInteger(PValue)^;
    ftLargeint:
      Value := PInt64(PValue)^;
    ftFloat, ftCurrency:
    begin
      // CRITICAL: We must use TValue.Make with the original PropertyType (PTypeInfo)
      // because types like 'Currency' have a unique 8-byte binary layout (scaled Int64)
      // that differs from standard 'Double' (IEEE 754). Using TValue ensures safe 
      // extraction from the object memory and correct conversion to a Variant type.
      if PropMap.PropertyType <> nil then
      begin
        TValue.Make(PValue, PropMap.PropertyType, LVal);
        Value := LVal.AsVariant;
      end
      else
      begin
        // Fallback to raw bit reading if TypeInfo is missing
        if PropMap.DataType = ftCurrency then
          Value := PCurrency(PValue)^
        else
          Value := PDouble(PValue)^;
      end;
    end;
    ftBoolean:
      Value := PBoolean(PValue)^;
    ftDateTime, ftDate, ftTime:
      Value := PDateTime(PValue)^;
    ftBlob:
    begin
      try
        // PValue is Pointer, we need to cast to PBytes to dereference properly
        BlobData := PBytes(PValue)^;
        if Length(BlobData) > 0 then
        begin
          Value := True;
          Result := True;
          Exit;
        end;
      except
        // Safety for invalid pointers during stress/navigation
      end;
      Result := False;
      Exit;
    end;
  else
    Exit; // Tipo não mapeado diretamente
  end;
  
  Result := not VarIsEmpty(Value);
end;

// ---------------------------------------------------------------------------
//  GetFieldData — Override with TValueBuffer (TArray<Byte>) for modern Delphi
//  This is the method TField.GetData actually calls (XE3+).
// ---------------------------------------------------------------------------
function TEntityDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  V: Variant;
  Offset: Integer;
  BufferPtr: Pointer;
  P: PByte;
begin
  Result := False;
  if not Assigned(Field) then Exit;
  
  if Field.FieldKind in [fkCalculated, fkLookup, fkInternalCalc] then
  begin
    if Field.FieldKind = fkCalculated then
      BufferPtr := Pointer(CalcBuffer)
    else
      BufferPtr := Pointer(ActiveBuffer);

    if BufferPtr = nil then Exit;
    if not FCalcOffsets.TryGetValue(Field.FieldName, Offset) then Exit;
    
    // Check Null Flag (1 byte before data)
    P := PByte(BufferPtr);
    Inc(P, Offset - 1);
    
    if P^ = 0 then
    begin
       Result := False; // Null
       Exit;
    end;
    
    // Not Null
    if Buffer = nil then
    begin
       Result := True; // IsNull check -> I have data
       Exit;
    end;
    
    Inc(P);
    Move(P^, Buffer^, Field.DataSize);
    Result := True;
    Exit;
  end;

  if ReadFieldValue(Field, V) then
  begin
    if VarIsNull(V) or VarIsClear(V) then
    begin
       Result := False;
       Exit;
    end;
    
    // If Buffer is nil, just check for data existence (IsNull test)
    if Buffer = nil then
    begin
      Result := True;
      Exit;
    end;

    case Field.DataType of
      ftString, ftFixedChar: System.AnsiStrings.StrPLCopy(PAnsiChar(Buffer), AnsiString(string(V)), Field.Size);
      ftWideString, ftFixedWideChar: System.SysUtils.StrPLCopy(PWideChar(Buffer), string(V), Field.Size);
      ftShortint: PShortint(Buffer)^ := V;
      ftByte: PByte(Buffer)^ := V;
      ftSmallint: PSmallint(Buffer)^ := V;
      ftWord: PWord(Buffer)^ := V;
      ftInteger, ftAutoInc: PInteger(Buffer)^ := V;
      ftLongWord: PLongWord(Buffer)^ := V;
      ftFloat:
      begin
        var LFloat: Double := V;
        PDouble(Buffer)^ := LFloat;
      end;
      ftBoolean: PWordBool(Buffer)^ := V;
      ftDateTime, ftDate, ftTime:
      begin
        var LDT: TDateTime := V;
        PDouble(Buffer)^ := LDT;
      end;
      ftLargeint:
      begin
        var LInt64: Int64 := V;
        PInt64(Buffer)^ := LInt64;
      end;
      ftCurrency:
      begin
        // IMPORTANT: TCurrencyField and TFloatField in Delphi's TDataSet framework (Data.DB)
        // internally store values as an 8-byte DOUBLE (IEEE 754) in their record buffers.
        // Attempting to write a raw 8-byte binary 'Currency' (scaled Int64) pattern directly 
        // to the buffer would result in corrupted values (e.g., 4.87E-317) when the 
        // field's GetAsCurrency or GetValue methods are called. 
        // Therefore, we MUST convert the value to a Double before writing to the buffer.
        var LDoubleVal: Double := V;
        PDouble(Buffer)^ := LDoubleVal;
      end;
      ftVariant: PVariant(Buffer)^ := V;
    else
      Result := False;
      Exit;
    end;
    Result := True;
  end;
end;

function TEntityDataSet.GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean;
begin
  Result := False;
  if not Assigned(Field) then Exit;

  if Length(Buffer) = 0 then
    Exit(GetFieldData(Field, nil));

  if Length(Buffer) < Field.DataSize then
    SetLength(Buffer, Field.DataSize);

  Result := GetFieldData(Field, @Buffer[0]);
end;

procedure TEntityDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  CurrentObj: TObject;
  Header: PEntityRecordHeader;
  PropMap: TPropertyMap;
  Offset: Integer;
  BufferPtr: Pointer;
  P: PByte;
begin
  if not Assigned(Field) then Exit;
  if Field.ReadOnly or (State = dsBrowse) then Exit;
  
  if Field.FieldKind in [fkCalculated, fkLookup, fkInternalCalc] then
  begin
    if Field.FieldKind = fkCalculated then
      BufferPtr := Pointer(CalcBuffer)
    else
      BufferPtr := Pointer(ActiveBuffer);

    if BufferPtr = nil then Exit;
    if not FCalcOffsets.TryGetValue(Field.FieldName, Offset) then Exit;
    
    P := PByte(BufferPtr);
    Inc(P, Offset - 1);
    
    if Buffer = nil then
    begin
       P^ := 0; // Null
    end
    else
    begin
       P^ := 1; // Not Null
       Inc(P);
       Move(Buffer^, P^, Field.DataSize);
    end;
    DataEvent(deFieldChange, NativeInt(Field));
    Exit;
  end;

  CurrentObj := nil;
  Header := nil;
  if Pointer(ActiveBuffer) <> nil then
    Header := PEntityRecordHeader(ActiveBuffer);

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
  if not FEntityMap.Properties.TryGetValue(Field.FieldName, PropMap) then Exit;

  // 1. Shadow Property support
  if PropMap.IsShadow and (FDbContext <> nil) then
  begin
    if Buffer = nil then
      FDbContext.Entry(CurrentObj).Member(Field.FieldName).SetCurrentValue(TValue.Empty)
    else
    begin
      var V: TValue;
      case Field.DataType of
        ftString, ftWideString: V := string(PWideChar(Buffer));
        ftInteger: V := PInteger(Buffer)^;
        ftFloat: V := PDouble(Buffer)^;
        ftBoolean: V := PBoolean(Buffer)^;
      else
        V := TValue.Empty;
      end;
      FDbContext.Entry(CurrentObj).Member(Field.FieldName).SetCurrentValue(V);
    end;
    SetModified(True);
    DataEvent(deFieldChange, NativeInt(Field));
    Exit;
  end;

  // 2. Direct RTTI/Offset writing
  if PropMap.FieldValueOffset > 0 then
  begin
    P := PByte(CurrentObj);
    Inc(P, PropMap.FieldValueOffset);
    if Buffer <> nil then
    begin
       case Field.DataType of
         ftString, ftWideString: PString(P)^ := string(PWideChar(Buffer));
         ftBoolean: PBoolean(P)^ := PBoolean(Buffer)^;
       else
         Move(Buffer^, P^, Field.DataSize);
       end;
    end;
    
    // Set HasValue flag if available
    if PropMap.FieldOffset > 0 then
    begin
       P := PByte(CurrentObj);
       Inc(P, PropMap.FieldOffset);
       PBoolean(P)^ := (Buffer <> nil);
    end;
    SetModified(True);
    DataEvent(deFieldChange, NativeInt(Field));
  end
  else if (CurrentObj <> nil) then
  begin
    // RTTI Fallback for properties without direct FieldOffset
    var V: TValue := TValue.Empty;
    if Buffer <> nil then
    begin
       case Field.DataType of
         ftString, ftWideString: V := string(PWideChar(Buffer));
         ftInteger, ftSmallint, ftAutoInc: V := PInteger(Buffer)^;
         ftLargeint: V := PInt64(Buffer)^;
         ftFloat: V := PDouble(Buffer)^;
        ftCurrency: V := PCurrency(Buffer)^;
        ftBoolean: V := PBoolean(Buffer)^;
        ftDateTime, ftDate, ftTime: V := TDateTime(PDouble(Buffer)^);
      end;
    end;

    var RttiProp := GetProperty(Field.FieldName);
    if RttiProp <> nil then
      RttiProp.SetValue(CurrentObj, V);
      
    SetModified(True);
    DataEvent(deFieldChange, NativeInt(Field));
  end;
end;

procedure TEntityDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer);
begin
  if Length(Buffer) = 0 then
    SetFieldData(Field, nil)
  else
    SetFieldData(Field, @Buffer[0]);
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
