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
    FItems: IList<TObject>;            // Referência real para a lista de objetos
    FOwnsItems: Boolean;               // Se o dataset é dono da lista e deve limpá-la
    FVirtualIndex: TVector<Integer>;   // Ordered/filtered view over FItems (contém índices para FItems)
    
    FRecordSize: Integer;
    FHeaderSize: Integer;
    
    // Internal Settings
    FReadOnly: Boolean;
    FIncludeShadowProperties: Boolean;
    FIndexFieldNames: string;
    FCurrentRec: Integer; // Controle de cursor nativo do dataset
    FIsCursorOpen: Boolean;
    FInsertObj: TObject; // Temporary object for uncommitted dsInsert
    
    procedure SetItems(const Value: IList<TObject>);
    procedure SetIndexFieldNames(const Value: string);
    procedure ApplyFilterAndSort; overload;
    procedure ApplyFilterAndSort(AFiltered: Boolean); overload;
    function CompareObjects(const A, B: TObject; const AFields: string): Integer;
    procedure BuildFieldDefs;
    
    /// <summary>
    ///   Core internal method that reads a field value from an entity object.
    ///   Used by both GetFieldData overloads.
    ///   Returns the value as Variant (Unassigned if not found).
    /// </summary>
    function ReadFieldValue(Field: TField; out Value: Variant): Boolean;
  protected
    // Overrides do TDataSet para filtros e ordenação
    procedure InternalHandleException; override;
    function IsCursorOpen: Boolean; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoSearch: Boolean): TGetResult; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterText(const Value: string); override;
    
    // Overrides obrigatórios do TDataSet
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalInitFieldDefs; override;

    // Buffer Alocations
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    
    // Bookmark e Navegação
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    
    function GetRecordSize: Word; override;
    function GetRecordCount: Integer; override;

    // DML e Edição
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure InternalCancel; override;
    procedure InternalEdit; override;
    procedure InternalInsert; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;

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
    /// <summary>
    ///  Carga de dados de Objetos
    /// </summary>
    procedure Load(const AItems: IList<TObject>; AClass: TClass; AOwns: Boolean = False); overload;
    procedure Load(const AItems: TArray<TObject>; AClass: TClass); overload;
    
    /// <summary>
    ///  Carga de dados de JSON Utf8 (Zero-Alloc Pipeline)
    /// </summary>
    procedure LoadFromUtf8Json(const ASpan: TByteSpan; AClass: TClass);

    property Items: IList<TObject> read FItems write SetItems;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property IncludeShadowProperties: Boolean read FIncludeShadowProperties write FIncludeShadowProperties default False;
    property IndexFieldNames: string read FIndexFieldNames write SetIndexFieldNames;
  end;

implementation

uses
  System.StrUtils,
  Dext.Collections.Comparers,
  Dext.Specifications.Interfaces,
  Dext.Specifications.Parser,
  Dext.Specifications.Evaluator;

{ TEntityDataSet }

constructor TEntityDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecordSize := SizeOf(TEntityRecordHeader);
  FHeaderSize := SizeOf(TEntityRecordHeader);
  FReadOnly := False;
  BookmarkSize := SizeOf(Integer);
end;

destructor TEntityDataSet.Destroy;
begin
  if FOwnsItems then
    FItems := nil;
  FItems := nil;
  if Assigned(FEntityMap) then
    FEntityMap.Free;
  inherited Destroy;
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
  Expr: IExpression;
  i, j: Integer;
  Passing: Boolean;
  SorterList: IList<Integer>;
begin
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
        // Setup temporário se houver OnFilterRecord
        Passing := True; // Fallback ou OnFilterRecord(Self, Passing);
      end;
    end;

    if Passing then
      FVirtualIndex.Add(I);
  end;

  if (FIndexFieldNames <> '') and (FVirtualIndex.Count > 1) then
  begin
    SorterList := TCollections.CreateList<Integer>(False);
    for j := 0 to FVirtualIndex.Count - 1 do
      SorterList.Add(FVirtualIndex[j]);

    SorterList.Sort(TComparer<Integer>.Construct(
      function(const A, B: Integer): Integer
      begin
        Result := CompareObjects(FItems[A], FItems[B], FIndexFieldNames);
      end));

    FVirtualIndex.Clear;
    for j := 0 to SorterList.Count - 1 do
      FVirtualIndex.Add(SorterList[j]);
  end;
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
    if PropMap.FieldValueOffset > 0 then
    begin
      PValue := Pointer(PByte(FItems[FVirtualIndex[I]]) + PropMap.FieldValueOffset);
      case PropMap.DataType of
        ftInteger, ftSmallint: CurVal := PInteger(PValue)^;
        ftLargeint: CurVal := PInt64(PValue)^;
        ftString, ftWideString: CurVal := PString(PValue)^;
        ftFloat: CurVal := PDouble(PValue)^;
        ftCurrency: CurVal := PCurrency(PValue)^;
        ftBoolean: CurVal := PBoolean(PValue)^;
      else
        Continue;
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
  end; // Fim do Locate
end;

function TEntityDataSet.CompareObjects(const A, B: TObject; const AFields: string): Integer;
var
  Context: TRttiContext;
  i: Integer;
  PA, PB: Pointer;
  PropMap: TPropertyMap;
  PropNames: TArray<string>;
  RttiProp: TRttiProperty;
  RttiType: TRttiType;
  ValA, ValB: Variant;
begin
  Result := 0;
  if AFields = '' then Exit;

  PropNames := AFields.Split([';']);
  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(FEntityClass);
    for i := 0 to High(PropNames) do
    begin
      if not FEntityMap.Properties.TryGetValue(PropNames[i], PropMap) then Continue;

      if PropMap.FieldValueOffset > 0 then
      begin
        PA := Pointer(PByte(A) + PropMap.FieldValueOffset);
        PB := Pointer(PByte(B) + PropMap.FieldValueOffset);

        case PropMap.DataType of
          ftInteger, ftSmallint: Result := PInteger(PA)^ - PInteger(PB)^;
          ftLargeint:            if PInt64(PA)^ < PInt64(PB)^ then Result := -1 else if PInt64(PA)^ > PInt64(PB)^ then Result := 1;
          ftFloat:               if PDouble(PA)^ < PDouble(PB)^ then Result := -1 else if PDouble(PA)^ > PDouble(PB)^ then Result := 1;
          ftCurrency:            if PCurrency(PA)^ < PCurrency(PB)^ then Result := -1 else if PCurrency(PA)^ > PCurrency(PB)^ then Result := 1;
          ftString, ftWideString: Result := CompareText(PString(PA)^, PString(PB)^);
          ftDateTime, ftDate, ftTime: if PDateTime(PA)^ < PDateTime(PB)^ then Result := -1 else if PDateTime(PA)^ > PDateTime(PB)^ then Result := 1;
          ftBoolean:             Result := Ord(PBoolean(PA)^) - Ord(PBoolean(PB)^);
        end;
      end
      else if RttiType <> nil then
      begin
        RttiProp := RttiType.GetProperty(PropNames[i]);
        if RttiProp <> nil then
        begin
          ValA := RttiProp.GetValue(A).AsVariant;
          ValB := RttiProp.GetValue(B).AsVariant;
          if ValA < ValB then Result := -1
          else if ValA > ValB then Result := 1
          else Result := 0;
        end;
      end;

      if Result <> 0 then Break;
    end;
  finally
    Context.Free;
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
  FCurrentRec := -1; // Reset de cursor nativo
  BindFields(True);
end;

procedure TEntityDataSet.InternalClose;
begin
  FIsCursorOpen := False;
  FVirtualIndex.Clear;
end;

procedure TEntityDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
end;

procedure TEntityDataSet.InternalDelete;
var
  ActualRow: Integer;
  Header: PEntityRecordHeader;
begin
  if (Pointer(ActiveBuffer) <> nil) and Assigned(FItems) then
  begin
    Header := PEntityRecordHeader(ActiveBuffer);
    if (Header.BookmarkIndex >= 0) and (Header.BookmarkIndex < FVirtualIndex.Count) then
    begin
      // 1. Identificar o índice real na lista física
      ActualRow := FVirtualIndex[Header.BookmarkIndex];
      
      // 2. Remover da lista virtual primeiro
      FVirtualIndex.RemoveAt(Header.BookmarkIndex);
      
      // 3. Remover da lista física (referência real)
      // Se formos donos da lista, o TList.Delete lidará com a liberação do objeto
      // se configurado (embora aqui usemos TList<TObject>, geralmente o usuário libera)
      FItems.Delete(ActualRow);
      
      // 4. IMPORTANTE: Como os índices físicos mudaram, PRECISAMOS reconstruir a visão virtual
      ApplyFilterAndSort;
      Resync([]);
    end;
  end;
end;

procedure TEntityDataSet.InternalPost;
var
  NewRow: Integer;
begin
  if State = dsInsert then
  begin
    if FInsertObj <> nil then
    begin
      // 1. Persistir o objeto novo na lista real (sempre no fim da lista física)
      FItems.Add(FInsertObj);
      NewRow := FItems.Count - 1;
      FInsertObj := nil; 
      
      // 2. Re-aplicar filtros e ordenação para que o novo registro apareça na posição correta da visão
      ApplyFilterAndSort;
      
      // 3. Sincronizar FCurrentRec com a posição em que o novo registro foi parar na visão virtual
      FCurrentRec := FVirtualIndex.IndexOf(NewRow);
      
      // 4. Se o registro é visível (passou no filtro), atualizar o buffer ativo para que o Grid entenda a mudança
      if (FCurrentRec >= 0) and (Pointer(ActiveBuffer) <> nil) then
        PEntityRecordHeader(ActiveBuffer).BookmarkIndex := FCurrentRec
      else if FCurrentRec < 0 then
        FCurrentRec := -1; // Registro oculto pelo filtro, reseta cursor interno
    end;
  end;
  // Nota: Para dsEdit as mudanças já foram escritas diretamente no objeto via SetFieldData/Pointer
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
  // No-op. A edição (dsEdit) não exige alocação em buffer físico (Update direto via pointer).
end;

procedure TEntityDataSet.InternalInsert;
begin
  if FInsertObj <> nil then
  begin
    FInsertObj.Free;
    FInsertObj := nil;
  end;
  FInsertObj := CreateNewEntity;
  
  if FInsertObj = nil then
    raise Exception.Create('Auto-append needs a parameterless constructor for ' + FEntityClass.ClassName);
    
  if Pointer(ActiveBuffer) <> nil then
  begin
    PEntityRecordHeader(ActiveBuffer).BookmarkIndex := -2; // Insert phantom index
    PEntityRecordHeader(ActiveBuffer).BookmarkFlag := bfInserted;
  end;
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
  begin
    if ATypeInfo = nil then Exit(ftUnknown);
    case ATypeInfo.Kind of
      tkInteger, tkEnumeration:
      begin
        if ATypeInfo = TypeInfo(Boolean) then
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
  FieldType: TFieldType;
  NewField: TField;
  PropMap: TPropertyMap;
  PropMapPair: TPair<string, TPropertyMap>;
  RttiProp: TRttiProperty;
  RttiType: TRttiType;
begin
  if (FEntityMap = nil) or (FEntityClass = nil) then Exit;

  FieldDefs.Clear;

  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(FEntityClass);

    for PropMapPair in FEntityMap.Properties do
    begin
      PropMap := PropMapPair.Value;
      if PropMap.IsIgnored or PropMap.IsNavigation then Continue;

      // Calcular DataType dinamicamente
      FieldType := PropMap.DataType;

      // Se tivermos Shadow mapping, ler do RTTI da Classe estática
      if (FieldType = ftUnknown) and (RttiType <> nil) then
      begin
        RttiProp := RttiType.GetProperty(PropMap.PropertyName);
        if RttiProp <> nil then
        begin
          if IsTBytesType(RttiProp.PropertyType.Handle) then
            FieldType := ftBlob
          else
            FieldType := MapTypeToFieldType(RttiProp.PropertyType.Handle);
        end;
      end;

      // Se ainda for desconhecido e houver PTypeInfo no map
      if (FieldType = ftUnknown) and Assigned(PropMap.PropertyType) then
        FieldType := MapTypeToFieldType(PropMap.PropertyType);

      // CRITICAL: Persist resolved DataType back into PropMap
      // so that Sort (CompareObjects) and Locate can use it
      if (PropMap.DataType = ftUnknown) and (FieldType <> ftUnknown) then
        PropMap.DataType := FieldType;

      // 1. Popular FieldDefs para metadados (Tamanho de string, grid, layouts)
      FieldDef := FieldDefs.AddFieldDef;
      FieldDef.Name := PropMap.PropertyName;
      FieldDef.DataType := FieldType;
      if PropMap.MaxLength > 0 then
        FieldDef.Size := PropMap.MaxLength
      else if FieldType in [ftString, ftWideString] then
        FieldDef.Size := 255;

      // 2. Instanciar os TFields dinamicamente para o FieldByName não dar "not found"
      if Fields.FindField(PropMap.PropertyName) = nil then
      begin
        NewField := nil;
        case FieldType of
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

          // Propagar metadados dos atributos para o TField
          NewField.Required := PropMap.IsRequired and (not PropMap.IsAutoInc);
          NewField.ReadOnly := PropMap.IsAutoInc;

          NewField.DataSet := Self;  // <-- Conecta o TField ao DataSet
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
  Index: Integer;
begin
  Index := PEntityRecordHeader(Buffer).BookmarkIndex;
  if Index >= 0 then
    FCurrentRec := Index;
end;

function TEntityDataSet.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

function TEntityDataSet.GetRecordCount: Integer;
begin
  Result := FVirtualIndex.Count;
end;

function TEntityDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoSearch: Boolean): TGetResult;
var
  Header: PEntityRecordHeader;
begin
  Header := PEntityRecordHeader(Buffer);
  Result := grOK;

  case GetMode of
    gmNext:
      begin
        if FCurrentRec < FVirtualIndex.Count then
          Inc(FCurrentRec);
        if FCurrentRec >= FVirtualIndex.Count then
          Result := grEOF;
      end;
      
    gmPrior:
      begin
        if FCurrentRec >= 0 then
          Dec(FCurrentRec);
        if FCurrentRec < 0 then
          Result := grBOF;
      end;
      
    gmCurrent:
      begin
        if (FCurrentRec < 0) or (FCurrentRec >= FVirtualIndex.Count) then
          Result := grError;
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
  ActualRow: Integer;
  BlobData: TArray<Byte>;
  Context: TRttiContext;
  CurrentObj: TObject;
  Header: PEntityRecordHeader;
  PropMap: TPropertyMap;
  PValue: Pointer;
  RttiField: TRttiField;
  RttiProp: TRttiProperty;
  RttiType: TRttiType;
begin
  Result := False;
  Value := Unassigned;

  if not Active then Exit;

  Header := PEntityRecordHeader(ActiveBuffer);

  if (Header = nil) then
  begin
    // Fallback para o registro atual se estivermos em navegação segura mas sem ActiveBuffer (ex: CheckActive)
    if (FCurrentRec >= 0) and (FCurrentRec < FVirtualIndex.Count) then
    begin
       ActualRow := FVirtualIndex[FCurrentRec];
       CurrentObj := FItems[ActualRow];
    end
    else
       Exit;
  end
  else

  // Suporte a novo registro sendo inserido (phantom index -2)
  if Header.BookmarkIndex = -2 then
  begin
    // Se ainda estamos em insert, usamos o objeto de inserção
    if (State = dsInsert) and (FInsertObj <> nil) then
    begin
      CurrentObj := FInsertObj;
    end
    else
    begin
      // Se saímos do estado de insert mas o buffer ainda tem -2 (ex: repaints durante transição),
      // tentamos usar o registro atual se o cursor estiver apontando para algo válido.
      if (FCurrentRec >= 0) and (FCurrentRec < FVirtualIndex.Count) then
      begin
        ActualRow := FVirtualIndex[FCurrentRec];
        CurrentObj := FItems[ActualRow];
      end
      else
        Exit;
    end;
  end
  else
  begin
    if (Header.BookmarkIndex < 0) or (Header.BookmarkIndex >= FVirtualIndex.Count) then
      Exit;

    ActualRow := FVirtualIndex[Header.BookmarkIndex];
    CurrentObj := FItems[ActualRow];
  end;

  if (CurrentObj = nil) or (FEntityMap = nil) then Exit;

  if not FEntityMap.Properties.TryGetValue(Field.FieldName, PropMap) then
    Exit;

  PValue := Pointer(PByte(CurrentObj) + PropMap.FieldValueOffset);

  // Fallback por RTTI se offset não estiver definido
  if (PropMap.FieldValueOffset <= 0) then
  begin
   Context := TRttiContext.Create;
    try
      RttiType := Context.GetType(FEntityClass);
      if RttiType <> nil then
      begin
       RttiProp := RttiType.GetProperty(Field.FieldName);
        if RttiProp <> nil then
        begin
          case Field.DataType of
            ftString, ftWideString:
              Value := RttiProp.GetValue(CurrentObj).AsString;
            ftInteger, ftSmallint:
              Value := RttiProp.GetValue(CurrentObj).AsInteger;
            ftLargeint:
              Value := RttiProp.GetValue(CurrentObj).AsInt64;
            ftFloat:
              Value := RttiProp.GetValue(CurrentObj).AsExtended;
            ftCurrency:
              Value := RttiProp.GetValue(CurrentObj).AsCurrency;
            ftBoolean:
              Value := RttiProp.GetValue(CurrentObj).AsBoolean;
            ftDateTime, ftDate, ftTime:
              Value := RttiProp.GetValue(CurrentObj).AsExtended;
          else
            Exit;
          end;
          Result := True;
          Exit;
        end;

        RttiField := RttiType.GetField(Field.FieldName);
        if RttiField <> nil then
        begin
          case Field.DataType of
            ftString, ftWideString:
              Value := RttiField.GetValue(CurrentObj).AsString;
            ftInteger, ftSmallint:
              Value := RttiField.GetValue(CurrentObj).AsInteger;
            ftLargeint:
              Value := RttiField.GetValue(CurrentObj).AsInt64;
            ftFloat:
              Value := RttiField.GetValue(CurrentObj).AsExtended;
            ftCurrency:
              Value := RttiField.GetValue(CurrentObj).AsCurrency;
            ftBoolean:
              Value := RttiField.GetValue(CurrentObj).AsBoolean;
            ftDateTime, ftDate, ftTime:
              Value := RttiField.GetValue(CurrentObj).AsExtended;
          else
            Exit;
          end;
          Result := True;
          Exit;
        end;
      end;
    finally
      Context.Free;
    end;
    Exit;
  end;

  // Leitura direta por offset (fast-path)
  case Field.DataType of
    ftString, ftWideString:
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
  ActualRow: Integer;
  Context: TRttiContext;
  CurrentObj: TObject;
  Header: PEntityRecordHeader;
  P: Pointer;
  PropMap: TPropertyMap;
  PValue: Pointer;
  RttiField: TRttiField;
  RttiProp: TRttiProperty;
  RttiType: TRttiType;
begin
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
  begin
    if (State <> dsInsert) or (FInsertObj = nil) then Exit;
    CurrentObj := FInsertObj;
  end
  else if (State = dsInsert) and (FInsertObj <> nil) then
  begin
    CurrentObj := FInsertObj;
  end
  else
  begin
    // Fallback para edição direta via cursor se o header estiver ausente ou inválido
    ActualRow := -1;
    if (Header <> nil) and (Header.BookmarkIndex >= 0) and (Header.BookmarkIndex < FVirtualIndex.Count) then
      ActualRow := FVirtualIndex[Header.BookmarkIndex]
    else if (FCurrentRec >= 0) and (FCurrentRec < FVirtualIndex.Count) then
      ActualRow := FVirtualIndex[FCurrentRec];

    if ActualRow < 0 then
    begin
        if (State = dsInsert) then
          CurrentObj := FInsertObj
        else
          Exit;
    end
    else
      CurrentObj := FItems[ActualRow];
  end;

  if (CurrentObj = nil) or (FEntityMap = nil) then Exit;

  if not FEntityMap.Properties.TryGetValue(Field.FieldName, PropMap) then
    Exit;

  PValue := Pointer(PByte(CurrentObj) + PropMap.FieldValueOffset);

  // RTTI fallback path (for properties without direct offset)
  if (PropMap.FieldValueOffset <= 0) then
  begin
    Context := TRttiContext.Create;
    try
     RttiType := Context.GetType(FEntityClass);
      if RttiType <> nil then
      begin
        RttiProp := RttiType.GetProperty(Field.FieldName);
        if (RttiProp <> nil) and (P <> nil) then
        begin
          case Field.DataType of
            ftString, ftWideString:
              RttiProp.SetValue(CurrentObj, string(PWideChar(P)));
            ftInteger, ftSmallint:
              RttiProp.SetValue(CurrentObj, PInteger(P)^);
            ftLargeint:
              RttiProp.SetValue(CurrentObj, PInt64(P)^);
            ftFloat, ftCurrency:
              RttiProp.SetValue(CurrentObj, PDouble(P)^);
            ftBoolean:
              RttiProp.SetValue(CurrentObj, PWordBool(P)^ <> False);
            ftDateTime, ftDate, ftTime:
              RttiProp.SetValue(CurrentObj, TValue.From<Double>(PDouble(P)^));
          end;
          SetModified(True);
          Exit;
        end;

        RttiField := RttiType.GetField(Field.FieldName);
        if (RttiField <> nil) and (P <> nil) then
        begin
          case Field.DataType of
            ftString, ftWideString:
              RttiField.SetValue(CurrentObj, string(PWideChar(P)));
            ftInteger, ftSmallint:
              RttiField.SetValue(CurrentObj, PInteger(P)^);
            ftLargeint:
              RttiField.SetValue(CurrentObj, PInt64(P)^);
            ftFloat, ftCurrency:
              RttiField.SetValue(CurrentObj, PDouble(P)^);
            ftBoolean:
              RttiField.SetValue(CurrentObj, PWordBool(P)^ <> False);
            ftDateTime, ftDate, ftTime:
              RttiField.SetValue(CurrentObj, TValue.From<Double>(PDouble(P)^));
          end;
          SetModified(True);
          Exit;
        end;
      end;
    finally
      Context.Free;
    end;
  end;

  // Direct offset write path (fast-path)
  if P <> nil then
  begin
    case Field.DataType of
      ftString, ftWideString:
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
      ftString, ftWideString:
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
