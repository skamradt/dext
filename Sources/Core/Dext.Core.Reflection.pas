unit Dext.Core.Reflection;

interface

uses
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  System.StrUtils,
  System.Character,
  Dext.Collections,
  Dext.Collections.Dict,
  Dext.Types.Lazy;

type
  TCustomAttributeClass = class of TCustomAttribute;

  /// <summary>
  ///   Indicates that a record type is a Dext Smart Property.
  /// </summary>
  SmartPropAttribute = class(TCustomAttribute);

  TRttiObjectHelper = class helper for TRttiObject
  public
    function GetAttribute<T: TCustomAttribute>: T; overload;
    function GetAttribute(AClass: TCustomAttributeClass): TCustomAttribute; overload;
    function HasAttribute<T: TCustomAttribute>: Boolean; overload;
    function HasAttribute(AClass: TCustomAttributeClass): Boolean; overload;
  end;

  /// <summary>
  ///   Cached structural information about a type.
  /// </summary>
  TTypeMetadata = class
  public
    InnerType: PTypeInfo;
    IsLazy: Boolean;
    IsNullable: Boolean;
    IsSmartProp: Boolean;
    HasValueField: TRttiField;
    RttiType: TRttiType;
    ValueField: TRttiField;
    constructor Create(AType: PTypeInfo);
  end;

  TReflection = class
  private
    class var FCache: IDictionary<PTypeInfo, TTypeMetadata>;
    class var FContext: TRttiContext;
    class constructor Create;
    class destructor Destroy;
  public
    class function GetMetadata(AType: PTypeInfo): TTypeMetadata; static;
    class function GetValue(AInstance: TObject; const APropertyName: string): TValue; static;
    class procedure SetValue(AInstance: Pointer; AMember: TRttiMember; const AValue: TValue); static;
    class procedure SetValueByPath(AInstance: TObject; const APath: string; const AValue: TValue); static;
    class function IsSmartProp(AType: PTypeInfo): Boolean; static;
    class function GetUnderlyingType(AType: PTypeInfo): PTypeInfo; overload; static;
    class function GetUnderlyingType(const AValue: TValue): PTypeInfo; overload; static;
    class function TryUnwrapProp(const ASource: TValue; var ADest: TValue): Boolean; static;
    class function TryWrapProp(var ADest: TValue; const ASource: TValue): Boolean; static;
    class function CreateInstance(AClass: TClass): TObject; static;
    class function GetFieldPtr(Instance: TObject; const FieldName: string): Pointer; static;
    class function NormalizeFieldName(const AFieldName: string): string; static;
    class property Context: TRttiContext read FContext;
  end;

implementation

uses
  Dext.Core.Activator,
  Dext.Core.ValueConverters,
  Dext.DI.Core;

{ TRttiObjectHelper }

function TRttiObjectHelper.GetAttribute<T>: T;
begin
  Result := nil;
  for var Attr in GetAttributes do if Attr is T then Exit(T(Attr));
end;

function TRttiObjectHelper.GetAttribute(AClass: TCustomAttributeClass): TCustomAttribute;
begin
  Result := nil;
  for var Attr in GetAttributes do if Attr.InheritsFrom(AClass) then Exit(Attr);
end;

function TRttiObjectHelper.HasAttribute<T>: Boolean;
begin
  Result := GetAttribute<T> <> nil;
end;

function TRttiObjectHelper.HasAttribute(AClass: TCustomAttributeClass): Boolean;
begin
  Result := GetAttribute(AClass) <> nil;
end;

{ TTypeMetadata }

constructor TTypeMetadata.Create(AType: PTypeInfo);
begin
  RttiType := TReflection.FContext.GetType(AType);
  IsSmartProp := False;
  IsNullable := False;
  ValueField := nil;
  HasValueField := nil;
  InnerType := nil;

  if (RttiType <> nil) and (RttiType.TypeKind = tkRecord) then
  begin
    var TypeName := RttiType.Name;
    IsNullable := TypeName.Contains('Nullable');
    IsSmartProp := RttiType.HasAttribute(SmartPropAttribute);

    for var Field in RttiType.GetFields do
    begin
      var LFieldName := Field.Name;
      if SameText(LFieldName, 'FValue') or SameText(LFieldName, 'Value') then
      begin
        ValueField := Field;
        InnerType := Field.FieldType.Handle;
        IsSmartProp := True;
      end
      else if LFieldName.ToLower.Contains('hasvalue') or SameText(LFieldName, 'FInfo') or SameText(LFieldName, 'Info') then
        HasValueField := Field
      else if SameText(LFieldName, 'FInstance') and (string(Field.FieldType.Handle.Name).Contains('ILazy')) then
      begin
        IsLazy := True;
        ValueField := Field;
      end;
    end;

    // Fallback: se não encontrou via RTTI (comum em records genéricos sem metadados explícitos)
    if not IsSmartProp then
    begin
      var LTypeName := string(RttiType.Handle.Name);
      if (LTypeName.Contains('Prop<') or LTypeName.Contains('Nullable<') or
         LTypeName.Contains('Proxy<') or LTypeName.Contains('TProxy<') or
         LTypeName.Contains('PropType')) then
      begin
        IsSmartProp := True;
        
        // Tenta encontrar o campo por convenção se o GetFields falhou
        if ValueField = nil then
          ValueField := RttiType.GetField('FValue');
        if ValueField = nil then
          ValueField := RttiType.GetField('FProxy'); // Added for Proxy<T>
        if ValueField = nil then
          ValueField := RttiType.GetField('Value');
      end;
    end;

    // Se não encontrou o tipo interno via campos/propriedades, tenta via nome (último recurso)
    if (InnerType = nil) and (IsSmartProp or IsLazy) then
    begin
      // Try 'Value' property first (e.g. Nullable<T>)
      var LValueProp := RttiType.GetProperty('Value');
      if LValueProp <> nil then
        InnerType := LValueProp.PropertyType.Handle;

      if (InnerType = nil) and (ValueField <> nil) and not IsLazy then
        InnerType := ValueField.FieldType.Handle;

      if InnerType = nil then
      begin
        var LTypeName := string(RttiType.Handle.Name);
        if LTypeName.Contains('<') and LTypeName.EndsWith('>') then
        begin
          // Tenta extrair o tipo do nome genérico: Nome<T>
          var LTMark := LTypeName.IndexOf('<');
          var LInnerTypeName := LTypeName.Substring(LTMark + 1, LTypeName.Length - LTMark - 2);
          
          // Tenta encontrar o tipo via contexto RTTI
          var LInnerRtti := TReflection.Context.FindType(LInnerTypeName);
          if LInnerRtti = nil then
            LInnerRtti := TReflection.Context.FindType('System.' + LInnerTypeName);
            
          if LInnerRtti <> nil then
            InnerType := LInnerRtti.Handle;

          // Map common simple types if FindType fails
          if InnerType = nil then
          begin
            if SameText(LInnerTypeName, 'Integer') or SameText(LInnerTypeName, 'System.Integer') then InnerType := TypeInfo(Integer)
            else if SameText(LInnerTypeName, 'string') or SameText(LInnerTypeName, 'System.string') then InnerType := TypeInfo(string)
            else if SameText(LInnerTypeName, 'Boolean') or SameText(LInnerTypeName, 'System.Boolean') then InnerType := TypeInfo(Boolean)
            else if SameText(LInnerTypeName, 'Double') or SameText(LInnerTypeName, 'System.Double') then InnerType := TypeInfo(Double)
            else if SameText(LInnerTypeName, 'TDateTime') or SameText(LInnerTypeName, 'System.TDateTime') then InnerType := TypeInfo(TDateTime)
            else if SameText(LInnerTypeName, 'Currency') or SameText(LInnerTypeName, 'System.Currency') then InnerType := TypeInfo(Currency)
            else if SameText(LInnerTypeName, 'Int64') or SameText(LInnerTypeName, 'System.Int64') then InnerType := TypeInfo(Int64);
          end;
        end;
      end;
    end;
  end
  else if (RttiType <> nil) and (RttiType.TypeKind = tkInterface) then
  begin
    var LTypeName := string(RttiType.Handle.Name);
    if (LTypeName = 'ILazy') or LTypeName.Contains('ILazy<') then
    begin
        IsLazy := True;
        IsSmartProp := False; // Lazy is NOT a SmartProp (it doesn't have FValue: T)
        var LTMark := LTypeName.IndexOf('<');
        if LTMark > 0 then
        begin
          var LInnerTypeName := LTypeName.Substring(LTMark + 1, LTypeName.Length - LTMark - 2);
          
          var LInnerRtti := TReflection.Context.FindType(LInnerTypeName);
          if LInnerRtti = nil then
            LInnerRtti := TReflection.Context.FindType('System.' + LInnerTypeName);
            
          if LInnerRtti <> nil then
            InnerType := LInnerRtti.Handle;
  
          if InnerType = nil then
          begin
            if SameText(LInnerTypeName, 'Integer') or SameText(LInnerTypeName, 'System.Integer') then InnerType := TypeInfo(Integer)
            else if SameText(LInnerTypeName, 'string') or SameText(LInnerTypeName, 'System.string') then InnerType := TypeInfo(string)
            else if SameText(LInnerTypeName, 'Boolean') or SameText(LInnerTypeName, 'System.Boolean') then InnerType := TypeInfo(Boolean)
            else if SameText(LInnerTypeName, 'Double') or SameText(LInnerTypeName, 'System.Double') then InnerType := TypeInfo(Double)
            else if SameText(LInnerTypeName, 'TDateTime') or SameText(LInnerTypeName, 'System.TDateTime') then InnerType := TypeInfo(TDateTime);
          end;
        end;
    end;
  end;
end;

{ TReflection }

class constructor TReflection.Create;
begin
  FContext := TRttiContext.Create;
  FCache := TCollections.CreateDictionary<PTypeInfo, TTypeMetadata>(True);
end;

class destructor TReflection.Destroy;
begin
  FCache := nil;
  FContext.Free;
end;

class function TReflection.GetMetadata(AType: PTypeInfo): TTypeMetadata;
begin
  if not FCache.TryGetValue(AType, Result) then
  begin
    Result := TTypeMetadata.Create(AType);
    FCache.Add(AType, Result);
  end;
end;

class procedure TReflection.SetValue(AInstance: Pointer; AMember: TRttiMember; const AValue: TValue);
var
  TargetType: PTypeInfo;
begin
  if (AInstance = nil) or (AMember = nil) then Exit;
  if AMember is TRttiProperty then TargetType := TRttiProperty(AMember).PropertyType.Handle
  else if AMember is TRttiField then TargetType := TRttiField(AMember).FieldType.Handle
  else Exit;
  var Meta := GetMetadata(TargetType);
  if Meta.IsSmartProp or Meta.IsLazy then
  begin
    // Fast path: if the value is already of the target type, just set it directly
    if AValue.TypeInfo = TargetType then
    begin
      if AMember is TRttiProperty then TRttiProperty(AMember).SetValue(AInstance, AValue)
      else if AMember is TRttiField then TRttiField(AMember).SetValue(AInstance, AValue);
      Exit;
    end;

    var Current: TValue;
    if AMember is TRttiProperty then Current := TRttiProperty(AMember).GetValue(AInstance)
    else if AMember is TRttiField then Current := TRttiField(AMember).GetValue(AInstance);
    
    if TryWrapProp(Current, AValue) then
    begin
      if AMember is TRttiProperty then TRttiProperty(AMember).SetValue(AInstance, Current)
      else if AMember is TRttiField then TRttiField(AMember).SetValue(AInstance, Current);
      Exit;
    end;
  end;

  var Converted := TValueConverter.Convert(AValue, TargetType);
  if AMember is TRttiProperty then TRttiProperty(AMember).SetValue(AInstance, Converted)
  else if AMember is TRttiField then TRttiField(AMember).SetValue(AInstance, Converted);
end;

class function TReflection.GetValue(AInstance: TObject; const APropertyName: string): TValue;
var
  RType: TRttiType;
  Prop: TRttiProperty;
  Field: TRttiField;
  Raw, Unwrapped: TValue;
begin
  Result := TValue.Empty;
  if AInstance = nil then Exit;
  
  RType := FContext.GetType(AInstance.ClassType);
  if RType = nil then Exit;

  // 1. Try Property
  Prop := RType.GetProperty(APropertyName);
  if Prop <> nil then
    Raw := Prop.GetValue(Pointer(AInstance))
  else
  begin
    // 2. Try Field (FPropName or PropName)
    Field := RType.GetField(APropertyName);
    if Field = nil then
      Field := RType.GetField('F' + APropertyName);
      
    if Field <> nil then
      Raw := Field.GetValue(Pointer(AInstance))
    else
      Exit;
  end;

  if TryUnwrapProp(Raw, Unwrapped) then
    Result := Unwrapped
  else
    Result := Raw;
end;

class procedure TReflection.SetValueByPath(AInstance: TObject; const APath: string; const AValue: TValue);
var
  Parts: TArray<string>;
  Prop: TRttiProperty;
  CurObj: TObject;
  SubPath: string;
begin
  if (AInstance = nil) or (APath = '') then Exit;
  
  // Support both . and _ as separators
  if APath.Contains('_') then Parts := APath.Split(['_'])
  else Parts := APath.Split(['.']);

  if Length(Parts) = 1 then
  begin
    Prop := FContext.GetType(AInstance.ClassInfo).GetProperty(Parts[0]);
    if Prop <> nil then SetValue(Pointer(AInstance), Prop, AValue);
    Exit;
  end;

  // Level 1: Find the first part
  Prop := FContext.GetType(AInstance.ClassInfo).GetProperty(Parts[0]);
  if (Prop <> nil) and (Prop.PropertyType.TypeKind = tkClass) then
  begin
    CurObj := Prop.GetValue(Pointer(AInstance)).AsObject;
    if CurObj = nil then
    begin
       // Auto-instantiate nested classes
       CurObj := TActivator.CreateInstance(GetTypeData(Prop.PropertyType.Handle)^.ClassType, []);
       Prop.SetValue(Pointer(AInstance), CurObj);
    end;
    
    // Recursive call for the rest of the path
    SubPath := string.Join('.', Parts, 1, Length(Parts) - 1);
    SetValueByPath(CurObj, SubPath, AValue);
  end;
end;

class function TReflection.IsSmartProp(AType: PTypeInfo): Boolean;
begin
  Result := GetMetadata(AType).IsSmartProp;
end;

class function TReflection.GetUnderlyingType(AType: PTypeInfo): PTypeInfo;
begin
  Result := GetMetadata(AType).InnerType;
  if Result = nil then
    Result := AType;
end;

class function TReflection.GetUnderlyingType(const AValue: TValue): PTypeInfo;
var
  LLazy: ILazy;
begin
  Result := nil;
  // If it's an interface, check if it's an ILazy to get TargetType without loading
  if AValue.Kind = tkInterface then
  begin
    var LIntf := AValue.AsInterface;
    if (LIntf <> nil) and (LIntf.QueryInterface(ILazy, LLazy) = S_OK) then
      Exit(LLazy.TargetType);
  end;

  // Fallback to static type information
  if AValue.TypeInfo <> nil then
    Result := GetUnderlyingType(AValue.TypeInfo);

  if Result = nil then
    Result := AValue.TypeInfo;
end;

class function TReflection.TryUnwrapProp(const ASource: TValue; var ADest: TValue): Boolean;
var
  PData: Pointer;
  Unwrapped: TValue;
begin
  ADest := ASource;
  Result := False;

  if ASource.TypeInfo = nil then Exit;

  // Direct support for ILazy interface
  if ASource.Kind = tkInterface then
  begin
    var LIntf := ASource.AsInterface;
    var LLazy: ILazy;
    if (LIntf <> nil) and (LIntf.QueryInterface(ILazy, LLazy) = S_OK) then
    begin
       Unwrapped := LLazy.Value;
       if TryUnwrapProp(Unwrapped, ADest) then
         Result := True
       else
       begin
         ADest := Unwrapped;
         Result := True;
       end;
       Exit;
    end;
  end;

  // For other types, only unwrap if it is a registered SmartProp (Record or Class)
  if not (ASource.Kind in [tkRecord, tkClass]) then Exit;

  var Meta := GetMetadata(ASource.TypeInfo);
  if (Meta.IsSmartProp or Meta.IsLazy) and (Meta.ValueField <> nil) then
  begin
    PData := ASource.GetReferenceToRawData;
    if PData = nil then Exit;

    // For Nullable<T>, check HasValue first
    if Meta.IsNullable and (Meta.HasValueField <> nil) then
    begin
        if not Meta.HasValueField.GetValue(PData).AsBoolean then
        begin
          ADest := TValue.Empty;
          Result := True;
          Exit;
        end;
    end;

    Unwrapped := Meta.ValueField.GetValue(PData);

    // If unwrapped is an ILazy interface, extract its value
    if (Unwrapped.Kind = tkInterface) then
    begin
       var LIntf := Unwrapped.AsInterface;
       var LLazy: ILazy;
       if (LIntf <> nil) and (LIntf.QueryInterface(ILazy, LLazy) = S_OK) then
       begin
          Unwrapped := LLazy.Value;
       end;
    end;
    // RECURSIVE UNWRAP
    if TryUnwrapProp(Unwrapped, ADest) then
      Result := True
    else
    begin
      ADest := Unwrapped;
      Result := True;
    end;
  end;
end;

class function TReflection.TryWrapProp(var ADest: TValue; const ASource: TValue): Boolean;
var
  PData: Pointer;
begin
  Result := False;
  if ADest.TypeInfo = nil then Exit;

  var Meta := GetMetadata(ADest.TypeInfo);
  if Meta.IsSmartProp and (Meta.ValueField <> nil) then
  begin
    PData := ADest.GetReferenceToRawData;
    if PData <> nil then
    begin
      // If it's Nullable, also set HasValue
      if Meta.IsNullable and (Meta.HasValueField <> nil) then
        Meta.HasValueField.SetValue(PData, not ASource.IsEmpty);

      if not ASource.IsEmpty then
      begin
        var Converted := TValueConverter.Convert(ASource, Meta.InnerType);
        Meta.ValueField.SetValue(PData, Converted);
      end;
      Result := True;
    end;
  end;
end;

class function TReflection.CreateInstance(AClass: TClass): TObject;
begin
  Result := TActivator.CreateInstance(AClass, []);
end;

class function TReflection.GetFieldPtr(Instance: TObject; const FieldName: string): Pointer;
var
  RField: TRttiField;
begin
  RField := FContext.GetType(Instance.ClassType).GetField(FieldName);
  if RField <> nil then
    Result := PByte(Instance) + RField.Offset
  else
    Result := nil;
end;

class function TReflection.NormalizeFieldName(const AFieldName: string): string;
begin
  Result := AFieldName;
  
  // Remove 'F' prefix if it exists (0-based Char.IsUpper check)
  if (Result.Length > 1) and (Result[1] = 'F') and (Char.IsUpper(Result, 1)) then
    Result := Result.Substring(1);

  // Normalize Smart Property prefixes: Lazy, Prop, Proxy, Nullable
  if Result.StartsWith('Lazy', True) and (Result.Length > 4) and (Char.IsUpper(Result, 4)) then
    Result := Result.Substring(4)
  else if Result.StartsWith('Prop', True) and (Result.Length > 4) and (Char.IsUpper(Result, 4)) then
    Result := Result.Substring(4)
  else if Result.StartsWith('Proxy', True) and (Result.Length > 5) and (Char.IsUpper(Result, 5)) then
    Result := Result.Substring(5)
  else if Result.StartsWith('Nullable', True) and (Result.Length > 8) and (Char.IsUpper(Result, 8)) then
    Result := Result.Substring(8);
end;

end.
