unit Dext.Core.Reflection;

interface

uses
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections;

type
  TCustomAttributeClass = class of TCustomAttribute;

  TRttiObjectHelper = class helper for TRttiObject
  public
    function GetAttribute<T: TCustomAttribute>: T; overload;
    function GetAttribute(AClass: TCustomAttributeClass): TCustomAttribute; overload;
    function HasAttribute<T: TCustomAttribute>: Boolean; overload;
    function HasAttribute(AClass: TCustomAttributeClass): Boolean; overload;
  end;

  /// <summary>
  ///   Cached structural information about a type to avoid redundant RTTI lookups.
  /// </summary>
  TTypeMetadata = class
  public
    RttiType: TRttiType;
    IsSmartProp: Boolean;
    IsNullable: Boolean;
    ValueField: TRttiField;
    HasValueField: TRttiField;
    InnerType: PTypeInfo;
    constructor Create(AType: PTypeInfo);
  end;

  /// <summary>
  ///   Centralized and optimized reflection helper for the Dext Framework.
  ///   Handles Smart Types (Prop<T>), Nullables, and standard type conversions.
  /// </summary>
  TReflection = class
  private
    class var FCache: TObjectDictionary<PTypeInfo, TTypeMetadata>;
    class var FContext: TRttiContext;
    class constructor Create;
    class destructor Destroy;
  public
    /// <summary>
    ///   Gets cached metadata for a type. Creates and caches if not found.
    /// </summary>
    class function GetMetadata(AType: PTypeInfo): TTypeMetadata; static;
    
    /// <summary>
    ///   Sets a value to a property or field, automatically handling Smart Types, 
    ///   Nullables, and conversion using TValueConverter.
    /// </summary>
    class procedure SetValue(AInstance: Pointer; AMember: TRttiMember; const AValue: TValue); static;
    
    /// <summary>
    ///   Checks if a type is a Smart Type (Prop<T>).
    /// </summary>
    class function IsSmartProp(AType: PTypeInfo): Boolean; static;
  end;

implementation

uses
  Dext.Core.ValueConverters;

{ TRttiObjectHelper }

function TRttiObjectHelper.GetAttribute<T>: T;
begin
  Result := nil;
  for var Attr in GetAttributes do
  begin
    if Attr is T then
      Exit(T(Attr));
  end;
end;

function TRttiObjectHelper.GetAttribute(AClass: TCustomAttributeClass): TCustomAttribute;
begin
  Result := nil;
  for var Attr in GetAttributes do
  begin
    if Attr.InheritsFrom(AClass) then
      Exit(Attr);
  end;
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
var
  TypeName: string;
begin
  RttiType := TReflection.FContext.GetType(AType);
  IsSmartProp := False;
  IsNullable := False;
  ValueField := nil;
  HasValueField := nil;
  InnerType := nil;

  if (RttiType <> nil) and (RttiType.TypeKind = tkRecord) then
  begin
    TypeName := RttiType.Name;
    
    // Detect Nullable<T>
    if TypeName.StartsWith('Nullable<') or TypeName.StartsWith('TNullable') then
    begin
      IsNullable := True;
      for var Field in RttiType.GetFields do
      begin
        if Field.Name.ToLower.Contains('hasvalue') then
          HasValueField := Field
        else if Field.Name.ToLower = 'fvalue' then
          ValueField := Field;
      end;
      if ValueField <> nil then
        InnerType := ValueField.FieldType.Handle;
    end
    // Detect Prop<T> (Smart Type)
    else if TypeName.Contains('Prop<') or TypeName.Contains('TProp') or (RttiType.GetField('FValue') <> nil) then
    begin
      ValueField := RttiType.GetField('FValue');
      if ValueField <> nil then
      begin
        IsSmartProp := True;
        InnerType := ValueField.FieldType.Handle;
      end
      else if TypeName.Contains('Prop<') then
      begin
         // If name matches but field not found, still mark as SmartProp
         // to avoid falling back to incompatible Casts.
         IsSmartProp := True;
         // Try to find ANY field as a last resort? 
         // No, the $RTTI directive in SmartTypes.pas is the definitive fix.
      end;
    end;
  end;
end;

{ TReflection }

class constructor TReflection.Create;
begin
  FContext := TRttiContext.Create;
  FCache := TObjectDictionary<PTypeInfo, TTypeMetadata>.Create([doOwnsValues]);
end;

class destructor TReflection.Destroy;
begin
  FCache.Free;
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
  Converted: TValue;
begin
  if (AInstance = nil) or (AMember = nil) then Exit;

  if AMember is TRttiProperty then
    TargetType := TRttiProperty(AMember).PropertyType.Handle
  else if AMember is TRttiField then
    TargetType := TRttiField(AMember).FieldType.Handle
  else
    Exit;

  // Use TValueConverter which properly handles Smart Types, Nullables, and standard types.
  // For Prop<T>, it creates a new record with FValue set correctly.
  // For Nullable<T>, it creates a new record with FValue and HasValue set.
  Converted := TValueConverter.Convert(AValue, TargetType);
  
  // Assign the converted value
  if AMember is TRttiProperty then
    TRttiProperty(AMember).SetValue(AInstance, Converted)
  else if AMember is TRttiField then
    TRttiField(AMember).SetValue(AInstance, Converted);
end;

class function TReflection.IsSmartProp(AType: PTypeInfo): Boolean;
begin
  Result := GetMetadata(AType).IsSmartProp;
end;

end.
