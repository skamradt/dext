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
unit Dext.Entity.Mapping;

interface

uses
  Dext.Collections,
  System.SysUtils,
  Dext.Collections.Dict,
  System.TypInfo,
  System.Rtti,
  System.Variants,
  System.Character,
  Data.DB,
  Dext.Entity.Attributes,
  Dext.Entity.TypeConverters,
  Dext.Core.SmartTypes,
  Dext.Core.Reflection,
  Dext.Specifications.Interfaces;

type
  TRelationshipType = (rtNone, rtOneToMany, rtManyToOne, rtOneToOne, rtManyToMany);

  // Forward declarations
  IPropertyBuilder<T: class> = interface;
  IRelationshipBuilder<T: class> = interface;

  /// <summary>
  ///   Fluent interface to configure an entity.
  /// </summary>
  IEntityTypeBuilder<T: class> = interface
    ['{6DC34AF2-B40E-428F-85B6-94D77209476F}']
    function ToTable(const AName: string): IEntityTypeBuilder<T>;
    function HasKey(const APropertyName: string): IEntityTypeBuilder<T>; overload;
    function HasKey(const APropertyNames: array of string): IEntityTypeBuilder<T>; overload;
    function HasDiscriminator(const AColumn: string; const AValue: Variant): IEntityTypeBuilder<T>;
    function MapInheritance(AStrategy: TInheritanceStrategy): IEntityTypeBuilder<T>;
    function Prop(const APropertyName: string): IPropertyBuilder<T>; overload;
    function Prop(const AProp: IPropInfo): IPropertyBuilder<T>; overload;
    function ShadowProperty(const APropName: string): IPropertyBuilder<T>;
    function Ignore(const APropertyName: string): IEntityTypeBuilder<T>;
    
    // Relationships
    function HasMany(const APropertyName: string): IRelationshipBuilder<T>;
    function HasOne(const APropertyName: string): IRelationshipBuilder<T>;
    function BelongsTo(const APropertyName: string): IRelationshipBuilder<T>;
    function HasManyToMany(const APropertyName: string): IRelationshipBuilder<T>;
  end;

  /// <summary>
  ///   Fluent interface to configure a relationship.
  /// </summary>
  IRelationshipBuilder<T: class> = interface
    ['{ADF7AC66-198B-4FD2-9E43-9D342A37542C}']
    function WithOne(const AInversePropName: string = ''): IRelationshipBuilder<T>;
    function WithMany(const AInversePropName: string = ''): IRelationshipBuilder<T>;
    function HasForeignKey(const AFKPropertyName: string): IRelationshipBuilder<T>;
    function HasPrincipalKey(const AKeyPropertyName: string): IRelationshipBuilder<T>;
    function OnDelete(AAction: TCascadeAction): IRelationshipBuilder<T>;
    // Many-to-Many specific
    function UsingEntity(const AJoinTableName: string): IRelationshipBuilder<T>; overload;
    function UsingEntity(const AJoinTableName, ALeftKey, ARightKey: string): IRelationshipBuilder<T>; overload;
  end;

  /// <summary>
  ///   Fluent interface to configure a property.
  /// </summary>
  IPropertyBuilder<T: class> = interface
    ['{31A85F1F-53AD-4130-85DC-AC6FCC062AE2}']
    function HasColumnName(const AName: string): IPropertyBuilder<T>;
    function HasFieldName(const AName: string): IPropertyBuilder<T>;
    function UseField: IPropertyBuilder<T>;
    function IsRequired(AValue: Boolean = True): IPropertyBuilder<T>;
    function IsAutoInc(AValue: Boolean = True): IPropertyBuilder<T>;
    function HasMaxLength(ALength: Integer): IPropertyBuilder<T>;
    function HasMinLength(ALength: Integer): IPropertyBuilder<T>;
    function HasPrecision(APrecision, AScale: Integer): IPropertyBuilder<T>;
    function HasDbType(ADataType: TFieldType): IPropertyBuilder<T>;
    function HasConverter(AConverterClass: TClass): IPropertyBuilder<T>;
    function IsLazy(AValue: Boolean = True): IPropertyBuilder<T>;
    function IsVersion(AValue: Boolean = True): IPropertyBuilder<T>;
    function IsCreatedAt(AValue: Boolean = True): IPropertyBuilder<T>;
    function IsUpdatedAt(AValue: Boolean = True): IPropertyBuilder<T>;
    function IsShadow(AValue: Boolean = True): IPropertyBuilder<T>;
  end;

  /// <summary>
  ///   Base interface for user-defined mapping configurations.
  /// </summary>
  IEntityTypeConfiguration<T: class> = interface
    ['{F8D58DC7-8AFC-4EC4-ACE5-6CA95E9978F6}']
    procedure Configure(Builder: IEntityTypeBuilder<T>);
  end;

  /// <summary>
  ///   Factory interface for dynamic creation of generic DbSets.
  /// </summary>
  IDynamicDbSetFactory = interface
    ['{A578C63F-1596-46EE-A9B5-AA86EB5E831B}']
    function CreateDbSet(const AContext: IInterface): IInterface;
  end;

  // ---------------------------------------------------------------------------
  // Internal Model Representation (The result of the mapping)
  // ---------------------------------------------------------------------------

  TPropertyMap = class
  public
    PropertyName: string;
    ColumnName: string;
    FieldName: string;
    ForeignKeyColumn: string; // Added for FK support
    IsPK: Boolean;
    IsAutoInc: Boolean;
    IsRequired: Boolean;
    MaxLength: Integer;
    MinLength: Integer;
    Precision: Integer;
    Scale: Integer;
    IsIgnored: Boolean;
    IsNavigation: Boolean;
    Relationship: TRelationshipType;
    InverseProperty: string;
    PrincipalKey: string;
    DeleteBehavior: TCascadeAction;
    // Many-to-Many Join Table
    JoinTableName: string;
    LeftKeyColumn: string;
    RightKeyColumn: string;
    Converter: ITypeConverter;
    DataType: TFieldType;
    ConverterClass: TClass;
    // Optimistic Concurrency
    IsVersion: Boolean;
    // Audit Timestamps
    IsCreatedAt: Boolean;
    IsUpdatedAt: Boolean;
    // Internal engine optimization
    FieldOffset: Integer;      // Offset of Null flag (Boolean) - Used by TEntityDataSet
    MetadataOffset: Integer;   // Offset of FInfo (IPropInfo) - Used by Prototype
    FieldValueOffset: Integer; // Offset of FValue
    PropertyType: PTypeInfo;   // Type of T in Prop<T>
    // Shadow Property support
    IsShadow: Boolean;
    // JSON Column Support
    IsJsonColumn: Boolean;
    UseJsonB: Boolean; // PostgreSQL JSONB vs JSON
    IsLazy: Boolean; // New: Support for Auto-Proxies / Explicit Lazy
    IsEnum: Boolean; // New: Flag to identify enumeration types
    Prop: TRttiProperty; // Cached RTTI property
    constructor Create(const APropName: string);
  end;

  TEntityMap = class
  private
    FEntityType: PTypeInfo;
    FTableName: string;
    FProperties: IDictionary<string, TPropertyMap>;
    FKeys: IList<string>;
    // Soft Delete Configuration
    FIsSoftDelete: Boolean;
    FSoftDeleteProp: string;
    FSoftDeleteDeletedValue: Variant;
    FSoftDeleteNotDeletedValue: Variant;
    // Inheritance Configuration
    FInheritanceStrategy: TInheritanceStrategy;
    FDiscriminatorColumn: string;
    FDiscriminatorValue: Variant;
    // Global Query Filters
    FQueryFilters: IList<IExpression>;

  public
    constructor Create(AEntityType: PTypeInfo);
    destructor Destroy; override;
    
    property EntityType: PTypeInfo read FEntityType;
    property TableName: string read FTableName write FTableName;
    property Properties: IDictionary<string, TPropertyMap> read FProperties;
    property Keys: IList<string> read FKeys;
    
    property IsSoftDelete: Boolean read FIsSoftDelete;
    property SoftDeleteProp: string read FSoftDeleteProp;
    property SoftDeleteDeletedValue: Variant read FSoftDeleteDeletedValue;
    property SoftDeleteNotDeletedValue: Variant read FSoftDeleteNotDeletedValue;
    property QueryFilters: IList<IExpression> read FQueryFilters;
    
    property InheritanceStrategy: TInheritanceStrategy read FInheritanceStrategy write FInheritanceStrategy;
    property DiscriminatorColumn: string read FDiscriminatorColumn write FDiscriminatorColumn;
    property DiscriminatorValue: Variant read FDiscriminatorValue write FDiscriminatorValue;

    procedure DiscoverAttributes;
    procedure ProcessAttribute(APropMap: TPropertyMap; AAttr: TCustomAttribute);
    function GetOrAddProperty(const APropName: string): TPropertyMap;
  end;

  // ---------------------------------------------------------------------------
  // Fluent API Records
  // ---------------------------------------------------------------------------

  TEntityBuilder<T: class> = record
  private
    FMap: TEntityMap;
    FCurrentProp: TPropertyMap;
    function GetCurrentProp: TPropertyMap;
  public
    constructor Create(AMap: TEntityMap);
    
    // Entity Configuration
    function Table(const AName: string): TEntityBuilder<T>;
    function HasKey(const APropertyName: string): TEntityBuilder<T>; overload;
    function HasKey(const APropertyNames: array of string): TEntityBuilder<T>; overload;
    function HasDiscriminator(const AColumn: string; const AValue: Variant): TEntityBuilder<T>;
    function MapInheritance(AStrategy: TInheritanceStrategy): TEntityBuilder<T>;
    
    // Property Selection
    function Prop(const APropertyName: string): TEntityBuilder<T>;
    function ShadowProperty(const APropName: string): TEntityBuilder<T>;
    function HasProperty(const APropertyName: string): TEntityBuilder<T>;
    
    // Property Configuration (Applied to current property)
    function Column(const AName: string): TEntityBuilder<T>;
    function HasForeignKey(const AColumnName: string): TEntityBuilder<T>;
    function IsRequired(AValue: Boolean = True): TEntityBuilder<T>;
    function IsAutoInc(AValue: Boolean = True): TEntityBuilder<T>;
    function MaxLength(ALength: Integer): TEntityBuilder<T>;
    function MinLength(ALength: Integer): TEntityBuilder<T>;
    function Precision(APrecision, AScale: Integer): TEntityBuilder<T>;
    function HasDbType(ADataType: TFieldType): TEntityBuilder<T>;
    function HasConverter(AConverterClass: TClass): TEntityBuilder<T>;
    function IsJson(AUseJsonB: Boolean = True): TEntityBuilder<T>;
    function IsLazy(AValue: Boolean = True): TEntityBuilder<T>;
    function IsVersion(AValue: Boolean = True): TEntityBuilder<T>;
    function IsCreatedAt(AValue: Boolean = True): TEntityBuilder<T>;
    function IsUpdatedAt(AValue: Boolean = True): TEntityBuilder<T>;
    function IsShadow(AValue: Boolean = True): TEntityBuilder<T>;
    function Ignore: TEntityBuilder<T>;
    
    // Relationship Support (Returning IRelationshipBuilder)
    function HasMany(const APropertyName: string): IRelationshipBuilder<T>;
    function HasOne(const APropertyName: string): IRelationshipBuilder<T>;
    function BelongsTo(const APropertyName: string): IRelationshipBuilder<T>;
    function HasManyToMany(const APropertyName: string): IRelationshipBuilder<T>;
    
    // Soft Delete Configuration
    function HasSoftDelete(const APropertyName: string): TEntityBuilder<T>; overload;
    function HasSoftDelete(const APropertyName: string; const ADeletedValue, ANotDeletedValue: Variant): TEntityBuilder<T>; overload;

    // Global Query Filters
    function HasQueryFilter(AFilter: IExpression): TEntityBuilder<T>;
  end;

  // ---------------------------------------------------------------------------
  // Concrete Builders (Legacy / Interface based)
  // ---------------------------------------------------------------------------

  TEntityTypeBuilder<T: class> = class(TInterfacedObject, IEntityTypeBuilder<T>)
  private
    FMap: TEntityMap;
  public
    constructor Create(AMap: TEntityMap);
    function ToTable(const AName: string): IEntityTypeBuilder<T>;
    function HasKey(const APropertyName: string): IEntityTypeBuilder<T>; overload;
    function HasKey(const APropertyNames: array of string): IEntityTypeBuilder<T>; overload;
    function HasDiscriminator(const AColumn: string; const AValue: Variant): IEntityTypeBuilder<T>;
    function MapInheritance(AStrategy: TInheritanceStrategy): IEntityTypeBuilder<T>;
    function Prop(const APropertyName: string): IPropertyBuilder<T>; overload;
    function Prop(const AProp: IPropInfo): IPropertyBuilder<T>; overload;
    function ShadowProperty(const APropName: string): IPropertyBuilder<T>;
    function Ignore(const APropertyName: string): IEntityTypeBuilder<T>;
    function HasMany(const APropertyName: string): IRelationshipBuilder<T>;
    function HasOne(const APropertyName: string): IRelationshipBuilder<T>;
    function BelongsTo(const APropertyName: string): IRelationshipBuilder<T>;
    function HasManyToMany(const APropertyName: string): IRelationshipBuilder<T>;
    function HasSoftDelete(const APropertyName: string): IEntityTypeBuilder<T>; overload;
    function HasSoftDelete(const APropertyName: string; const ADeletedValue, ANotDeletedValue: Variant): IEntityTypeBuilder<T>; overload;
    function HasQueryFilter(AFilter: IExpression): IEntityTypeBuilder<T>;
  end;

  TPropertyBuilder<T: class> = class(TInterfacedObject, IPropertyBuilder<T>)
  private
    FPropMap: TPropertyMap;
  public
    constructor Create(APropMap: TPropertyMap);
    function HasColumnName(const AName: string): IPropertyBuilder<T>;
    function IsRequired(AValue: Boolean = True): IPropertyBuilder<T>;
    function IsAutoInc(AValue: Boolean = True): IPropertyBuilder<T>;
    function HasMaxLength(ALength: Integer): IPropertyBuilder<T>;
    function HasMinLength(ALength: Integer): IPropertyBuilder<T>;
    function HasPrecision(APrecision, AScale: Integer): IPropertyBuilder<T>;
    function HasDbType(ADataType: TFieldType): IPropertyBuilder<T>;
    function HasConverter(AConverterClass: TClass): IPropertyBuilder<T>;
    function HasFieldName(const AName: string): IPropertyBuilder<T>;
    function UseField: IPropertyBuilder<T>;
    function IsLazy(AValue: Boolean = True): IPropertyBuilder<T>;
    function IsVersion(AValue: Boolean = True): IPropertyBuilder<T>;
    function IsCreatedAt(AValue: Boolean = True): IPropertyBuilder<T>;
    function IsUpdatedAt(AValue: Boolean = True): IPropertyBuilder<T>;
    function IsShadow(AValue: Boolean = True): IPropertyBuilder<T>;
  end;

  /// <summary>
  ///   Base class for user configurations (easier to inherit from).
  /// </summary>
  TEntityTypeConfiguration<T: class> = class(TInterfacedObject, IEntityTypeConfiguration<T>)
  public
    procedure Configure(Builder: IEntityTypeBuilder<T>); virtual; abstract;
  end;

  /// <summary>
  ///   Central registry for mappings.
  /// </summary>
  TModelBuilder = class
  private
    class var FInstance: TModelBuilder;
  private var
    FMaps: IDictionary<PTypeInfo, TEntityMap>;
    FDiscoveryNames: IDictionary<PTypeInfo, string>;
    FFactories: IDictionary<PTypeInfo, IDynamicDbSetFactory>;
    class constructor Create;

    class destructor Destroy;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure ApplyConfiguration<T: class>(AConfig: IEntityTypeConfiguration<T>);
    function Entity<T: class>: TEntityBuilder<T>;
    
    function GetMap(AType: PTypeInfo): TEntityMap;
    function HasMap(AType: PTypeInfo): Boolean;
    function GetMaps: TArray<TEntityMap>;
    
    procedure RegisterDiscoveryName(AType: PTypeInfo; const AName: string);
    function GetDiscoveryName(AType: PTypeInfo): string;
    
    function FindMapByDiscriminator(ABaseType: PTypeInfo; const AValue: Variant): TEntityMap;
    procedure Clear;
    
    procedure RegisterFactory(AType: PTypeInfo; const AFactory: IDynamicDbSetFactory);
    function GetFactory(AType: PTypeInfo): IDynamicDbSetFactory;
    
    class property Instance: TModelBuilder read FInstance;

  end;

  TRelationshipBuilder<T: class> = class(TInterfacedObject, IRelationshipBuilder<T>)
  private
    FMap: TEntityMap;
    FPropMap: TPropertyMap;
  public
    constructor Create(AMap: TEntityMap; APropMap: TPropertyMap);
    function WithOne(const AInversePropName: string = ''): IRelationshipBuilder<T>;
    function WithMany(const AInversePropName: string = ''): IRelationshipBuilder<T>;
    function HasForeignKey(const AFKPropertyName: string): IRelationshipBuilder<T>;
    function HasPrincipalKey(const AKeyPropertyName: string): IRelationshipBuilder<T>;
    function OnDelete(AAction: TCascadeAction): IRelationshipBuilder<T>;
    function UsingEntity(const AJoinTableName: string): IRelationshipBuilder<T>; overload;
    function UsingEntity(const AJoinTableName, ALeftKey, ARightKey: string): IRelationshipBuilder<T>; overload;
  end;

implementation

uses
  System.StrUtils;

constructor TEntityMap.Create(AEntityType: PTypeInfo);
begin
  FEntityType := AEntityType;
  FProperties := TCollections.CreateDictionaryIgnoreCase<string, TPropertyMap>(True);
  FKeys := TCollections.CreateList<string>;
  FQueryFilters := TCollections.CreateList<IExpression>;
  FIsSoftDelete := False;
  FSoftDeleteProp := '';
  FSoftDeleteDeletedValue := 1;  // Default (1 = Deleted)
  FSoftDeleteNotDeletedValue := 0; // Default (0 = Not Deleted)
  FDiscriminatorColumn := '';
  FDiscriminatorValue := Null;

  DiscoverAttributes;
end;

function TypeInfoToFieldType(ATypeInfo: PTypeInfo): TFieldType;
begin
  if ATypeInfo = nil then Exit(ftUnknown);
  case ATypeInfo.Kind of
    tkInteger: Result := ftInteger;
    tkChar, tkWChar, tkLString, tkWString, tkUString: Result := ftWideString;
    tkFloat:
    begin
      if (ATypeInfo = TypeInfo(TDateTime)) or SameText(string(ATypeInfo.Name), 'TDateTime') then Result := ftDateTime
      else if (ATypeInfo = TypeInfo(TDate)) or SameText(string(ATypeInfo.Name), 'TDate') then Result := ftDate
      else if (ATypeInfo = TypeInfo(TTime)) or SameText(string(ATypeInfo.Name), 'TTime') then Result := ftTime
      else if (ATypeInfo = TypeInfo(Currency)) or SameText(string(ATypeInfo.Name), 'Currency') then Result := ftCurrency
      else Result := ftFloat;
    end;
    tkInt64: Result := ftLargeint;
    tkEnumeration:
    begin
      if (ATypeInfo = TypeInfo(Boolean)) or SameText(string(ATypeInfo.Name), 'Boolean') then Result := ftBoolean
      else Result := ftInteger;
    end;
  else
    Result := ftUnknown;
  end;
end;

procedure TEntityMap.ProcessAttribute(APropMap: TPropertyMap; AAttr: TCustomAttribute);
begin
  if (AAttr is ColumnAttribute) or (AAttr is PrimaryKeyAttribute) or (AAttr is AutoIncAttribute) or 
      (AAttr is ForeignKeyAttribute) or (AAttr is NotMappedAttribute) or (AAttr is FieldAttribute) or
      (AAttr is RequiredAttribute) or (AAttr is MaxLengthAttribute) or (AAttr is MinLengthAttribute) or (AAttr is PrecisionAttribute) or
      (AAttr is TypeConverterAttribute) or (AAttr is HasManyAttribute) or (AAttr is BelongsToAttribute) or
      (AAttr is HasOneAttribute) or (AAttr is InversePropertyAttribute) or (AAttr is DeleteBehaviorAttribute) or
      (AAttr is ManyToManyAttribute) or (AAttr is VersionAttribute) or (AAttr is CreatedAtAttribute) or
      (AAttr is UpdatedAtAttribute) or (AAttr is JsonColumnAttribute) or (AAttr is DbTypeAttribute) then
  begin
    if AAttr is ColumnAttribute then APropMap.ColumnName := ColumnAttribute(AAttr).Name;
    if AAttr is FieldAttribute then 
    begin
      if FieldAttribute(AAttr).Name <> '' then
        APropMap.FieldName := FieldAttribute(AAttr).Name
      else
        APropMap.FieldName := TReflection.NormalizeFieldName(APropMap.PropertyName);
    end;
    if AAttr is PrimaryKeyAttribute then 
    begin
      APropMap.IsPK := True;
      if not FKeys.Contains(APropMap.PropertyName) then FKeys.Add(APropMap.PropertyName);
    end;
    if AAttr is AutoIncAttribute then APropMap.IsAutoInc := True;
    if AAttr is NotMappedAttribute then APropMap.IsIgnored := True;
    if AAttr is ForeignKeyAttribute then APropMap.ForeignKeyColumn := ForeignKeyAttribute(AAttr).ColumnName;
    if AAttr is DbTypeAttribute then APropMap.DataType := DbTypeAttribute(AAttr).DataType;
    if AAttr is TypeConverterAttribute then APropMap.ConverterClass := TypeConverterAttribute(AAttr).ConverterClass;

    if AAttr is RequiredAttribute then APropMap.IsRequired := True;
    if AAttr is MaxLengthAttribute then APropMap.MaxLength := MaxLengthAttribute(AAttr).Length;
    if AAttr is MinLengthAttribute then APropMap.MinLength := MinLengthAttribute(AAttr).Length;
    if AAttr is PrecisionAttribute then
    begin
      APropMap.Precision := PrecisionAttribute(AAttr).Precision;
      APropMap.Scale := PrecisionAttribute(AAttr).Scale;
    end;

    // Relationships
    if AAttr is HasManyAttribute then 
    begin
      APropMap.Relationship := rtOneToMany;
      APropMap.IsNavigation := True;
    end;
    if AAttr is BelongsToAttribute then 
    begin
      APropMap.Relationship := rtManyToOne;
      APropMap.IsNavigation := True;
    end;
    if AAttr is HasOneAttribute then 
    begin
      APropMap.Relationship := rtOneToOne;
      APropMap.IsNavigation := True;
    end;
    if AAttr is ManyToManyAttribute then 
    begin
      APropMap.Relationship := rtManyToMany;
      APropMap.IsNavigation := True;
      APropMap.JoinTableName := ManyToManyAttribute(AAttr).JoinTableName;
      APropMap.LeftKeyColumn := ManyToManyAttribute(AAttr).LeftKeyColumn;
      APropMap.RightKeyColumn := ManyToManyAttribute(AAttr).RightKeyColumn;
    end;
    if AAttr is InversePropertyAttribute then 
    begin
      APropMap.InverseProperty := InversePropertyAttribute(AAttr).Name;
      APropMap.IsNavigation := True;
    end;
    if AAttr is DeleteBehaviorAttribute then APropMap.DeleteBehavior := DeleteBehaviorAttribute(AAttr).Behavior;
    
    // Optimistic Concurrency
    if AAttr is VersionAttribute then APropMap.IsVersion := True;
    
    // Audit Timestamps
    if AAttr is CreatedAtAttribute then APropMap.IsCreatedAt := True;
    if AAttr is UpdatedAtAttribute then APropMap.IsUpdatedAt := True;
    
    // JSON Column
    if AAttr is JsonColumnAttribute then
    begin
      APropMap.IsJsonColumn := True;
      APropMap.UseJsonB := JsonColumnAttribute(AAttr).UseJsonB;
    end;
  end;
end;

procedure TEntityMap.DiscoverAttributes;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Attr: TCustomAttribute;
  Prop: TRttiProperty;
  PropMap: TPropertyMap;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(FEntityType);
    if Typ = nil then Exit;

    for Attr in Typ.GetAttributes do
    begin
      if Attr is TableAttribute then FTableName := TableAttribute(Attr).Name;
      if Attr is SoftDeleteAttribute then
      begin
        FIsSoftDelete := True;
        FSoftDeleteProp := SoftDeleteAttribute(Attr).ColumnName;
        FSoftDeleteDeletedValue := SoftDeleteAttribute(Attr).DeletedValue;
        FSoftDeleteNotDeletedValue := SoftDeleteAttribute(Attr).NotDeletedValue;
      end;
      if Attr is InheritanceAttribute then FInheritanceStrategy := InheritanceAttribute(Attr).Strategy;
      if Attr is DiscriminatorColumnAttribute then FDiscriminatorColumn := DiscriminatorColumnAttribute(Attr).Name;
      if Attr is DiscriminatorValueAttribute then FDiscriminatorValue := DiscriminatorValueAttribute(Attr).Value;
    end;
    
    for var Fld in Typ.GetFields do
    begin
      // 1. Smart Properties (detected by TReflection which handles attributes, naming and aliases)
      var Metadata := TReflection.GetMetadata(Fld.FieldType.Handle);
      var IsSmart := Metadata.IsSmartProp or Metadata.IsNullable or Metadata.IsLazy;

      if IsSmart then
      begin
        var FldName := TReflection.NormalizeFieldName(Fld.Name);
          
        PropMap := GetOrAddProperty(FldName);
        PropMap.FieldOffset := -1; // Reset to avoid incorrect null detection for records
        PropMap.FieldValueOffset := -1;
        PropMap.IsLazy := Metadata.IsLazy;

        // Skip fast path for Lazy to force RTTI (which triggers Load)
        if not PropMap.IsLazy then
        begin
          // CRITICAL: We separate NullFlag and MetadataOffset.
          // TEntityDataSet uses FieldOffset to check for NULL via PBoolean (must be boolean).
          // Prototype usage needs FInfo, which is an Interface (must NOT be treated as null flag).
          if Metadata.HasValueField <> nil then
          begin
            if Metadata.HasValueField.FieldType.Handle = TypeInfo(Boolean) then
              PropMap.FieldOffset := Fld.Offset + Metadata.HasValueField.Offset
            else if Metadata.HasValueField.FieldType.TypeKind = tkInterface then
              PropMap.MetadataOffset := Fld.Offset + Metadata.HasValueField.Offset;
          end;

          if Metadata.ValueField <> nil then
          begin
            PropMap.FieldValueOffset := Fld.Offset + Metadata.ValueField.Offset;
            PropMap.PropertyType := Metadata.InnerType;
            PropMap.DataType := TypeInfoToFieldType(Metadata.InnerType);
            PropMap.IsEnum := (Metadata.InnerType <> nil) and (Metadata.InnerType.Kind = tkEnumeration) and (Metadata.InnerType <> TypeInfo(Boolean));
          end;
        end;
        
        // Processar atributos do campo
        for Attr in Fld.GetAttributes do
          ProcessAttribute(PropMap, Attr);
      end
      // 2. Campos normais públicos ou com atributos
      else if (Fld.Visibility in [mvPublic, mvPublished]) or (Length(Fld.GetAttributes) > 0) then
      begin
        var FldName := Fld.Name;
        if (FldName.Length > 1) and (FldName[1] = 'F') then
          FldName := FldName.Substring(1);
          
        PropMap := GetOrAddProperty(FldName);
        if PropMap.FieldValueOffset <= 0 then
        begin
           PropMap.FieldValueOffset := Fld.Offset;
           PropMap.PropertyType := Fld.FieldType.Handle;
           PropMap.DataType := TypeInfoToFieldType(Fld.FieldType.Handle);
           PropMap.IsEnum := (Fld.FieldType.TypeKind = tkEnumeration) and (Fld.FieldType.Handle <> TypeInfo(Boolean));
        end;
        
        for Attr in Fld.GetAttributes do
          ProcessAttribute(PropMap, Attr);
      end;
    end;

    for Prop in Typ.GetProperties do
    begin
      PropMap := GetOrAddProperty(Prop.Name);
      
      // Basic type info
      if PropMap.PropertyType = nil then
        PropMap.PropertyType := Prop.PropertyType.Handle;
      if PropMap.DataType = ftUnknown then
        PropMap.DataType := TypeInfoToFieldType(Prop.PropertyType.Handle);

      PropMap.IsEnum := (Prop.PropertyType.TypeKind = tkEnumeration) and (Prop.PropertyType.Handle <> TypeInfo(Boolean));
      PropMap.Prop := Prop; // Cache the RTTI property for fast access
      
      // If the property has a getter or setter method, we should not use direct field access (Fast Path)
      // as it might bypass business logic or calculated values.
      if Prop is TRttiInstanceProperty then
      begin
        var LPropInfo := TRttiInstanceProperty(Prop).PropInfo;
        if Assigned(LPropInfo) then
        begin
          // If GetProc points to a field (high-byte $FF), it's a direct field access property (e.g., 'read FCurrencyVal').
          // In this case, we extract the actual memory offset of the private field to enable the 'Fast Path' performance boost.
          // Note: We MUST NOT overwrite offsets for SmartProps (Nullable/Lazy) already detected in the Fields loop.
          if (not PropMap.IsLazy) and ((NativeInt(LPropInfo.GetProc) and $FF000000) = $FF000000) then
          begin
            if PropMap.FieldValueOffset = 0 then
              PropMap.FieldValueOffset := NativeInt(LPropInfo.GetProc) and $00FFFFFF;
          end
          else
          begin
             // It's a method getter - we cannot use direct memory access (Fast Path).
             PropMap.FieldValueOffset := 0;
          end;

          // Double check SetProc to ensure it's also a field mapping or a method
          if (PropMap.FieldValueOffset <> 0) and (NativeInt(LPropInfo.SetProc) <> 0) and 
             ((NativeInt(LPropInfo.SetProc) and $FF000000) <> $FF000000) then
          begin
             PropMap.FieldValueOffset := 0;
          end;
        end;
      end;

      if Prop.PropertyType.TypeKind in [tkClass, tkInterface] then
      begin
        // Filter out classes that have a registered converter (e.g. TStrings)
        // These should be treated as columns, not navigation properties.
        var LTypeConv := TTypeConverterRegistry.Instance.GetConverter(Prop.PropertyType.Handle);
        
        if LTypeConv = nil then
        begin
          PropMap.IsNavigation := True;
          if Prop.PropertyType.TypeKind = tkInterface then
            PropMap.Relationship := rtOneToMany // Likely IList<T>
          else
            PropMap.Relationship := rtManyToOne; // Likely an entity reference
        end
        else
        begin
            // It's a convertible class (like TStrings). Treat as a normal column.
            PropMap.IsNavigation := False;
            PropMap.Relationship := rtNone;
            PropMap.Converter := LTypeConv;
        end;
      end;

      // Try to find the backing field by convention (FPropName) if offset is not set
      if PropMap.FieldValueOffset <= 0 then
      begin
        var BackingFld := Typ.GetField(TReflection.NormalizeFieldName(Prop.Name));
        if BackingFld <> nil then
        begin
           // If the backing field is a SmartProp, we need to extract the inner offset
           if TReflection.IsSmartProp(BackingFld.FieldType.Handle) then
           begin
             var Meta := TReflection.GetMetadata(BackingFld.FieldType.Handle);
                if Meta.IsSmartProp then
                begin
                  // Detect Lazy
                  if Meta.RttiType.Name.Contains('Lazy<') then
                    PropMap.IsLazy := True;

                  // Lazy types store an interface (ILazy), not the value T directly in the record.
                  // We MUST NOT set FieldValueOffset for them if we want to support direct memory access for T.
                  // Instead, we skip direct offset mapping so the engine falls back to RTTI (which handles ILazy).
                  if not PropMap.IsLazy then
                  begin
                    PropMap.FieldValueOffset := BackingFld.Offset + Meta.ValueField.Offset;

                    // Only use FieldOffset if it's a Boolean (real null flag)
                    if (Meta.HasValueField <> nil) and (Meta.HasValueField.FieldType.Handle = TypeInfo(Boolean)) then
                      PropMap.FieldOffset := BackingFld.Offset + Meta.HasValueField.Offset;
                  end;
                  
                  PropMap.PropertyType := Meta.InnerType;
                  // Update FieldOffset if not yet set
                  if PropMap.FieldValueOffset <= 0 then
                    PropMap.FieldValueOffset := BackingFld.Offset;

                  PropMap.Prop := Prop;
                end;
           end
           else
           begin
             PropMap.FieldValueOffset := BackingFld.Offset;
           end;
        end;
      end;

      for Attr in Prop.GetAttributes do
        ProcessAttribute(PropMap, Attr);
      
      // Resolve Converter (Optimization)
      if PropMap.Converter = nil then
        PropMap.Converter := TTypeConverterRegistry.Instance.GetConverter(Prop.PropertyType.Handle);

      // Automatically mark large types (TStrings, TBytes) as Lazy
      if (PropMap.Converter <> nil) and not PropMap.IsPK and not PropMap.IsNavigation then
      begin
          var LTypeName := string(Prop.PropertyType.Handle.Name);
          if (LTypeName = 'TStrings') or (LTypeName = 'TBytes') then
            PropMap.IsLazy := True;
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

destructor TEntityMap.Destroy;
begin
  FQueryFilters := nil;
  FKeys := nil;
  FProperties := nil;
  inherited;
end;

function TEntityMap.GetOrAddProperty(const APropName: string): TPropertyMap;
begin
  if not FProperties.TryGetValue(APropName, Result) then
  begin
    Result := TPropertyMap.Create(APropName);
    FProperties.Add(APropName, Result);
  end;
end;

{ TPropertyMap }

constructor TPropertyMap.Create(const APropName: string);
begin
  PropertyName := APropName;
  ColumnName := APropName; // Default
  ForeignKeyColumn := '';
  IsPK := False;
  IsAutoInc := False;
  IsRequired := False;
  MaxLength := 0;
  MinLength := 0;
  Precision := 0;
  Scale := 0;
  IsIgnored := False;
  IsNavigation := False;
  Relationship := rtNone;
  InverseProperty := '';
  PrincipalKey := '';
  DeleteBehavior := caNoAction;
  JoinTableName := '';
  LeftKeyColumn := '';
  RightKeyColumn := '';
  DataType := ftUnknown;
  ConverterClass := nil;
  Converter := nil;
  // New fields
  IsVersion := False;
  IsCreatedAt := False;
  IsUpdatedAt := False;
  FieldOffset := -1; 
  MetadataOffset := -1;
  FieldValueOffset := -1;
  PropertyType := nil;
  IsShadow := False;
  IsJsonColumn := False;
  UseJsonB := True; // Default for PostgreSQL
  IsEnum := False;
end;

{ TRelationshipBuilder<T> }

constructor TRelationshipBuilder<T>.Create(AMap: TEntityMap; APropMap: TPropertyMap);
begin
  inherited Create;
  FMap := AMap;
  FPropMap := APropMap;
end;

function TRelationshipBuilder<T>.WithOne(const AInversePropName: string): IRelationshipBuilder<T>;
begin
  FPropMap.InverseProperty := AInversePropName;
  Result := Self;
end;

function TRelationshipBuilder<T>.WithMany(const AInversePropName: string): IRelationshipBuilder<T>;
begin
  FPropMap.InverseProperty := AInversePropName;
  Result := Self;
end;

function TRelationshipBuilder<T>.HasForeignKey(const AFKPropertyName: string): IRelationshipBuilder<T>;
begin
  FPropMap.ForeignKeyColumn := AFKPropertyName;
  Result := Self;
end;

function TRelationshipBuilder<T>.HasPrincipalKey(const AKeyPropertyName: string): IRelationshipBuilder<T>;
begin
  FPropMap.PrincipalKey := AKeyPropertyName;
  Result := Self;
end;

function TRelationshipBuilder<T>.OnDelete(AAction: TCascadeAction): IRelationshipBuilder<T>;
begin
  FPropMap.DeleteBehavior := AAction;
  Result := Self;
end;

function TRelationshipBuilder<T>.UsingEntity(const AJoinTableName: string): IRelationshipBuilder<T>;
begin
  FPropMap.JoinTableName := AJoinTableName;
  Result := Self;
end;

function TRelationshipBuilder<T>.UsingEntity(const AJoinTableName, ALeftKey, ARightKey: string): IRelationshipBuilder<T>;
begin
  FPropMap.JoinTableName := AJoinTableName;
  FPropMap.LeftKeyColumn := ALeftKey;
  FPropMap.RightKeyColumn := ARightKey;
  Result := Self;
end;

{ TEntityBuilder<T> }

constructor TEntityBuilder<T>.Create(AMap: TEntityMap);
begin
  FMap := AMap;
  FCurrentProp := nil;
end;

function TEntityBuilder<T>.GetCurrentProp: TPropertyMap;
begin
  if FCurrentProp = nil then
    raise Exception.Create('No property selected. Call Property() first.');
  Result := FCurrentProp;
end;

function TEntityBuilder<T>.Table(const AName: string): TEntityBuilder<T>;
begin
  FMap.TableName := AName;
  Result := Self;
end;

function TEntityBuilder<T>.HasKey(const APropertyName: string): TEntityBuilder<T>;
begin
  FMap.Keys.Clear;
  FMap.Keys.Add(APropertyName);
  FMap.GetOrAddProperty(APropertyName).IsPK := True;
  Result := Self;
end;

function TEntityBuilder<T>.HasKey(const APropertyNames: array of string): TEntityBuilder<T>;
var
  Prop: string;
begin
  FMap.Keys.Clear;
  for Prop in APropertyNames do
  begin
    FMap.Keys.Add(Prop);
    FMap.GetOrAddProperty(Prop).IsPK := True;
  end;
  Result := Self;
end;

function TEntityBuilder<T>.HasDiscriminator(const AColumn: string; const AValue: Variant): TEntityBuilder<T>;
begin
  FMap.DiscriminatorColumn := AColumn;
  FMap.DiscriminatorValue := AValue;
  FMap.InheritanceStrategy := TInheritanceStrategy.TablePerHierarchy; // Default to TPH if Discriminator is set
  Result := Self;
end;

function TEntityBuilder<T>.MapInheritance(AStrategy: TInheritanceStrategy): TEntityBuilder<T>;
begin
  FMap.InheritanceStrategy := AStrategy;
  Result := Self;
end;

function TEntityBuilder<T>.Prop(const APropertyName: string): TEntityBuilder<T>;
begin
  FCurrentProp := FMap.GetOrAddProperty(APropertyName);
  Result := Self;
end;

function TEntityBuilder<T>.ShadowProperty(const APropName: string): TEntityBuilder<T>;
begin
  FCurrentProp := FMap.GetOrAddProperty(APropName);
  FCurrentProp.IsShadow := True;
  Result := Self;
end;

function TEntityBuilder<T>.HasProperty(const APropertyName: string): TEntityBuilder<T>;
begin
  Result := Prop(APropertyName);
end;

function TEntityBuilder<T>.Column(const AName: string): TEntityBuilder<T>;
begin
  GetCurrentProp.ColumnName := AName;
  Result := Self;
end;

function TEntityBuilder<T>.HasForeignKey(const AColumnName: string): TEntityBuilder<T>;
begin
  GetCurrentProp.ForeignKeyColumn := AColumnName;
  Result := Self;
end;

function TEntityBuilder<T>.IsRequired(AValue: Boolean): TEntityBuilder<T>;
begin
  GetCurrentProp.IsRequired := AValue;
  Result := Self;
end;

function TEntityBuilder<T>.IsAutoInc(AValue: Boolean): TEntityBuilder<T>;
begin
  GetCurrentProp.IsAutoInc := AValue;
  Result := Self;
end;

function TEntityBuilder<T>.MaxLength(ALength: Integer): TEntityBuilder<T>;
begin
  GetCurrentProp.MaxLength := ALength;
  Result := Self;
end;

function TEntityBuilder<T>.MinLength(ALength: Integer): TEntityBuilder<T>;
begin
  GetCurrentProp.MinLength := ALength;
  Result := Self;
end;

function TEntityBuilder<T>.Precision(APrecision, AScale: Integer): TEntityBuilder<T>;
begin
  GetCurrentProp.Precision := APrecision;
  GetCurrentProp.Scale := AScale;
  Result := Self;
end;

function TEntityBuilder<T>.HasDbType(ADataType: TFieldType): TEntityBuilder<T>;
begin
  GetCurrentProp.DataType := ADataType;
  Result := Self;
end;

function TEntityBuilder<T>.IsJson(AUseJsonB: Boolean): TEntityBuilder<T>;
begin
  GetCurrentProp.IsJsonColumn := True;
  GetCurrentProp.UseJsonB := AUseJsonB;
  Result := Self;
end;

function TEntityBuilder<T>.IsLazy(AValue: Boolean): TEntityBuilder<T>;
begin
  GetCurrentProp.IsLazy := AValue;
  Result := Self;
end;

function TEntityBuilder<T>.IsVersion(AValue: Boolean): TEntityBuilder<T>;
begin
  GetCurrentProp.IsVersion := AValue;
  Result := Self;
end;

function TEntityBuilder<T>.IsCreatedAt(AValue: Boolean): TEntityBuilder<T>;
begin
  GetCurrentProp.IsCreatedAt := AValue;
  Result := Self;
end;

function TEntityBuilder<T>.IsUpdatedAt(AValue: Boolean): TEntityBuilder<T>;
begin
  GetCurrentProp.IsUpdatedAt := AValue;
  Result := Self;
end;

function TEntityBuilder<T>.IsShadow(AValue: Boolean): TEntityBuilder<T>;
begin
  GetCurrentProp.IsShadow := AValue;
  Result := Self;
end;

function TEntityBuilder<T>.Ignore: TEntityBuilder<T>;
begin
  GetCurrentProp.IsIgnored := True;
  Result := Self;
end;

function TEntityBuilder<T>.HasMany(const APropertyName: string): IRelationshipBuilder<T>;
var
  LProp: TPropertyMap;
begin
  LProp := FMap.GetOrAddProperty(APropertyName);
  LProp.IsNavigation := True;
  LProp.Relationship := rtOneToMany;
  Result := TRelationshipBuilder<T>.Create(FMap, LProp);
end;

function TEntityBuilder<T>.HasOne(const APropertyName: string): IRelationshipBuilder<T>;
var
  LProp: TPropertyMap;
begin
  LProp := FMap.GetOrAddProperty(APropertyName);
  LProp.IsNavigation := True;
  LProp.Relationship := rtOneToOne;
  Result := TRelationshipBuilder<T>.Create(FMap, LProp);
end;

function TEntityBuilder<T>.BelongsTo(const APropertyName: string): IRelationshipBuilder<T>;
var
  LProp: TPropertyMap;
begin
  LProp := FMap.GetOrAddProperty(APropertyName);
  LProp.IsNavigation := True;
  LProp.Relationship := rtManyToOne;
  Result := TRelationshipBuilder<T>.Create(FMap, LProp);
end;

function TEntityBuilder<T>.HasManyToMany(const APropertyName: string): IRelationshipBuilder<T>;
var
  LProp: TPropertyMap;
begin
  LProp := FMap.GetOrAddProperty(APropertyName);
  LProp.IsNavigation := True;
  LProp.Relationship := rtManyToMany;
  Result := TRelationshipBuilder<T>.Create(FMap, LProp);
end;

function TEntityBuilder<T>.HasSoftDelete(const APropertyName: string): TEntityBuilder<T>;
begin
  Result := HasSoftDelete(APropertyName, True, False);
end;

function TEntityBuilder<T>.HasSoftDelete(const APropertyName: string; const ADeletedValue, ANotDeletedValue: Variant): TEntityBuilder<T>;
begin
  FMap.FIsSoftDelete := True;
  FMap.FSoftDeleteProp := APropertyName;
  FMap.FSoftDeleteDeletedValue := ADeletedValue;
  FMap.FSoftDeleteNotDeletedValue := ANotDeletedValue;
  Result := Self;
end;

function TEntityBuilder<T>.HasQueryFilter(AFilter: IExpression): TEntityBuilder<T>;
begin
  FMap.FQueryFilters.Add(AFilter);
  Result := Self;
end;


{ TEntityTypeBuilder<T> }

constructor TEntityTypeBuilder<T>.Create(AMap: TEntityMap);
begin
  FMap := AMap;
end;

function TEntityTypeBuilder<T>.ToTable(const AName: string): IEntityTypeBuilder<T>;
begin
  FMap.TableName := AName;
  Result := Self;
end;

function TEntityTypeBuilder<T>.HasKey(const APropertyName: string): IEntityTypeBuilder<T>;
begin
  FMap.Keys.Clear;
  FMap.Keys.Add(APropertyName);
  
  // Mark property as PK
  FMap.GetOrAddProperty(APropertyName).IsPK := True;
  Result := Self;
end;

function TEntityTypeBuilder<T>.HasKey(const APropertyNames: array of string): IEntityTypeBuilder<T>;
var
  Prop: string;
begin
  FMap.Keys.Clear;
  for Prop in APropertyNames do
  begin
    FMap.Keys.Add(Prop);
    FMap.GetOrAddProperty(Prop).IsPK := True;
  end;
  Result := Self;
end;

function TEntityTypeBuilder<T>.HasDiscriminator(const AColumn: string; const AValue: Variant): IEntityTypeBuilder<T>;
begin
  FMap.DiscriminatorColumn := AColumn;
  FMap.DiscriminatorValue := AValue;
  FMap.InheritanceStrategy := TInheritanceStrategy.TablePerHierarchy; // Default to TPH
  Result := Self;
end;

function TEntityTypeBuilder<T>.MapInheritance(AStrategy: TInheritanceStrategy): IEntityTypeBuilder<T>;
begin
  FMap.InheritanceStrategy := AStrategy;
  Result := Self;
end;

function TEntityTypeBuilder<T>.Prop(const APropertyName: string): IPropertyBuilder<T>;
begin
  Result := TPropertyBuilder<T>.Create(FMap.GetOrAddProperty(APropertyName));
end;

function TEntityTypeBuilder<T>.Prop(const AProp: IPropInfo): IPropertyBuilder<T>;
begin
  Result := Prop(AProp.PropertyName);
end;

function TEntityTypeBuilder<T>.ShadowProperty(const APropName: string): IPropertyBuilder<T>;
begin
  Result := Prop(APropName).IsShadow(True);
end;

function TEntityTypeBuilder<T>.Ignore(const APropertyName: string): IEntityTypeBuilder<T>;
begin
  FMap.GetOrAddProperty(APropertyName).IsIgnored := True;
  Result := Self;
end;

function TEntityTypeBuilder<T>.HasMany(const APropertyName: string): IRelationshipBuilder<T>;
var
  LProp: TPropertyMap;
begin
  LProp := FMap.GetOrAddProperty(APropertyName);
  LProp.IsNavigation := True;
  LProp.Relationship := rtOneToMany;
  Result := TRelationshipBuilder<T>.Create(FMap, LProp);
end;

function TEntityTypeBuilder<T>.HasOne(const APropertyName: string): IRelationshipBuilder<T>;
var
  LProp: TPropertyMap;
begin
  LProp := FMap.GetOrAddProperty(APropertyName);
  LProp.IsNavigation := True;
  LProp.Relationship := rtOneToOne;
  Result := TRelationshipBuilder<T>.Create(FMap, LProp);
end;

function TEntityTypeBuilder<T>.BelongsTo(const APropertyName: string): IRelationshipBuilder<T>;
var
  LProp: TPropertyMap;
begin
  LProp := FMap.GetOrAddProperty(APropertyName);
  LProp.IsNavigation := True;
  LProp.Relationship := rtManyToOne;
  Result := TRelationshipBuilder<T>.Create(FMap, LProp);
end;

function TEntityTypeBuilder<T>.HasManyToMany(const APropertyName: string): IRelationshipBuilder<T>;
var
  LProp: TPropertyMap;
begin
  LProp := FMap.GetOrAddProperty(APropertyName);
  LProp.IsNavigation := True;
  LProp.Relationship := rtManyToMany;
  Result := TRelationshipBuilder<T>.Create(FMap, LProp);
end;

function TEntityTypeBuilder<T>.HasSoftDelete(const APropertyName: string): IEntityTypeBuilder<T>;
begin
  Result := HasSoftDelete(APropertyName, True, False);
end;

function TEntityTypeBuilder<T>.HasSoftDelete(const APropertyName: string; const ADeletedValue, ANotDeletedValue: Variant): IEntityTypeBuilder<T>;
begin
  FMap.FIsSoftDelete := True;
  FMap.FSoftDeleteProp := APropertyName;
  FMap.FSoftDeleteDeletedValue := ADeletedValue;
  FMap.FSoftDeleteNotDeletedValue := ANotDeletedValue;
  Result := Self;
end;

function TEntityTypeBuilder<T>.HasQueryFilter(AFilter: IExpression): IEntityTypeBuilder<T>;
begin
  FMap.FQueryFilters.Add(AFilter);
  Result := Self;
end;

function TEntityBuilder<T>.HasConverter(AConverterClass: TClass): TEntityBuilder<T>;
begin
  GetCurrentProp.ConverterClass := AConverterClass;
  Result := Self;
end;

{ TPropertyBuilder<T> }

constructor TPropertyBuilder<T>.Create(APropMap: TPropertyMap);
begin
  FPropMap := APropMap;
end;

function TPropertyBuilder<T>.HasColumnName(const AName: string): IPropertyBuilder<T>;
begin
  FPropMap.ColumnName := AName;
  Result := Self;
end;

function TPropertyBuilder<T>.HasFieldName(const AName: string): IPropertyBuilder<T>;
begin
  FPropMap.FieldName := AName;
  Result := Self;
end;

function TPropertyBuilder<T>.UseField: IPropertyBuilder<T>;
begin
  FPropMap.FieldName := TReflection.NormalizeFieldName(FPropMap.PropertyName);
  Result := Self;
end;

function TPropertyBuilder<T>.IsRequired(AValue: Boolean): IPropertyBuilder<T>;
begin
  FPropMap.IsRequired := AValue;
  Result := Self;
end;

function TPropertyBuilder<T>.IsAutoInc(AValue: Boolean): IPropertyBuilder<T>;
begin
  FPropMap.IsAutoInc := AValue;
  Result := Self;
end;

function TPropertyBuilder<T>.HasMaxLength(ALength: Integer): IPropertyBuilder<T>;
begin
  FPropMap.MaxLength := ALength;
  Result := Self;
end;

function TPropertyBuilder<T>.HasMinLength(ALength: Integer): IPropertyBuilder<T>;
begin
  FPropMap.MinLength := ALength;
  Result := Self;
end;

function TPropertyBuilder<T>.HasPrecision(APrecision, AScale: Integer): IPropertyBuilder<T>;
begin
  FPropMap.Precision := APrecision;
  FPropMap.Scale := AScale;
  Result := Self;
end;

function TPropertyBuilder<T>.HasDbType(ADataType: TFieldType): IPropertyBuilder<T>;
begin
  FPropMap.DataType := ADataType;
  Result := Self;
end;

function TPropertyBuilder<T>.IsLazy(AValue: Boolean): IPropertyBuilder<T>;
begin
  FPropMap.IsLazy := AValue;
  Result := Self;
end;

function TPropertyBuilder<T>.IsVersion(AValue: Boolean): IPropertyBuilder<T>;
begin
  FPropMap.IsVersion := AValue;
  Result := Self;
end;



function TPropertyBuilder<T>.IsCreatedAt(AValue: Boolean): IPropertyBuilder<T>;
begin
  FPropMap.IsCreatedAt := AValue;
  Result := Self;
end;

function TPropertyBuilder<T>.IsUpdatedAt(AValue: Boolean): IPropertyBuilder<T>;
begin
  FPropMap.IsUpdatedAt := AValue;
  Result := Self;
end;

function TPropertyBuilder<T>.IsShadow(AValue: Boolean): IPropertyBuilder<T>;
begin
  FPropMap.IsShadow := AValue;
  Result := Self;
end;

function TPropertyBuilder<T>.HasConverter(AConverterClass: TClass): IPropertyBuilder<T>;
begin
  FPropMap.ConverterClass := AConverterClass;
  Result := Self;
end;

{ TModelBuilder }

constructor TModelBuilder.Create;
begin
  FMaps := TCollections.CreateDictionary<PTypeInfo, TEntityMap>(True);
  FDiscoveryNames := TCollections.CreateDictionary<PTypeInfo, string>;
  FFactories := TCollections.CreateDictionary<PTypeInfo, IDynamicDbSetFactory>;
end;

destructor TModelBuilder.Destroy;
begin
  FMaps := nil;
  FDiscoveryNames := nil;
  FFactories := nil;
  inherited;
end;

procedure TModelBuilder.Clear;
begin
  if Assigned(FMaps) then
    FMaps.Clear;
  if Assigned(FDiscoveryNames) then
    FDiscoveryNames.Clear;
  if Assigned(FFactories) then
    FFactories.Clear;
end;

procedure TModelBuilder.RegisterFactory(AType: PTypeInfo; const AFactory: IDynamicDbSetFactory);
begin
  if not FFactories.ContainsKey(AType) then
    FFactories.Add(AType, AFactory);
end;

function TModelBuilder.GetFactory(AType: PTypeInfo): IDynamicDbSetFactory;
begin
  if not FFactories.TryGetValue(AType, Result) then
  begin
    // Fallback to global instance if this is a local/context builder (common in tests)
    if (Self <> FInstance) and (FInstance <> nil) then
      Result := FInstance.GetFactory(AType)
    else
      Result := nil;
  end;
end;

class constructor TModelBuilder.Create;
begin
  FInstance := TModelBuilder.Create;
end;

class destructor TModelBuilder.Destroy;
begin
  FreeAndNil(FInstance);
end;

procedure TModelBuilder.RegisterDiscoveryName(AType: PTypeInfo; const AName: string);
begin
  FDiscoveryNames.AddOrSetValue(AType, AName);
end;

function TModelBuilder.GetDiscoveryName(AType: PTypeInfo): string;
begin
  if not FDiscoveryNames.TryGetValue(AType, Result) then
    Result := '';
end;

procedure TModelBuilder.ApplyConfiguration<T>(AConfig: IEntityTypeConfiguration<T>);
var
  Map: TEntityMap;
  Builder: IEntityTypeBuilder<T>;
begin
  if not FMaps.TryGetValue(TypeInfo(T), Map) then
  begin
    Map := TEntityMap.Create(TypeInfo(T));
    FMaps.Add(TypeInfo(T), Map);
  end;
  
  Builder := TEntityTypeBuilder<T>.Create(Map);
  AConfig.Configure(Builder);
end;

function TModelBuilder.Entity<T>: TEntityBuilder<T>;
var
  Map: TEntityMap;
begin
  if not FMaps.TryGetValue(TypeInfo(T), Map) then
  begin
    Map := TEntityMap.Create(TypeInfo(T));
    FMaps.Add(TypeInfo(T), Map);
  end;
  
  Result := TEntityBuilder<T>.Create(Map);
end;

function TModelBuilder.GetMap(AType: PTypeInfo): TEntityMap;
begin
  if not FMaps.TryGetValue(AType, Result) then
  begin
    // Auto-Discovery: Create and cache the map if it doesn't exist
    Result := TEntityMap.Create(AType);
    FMaps.Add(AType, Result);
  end;
end;

function TModelBuilder.HasMap(AType: PTypeInfo): Boolean;
begin
  Result := FMaps.ContainsKey(AType);
end;

function TModelBuilder.GetMaps: TArray<TEntityMap>;
begin
  Result := FMaps.Values;
end;

function TModelBuilder.FindMapByDiscriminator(ABaseType: PTypeInfo; const AValue: Variant): TEntityMap;
var
  Map: TEntityMap;
  Ctx: TRttiContext;
  Typ, BaseTyp: TRttiType;
begin
  Result := nil;
  Ctx := TRttiContext.Create;
  try
    BaseTyp := Ctx.GetType(ABaseType);
    if BaseTyp = nil then Exit;
    
    for Map in FMaps.Values do
    begin
      if (Map.DiscriminatorValue <> Null) and (Map.DiscriminatorValue = AValue) then
      begin
         Typ := Ctx.GetType(Map.EntityType);
         if (Typ <> nil) and (Typ is TRttiInstanceType) and (BaseTyp is TRttiInstanceType) then
         begin
           if TRttiInstanceType(Typ).MetaclassType.InheritsFrom(TRttiInstanceType(BaseTyp).MetaclassType) then
             Exit(Map);
         end;
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

end.


