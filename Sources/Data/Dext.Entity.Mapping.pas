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
  System.Generics.Collections,
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  System.Variants,
  System.Character,
  Data.DB,
  Dext.Entity.Attributes,
  Dext.Entity.TypeConverters,
  Dext.Core.SmartTypes,
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
    ['{50000000-0000-0000-0000-000000000001}']
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
    ['{50000000-0000-0000-0000-000000000004}']
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
    ['{50000000-0000-0000-0000-000000000002}']
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
    ['{50000000-0000-0000-0000-000000000003}']
    procedure Configure(Builder: IEntityTypeBuilder<T>);
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
    FieldOffset: Integer;      // Offset of FInfo
    FieldValueOffset: Integer; // Offset of FValue
    PropertyType: PTypeInfo;   // Type of T in Prop<T>
    // Shadow Property support
    IsShadow: Boolean;
    // JSON Column Support
    IsJsonColumn: Boolean;
    UseJsonB: Boolean; // PostgreSQL JSONB vs JSON
    IsLazy: Boolean; // New: Support for Auto-Proxies / Explicit Lazy
    constructor Create(const APropName: string);
  end;

  TEntityMap = class
  private
    FEntityType: PTypeInfo;
    FTableName: string;
    FProperties: TObjectDictionary<string, TPropertyMap>;
    FKeys: TList<string>;
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
    FQueryFilters: TList<IExpression>;

  public
    constructor Create(AEntityType: PTypeInfo);
    destructor Destroy; override;
    
    property EntityType: PTypeInfo read FEntityType;
    property TableName: string read FTableName write FTableName;
    property Properties: TObjectDictionary<string, TPropertyMap> read FProperties;
    property Keys: TList<string> read FKeys;
    
    property IsSoftDelete: Boolean read FIsSoftDelete;
    property SoftDeleteProp: string read FSoftDeleteProp;
    property SoftDeleteDeletedValue: Variant read FSoftDeleteDeletedValue;
    property SoftDeleteNotDeletedValue: Variant read FSoftDeleteNotDeletedValue;
    property QueryFilters: TList<IExpression> read FQueryFilters;
    
    property InheritanceStrategy: TInheritanceStrategy read FInheritanceStrategy write FInheritanceStrategy;
    property DiscriminatorColumn: string read FDiscriminatorColumn write FDiscriminatorColumn;
    property DiscriminatorValue: Variant read FDiscriminatorValue write FDiscriminatorValue;

    procedure DiscoverAttributes;
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
    FMaps: TObjectDictionary<PTypeInfo, TEntityMap>;
    FDiscoveryNames: TDictionary<PTypeInfo, string>;
    class constructor Create;
    class destructor Destroy;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure ApplyConfiguration<T: class>(AConfig: IEntityTypeConfiguration<T>);
    function Entity<T: class>: TEntityBuilder<T>;
    
    function GetMap(AType: PTypeInfo): TEntityMap;
    function HasMap(AType: PTypeInfo): Boolean;
    function GetMaps: TEnumerable<TEntityMap>;
    
    procedure RegisterDiscoveryName(AType: PTypeInfo; const AName: string);
    function GetDiscoveryName(AType: PTypeInfo): string;
    
    function FindMapByDiscriminator(ABaseType: PTypeInfo; const AValue: Variant): TEntityMap;
    procedure Clear;
    
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

{ TEntityMap }

constructor TEntityMap.Create(AEntityType: PTypeInfo);
begin
  FEntityType := AEntityType;
  FProperties := TObjectDictionary<string, TPropertyMap>.Create([doOwnsValues]);
  FKeys := TList<string>.Create;
  FQueryFilters := TList<IExpression>.Create;
  FIsSoftDelete := False;
  FSoftDeleteProp := '';
  FSoftDeleteDeletedValue := 1;  // Default (1 = Deleted)
  FSoftDeleteNotDeletedValue := 0; // Default (0 = Not Deleted)
  FDiscriminatorColumn := '';
  FDiscriminatorValue := Null;

  DiscoverAttributes;
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
        if Fld.FieldType.Name.StartsWith('Prop<') then
        begin
           var FldName := Fld.Name;
           if (FldName.Length > 1) and (FldName[1] = 'F') and FldName[2].IsUpper then
             FldName := FldName.Substring(1);
           
           PropMap := GetOrAddProperty(FldName);
           
           PropMap.FieldOffset := -1;
           PropMap.FieldValueOffset := -1;

           for var InnerFld in Fld.FieldType.GetFields do
           begin
             if SameText(InnerFld.Name, 'FInfo') then
               PropMap.FieldOffset := Fld.Offset + InnerFld.Offset
             else if SameText(InnerFld.Name, 'FValue') then
             begin
               PropMap.FieldValueOffset := Fld.Offset + InnerFld.Offset;
               PropMap.PropertyType := InnerFld.FieldType.Handle;
             end;
           end;
        end;
    end;

    for Prop in Typ.GetProperties do
    begin
      PropMap := nil;
      
      if Prop.PropertyType.TypeKind in [tkClass, tkInterface] then
      begin
        // Filter out classes that have a registered converter (e.g. TStrings)
        // These should be treated as columns, not navigation properties.
        var LConverter := TTypeConverterRegistry.Instance.GetConverter(Prop.PropertyType.Handle);
        
        if LConverter = nil then
        begin
          PropMap := GetOrAddProperty(Prop.Name);
          PropMap.IsNavigation := True;
          if Prop.PropertyType.TypeKind = tkInterface then
            PropMap.Relationship := rtOneToMany // Likely IList<T>
          else
            PropMap.Relationship := rtManyToOne; // Likely an entity reference
        end
        else
        begin
           // It's a convertible class (like TStrings). Treat as a normal column but enable default lazy load for large types.
           PropMap := GetOrAddProperty(Prop.Name);
           PropMap.IsNavigation := False;
           PropMap.Relationship := rtNone;
           PropMap.Converter := LConverter;
        end;
      end;

      for Attr in Prop.GetAttributes do
      begin
        if (Attr is ColumnAttribute) or (Attr is PrimaryKeyAttribute) or (Attr is AutoIncAttribute) or 
            (Attr is ForeignKeyAttribute) or (Attr is NotMappedAttribute) or (Attr is FieldAttribute) or
            (Attr is RequiredAttribute) or (Attr is MaxLengthAttribute) or (Attr is MinLengthAttribute) or (Attr is PrecisionAttribute) or
            (Attr is TypeConverterAttribute) or (Attr is HasManyAttribute) or (Attr is BelongsToAttribute) or
            (Attr is HasOneAttribute) or (Attr is InversePropertyAttribute) or (Attr is DeleteBehaviorAttribute) or
            (Attr is ManyToManyAttribute) or (Attr is VersionAttribute) or (Attr is CreatedAtAttribute) or
            (Attr is UpdatedAtAttribute) or (Attr is JsonColumnAttribute) or (Attr is DbTypeAttribute) then
        begin
          if PropMap = nil then PropMap := GetOrAddProperty(Prop.Name);
          
          if Attr is ColumnAttribute then PropMap.ColumnName := ColumnAttribute(Attr).Name;
          if Attr is FieldAttribute then 
          begin
            if FieldAttribute(Attr).Name <> '' then
              PropMap.FieldName := FieldAttribute(Attr).Name
            else
              PropMap.FieldName := 'F' + Prop.Name;
          end;
          if Attr is PrimaryKeyAttribute then 
          begin
            PropMap.IsPK := True;
            if not FKeys.Contains(Prop.Name) then FKeys.Add(Prop.Name);
          end;
          if Attr is AutoIncAttribute then PropMap.IsAutoInc := True;
          if Attr is NotMappedAttribute then PropMap.IsIgnored := True;
          if Attr is ForeignKeyAttribute then PropMap.ForeignKeyColumn := ForeignKeyAttribute(Attr).ColumnName;
          if Attr is DbTypeAttribute then PropMap.DataType := DbTypeAttribute(Attr).DataType;
          if Attr is TypeConverterAttribute then PropMap.ConverterClass := TypeConverterAttribute(Attr).ConverterClass;

          if Attr is RequiredAttribute then PropMap.IsRequired := True;
          if Attr is MaxLengthAttribute then PropMap.MaxLength := MaxLengthAttribute(Attr).Length;
          if Attr is MinLengthAttribute then PropMap.MinLength := MinLengthAttribute(Attr).Length;
          if Attr is PrecisionAttribute then
          begin
            PropMap.Precision := PrecisionAttribute(Attr).Precision;
            PropMap.Scale := PrecisionAttribute(Attr).Scale;
          end;

          // Relationships
          if Attr is HasManyAttribute then 
          begin
            PropMap.Relationship := rtOneToMany;
            PropMap.IsNavigation := True;
          end;
          if Attr is BelongsToAttribute then 
          begin
            PropMap.Relationship := rtManyToOne;
            PropMap.IsNavigation := True;
          end;
          if Attr is HasOneAttribute then 
          begin
            PropMap.Relationship := rtOneToOne;
            PropMap.IsNavigation := True;
          end;
          if Attr is ManyToManyAttribute then 
          begin
            PropMap.Relationship := rtManyToMany;
            PropMap.IsNavigation := True;
            PropMap.JoinTableName := ManyToManyAttribute(Attr).JoinTableName;
            PropMap.LeftKeyColumn := ManyToManyAttribute(Attr).LeftKeyColumn;
            PropMap.RightKeyColumn := ManyToManyAttribute(Attr).RightKeyColumn;
          end;
          if Attr is InversePropertyAttribute then 
          begin
            PropMap.InverseProperty := InversePropertyAttribute(Attr).Name;
            PropMap.IsNavigation := True;
          end;
          if Attr is DeleteBehaviorAttribute then PropMap.DeleteBehavior := DeleteBehaviorAttribute(Attr).Behavior;
          
          // Optimistic Concurrency
          if Attr is VersionAttribute then PropMap.IsVersion := True;
          
          // Audit Timestamps
          if Attr is CreatedAtAttribute then PropMap.IsCreatedAt := True;
          if Attr is UpdatedAtAttribute then PropMap.IsUpdatedAt := True;
          
          // JSON Column
          if Attr is JsonColumnAttribute then
          begin
            PropMap.IsJsonColumn := True;
            PropMap.UseJsonB := JsonColumnAttribute(Attr).UseJsonB;
          end;
        end;
      end;
      
      // Resolve Converter (Optimization)
      // Even if no attributes, we might want to resolve converter for standard types (like TDateTime or Enums)
      if PropMap = nil then PropMap := GetOrAddProperty(Prop.Name);
      
      if PropMap <> nil then
      begin
        // If a specific converter class is defined (fluent or attribute), use it
        if (PropMap.ConverterClass <> nil) then
        begin
            // We need to instantiate it. For now, assume parameterless constructor or standard pattern.
            // TValueConverterRegistry helpers often take instances.
            // Ideally, we should cache these instances or use Dependency Injection.
            // Simple instantiation via RTTI for now.
            var RttiCtx := TRttiContext.Create;
            try
              var RType := RttiCtx.GetType(PropMap.ConverterClass);
              if (RType <> nil) and (RType.IsInstance) then
              begin
                  var Method := RType.GetMethod('Create');
                  if Method <> nil then
                    PropMap.Converter := Method.Invoke(RType.AsInstance.MetaclassType, []).AsType<ITypeConverter>
                  else
                  begin
                      // Try basic Create
                      var Obj := PropMap.ConverterClass.Create;
                      if Supports(Obj, ITypeConverter, PropMap.Converter) then
                        // OK
                      else
                        PropMap.Converter := nil; 
                  end;
              end;
            finally
              RttiCtx.Free;
            end;
        end;

        if PropMap.Converter = nil then
          PropMap.Converter := TTypeConverterRegistry.Instance.GetConverter(Prop.PropertyType.Handle);

        // Automatically mark large types (TStrings, TBytes) as Lazy if not explicitly configured otherwise
        // unless they are part of the primary key or explicitly excluded.
        if (PropMap.Converter <> nil) and not PropMap.IsPK and not PropMap.IsNavigation then
        begin
            var LTypeName := string(Prop.PropertyType.Handle.Name);
            if (LTypeName = 'TStrings') or (LTypeName = 'TBytes') then
              PropMap.IsLazy := True;
        end;
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

destructor TEntityMap.Destroy;
begin
  FQueryFilters.Free;
  FKeys.Free;
  FProperties.Free;
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
  FieldValueOffset := -1;
  PropertyType := nil;
  IsShadow := False;
  IsJsonColumn := False;
  UseJsonB := True; // Default for PostgreSQL
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
  FPropMap.FieldName := 'F' + FPropMap.PropertyName;
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
  inherited;
  FMaps := TObjectDictionary<PTypeInfo, TEntityMap>.Create([doOwnsValues]);
  FDiscoveryNames := TDictionary<PTypeInfo, string>.Create;
end;

destructor TModelBuilder.Destroy;
begin
  FDiscoveryNames.Free;
  if Assigned(FMaps) then
  begin
    FMaps.Clear;  // Explicitly clear items first
    FreeAndNil(FMaps);
  end;
  inherited;
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

procedure TModelBuilder.Clear;
begin
  if Assigned(FMaps) then
    FMaps.Clear;
  if Assigned(FDiscoveryNames) then
    FDiscoveryNames.Clear;
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

function TModelBuilder.GetMaps: TEnumerable<TEntityMap>;
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


