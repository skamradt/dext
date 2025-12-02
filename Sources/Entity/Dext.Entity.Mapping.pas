unit Dext.Entity.Mapping;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  System.TypInfo,
  System.Rtti;

type
  // Forward declarations
  IPropertyBuilder<T: class> = interface;

  /// <summary>
  ///   Fluent interface to configure an entity.
  /// </summary>
  IEntityTypeBuilder<T: class> = interface
    ['{50000000-0000-0000-0000-000000000001}']
    function ToTable(const AName: string): IEntityTypeBuilder<T>;
    function HasKey(const APropertyName: string): IEntityTypeBuilder<T>; overload;
    function HasKey(const APropertyNames: array of string): IEntityTypeBuilder<T>; overload;
    function Prop(const APropertyName: string): IPropertyBuilder<T>;
    function Ignore(const APropertyName: string): IEntityTypeBuilder<T>;
  end;

  /// <summary>
  ///   Fluent interface to configure a property.
  /// </summary>
  IPropertyBuilder<T: class> = interface
    ['{50000000-0000-0000-0000-000000000002}']
    function HasColumnName(const AName: string): IPropertyBuilder<T>;
    function IsRequired(AValue: Boolean = True): IPropertyBuilder<T>;
    function IsAutoInc(AValue: Boolean = True): IPropertyBuilder<T>;
    function HasMaxLength(ALength: Integer): IPropertyBuilder<T>;
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
    IsPK: Boolean;
    IsAutoInc: Boolean;
    IsRequired: Boolean;
    MaxLength: Integer;
    IsIgnored: Boolean;
    constructor Create(const APropName: string);
  end;

  TEntityMap = class
  private
    FEntityType: PTypeInfo;
    FTableName: string;
    FProperties: TObjectDictionary<string, TPropertyMap>;
    FKeys: TList<string>;
  public
    constructor Create(AEntityType: PTypeInfo);
    destructor Destroy; override;
    
    property EntityType: PTypeInfo read FEntityType;
    property TableName: string read FTableName write FTableName;
    property Properties: TObjectDictionary<string, TPropertyMap> read FProperties;
    property Keys: TList<string> read FKeys;
    
    function GetOrAddProperty(const APropName: string): TPropertyMap;
  end;

  // ---------------------------------------------------------------------------
  // Concrete Builders
  // ---------------------------------------------------------------------------

  TEntityTypeBuilder<T: class> = class(TInterfacedObject, IEntityTypeBuilder<T>)
  private
    FMap: TEntityMap;
  public
    constructor Create(AMap: TEntityMap);
    function ToTable(const AName: string): IEntityTypeBuilder<T>;
    function HasKey(const APropertyName: string): IEntityTypeBuilder<T>; overload;
    function HasKey(const APropertyNames: array of string): IEntityTypeBuilder<T>; overload;
    function Prop(const APropertyName: string): IPropertyBuilder<T>;
    function Ignore(const APropertyName: string): IEntityTypeBuilder<T>;
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
    FMaps: TObjectDictionary<PTypeInfo, TEntityMap>;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure ApplyConfiguration<T: class>(const AConfig: IEntityTypeConfiguration<T>);
    
    function GetMap(AType: PTypeInfo): TEntityMap;
    function HasMap(AType: PTypeInfo): Boolean;
  end;

implementation

{ TEntityMap }

constructor TEntityMap.Create(AEntityType: PTypeInfo);
begin
  FEntityType := AEntityType;
  FProperties := TObjectDictionary<string, TPropertyMap>.Create([doOwnsValues]);
  FKeys := TList<string>.Create;
end;

destructor TEntityMap.Destroy;
begin
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
  IsPK := False;
  IsAutoInc := False;
  IsRequired := False;
  MaxLength := 0;
  IsIgnored := False;
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

function TEntityTypeBuilder<T>.Prop(const APropertyName: string): IPropertyBuilder<T>;
begin
  Result := TPropertyBuilder<T>.Create(FMap.GetOrAddProperty(APropertyName));
end;

function TEntityTypeBuilder<T>.Ignore(const APropertyName: string): IEntityTypeBuilder<T>;
begin
  FMap.GetOrAddProperty(APropertyName).IsIgnored := True;
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

{ TModelBuilder }

constructor TModelBuilder.Create;
begin
  FMaps := TObjectDictionary<PTypeInfo, TEntityMap>.Create([doOwnsValues]);
end;

destructor TModelBuilder.Destroy;
begin
  FMaps.Free;
  inherited;
end;

procedure TModelBuilder.ApplyConfiguration<T>(const AConfig: IEntityTypeConfiguration<T>);
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

function TModelBuilder.GetMap(AType: PTypeInfo): TEntityMap;
begin
  FMaps.TryGetValue(AType, Result);
end;

function TModelBuilder.HasMap(AType: PTypeInfo): Boolean;
begin
  Result := FMaps.ContainsKey(AType);
end;

end.
