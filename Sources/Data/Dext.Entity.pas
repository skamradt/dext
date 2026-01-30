unit Dext.Entity;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  FireDAC.Comp.Client,
  System.SysUtils,
  System.Classes,
  // Dext,
  Dext.Types.Lazy,
  Dext.DI.Interfaces,
  Dext.DI.Core,
  Dext.Configuration.Interfaces,
  // {BEGIN_DEXT_USES}
  // Generated Uses
  Dext.Entity.Attributes,
  Dext.Entity.Cache,
  Dext.Entity.Context,
  Dext.Entity.Core,
  Dext.Entity.DbSet,
  Dext.Entity.Dialects,
  Dext.Entity.Drivers.FireDAC.Manager,
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.Drivers.FireDAC.Phys,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Grouping,
  Dext.Entity.Joining,
  Dext.Entity.LazyLoading,
  Dext.Entity.Mapping,
  Dext.Entity.Migrations.Builder,
  Dext.Entity.Migrations.Differ,
  Dext.Entity.Migrations.Extractor,
  Dext.Entity.Migrations.Generator,
  Dext.Entity.Migrations.Json,
  Dext.Entity.Migrations.Model,
  Dext.Entity.Migrations.Operations,
  Dext.Entity.Migrations,
  Dext.Entity.Migrations.Runner,
  Dext.Entity.Migrations.Serializers.Json,
  Dext.Entity.Naming,
  Dext.Entity.Prototype,
  Dext.Entity.Query,
  Dext.Entity.Scaffolding,
  Dext.Entity.Setup,
  Dext.Entity.Tenancy,
  Dext.Entity.TypeConverters,
  Dext.Entity.TypeSystem,
  Dext.Specifications.SQL.Generator,
  Dext.Validation
  // {END_DEXT_USES}
  ;

type
  // External Aliases
  TFDConnection = FireDAC.Comp.Client.TFDConnection;

  // {BEGIN_DEXT_ALIASES}
  // Generated Aliases

  // Dext.Entity.Attributes
  TInheritanceStrategy = Dext.Entity.Attributes.TInheritanceStrategy;
  TableAttribute = Dext.Entity.Attributes.TableAttribute;
  ColumnAttribute = Dext.Entity.Attributes.ColumnAttribute;
  PKAttribute = Dext.Entity.Attributes.PKAttribute;
  AutoIncAttribute = Dext.Entity.Attributes.AutoIncAttribute;
  RequiredAttribute = Dext.Entity.Attributes.RequiredAttribute;
  MaxLengthAttribute = Dext.Entity.Attributes.MaxLengthAttribute;
  PrecisionAttribute = Dext.Entity.Attributes.PrecisionAttribute;
  NotMappedAttribute = Dext.Entity.Attributes.NotMappedAttribute;
  FieldAttribute = Dext.Entity.Attributes.FieldAttribute;
  JsonColumnAttribute = Dext.Entity.Attributes.JsonColumnAttribute;
  VersionAttribute = Dext.Entity.Attributes.VersionAttribute;
  SoftDeleteAttribute = Dext.Entity.Attributes.SoftDeleteAttribute;
  TCascadeAction = Dext.Entity.Attributes.TCascadeAction;
  ForeignKeyAttribute = Dext.Entity.Attributes.ForeignKeyAttribute;
  InheritanceAttribute = Dext.Entity.Attributes.InheritanceAttribute;
  DiscriminatorColumnAttribute = Dext.Entity.Attributes.DiscriminatorColumnAttribute;
  DiscriminatorValueAttribute = Dext.Entity.Attributes.DiscriminatorValueAttribute;
  DbTypeAttribute = Dext.Entity.Attributes.DbTypeAttribute;
  TypeConverterAttribute = Dext.Entity.Attributes.TypeConverterAttribute;
  CreatedAtAttribute = Dext.Entity.Attributes.CreatedAtAttribute;
  UpdatedAtAttribute = Dext.Entity.Attributes.UpdatedAtAttribute;
  EmailAddressAttribute = Dext.Validation.EmailAddressAttribute;

  // Dext.Entity.Cache
  TSQLCache = Dext.Entity.Cache.TSQLCache;

  // Dext.Entity.Context
  TFluentExpression = Dext.Entity.Context.TFluentExpression;
  TPropertyInfo = Dext.Entity.Context.TPropertyInfo;
  TChangeTracker = Dext.Entity.Context.TChangeTracker;
  TDbContext = Dext.Entity.Context.TDbContext;
  TCollectionEntry = Dext.Entity.Context.TCollectionEntry;
  TReferenceEntry = Dext.Entity.Context.TReferenceEntry;
  TEntityEntry = Dext.Entity.Context.TEntityEntry;

  // Dext.Entity.Core
  EOptimisticConcurrencyException = Dext.Entity.Core.EOptimisticConcurrencyException;
  TEntityState = Dext.Entity.Core.TEntityState;
  IChangeTracker = Dext.Entity.Core.IChangeTracker;
  IDbSet = Dext.Entity.Core.IDbSet;
  ICollectionEntry = Dext.Entity.Core.ICollectionEntry;
  IReferenceEntry = Dext.Entity.Core.IReferenceEntry;
  IEntityEntry = Dext.Entity.Core.IEntityEntry;
  IDbContext = Dext.Entity.Core.IDbContext;

  // Dext.Entity.Dialects
  TDatabaseDialect = Dext.Entity.Dialects.TDatabaseDialect;
  TReturningPosition = Dext.Entity.Dialects.TReturningPosition;
  ISQLDialect = Dext.Entity.Dialects.ISQLDialect;
  TDialectFactory = Dext.Entity.Dialects.TDialectFactory;
  TBaseDialect = Dext.Entity.Dialects.TBaseDialect;
  TSQLiteDialect = Dext.Entity.Dialects.TSQLiteDialect;
  TPostgreSQLDialect = Dext.Entity.Dialects.TPostgreSQLDialect;
  TFirebirdDialect = Dext.Entity.Dialects.TFirebirdDialect;
  TSQLServerDialect = Dext.Entity.Dialects.TSQLServerDialect;
  TMySQLDialect = Dext.Entity.Dialects.TMySQLDialect;
  TOracleDialect = Dext.Entity.Dialects.TOracleDialect;
  TInterBaseDialect = Dext.Entity.Dialects.TInterBaseDialect;

  // Dext.Entity.Drivers.FireDAC
  TFireDACConnection = Dext.Entity.Drivers.FireDAC.TFireDACConnection;
  TFireDACTransaction = Dext.Entity.Drivers.FireDAC.TFireDACTransaction;
  TFireDACReader = Dext.Entity.Drivers.FireDAC.TFireDACReader;
  TFireDACCommand = Dext.Entity.Drivers.FireDAC.TFireDACCommand;

  // Dext.Entity.Drivers.FireDAC.Manager
  TComponentHelper = Dext.Entity.Drivers.FireDAC.Manager.TComponentHelper;
  TFireDACOptimization = Dext.Entity.Drivers.FireDAC.Manager.TFireDACOptimization;
  TFireDACOptimizations = Dext.Entity.Drivers.FireDAC.Manager.TFireDACOptimizations;
  TDextFireDACManager = Dext.Entity.Drivers.FireDAC.Manager.TDextFireDACManager;

  // Dext.Entity.Drivers.FireDAC.Phys
  TFireDACPhysTransaction = Dext.Entity.Drivers.FireDAC.Phys.TFireDACPhysTransaction;
  TFireDACPhysReader = Dext.Entity.Drivers.FireDAC.Phys.TFireDACPhysReader;
  TFireDACPhysCommand = Dext.Entity.Drivers.FireDAC.Phys.TFireDACPhysCommand;
  TFireDACPhysConnection = Dext.Entity.Drivers.FireDAC.Phys.TFireDACPhysConnection;

  // Dext.Entity.Drivers.Interfaces
  IDbReader = Dext.Entity.Drivers.Interfaces.IDbReader;
  IDbTransaction = Dext.Entity.Drivers.Interfaces.IDbTransaction;
  IDbCommand = Dext.Entity.Drivers.Interfaces.IDbCommand;
  IDbConnection = Dext.Entity.Drivers.Interfaces.IDbConnection;

  // Dext.Entity.Grouping
  TQuery = Dext.Entity.Grouping.TQuery;
  // IGrouping<T> = Dext.Entity.Grouping.IGrouping<T>;
  // TGrouping<T> = Dext.Entity.Grouping.TGrouping<T>;
  // TGroupByIterator<T> = Dext.Entity.Grouping.TGroupByIterator<T>;

  // Dext.Entity.Joining
  TJoining = Dext.Entity.Joining.TJoining;
  // TJoinIterator<T> = Dext.Entity.Joining.TJoinIterator<T>;

  // Dext.Entity.LazyLoading
  TLazyInjector = Dext.Entity.LazyLoading.TLazyInjector;
  TLazyLoader = Dext.Entity.LazyLoading.TLazyLoader;

  // Dext.Entity.Mapping
  TPropertyMap = Dext.Entity.Mapping.TPropertyMap;
  TEntityMap = Dext.Entity.Mapping.TEntityMap;
  TModelBuilder = Dext.Entity.Mapping.TModelBuilder;
  // IPropertyBuilder<T> = Dext.Entity.Mapping.IPropertyBuilder<T>;
  // IEntityTypeBuilder<T> = Dext.Entity.Mapping.IEntityTypeBuilder<T>;
  // IPropertyBuilder<T> = Dext.Entity.Mapping.IPropertyBuilder<T>;
  // IEntityTypeConfiguration<T> = Dext.Entity.Mapping.IEntityTypeConfiguration<T>;
  // TEntityBuilder<T> = Dext.Entity.Mapping.TEntityBuilder<T>;
  // TEntityTypeBuilder<T> = Dext.Entity.Mapping.TEntityTypeBuilder<T>;
  // TPropertyBuilder<T> = Dext.Entity.Mapping.TPropertyBuilder<T>;
  // TEntityTypeConfiguration<T> = Dext.Entity.Mapping.TEntityTypeConfiguration<T>;

  // Dext.Entity.Migrations
  IMigration = Dext.Entity.Migrations.IMigration;
  TMigrationRegistry = Dext.Entity.Migrations.TMigrationRegistry;

  // Dext.Entity.Migrations.Builder
  TSchemaBuilder = Dext.Entity.Migrations.Builder.TSchemaBuilder;
  TTableBuilder = Dext.Entity.Migrations.Builder.TTableBuilder;
  TColumnBuilder = Dext.Entity.Migrations.Builder.TColumnBuilder;
  IColumnBuilder = Dext.Entity.Migrations.Builder.IColumnBuilder;

  // Dext.Entity.Migrations.Differ
  TModelDiffer = Dext.Entity.Migrations.Differ.TModelDiffer;

  // Dext.Entity.Migrations.Extractor
  TDbContextModelExtractor = Dext.Entity.Migrations.Extractor.TDbContextModelExtractor;

  // Dext.Entity.Migrations.Generator
  TMigrationGenerator = Dext.Entity.Migrations.Generator.TMigrationGenerator;

  // Dext.Entity.Migrations.Json
  TJsonMigration = Dext.Entity.Migrations.Json.TJsonMigration;
  TJsonMigrationLoader = Dext.Entity.Migrations.Json.TJsonMigrationLoader;

  // Dext.Entity.Migrations.Model
  TSnapshotColumn = Dext.Entity.Migrations.Model.TSnapshotColumn;
  TSnapshotForeignKey = Dext.Entity.Migrations.Model.TSnapshotForeignKey;
  TSnapshotTable = Dext.Entity.Migrations.Model.TSnapshotTable;
  TSnapshotModel = Dext.Entity.Migrations.Model.TSnapshotModel;

  // Dext.Entity.Migrations.Operations
  TOperationType = Dext.Entity.Migrations.Operations.TOperationType;
  TMigrationOperation = Dext.Entity.Migrations.Operations.TMigrationOperation;
  TColumnDefinition = Dext.Entity.Migrations.Operations.TColumnDefinition;
  TCreateTableOperation = Dext.Entity.Migrations.Operations.TCreateTableOperation;
  TDropTableOperation = Dext.Entity.Migrations.Operations.TDropTableOperation;
  TAddColumnOperation = Dext.Entity.Migrations.Operations.TAddColumnOperation;
  TDropColumnOperation = Dext.Entity.Migrations.Operations.TDropColumnOperation;
  TAlterColumnOperation = Dext.Entity.Migrations.Operations.TAlterColumnOperation;
  TAddForeignKeyOperation = Dext.Entity.Migrations.Operations.TAddForeignKeyOperation;
  TDropForeignKeyOperation = Dext.Entity.Migrations.Operations.TDropForeignKeyOperation;
  TCreateIndexOperation = Dext.Entity.Migrations.Operations.TCreateIndexOperation;
  TDropIndexOperation = Dext.Entity.Migrations.Operations.TDropIndexOperation;
  TSqlOperation = Dext.Entity.Migrations.Operations.TSqlOperation;

  // Dext.Entity.Migrations.Runner
  TMigrator = Dext.Entity.Migrations.Runner.TMigrator;

  // Dext.Entity.Migrations.Serializers.Json
  TMigrationJsonSerializer = Dext.Entity.Migrations.Serializers.Json.TMigrationJsonSerializer;

  // Dext.Entity.Naming
  INamingStrategy = Dext.Entity.Naming.INamingStrategy;
  TDefaultNamingStrategy = Dext.Entity.Naming.TDefaultNamingStrategy;
  TSnakeCaseNamingStrategy = Dext.Entity.Naming.TSnakeCaseNamingStrategy;
  TLowerCaseNamingStrategy = Dext.Entity.Naming.TLowerCaseNamingStrategy;
  TUppercaseNamingStrategy = Dext.Entity.Naming.TUppercaseNamingStrategy;

  // Dext.Entity.Prototype
  Prototype = Dext.Entity.Prototype.Prototype;
  Build = Dext.Entity.Prototype.Build;

  // Dext.Entity.Query
  // TQueryIterator<T> = Dext.Entity.Query.TQueryIterator<T>;
  // TSpecificationQueryIterator<T> = Dext.Entity.Query.TSpecificationQueryIterator<T>;
  // TProjectingIterator<T> = Dext.Entity.Query.TProjectingIterator<T>;
  // TFilteringIterator<T> = Dext.Entity.Query.TFilteringIterator<T>;
  // TSkipIterator<T> = Dext.Entity.Query.TSkipIterator<T>;
  // TTakeIterator<T> = Dext.Entity.Query.TTakeIterator<T>;
  // TDistinctIterator<T> = Dext.Entity.Query.TDistinctIterator<T>;
  // TEmptyIterator<T> = Dext.Entity.Query.TEmptyIterator<T>;

  // Dext.Entity.Scaffolding
  TMetaColumn = Dext.Entity.Scaffolding.TMetaColumn;
  TMetaForeignKey = Dext.Entity.Scaffolding.TMetaForeignKey;
  TMetaTable = Dext.Entity.Scaffolding.TMetaTable;
  ISchemaProvider = Dext.Entity.Scaffolding.ISchemaProvider;
  TMappingStyle = Dext.Entity.Scaffolding.TMappingStyle;
  IEntityGenerator = Dext.Entity.Scaffolding.IEntityGenerator;
  TFireDACSchemaProvider = Dext.Entity.Scaffolding.TFireDACSchemaProvider;
  TDelphiEntityGenerator = Dext.Entity.Scaffolding.TDelphiEntityGenerator;

  // Dext.Entity.Setup
  TDbContextOptions = Dext.Entity.Setup.TDbContextOptions;
  TDbContextOptionsBuilder = Dext.Entity.Setup.TDbContextOptionsBuilder;

  // Dext.Entity.Tenancy
  ITenantAware = Dext.Entity.Tenancy.ITenantAware;
  TTenantEntity = Dext.Entity.Tenancy.TTenantEntity;

  // Dext.Entity.TypeConverters
  EnumAsStringAttribute = Dext.Entity.TypeConverters.EnumAsStringAttribute;
  ArrayColumnAttribute = Dext.Entity.TypeConverters.ArrayColumnAttribute;
  ColumnTypeAttribute = Dext.Entity.TypeConverters.ColumnTypeAttribute;
  ITypeConverter = Dext.Entity.TypeConverters.ITypeConverter;
  TTypeConverterBase = Dext.Entity.TypeConverters.TTypeConverterBase;
  TGuidConverter = Dext.Entity.TypeConverters.TGuidConverter;
  TUuidConverter = Dext.Entity.TypeConverters.TUuidConverter;
  TEnumConverter = Dext.Entity.TypeConverters.TEnumConverter;
  TJsonConverter = Dext.Entity.TypeConverters.TJsonConverter;
  TArrayConverter = Dext.Entity.TypeConverters.TArrayConverter;
  TDateTimeConverter = Dext.Entity.TypeConverters.TDateTimeConverter;
  TDateConverter = Dext.Entity.TypeConverters.TDateConverter;
  TTimeConverter = Dext.Entity.TypeConverters.TTimeConverter;
  TBytesConverter = Dext.Entity.TypeConverters.TBytesConverter;
  TPropConverter = Dext.Entity.TypeConverters.TPropConverter;
  TStringsConverter = Dext.Entity.TypeConverters.TStringsConverter;
  TTypeConverterRegistry = Dext.Entity.TypeConverters.TTypeConverterRegistry;

  // Dext.Entity.TypeSystem
  // TProp<T> = Dext.Entity.TypeSystem.TProp<T>;
  // IEntityBuilder<T> = Dext.Entity.TypeSystem.IEntityBuilder<T>;
  // TEntityBuilder<T> = Dext.Entity.TypeSystem.TEntityBuilder<T>;
  // TEntityType<T> = Dext.Entity.TypeSystem.TEntityType<T>;

  // Dext.Specifications.SQL.Generator
  ISQLColumnMapper = Dext.Specifications.SQL.Generator.ISQLColumnMapper;
  TSQLWhereGenerator = Dext.Specifications.SQL.Generator.TSQLWhereGenerator;
  TSQLGeneratorHelper = Dext.Specifications.SQL.Generator.TSQLGeneratorHelper;
  TSQLParamCollector = Dext.Specifications.SQL.Generator.TSQLParamCollector;
  // TSQLColumnMapper<T> = Dext.Specifications.SQL.Generator.TSQLColumnMapper<T>;
  // TSQLGenerator<T> = Dext.Specifications.SQL.Generator.TSQLGenerator<T>;

const
  // Dext.Entity.Attributes
  None = Dext.Entity.Attributes.None;
  TablePerHierarchy = Dext.Entity.Attributes.TablePerHierarchy;
  TablePerType = Dext.Entity.Attributes.TablePerType;
  caNoAction = Dext.Entity.Attributes.caNoAction;
  caCascade = Dext.Entity.Attributes.caCascade;
  caSetNull = Dext.Entity.Attributes.caSetNull;
  caRestrict = Dext.Entity.Attributes.caRestrict;
  // Dext.Entity.Core
  esDetached = Dext.Entity.Core.esDetached;
  esUnchanged = Dext.Entity.Core.esUnchanged;
  esAdded = Dext.Entity.Core.esAdded;
  esDeleted = Dext.Entity.Core.esDeleted;
  esModified = Dext.Entity.Core.esModified;
  // Dext.Entity.Dialects
  ddUnknown = Dext.Entity.Dialects.ddUnknown;
  ddSQLite = Dext.Entity.Dialects.ddSQLite;
  ddPostgreSQL = Dext.Entity.Dialects.ddPostgreSQL;
  ddMySQL = Dext.Entity.Dialects.ddMySQL;
  ddSQLServer = Dext.Entity.Dialects.ddSQLServer;
  ddFirebird = Dext.Entity.Dialects.ddFirebird;
  ddInterbase = Dext.Entity.Dialects.ddInterbase;
  ddOracle = Dext.Entity.Dialects.ddOracle;
  rpAtEnd = Dext.Entity.Dialects.rpAtEnd;
  rpBeforeValues = Dext.Entity.Dialects.rpBeforeValues;
  // Dext.Entity.Drivers.FireDAC.Manager
  optDisableMacros = Dext.Entity.Drivers.FireDAC.Manager.optDisableMacros;
  optDisableEscapes = Dext.Entity.Drivers.FireDAC.Manager.optDisableEscapes;
  optDirectExecute = Dext.Entity.Drivers.FireDAC.Manager.optDirectExecute;
  // Dext.Entity.Migrations.Operations
  otCreateTable = Dext.Entity.Migrations.Operations.otCreateTable;
  otDropTable = Dext.Entity.Migrations.Operations.otDropTable;
  otAddColumn = Dext.Entity.Migrations.Operations.otAddColumn;
  otDropColumn = Dext.Entity.Migrations.Operations.otDropColumn;
  otAlterColumn = Dext.Entity.Migrations.Operations.otAlterColumn;
  otAddPrimaryKey = Dext.Entity.Migrations.Operations.otAddPrimaryKey;
  otDropPrimaryKey = Dext.Entity.Migrations.Operations.otDropPrimaryKey;
  otAddForeignKey = Dext.Entity.Migrations.Operations.otAddForeignKey;
  otDropForeignKey = Dext.Entity.Migrations.Operations.otDropForeignKey;
  otCreateIndex = Dext.Entity.Migrations.Operations.otCreateIndex;
  otDropIndex = Dext.Entity.Migrations.Operations.otDropIndex;
  otSql = Dext.Entity.Migrations.Operations.otSql;
  // Dext.Entity.Scaffolding
  msAttributes = Dext.Entity.Scaffolding.msAttributes;
  msFluent = Dext.Entity.Scaffolding.msFluent;
  // {END_DEXT_ALIASES}

  // ===========================================================================
  // ?? Local Types & Helpers
  // ===========================================================================
type
  Lazy<T> = record
  private
    FInstance: ILazy;
    function GetIsValueCreated: Boolean;
    function GetValue: T;
  public
    class function Create: Lazy<T>; overload; static;
    constructor Create(const AValueFactory: TFunc<T>; AOwnsValue: Boolean = True); overload;
    constructor CreateFrom(const AValue: T; AOwnsValue: Boolean = False);

    class operator Implicit(const Value: Lazy<T>): T;
    class operator Implicit(const Value: T): Lazy<T>;
    class operator Implicit(const ValueFactory: TFunc<T>): Lazy<T>;

    property IsValueCreated: Boolean read GetIsValueCreated;
    property Value: T read GetValue;
  end;

  /// <summary>
  ///   Persistence Setup Helper
  /// </summary>
  TDbContextClass = class of TDbContext;
  
  /// <summary>
  ///   Class helper for TDextServices to add Persistence features.
  ///   Inherits from TDextServicesHelper (Core) to enable helper inheritance chain.
  /// </summary>
  /// <remarks>
  ///   This helper adds Entity Framework-style persistence methods to TDextServices.
  ///   Web package extends this helper with TDextWebServicesHelper.
  /// </remarks>
  TDextEntityServicesHelper = class helper(TDextServicesHelper) for TDextServices
  public
    /// <summary>
    ///   Registers a DbContext with the dependency injection container.
    /// </summary>
    function AddDbContext<T: TDbContext>(Config: TProc<TDbContextOptions>): TDextServices; overload;
    
    /// <summary>
    ///   Registers a DbContext with configuration binding.
    /// </summary>
    function AddDbContext<T: TDbContext>(const AConfig: IConfigurationSection): TDextServices; overload;
  end;
  
  // Alias for backward compatibility
  TDextPersistenceServicesHelper = TDextEntityServicesHelper;


  TPersistence = class
  public
    /// <summary>
    ///   Registers a DbContext with the dependency injection container.
    /// </summary>
    class procedure AddDbContext<T: TDbContext>(Services: IServiceCollection; Config: TProc<TDbContextOptions>);
  end;

implementation

uses
  System.Rtti,
  Dext.Configuration.Binder,
  Dext.Specifications.OrderBy; // Added for IServiceProvider, TServiceType

{ TDextEntityServicesHelper }

function TDextEntityServicesHelper.AddDbContext<T>(Config: TProc<TDbContextOptions>): TDextServices;
begin
  TPersistence.AddDbContext<T>(Self.Unwrap, Config);
  Result := Self;
end;

function TDextEntityServicesHelper.AddDbContext<T>(const AConfig: IConfigurationSection): TDextServices;
begin
  Result := AddDbContext<T>(
    procedure(Options: TDbContextOptions)
    begin
      TConfigurationBinder.Bind(AConfig, Options);
    end
  );
end;


{ TPersistence }

class procedure TPersistence.AddDbContext<T>(Services: IServiceCollection; Config: TProc<TDbContextOptions>);
begin
  Services.AddScoped(
    TServiceType.FromClass(T),
    T,
    function(Provider: IServiceProvider): TObject
    begin
      var Options := TDbContextOptions.Create;
      try
        // Apply user configuration
        if Assigned(Config) then
          Config(Options);

        // 1. Connection Creation (Pooled)
        var Connection: IDbConnection;
        
        if Options.CustomConnection <> nil then
        begin
          Connection := Options.CustomConnection;
        end
        else
        begin
          // FireDAC Creation
          var FDConn := TFDConnection.Create(nil);
          
          if Options.ConnectionDefString <> '' then
          begin
            var DefName := Options.ConnectionDefName;
            if DefName = '' then DefName := 'DextMemoryDef_' + IntToHex(Options.ConnectionDefString.GetHashCode, 8);
            TDextFireDACManager.Instance.RegisterConnectionDefFromString(DefName, Options.ConnectionDefString);
            FDConn.ConnectionDefName := DefName;
          end
          else if Options.ConnectionDefName <> '' then
          begin
            FDConn.ConnectionDefName := Options.ConnectionDefName;
          end
          else if Options.Pooling then
          begin
             var Params := TStringList.Create;
             try
               for var Pair in Options.Params do
                 Params.Values[Pair.Key] := Pair.Value;
                 
               // Use Manager to register/get Def
               var DefName := TDextFireDACManager.Instance.RegisterConnectionDef(
                 Options.DriverName, 
                 Params, 
                 Options.PoolMax
               );
               
                FDConn.ConnectionDefName := DefName;
                
                // Apply performance and resource options
                // Apply performance and resource options
                TDextFireDACManager.Instance.ApplyResourceOptions(FDConn, Options.Optimizations);
              finally
                Params.Free;
              end;
          end
          else
          begin
            if Options.DriverName <> '' then
              FDConn.DriverName := Options.DriverName;
              
            if Options.ConnectionString <> '' then
              FDConn.ConnectionString := Options.ConnectionString;
              
            for var Pair in Options.Params do
               FDConn.Params.Values[Pair.Key] := Pair.Value;

            // Apply performance and resource options
            // Apply performance and resource options
            TDextFireDACManager.Instance.ApplyResourceOptions(FDConn, Options.Optimizations);
          end;
          
          try
            // Ensure unique name for components created in threads (request scoped)
            // to avoid global name conflicts in FireDAC manager
            FDConn.SetUniqueName;
            
            FDConn.Open; 
          except
             FDConn.Free;
             raise;
          end;

          Connection := TFireDACConnection.Create(FDConn, True); // Owns FDConn
        end;

        // 2. Dialect Resolution
        var Dialect := Options.Dialect;
        if Dialect = nil then
          Dialect := TSQLiteDialect.Create;

        // 3. Create Context
        var Ctx := TDbContextClass(T).Create(Connection, Dialect, Options.BuildNamingStrategy);
        Result := Ctx;
        
      except
        Options.Free;
        raise;
      end;
      Options.Free;
    end
  );
end;

{ Lazy<T> }

class function Lazy<T>.Create: Lazy<T>;
begin
  // Default constructor returns empty/default
  Result.FInstance := TValueLazy<T>.Create(Default(T));
end;

constructor Lazy<T>.Create(const AValueFactory: TFunc<T>; AOwnsValue: Boolean);
begin
  FInstance := TLazy<T>.Create(AValueFactory, AOwnsValue);
end;

constructor Lazy<T>.CreateFrom(const AValue: T; AOwnsValue: Boolean = False);
begin
  FInstance := TValueLazy<T>.Create(AValue, AOwnsValue);
end;

function Lazy<T>.GetIsValueCreated: Boolean;
begin
  if FInstance <> nil then
    Result := FInstance.IsValueCreated
  else
    Result := False;
end;

function Lazy<T>.GetValue: T;
begin
  if FInstance <> nil then
    Result := FInstance.Value.AsType<T>
  else
    Result := Default(T);
end;

class operator Lazy<T>.Implicit(const Value: Lazy<T>): T;
begin
  Result := Value.Value;
end;

class operator Lazy<T>.Implicit(const Value: T): Lazy<T>;
begin
  Result.CreateFrom(Value);
end;

class operator Lazy<T>.Implicit(const ValueFactory: TFunc<T>): Lazy<T>;
begin
  Result.Create(ValueFactory);
end;


end.
