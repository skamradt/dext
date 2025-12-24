// Auto-generated Aliases

  // Dext.Entity.Attributes
  TInheritanceStrategy = Dext.Entity.Attributes.TInheritanceStrategy;
  TableAttribute = Dext.Entity.Attributes.TableAttribute;

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

  // Dext.Entity.DbSet
  // TDbSet = Dext.Entity.DbSet.TDbSet;
  // FRttiContext = Dext.Entity.DbSet.FRttiContext;
  // FTableName = Dext.Entity.DbSet.FTableName;
  // FPKColumns = Dext.Entity.DbSet.FPKColumns;
  // FProps = Dext.Entity.DbSet.FProps;
  // FColumns = Dext.Entity.DbSet.FColumns;
  // FIdentityMap = Dext.Entity.DbSet.FIdentityMap;
  // FMap = Dext.Entity.DbSet.FMap;
  // FIgnoreQueryFilters = Dext.Entity.DbSet.FIgnoreQueryFilters;
  // FOnlyDeleted = Dext.Entity.DbSet.FOnlyDeleted;

  // Dext.Entity.Dialects
  TDatabaseDialect = Dext.Entity.Dialects.TDatabaseDialect;
  TReturningPosition = Dext.Entity.Dialects.TReturningPosition;
  ISQLDialect = Dext.Entity.Dialects.ISQLDialect;
  TBaseDialect = Dext.Entity.Dialects.TBaseDialect;

  // Dext.Entity.Drivers.FireDAC
  TFireDACConnection = Dext.Entity.Drivers.FireDAC.TFireDACConnection;
  TFireDACTransaction = Dext.Entity.Drivers.FireDAC.TFireDACTransaction;
  // FOwnsTransaction = Dext.Entity.Drivers.FireDAC.FOwnsTransaction;

  // Dext.Entity.Drivers.FireDAC.Manager
  TComponentHelper = Dext.Entity.Drivers.FireDAC.Manager.TComponentHelper;
  TDextFireDACManager = Dext.Entity.Drivers.FireDAC.Manager.TDextFireDACManager;

  // Dext.Entity.Drivers.FireDAC.Phys
  TFireDACPhysTransaction = Dext.Entity.Drivers.FireDAC.Phys.TFireDACPhysTransaction;

  // Dext.Entity.Drivers.Interfaces
  IDbReader = Dext.Entity.Drivers.Interfaces.IDbReader;
  IDbTransaction = Dext.Entity.Drivers.Interfaces.IDbTransaction;
  IDbCommand = Dext.Entity.Drivers.Interfaces.IDbCommand;
  IDbConnection = Dext.Entity.Drivers.Interfaces.IDbConnection;

  // Dext.Entity.Grouping
  // IGrouping = Dext.Entity.Grouping.IGrouping;
  TQuery = Dext.Entity.Grouping.TQuery;

  // Dext.Entity.Joining
  // TJoinIterator = Dext.Entity.Joining.TJoinIterator;
  // FInner = Dext.Entity.Joining.FInner;
  // FOuterKeySelector = Dext.Entity.Joining.FOuterKeySelector;
  // FInnerKeySelector = Dext.Entity.Joining.FInnerKeySelector;
  // FResultSelector = Dext.Entity.Joining.FResultSelector;
  // FExecuted = Dext.Entity.Joining.FExecuted;
  // FOuterEnumerator = Dext.Entity.Joining.FOuterEnumerator;
  // FInnerLookup = Dext.Entity.Joining.FInnerLookup;
  // FCurrentInnerList = Dext.Entity.Joining.FCurrentInnerList;
  // FCurrentInnerIndex = Dext.Entity.Joining.FCurrentInnerIndex;
  TJoining = Dext.Entity.Joining.TJoining;

  // Dext.Entity.LazyLoading
  TLazyInjector = Dext.Entity.LazyLoading.TLazyInjector;
  TLazyLoader = Dext.Entity.LazyLoading.TLazyLoader;
  // FEntity = Dext.Entity.LazyLoading.FEntity;
  // FPropName = Dext.Entity.LazyLoading.FPropName;
  // FLoaded = Dext.Entity.LazyLoading.FLoaded;
  // FValue = Dext.Entity.LazyLoading.FValue;
  // FIsCollection = Dext.Entity.LazyLoading.FIsCollection;

  // Dext.Entity.Mapping
  TPropertyMap = Dext.Entity.Mapping.TPropertyMap;
  TEntityMap = Dext.Entity.Mapping.TEntityMap;
  // FIsSoftDelete = Dext.Entity.Mapping.FIsSoftDelete;
  // FSoftDeleteProp = Dext.Entity.Mapping.FSoftDeleteProp;
  // FSoftDeleteDeletedValue = Dext.Entity.Mapping.FSoftDeleteDeletedValue;
  // FSoftDeleteNotDeletedValue = Dext.Entity.Mapping.FSoftDeleteNotDeletedValue;
  // FInheritanceStrategy = Dext.Entity.Mapping.FInheritanceStrategy;
  // FDiscriminatorColumn = Dext.Entity.Mapping.FDiscriminatorColumn;
  // FDiscriminatorValue = Dext.Entity.Mapping.FDiscriminatorValue;

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
  // FJsonContent = Dext.Entity.Migrations.Json.FJsonContent;
  // FDescription = Dext.Entity.Migrations.Json.FDescription;
  // FAuthor = Dext.Entity.Migrations.Json.FAuthor;

  // Dext.Entity.Migrations.Model
  TSnapshotColumn = Dext.Entity.Migrations.Model.TSnapshotColumn;

  // Dext.Entity.Migrations.Operations
  TOperationType = Dext.Entity.Migrations.Operations.TOperationType;
  TMigrationOperation = Dext.Entity.Migrations.Operations.TMigrationOperation;
  TColumnDefinition = Dext.Entity.Migrations.Operations.TColumnDefinition;
  TCreateTableOperation = Dext.Entity.Migrations.Operations.TCreateTableOperation;

  // Dext.Entity.Migrations.Runner
  TMigrator = Dext.Entity.Migrations.Runner.TMigrator;

  // Dext.Entity.Migrations.Serializers.Json
  TMigrationJsonSerializer = Dext.Entity.Migrations.Serializers.Json.TMigrationJsonSerializer;

  // Dext.Entity.Naming
  INamingStrategy = Dext.Entity.Naming.INamingStrategy;
  TDefaultNamingStrategy = Dext.Entity.Naming.TDefaultNamingStrategy;
  TSnakeCaseNamingStrategy = Dext.Entity.Naming.TSnakeCaseNamingStrategy;

  // Dext.Entity.Query
  // TPagedResult = Dext.Entity.Query.TPagedResult;
  // FTotalCount = Dext.Entity.Query.FTotalCount;
  // FPageNumber = Dext.Entity.Query.FPageNumber;
  // FPageSize = Dext.Entity.Query.FPageSize;
  // ATotalCount = Dext.Entity.Query.ATotalCount;

  // Dext.Entity.Scaffolding
  TMetaColumn = Dext.Entity.Scaffolding.TMetaColumn;
  TMetaForeignKey = Dext.Entity.Scaffolding.TMetaForeignKey;
  TMetaTable = Dext.Entity.Scaffolding.TMetaTable;

  // Dext.Entity.Setup
  TDbContextOptions = Dext.Entity.Setup.TDbContextOptions;
  // FPoolMax = Dext.Entity.Setup.FPoolMax;
  // FDialect = Dext.Entity.Setup.FDialect;
  // FCustomConnection = Dext.Entity.Setup.FCustomConnection;

  // Dext.Entity.Tenancy
  ITenantAware = Dext.Entity.Tenancy.ITenantAware;
  TTenantEntity = Dext.Entity.Tenancy.TTenantEntity;
  // out = Dext.Entity.Tenancy.out;
  // stdcall = Dext.Entity.Tenancy.stdcall;

  // Dext.Entity.TypeConverters
  EnumAsStringAttribute = Dext.Entity.TypeConverters.EnumAsStringAttribute;
  ArrayColumnAttribute = Dext.Entity.TypeConverters.ArrayColumnAttribute;
  ColumnTypeAttribute = Dext.Entity.TypeConverters.ColumnTypeAttribute;

  // Dext.Entity.TypeSystem
  // TPropertyInfo = Dext.Entity.TypeSystem.TPropertyInfo;
  // TProp = Dext.Entity.TypeSystem.TProp;

  // Dext.Specifications.SQL.Generator
  ISQLColumnMapper = Dext.Specifications.SQL.Generator.ISQLColumnMapper;
  TSQLWhereGenerator = Dext.Specifications.SQL.Generator.TSQLWhereGenerator;
  // FDialect = Dext.Specifications.SQL.Generator.FDialect;
  // FColumnMapper = Dext.Specifications.SQL.Generator.FColumnMapper;

const

  // Dext.Entity.Attributes
  None = Dext.Entity.Attributes.None;
  TablePerHierarchy = Dext.Entity.Attributes.TablePerHierarchy;
  TablePerType = Dext.Entity.Attributes.TablePerType;

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
