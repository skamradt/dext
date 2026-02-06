unit TestORMFeatures;

{$I ..\..\Sources\Dext.inc}

// JSON Query Tests Configuration:
// - Define DEXT_TEST_JSON_SQLITE to use SQLite (requires sqlite3.dll with JSON1 support)
// - Undefine DEXT_TEST_JSON_SQLITE to use PostgreSQL (default)
{$DEFINE DEXT_TEST_JSON_SQLITE}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Classes,
  System.DateUtils,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.DApt,
  Dext,
  Dext.Assertions,
  Dext.Testing.Attributes,
  Dext.Collections,
  Dext.Entity, 
  Dext.Entity.Core, 
  Dext.Entity.Attributes,
  Dext.Types.Lazy,
  Dext.Entity.Setup, 
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.Drivers.Interfaces;

type
  // ============================================================================
  // TEST ENTITIES
  // ============================================================================
  
  /// <summary>
  ///   Entity for testing Optimistic Concurrency (Version) 
  /// </summary>
  [Table('Products')]
  TProductVersion = class
  private
    FId: Integer;
    FName: string;
    FPrice: Currency;
    FVersion: Integer;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Price: Currency read FPrice write FPrice;
    [Version]
    property Version: Integer read FVersion write FVersion;
  end;

  /// <summary>
  ///   Entity for testing SoftDelete with Boolean flag
  /// </summary>
  [Table('Documents')]
  [SoftDelete('IsDeleted')]
  TDocument = class
  private
    FId: Integer;
    FTitle: string;
    FIsDeleted: Boolean;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property IsDeleted: Boolean read FIsDeleted write FIsDeleted;
  end;

  /// <summary>
  ///   Entity for testing CreatedAt/UpdatedAt automatic timestamps
  /// </summary>
  [Table('Articles')]
  TAuditedArticle = class
  private
    FId: Integer;
    FTitle: string;
    FContent: string;
    FCreatedAt: TDateTime;
    FUpdatedAt: TDateTime;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property Content: string read FContent write FContent;
    [CreatedAt]
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    [UpdatedAt]
    property UpdatedAt: TDateTime read FUpdatedAt write FUpdatedAt;
  end;
  
  /// <summary>
  ///   Related entity for testing 1:1 and N:1 relationships
  /// </summary>
  [Table('UserProfiles')]
  TUserProfile = class
  private
    FId: Integer;
    FBio: string;
    FAvatarUrl: string;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Bio: string read FBio write FBio;
    property AvatarUrl: string read FAvatarUrl write FAvatarUrl;
  end;
  
  /// <summary>
  ///   Entity for testing 1:1 relationship (User -> Profile)
  /// </summary>
  [Table('Users')]
  TUserWithProfile = class
  private
    FId: Integer;
    FUsername: string;
    FProfileId: Integer;
    FProfile: Lazy<TUserProfile>;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Username: string read FUsername write FUsername;
    
    [Column('profile_id')]
    property ProfileId: Integer read FProfileId write FProfileId;
    
    [HasOne]
    [ForeignKey('profile_id')]
    property Profile: Lazy<TUserProfile> read FProfile write FProfile;
  end;
  
  /// <summary>
  ///   Entity for testing N:1 relationship (Comment -> Author)
  /// </summary>
  [Table('Authors')]
  TAuthor = class
  private
    FId: Integer;
    FName: string;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
  end;
  
  [Table('Comments')]
  TComment = class
  private
    FId: Integer;
    FText: string;
    FAuthorId: Integer;
    FAuthor: Lazy<TAuthor>;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Text: string read FText write FText;
    
    [Column('author_id')]
    property AuthorId: Integer read FAuthorId write FAuthorId;
    
    [BelongsTo]
    [ForeignKey('author_id')]
    property Author: Lazy<TAuthor> read FAuthor write FAuthor;
  end;

  /// <summary>
  ///   Entity for testing JSON column queries
  /// </summary>
  [Table('UserMetadata')]
  TUserMetadata = class
  private
    FId: Integer;
    FName: string;
    FSettings: string;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    [JsonColumn]
    property Settings: string read FSettings write FSettings;
  end;

  // ============================================================================
  // DB CONTEXT
  // ============================================================================
  
  TORMFeaturesContext = class(TDbContext)
  protected
    procedure OnModelCreating(Builder: TModelBuilder); override;
  public
    function Products: IDbSet<TProductVersion>;
    function Documents: IDbSet<TDocument>;
    function Articles: IDbSet<TAuditedArticle>;
    function UserProfiles: IDbSet<TUserProfile>;
    function Users: IDbSet<TUserWithProfile>;
    function Authors: IDbSet<TAuthor>;
    function Comments: IDbSet<TComment>;
    function UserMetadata: IDbSet<TUserMetadata>;
  end;

  // ============================================================================
  // TEST FIXTURES
  // ============================================================================

  [TestFixture]
  TOptimisticConcurrencyTests = class
  private
    FConn: TFDConnection;
    FContext: TORMFeaturesContext;
    FContext2: TORMFeaturesContext; // Second context to simulate concurrent user
    FEntities: TObjectList<TObject>;
    procedure SetupSchema;
    procedure Track(Obj: TObject);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;
    
    [Test]
    procedure Test_Version_IncrementedOnUpdate;
    
    [Test]
    procedure Test_ConcurrencyException_WhenStaleUpdate;
    
    [Test]
    procedure Test_ConcurrencyException_WhenRecordDeleted;
  end;

  [TestFixture]
  TSoftDeleteTests = class
  private
    FConn: TFDConnection;
    FContext: TORMFeaturesContext;
    FEntities: TObjectList<TObject>;
    procedure SetupSchema;
    procedure Track(Obj: TObject);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;
    
    [Test]
    procedure Test_SoftDelete_MarksAsDeleted;
    
    [Test]
    procedure Test_SoftDelete_FilteredByDefault;
    
    [Test]
    procedure Test_IgnoreQueryFilters_ReturnsDeleted;
  end;

  [TestFixture]
  TAuditFieldsTests = class
  private
    FConn: TFDConnection;
    FContext: TORMFeaturesContext;
    FEntities: TObjectList<TObject>;
    procedure SetupSchema;
    procedure Track(Obj: TObject);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;
    
    [Test]
    procedure Test_CreatedAt_SetOnInsert;
    
    [Test]
    procedure Test_UpdatedAt_SetOnInsert;
    
    [Test]
    procedure Test_UpdatedAt_ChangedOnUpdate;
    
    [Test]
    procedure Test_CreatedAt_NotChangedOnUpdate;
  end;

  [TestFixture]
  TJsonQueryTests = class
  private
    class var FSQLiteDriver: TFDPhysSQLiteDriverLink;
  private
    FConn: TFDConnection;
    FContext: TORMFeaturesContext;
    FEntities: TObjectList<TObject>;
    procedure SetupSchema;
    procedure Track(Obj: TObject);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;
    
    [Test]
    procedure Test_JsonQuery_SimpleProperty;
    [Test]
    procedure Test_JsonQuery_NestedProperty;
    [Test]
    procedure Test_JsonQuery_IsNull;
  end;

  [TestFixture]
  TRelationshipTests = class
  private
    FConn: TFDConnection;
    FContext: TORMFeaturesContext;
    FEntities: TObjectList<TObject>;
    procedure SetupSchema;
    procedure Track(Obj: TObject);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;
    
    [Test]
    procedure Test_OneToOne_LazyLoading;
    
    [Test]
    procedure Test_ManyToOne_LazyLoading;
    
    [Test]
    procedure Test_LazyLoading_NoInvalidPointerOperation;
  end;

implementation

// ============================================================================
// TORMFeaturesContext
// ============================================================================

procedure TORMFeaturesContext.OnModelCreating(Builder: TModelBuilder);
begin
  inherited;
  Builder.Entity<TProductVersion>;
  Builder.Entity<TDocument>;
  Builder.Entity<TAuditedArticle>;
  Builder.Entity<TUserProfile>;
  Builder.Entity<TUserWithProfile>;
  Builder.Entity<TAuthor>;
  Builder.Entity<TComment>;
  Builder.Entity<TUserMetadata>;
end;

function TORMFeaturesContext.Products: IDbSet<TProductVersion>;
begin
  Result := Entities<TProductVersion>;
end;

function TORMFeaturesContext.Documents: IDbSet<TDocument>;
begin
  Result := Entities<TDocument>;
end;

function TORMFeaturesContext.Articles: IDbSet<TAuditedArticle>;
begin
  Result := Entities<TAuditedArticle>;
end;

function TORMFeaturesContext.UserProfiles: IDbSet<TUserProfile>;
begin
  Result := Entities<TUserProfile>;
end;

function TORMFeaturesContext.Users: IDbSet<TUserWithProfile>;
begin
  Result := Entities<TUserWithProfile>;
end;

function TORMFeaturesContext.Authors: IDbSet<TAuthor>;
begin
  Result := Entities<TAuthor>;
end;

function TORMFeaturesContext.Comments: IDbSet<TComment>;
begin
  Result := Entities<TComment>;
end;

function TORMFeaturesContext.UserMetadata: IDbSet<TUserMetadata>;
begin
  Result := Entities<TUserMetadata>;
end;

// ============================================================================
// TOptimisticConcurrencyTests
// ============================================================================

procedure TOptimisticConcurrencyTests.Setup;
var
  DbConn, DbConn2: IDbConnection;
begin
  // FEntities does NOT own objects - ORM's IdentityMap manages their lifetime
  FEntities := TObjectList<TObject>.Create(False);
  
  // Create In-Memory SQLite Connection (shared cache for both contexts)
  FConn := TFDConnection.Create(nil);
  FConn.DriverName := 'SQLite';
  FConn.Params.Add('Database=:memory:');
  FConn.Params.Add('SharedCache=True');
  FConn.LoginPrompt := False;
  FConn.Open;

  DbConn := TFireDACConnection.Create(FConn, False);
  FContext := TORMFeaturesContext.Create(DbConn);
  
  // Second context using same connection (simulates another session)
  DbConn2 := TFireDACConnection.Create(FConn, False);
  FContext2 := TORMFeaturesContext.Create(DbConn2);
  
  SetupSchema;
end;

procedure TOptimisticConcurrencyTests.SetupSchema;
begin
  FContext.Connection.CreateCommand(
    'CREATE TABLE "Products" ("Id" INTEGER PRIMARY KEY AUTOINCREMENT, "Name" TEXT, "Price" REAL, "Version" INTEGER DEFAULT 0)'
  ).Execute;
end;

procedure TOptimisticConcurrencyTests.Track(Obj: TObject);
begin
  if FEntities <> nil then
    FEntities.Add(Obj);
end;

procedure TOptimisticConcurrencyTests.Teardown;
begin
  // Clear entity tracking list FIRST (doesn't free objects, OwnsObjects=False)
  FreeAndNil(FEntities);
  // Now free contexts - IdentityMap will free tracked objects
  FreeAndNil(FContext2);
  FreeAndNil(FContext);
  FreeAndNil(FConn);
end;

procedure TOptimisticConcurrencyTests.Test_Version_IncrementedOnUpdate;
var
  Product: TProductVersion;
  Loaded: TProductVersion;
begin
  // Arrange: Create product with initial version
  Product := TProductVersion.Create; Track(Product);
  Product.Name := 'Widget';
  Product.Price := 9.99;
  Product.Version := 0;
  
  FContext.Products.Add(Product);
  FContext.SaveChanges;
  
  Should(Product.Id).BeGreaterThan(0);
  
  // Act: Update the product
  Product.Price := 14.99;
  FContext.Products.Update(Product);
  FContext.SaveChanges;
  
  // Assert: Version should be incremented
  Should(Product.Version).Be(1);
  
  // Verify in database
  FContext.DetachAll;
  Loaded := FContext.Products.Find(Product.Id);
  Should(Loaded).NotBeNil;
  Should(Loaded.Version).Be(1);
  Should(Loaded.Price).Be(14.99);
end;

procedure TOptimisticConcurrencyTests.Test_ConcurrencyException_WhenStaleUpdate;
var
  Product: TProductVersion;
  ProductCopy1, ProductCopy2: TProductVersion;
  ExceptionRaised: Boolean;
begin
  // Arrange: Create product
  Product := TProductVersion.Create; Track(Product);
  Product.Name := 'Gadget';
  Product.Price := 29.99;
  Product.Version := 0;
  
  FContext.Products.Add(Product);
  FContext.SaveChanges;
  
  // Simulate two users loading the same record
  FContext.DetachAll;
  FContext2.DetachAll;
  
  ProductCopy1 := FContext.Products.Find(Product.Id);
  ProductCopy2 := FContext2.Products.Find(Product.Id);
  
  // User 1 updates first
  ProductCopy1.Price := 39.99;
  FContext.Products.Update(ProductCopy1);
  FContext.SaveChanges;
  
  // User 2 tries to update with stale version
  ExceptionRaised := False;
  try
    ProductCopy2.Price := 49.99;
    FContext2.Products.Update(ProductCopy2);
    FContext2.SaveChanges;
  except
    on E: EOptimisticConcurrencyException do
      ExceptionRaised := True;
  end;
  
  // Assert: Exception should be raised for stale update
  Should(ExceptionRaised).BeTrue;
end;

procedure TOptimisticConcurrencyTests.Test_ConcurrencyException_WhenRecordDeleted;
var
  Product: TProductVersion;
  StaleProduct: TProductVersion;
  ExceptionRaised: Boolean;
begin
  // Arrange: Create and then delete product
  Product := TProductVersion.Create; Track(Product);
  Product.Name := 'Temporary';
  Product.Price := 5.99;
  Product.Version := 0;
  
  FContext.Products.Add(Product);
  FContext.SaveChanges;
  
  var ProductId := Product.Id;
  
  // Load in second context (simulates another user)
  FContext2.DetachAll;
  StaleProduct := FContext2.Products.Find(ProductId);
  
  // Delete in first context
  FContext.Connection.CreateCommand('DELETE FROM "Products" WHERE "Id" = ' + IntToStr(ProductId)).Execute;
  
  // Try to update deleted record
  ExceptionRaised := False;
  try
    StaleProduct.Price := 9.99;
    FContext2.Products.Update(StaleProduct);
    FContext2.SaveChanges;
  except
    on E: EOptimisticConcurrencyException do
      ExceptionRaised := True;
  end;
  
  // Assert: Exception should be raised
  Should(ExceptionRaised).BeTrue;
end;

// ============================================================================
// TSoftDeleteTests
// ============================================================================

procedure TSoftDeleteTests.Setup;
var
  DbConn: IDbConnection;
begin
  // FEntities does NOT own objects - ORM's IdentityMap manages their lifetime
  FEntities := TObjectList<TObject>.Create(False);
  
  FConn := TFDConnection.Create(nil);
  FConn.DriverName := 'SQLite';
  FConn.Params.Add('Database=:memory:');
  FConn.LoginPrompt := False;
  FConn.Open;

  DbConn := TFireDACConnection.Create(FConn, False);
  FContext := TORMFeaturesContext.Create(DbConn);
  
  SetupSchema;
end;

procedure TSoftDeleteTests.SetupSchema;
begin
  FContext.Connection.CreateCommand(
    'CREATE TABLE "Documents" ("Id" INTEGER PRIMARY KEY AUTOINCREMENT, "Title" TEXT, "IsDeleted" INTEGER DEFAULT 0)'
  ).Execute;
end;

procedure TSoftDeleteTests.Track(Obj: TObject);
begin
  if FEntities <> nil then
    FEntities.Add(Obj);
end;

procedure TSoftDeleteTests.Teardown;
begin
  FreeAndNil(FEntities);
  FreeAndNil(FContext);
  FreeAndNil(FConn);
end;

procedure TSoftDeleteTests.Test_SoftDelete_MarksAsDeleted;
var
  Doc: TDocument;
  Cmd: IDbCommand;
  IsDeletedVal: Integer;
begin
  // Arrange
  Doc := TDocument.Create; Track(Doc);
  Doc.Title := 'Important Document';
  Doc.IsDeleted := False;
  
  FContext.Documents.Add(Doc);
  FContext.SaveChanges;
  
  var DocId := Doc.Id;
  
  // Act: Remove (should soft delete)
  FContext.Documents.Remove(Doc);
  FContext.SaveChanges;
  
  // Assert: Record still exists but IsDeleted = 1
  Cmd := FContext.Connection.CreateCommand('SELECT "IsDeleted" FROM "Documents" WHERE "Id" = :id');
  Cmd.AddParam('id', DocId);
  IsDeletedVal := Cmd.ExecuteScalar.AsInteger;
  
  Should(IsDeletedVal).Be(1);
end;

procedure TSoftDeleteTests.Test_SoftDelete_FilteredByDefault;
var
  Doc1, Doc2: TDocument;
  AllDocs: IList<TDocument>;
begin
  // Arrange: Create 2 documents, delete 1
  Doc1 := TDocument.Create; Track(Doc1);
  Doc1.Title := 'Active Document';
  FContext.Documents.Add(Doc1);
  
  Doc2 := TDocument.Create; Track(Doc2);
  Doc2.Title := 'Deleted Document';
  FContext.Documents.Add(Doc2);
  
  FContext.SaveChanges;
  
  // Soft delete Doc2
  FContext.Documents.Remove(Doc2);
  FContext.SaveChanges;
  
  FContext.DetachAll;
  
  // Act: Query all documents (default filter should apply)
  AllDocs := FContext.Documents.ToList;
  
  // Assert: Only non-deleted document should be returned
  Should(AllDocs.Count).Be(1);
  Should(AllDocs[0].Title).Be('Active Document');
end;

procedure TSoftDeleteTests.Test_IgnoreQueryFilters_ReturnsDeleted;
var
  Doc1, Doc2: TDocument;
  AllDocs: IList<TDocument>;
begin
  // Arrange: Create 2 documents, delete 1
  Doc1 := TDocument.Create; Track(Doc1);
  Doc1.Title := 'Active Doc';
  FContext.Documents.Add(Doc1);
  
  Doc2 := TDocument.Create; Track(Doc2);
  Doc2.Title := 'Deleted Doc';
  FContext.Documents.Add(Doc2);
  
  FContext.SaveChanges;
  
  // Soft delete Doc2
  FContext.Documents.Remove(Doc2);
  FContext.SaveChanges;
  
  FContext.DetachAll;
  
  // Act: Query with IgnoreQueryFilters
  AllDocs := FContext.Documents.IgnoreQueryFilters.ToList;
  
  // Assert: Both documents should be returned
  Should(AllDocs.Count).Be(2);
end;

// ============================================================================
// TAuditFieldsTests
// ============================================================================

procedure TAuditFieldsTests.Setup;
var
  DbConn: IDbConnection;
begin
  // FEntities does NOT own objects - ORM's IdentityMap manages their lifetime
  FEntities := TObjectList<TObject>.Create(False);
  
  FConn := TFDConnection.Create(nil);
  FConn.DriverName := 'SQLite';
  FConn.Params.Add('Database=:memory:');
  FConn.LoginPrompt := False;
  FConn.Open;

  DbConn := TFireDACConnection.Create(FConn, False);
  FContext := TORMFeaturesContext.Create(DbConn);
  
  SetupSchema;
end;

procedure TAuditFieldsTests.SetupSchema;
begin
  FContext.Connection.CreateCommand(
    'CREATE TABLE "Articles" ("Id" INTEGER PRIMARY KEY AUTOINCREMENT, "Title" TEXT, "Content" TEXT, "CreatedAt" TEXT, "UpdatedAt" TEXT)'
  ).Execute;
end;

procedure TAuditFieldsTests.Track(Obj: TObject);
begin
  if FEntities <> nil then
    FEntities.Add(Obj);
end;

procedure TAuditFieldsTests.Teardown;
begin
  FreeAndNil(FEntities);
  FreeAndNil(FContext);
  FreeAndNil(FConn);
end;

procedure TAuditFieldsTests.Test_CreatedAt_SetOnInsert;
var
  Article: TAuditedArticle;
  BeforeInsert: TDateTime;
begin
  BeforeInsert := Now;
  
  // Arrange & Act
  Article := TAuditedArticle.Create; Track(Article);
  Article.Title := 'Test Article';
  Article.Content := 'Some content';
  // CreatedAt is NOT set manually
  
  FContext.Articles.Add(Article);
  FContext.SaveChanges;
  
  // Assert: CreatedAt should be set automatically
  Should(Article.CreatedAt).BeGreaterThan(0);
  Should(Article.CreatedAt >= BeforeInsert).BeTrue;
end;

procedure TAuditFieldsTests.Test_UpdatedAt_SetOnInsert;
var
  Article: TAuditedArticle;
  BeforeInsert: TDateTime;
begin
  BeforeInsert := Now;
  
  Article := TAuditedArticle.Create; Track(Article);
  Article.Title := 'Another Article';
  Article.Content := 'Content here';
  
  FContext.Articles.Add(Article);
  FContext.SaveChanges;
  
  // Assert: UpdatedAt should also be set on insert
  Should(Article.UpdatedAt).BeGreaterThan(0);
  Should(Article.UpdatedAt >= BeforeInsert).BeTrue;
end;

procedure TAuditFieldsTests.Test_UpdatedAt_ChangedOnUpdate;
var
  Article: TAuditedArticle;
  OriginalUpdatedAt: TDateTime;
begin
  // Arrange
  Article := TAuditedArticle.Create; Track(Article);
  Article.Title := 'Updateable Article';
  Article.Content := 'Initial content';
  
  FContext.Articles.Add(Article);
  FContext.SaveChanges;
  
  OriginalUpdatedAt := Article.UpdatedAt;
  
  // Wait a bit to ensure timestamp difference
  Sleep(100);
  
  // Act: Update the article
  Article.Content := 'Modified content';
  FContext.Articles.Update(Article);
  FContext.SaveChanges;
  
  // Assert: UpdatedAt should be changed
  Should(Article.UpdatedAt > OriginalUpdatedAt).BeTrue;
end;

procedure TAuditFieldsTests.Test_CreatedAt_NotChangedOnUpdate;
var
  Article: TAuditedArticle;
  OriginalCreatedAt: TDateTime;
begin
  // Arrange
  Article := TAuditedArticle.Create; Track(Article);
  Article.Title := 'Immutable CreatedAt';
  Article.Content := 'Original';
  
  FContext.Articles.Add(Article);
  FContext.SaveChanges;
  
  OriginalCreatedAt := Article.CreatedAt;
  
  Sleep(100);
  
  // Act: Update
  Article.Content := 'Updated';
  FContext.Articles.Update(Article);
  FContext.SaveChanges;
  
  // Assert: CreatedAt should NOT change
  Should(Article.CreatedAt).Be(OriginalCreatedAt);
end;

// ============================================================================
// TJsonQueryTests
// ============================================================================

procedure TJsonQueryTests.Setup;
var
  DbConn: IDbConnection;
begin
  FEntities := TObjectList<TObject>.Create(False);
  FConn := TFDConnection.Create(nil);
  
  {$IFDEF DEXT_TEST_JSON_SQLITE}
  // SQLite with JSON support (requires sqlite3.dll 3.9+ with JSON1 extension)
  if FSQLiteDriver = nil then
  begin
    FSQLiteDriver := TFDPhysSQLiteDriverLink.Create(nil);
    // Point to the output folder where sqlite3.dll 3.51.2 is located
    FSQLiteDriver.VendorLib := ExtractFilePath(ParamStr(0)) + 'sqlite3.dll';
  end;
  FConn.DriverName := 'SQLite';
  FConn.Params.Add('Database=:memory:');
  {$ELSE}
  // PostgreSQL with native JSON/JSONB support
  FConn.DriverName := 'PG';
  FConn.Params.Add('Server=localhost');
  FConn.Params.Add('Database=dext_test');
  FConn.Params.Add('User_Name=postgres');
  FConn.Params.Add('Password=root');
  {$ENDIF}
  
  FConn.LoginPrompt := False;
  FConn.Open;
  DbConn := TFireDACConnection.Create(FConn, False);
  FContext := TORMFeaturesContext.Create(DbConn);
  SetupSchema;
end;

procedure TJsonQueryTests.SetupSchema;
begin
  {$IFDEF DEXT_TEST_JSON_SQLITE}
  // SQLite schema
  FContext.Connection.CreateCommand(
    'CREATE TABLE "UserMetadata" ("Id" INTEGER PRIMARY KEY AUTOINCREMENT, "Name" TEXT, "Settings" TEXT)'
  ).Execute;
  {$ELSE}
  // PostgreSQL schema with JSONB
  try
    FContext.Connection.CreateCommand('DROP TABLE IF EXISTS "UserMetadata"').Execute;
  except
    // Ignore if table doesn't exist
  end;
  FContext.Connection.CreateCommand(
    'CREATE TABLE "UserMetadata" ("Id" SERIAL PRIMARY KEY, "Name" TEXT, "Settings" JSONB)'
  ).Execute;
  {$ENDIF}
end;


procedure TJsonQueryTests.Teardown;
begin
  FreeAndNil(FEntities);
  FreeAndNil(FContext);
  FreeAndNil(FConn);
end;

procedure TJsonQueryTests.Track(Obj: TObject);
begin
  if FEntities <> nil then FEntities.Add(Obj);
end;

procedure TJsonQueryTests.Test_JsonQuery_SimpleProperty;
var
  Meta: TUserMetadata;
  Result: IList<TUserMetadata>;
begin
  Meta := TUserMetadata.Create; Track(Meta);
  Meta.Name := 'Admin';
  Meta.Settings := '{"role": "admin", "theme": "dark"}';
  FContext.UserMetadata.Add(Meta);
  
  Meta := TUserMetadata.Create; Track(Meta);
  Meta.Name := 'User';
  Meta.Settings := '{"role": "user", "theme": "light"}';
  FContext.UserMetadata.Add(Meta);
  
  FContext.SaveChanges;
  FContext.DetachAll;
  
  // Act
  Result := FContext.UserMetadata.Where(Prop('Settings').Json('role') = 'admin').ToList;
  
  // Assert
  Should(Result.Count).Be(1);
  Should(Result[0].Name).Be('Admin');
end;

procedure TJsonQueryTests.Test_JsonQuery_NestedProperty;
var
  Meta: TUserMetadata;
  Result: IList<TUserMetadata>;
begin
  Meta := TUserMetadata.Create; Track(Meta);
  Meta.Name := 'Deep';
  Meta.Settings := '{"profile": {"details": {"level": 5}}}';
  FContext.UserMetadata.Add(Meta);
  
  FContext.SaveChanges;
  FContext.DetachAll;
  
  // Act
  Result := FContext.UserMetadata.Where(Prop('Settings').Json('profile.details.level') = 5).ToList;
  
  // Assert
  Should(Result.Count).Be(1);
  Should(Result[0].Name).Be('Deep');
end;

procedure TJsonQueryTests.Test_JsonQuery_IsNull;
var
  Meta: TUserMetadata;
  Result: IList<TUserMetadata>;
begin
  Meta := TUserMetadata.Create; Track(Meta);
  Meta.Name := 'Empty';
  Meta.Settings := '{}';
  FContext.UserMetadata.Add(Meta);
  
  FContext.SaveChanges;
  FContext.DetachAll;
  
  // Act: check if a missing json key results in null behavior
  Result := FContext.UserMetadata.Where(Prop('Settings').Json('nonexistent').IsNull).ToList;
  
  // Assert
  Should(Result.Count).Be(1);
  Should(Result[0].Name).Be('Empty');
end;

// ============================================================================
// TRelationshipTests
// ============================================================================

procedure TRelationshipTests.Setup;
var
  DbConn: IDbConnection;
begin
  // FEntities does NOT own objects - ORM's IdentityMap manages their lifetime
  FEntities := TObjectList<TObject>.Create(False);
  
  FConn := TFDConnection.Create(nil);
  FConn.DriverName := 'SQLite';
  FConn.Params.Add('Database=:memory:');
  FConn.LoginPrompt := False;
  FConn.Open;

  DbConn := TFireDACConnection.Create(FConn, False);
  FContext := TORMFeaturesContext.Create(DbConn);
  
  // Force RTTI for generics
  var Dummy1: Lazy<TUserProfile>;
  var Dummy2: Lazy<TAuthor>;
  
  SetupSchema;
end;

procedure TRelationshipTests.SetupSchema;
begin
  FContext.Connection.CreateCommand(
    'CREATE TABLE "UserProfiles" ("Id" INTEGER PRIMARY KEY AUTOINCREMENT, "Bio" TEXT, "AvatarUrl" TEXT)'
  ).Execute;
  
  FContext.Connection.CreateCommand(
    'CREATE TABLE "Users" ("Id" INTEGER PRIMARY KEY AUTOINCREMENT, "Username" TEXT, "profile_id" INTEGER)'
  ).Execute;
  
  FContext.Connection.CreateCommand(
    'CREATE TABLE "Authors" ("Id" INTEGER PRIMARY KEY AUTOINCREMENT, "Name" TEXT)'
  ).Execute;
  
  FContext.Connection.CreateCommand(
    'CREATE TABLE "Comments" ("Id" INTEGER PRIMARY KEY AUTOINCREMENT, "Text" TEXT, "author_id" INTEGER)'
  ).Execute;
end;

procedure TRelationshipTests.Track(Obj: TObject);
begin
  if FEntities <> nil then
    FEntities.Add(Obj);
end;

procedure TRelationshipTests.Teardown;
begin
  FreeAndNil(FEntities);
  FreeAndNil(FContext);
  FreeAndNil(FConn);
end;

procedure TRelationshipTests.Test_OneToOne_LazyLoading;
var
  Profile: TUserProfile;
  User: TUserWithProfile;
  LoadedUser: TUserWithProfile;
  LoadedProfile: TUserProfile;
begin
  // Arrange: Create Profile
  Profile := TUserProfile.Create; Track(Profile);
  Profile.Bio := 'Software Developer';
  Profile.AvatarUrl := 'https://example.com/avatar.jpg';
  FContext.UserProfiles.Add(Profile);
  FContext.SaveChanges;
  
  // Create User with reference to Profile
  User := TUserWithProfile.Create; Track(User);
  User.Username := 'johndoe';
  User.ProfileId := Profile.Id;
  FContext.Users.Add(User);
  FContext.SaveChanges;
  
  // Detach to ensure fresh load
  FContext.DetachAll;
  
  // Act: Load User
  LoadedUser := FContext.Users.Find(User.Id);
  Should(LoadedUser).NotBeNil;
  
  // Profile should NOT be loaded yet
  Should(LoadedUser.Profile.IsValueCreated).BeFalse;
  
  // Trigger lazy loading by accessing the property
  LoadedProfile := LoadedUser.Profile.Value;
  
  // Assert
  Should(LoadedUser.Profile.IsValueCreated).BeTrue;
  Should(LoadedProfile).NotBeNil;
  Should(LoadedProfile.Bio).Be('Software Developer');
end;

procedure TRelationshipTests.Test_ManyToOne_LazyLoading;
var
  Author: TAuthor;
  Comment: TComment;
  LoadedComment: TComment;
  LoadedAuthor: TAuthor;
begin
  // Arrange: Create Author
  Author := TAuthor.Create; Track(Author);
  Author.Name := 'Jane Smith';
  FContext.Authors.Add(Author);
  FContext.SaveChanges;
  
  // Create Comment referencing Author
  Comment := TComment.Create; Track(Comment);
  Comment.Text := 'Great article!';
  Comment.AuthorId := Author.Id;
  FContext.Comments.Add(Comment);
  FContext.SaveChanges;
  
  FContext.DetachAll;
  
  // Act: Load Comment
  LoadedComment := FContext.Comments.Find(Comment.Id);
  Should(LoadedComment).NotBeNil;
  
  // Author should NOT be loaded yet
  Should(LoadedComment.Author.IsValueCreated).BeFalse;
  
  // Trigger lazy loading
  LoadedAuthor := LoadedComment.Author.Value;
  
  // Assert
  Should(LoadedComment.Author.IsValueCreated).BeTrue;
  Should(LoadedAuthor).NotBeNil;
  Should(LoadedAuthor.Name).Be('Jane Smith');
end;

procedure TRelationshipTests.Test_LazyLoading_NoInvalidPointerOperation;
var
  Author: TAuthor;
  Comment1, Comment2, Comment3: TComment;
  AllComments: IList<TComment>;
  LoadedAuthor: TAuthor;
  i: Integer;
begin
  // Arrange: Create Author and multiple comments
  Author := TAuthor.Create; Track(Author);
  Author.Name := 'Multiple Comments Author';
  FContext.Authors.Add(Author);
  FContext.SaveChanges;
  
  Comment1 := TComment.Create; Track(Comment1);
  Comment1.Text := 'First comment';
  Comment1.AuthorId := Author.Id;
  FContext.Comments.Add(Comment1);
  
  Comment2 := TComment.Create; Track(Comment2);
  Comment2.Text := 'Second comment';
  Comment2.AuthorId := Author.Id;
  FContext.Comments.Add(Comment2);
  
  Comment3 := TComment.Create; Track(Comment3);
  Comment3.Text := 'Third comment';
  Comment3.AuthorId := Author.Id;
  FContext.Comments.Add(Comment3);
  
  FContext.SaveChanges;
  
  FContext.DetachAll;
  
  // Act & Assert: Load all comments and access lazy properties
  // This tests the fix for "Invalid pointer operation" (OwnsObjects := False)
  AllComments := FContext.Comments.ToList;
  
  Should(AllComments.Count).Be(3);
  
  // Access lazy property on each - should NOT cause Invalid Pointer Operation
  for i := 0 to AllComments.Count - 1 do
  begin
    LoadedAuthor := AllComments[i].Author.Value;
    Should(LoadedAuthor).NotBeNil;
    Should(LoadedAuthor.Name).Be('Multiple Comments Author');
  end;
  
  // If we reach here without exception, the test passes
  Should(True).BeTrue;
end;

end.
