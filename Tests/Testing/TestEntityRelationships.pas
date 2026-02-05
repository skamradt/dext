unit TestEntityRelationships;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  Dext.Assertions,
  Dext.Testing.Attributes,
  Dext.Entity.Attributes,
  Dext.Entity.Mapping,
  Dext.Specifications.SQL.Generator,
  Dext.Entity.Dialects,
  Dext.Collections,
  Dext.Types.Lazy,
  Dext.Entity.LazyLoading,
  Dext.Entity.Core;

type
  [Table('Customers')]
  TCustomer = class
  private
    FId: Integer;
    FName: string;
    FOrders: IList<TObject>; // Represented as TObject for simplicity in basic mapping test or specific type
  public
    [PK]
    property Id: Integer read FId write FId;
    [Column('FullName')]
    property Name: string read FName write FName;
    
    [HasMany, InverseProperty('Customer')]
    property Orders: IList<TObject> read FOrders write FOrders; // In real use this would be IList<TOrder>
  end;

  [Table('Orders')]
  TOrder = class
  private
    FId: Integer;
    FCustomerId: Integer;
    FOrderDate: TDateTime;
    FCustomer: TCustomer;
  public
    [PK]
    property Id: Integer read FId write FId;
    
    [Column('customer_id')]
    property CustomerId: Integer read FCustomerId write FCustomerId;
    
    property OrderDate: TDateTime read FOrderDate write FOrderDate;

    [BelongsTo, ForeignKey('CustomerId'), InverseProperty('Orders')]
    property Customer: TCustomer read FCustomer write FCustomer;
  end;

  // Fluent Entities
  TFluentAuthor = class
  private
    FId: Integer;
    FName: string;
    FBooks: IList<TObject>;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Books: IList<TObject> read FBooks write FBooks;
  end;

  TFluentBook = class
  private
    FId: Integer;
    FTitle: string;
    FAuthorId: Integer;
    FAuthor: TFluentAuthor;
  public
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property AuthorId: Integer read FAuthorId write FAuthorId;
    property Author: TFluentAuthor read FAuthor write FAuthor;
  end;

  [Table('Users')]
  TUser = class
  private
    FId: Integer;
    FUsername: string;
    FProfile: TObject; // Testing with TObject for generic attribute detection
  public
    [PK] property Id: Integer read FId write FId;
    property Username: string read FUsername write FUsername;
    [HasOne, InverseProperty('User')]
    property Profile: TObject read FProfile write FProfile;
  end;

  [Table('Profiles')]
  TUserProfile = class
  private
    FId: Integer;
    FUserId: Integer;
    FBio: string;
    FUser: TUser;
  public
    [PK] property Id: Integer read FId write FId;
    [Column('user_id')] property UserId: Integer read FUserId write FUserId;
    property Bio: string read FBio write FBio;
    [BelongsTo, ForeignKey('UserId'), InverseProperty('Profile'), DeleteBehavior(caCascade)]
    property User: TUser read FUser write FUser;
  end;

  [TestFixture('Entity Relationship Mapping Tests')]
  TEntityRelationshipTests = class
  public
    [Test]
    procedure TestAttributeMapping_Discovery;
    
    [Test]
    procedure TestFluentMapping_Discovery;
    
    [Test]
    procedure TestSQLGenerator_IgnoresNavigationProperties;

    [Test]
    procedure TestSQLGenerator_IncludeForeignKeyColumn;

    [Test]
    procedure TestOneToOneMapping_Discovery;

    [Test]
    procedure TestHasPrincipalKey_Fluent;

    [Test]
    procedure TestManyToMany_Discovery;
    
    [Test]
    procedure TestJoinTableSQL_Insert;
    
    [Test]
    procedure TestJoinTableSQL_Delete;
    
    [Test]
    procedure TestJoinTableSQL_DeleteByLeft;
    
    [Test]
    procedure TestManyToMany_AttributeDetection;

    [Test]
    procedure TestManyToMany_LazyConfiguration;
  end;

implementation

{ TEntityRelationshipTests }

procedure TEntityRelationshipTests.TestAttributeMapping_Discovery;
var
  Map: TEntityMap;
  PropMap: TPropertyMap;
begin
  Map := TEntityMap.Create(TypeInfo(TCustomer));
  try
    Map.DiscoverAttributes;
    
    // Check if Orders is detected as Navigation
    Should(Map.Properties.TryGetValue('Orders', PropMap)).BeTrue;
    Should(PropMap.IsNavigation).Because('Orders has [HasMany] attribute').BeTrue;
    Should(PropMap.Relationship).Be(rtOneToMany);
    Should(PropMap.InverseProperty).Be('Customer');
    
    // Check if Name is NOT navigation
    Should(Map.Properties.TryGetValue('Name', PropMap)).BeTrue;
    Should(PropMap.IsNavigation).BeFalse;
    Should(PropMap.ColumnName).Be('FullName');
  finally
    Map.Free;
  end;

  Map := TEntityMap.Create(TypeInfo(TOrder));
  try
    Map.DiscoverAttributes;
    
    // Check Customer reference
    Should(Map.Properties.TryGetValue('Customer', PropMap)).BeTrue;
    Should(PropMap.IsNavigation).BeTrue;
    Should(PropMap.Relationship).Be(rtManyToOne);
    Should(PropMap.ForeignKeyColumn).Be('CustomerId');
    Should(PropMap.InverseProperty).Be('Orders');
    
    // Check CustomerId (The FK column itself)
    Should(Map.Properties.TryGetValue('CustomerId', PropMap)).BeTrue;
    Should(PropMap.IsNavigation).BeFalse;
    Should(PropMap.ColumnName).Be('customer_id');
  finally
    Map.Free;
  end;
end;

procedure TEntityRelationshipTests.TestFluentMapping_Discovery;
var
  Map: TEntityMap;
  PropMap: TPropertyMap;
begin
  Map := TEntityMap.Create(TypeInfo(TFluentAuthor));
  try
    // TFluentAuthor has NO attributes, so DiscoverAttributes should only find basic stuff or nothing if no naming strategy
    Map.DiscoverAttributes; 
    
    // Manual Fluent Mapping (simulating IEntityTypeConfiguration)
    TEntityBuilder<TFluentAuthor>.Create(Map)
      .HasKey('Id')
      .HasMany('Books')
        .WithOne('Author')
        .HasForeignKey('AuthorId')
        .OnDelete(caCascade);
        
    Should(Map.Properties.TryGetValue('Books', PropMap)).BeTrue;
    Should(PropMap.IsNavigation).BeTrue;
    Should(PropMap.Relationship).Be(rtOneToMany);
    Should(PropMap.InverseProperty).Be('Author');
    Should(PropMap.ForeignKeyColumn).Be('AuthorId');
    Should(PropMap.DeleteBehavior).Be(caCascade);
  finally
    Map.Free;
  end;
end;

procedure TEntityRelationshipTests.TestSQLGenerator_IncludeForeignKeyColumn;
var
  Generator: TSQLGenerator<TOrder>;
  Order: TOrder;
  Sql: string;
begin
  Order := TOrder.Create;
  try
    Order.Id := 10;
    Order.CustomerId := 1;
    Order.OrderDate := Now;
    
    Generator := TSQLGenerator<TOrder>.Create(TSQLiteDialect.Create);
    try
      Sql := Generator.GenerateInsert(Order);
      
      // Should include customer_id but NOT Customer object
      Should(Sql).Contain('customer_id');
      Should(Sql).NotContain('"Customer"');
      
      // Params should be Id, customer_id, OrderDate (3 params)
      Should(Generator.Params.Count).Be(3);
    finally
      Generator.Free;
    end;
  finally
    Order.Free;
  end;
end;

procedure TEntityRelationshipTests.TestSQLGenerator_IgnoresNavigationProperties;
var
  Generator: TSQLGenerator<TCustomer>;
  Customer: TCustomer;
  Sql: string;
begin
  Customer := TCustomer.Create;
  try
    Customer.Id := 1;
    Customer.Name := 'Test Customer';
    // Orders is ignored by SQL generator
    
    Generator := TSQLGenerator<TCustomer>.Create(TSQLiteDialect.Create);
    try
      Sql := Generator.GenerateInsert(Customer);
      
      // INSERT INTO "Customers" ("Id", "FullName") VALUES (:p1, :p2)
      Should(Sql).NotContain('Orders');
      Should(Generator.Params.Count).Be(2); 
    finally
      Generator.Free;
    end;
  finally
    Customer.Free;
  end;
end;

procedure TEntityRelationshipTests.TestOneToOneMapping_Discovery;
var
  Map: TEntityMap;
  PropMap: TPropertyMap;
begin
  Map := TEntityMap.Create(TypeInfo(TUser));
  try
    Map.DiscoverAttributes;
    Should(Map.Properties.TryGetValue('Profile', PropMap)).BeTrue;
    Should(PropMap.IsNavigation).BeTrue;
    Should(PropMap.Relationship).Be(rtOneToOne);
    Should(PropMap.InverseProperty).Be('User');
  finally
    Map.Free;
  end;

  Map := TEntityMap.Create(TypeInfo(TUserProfile));
  try
    Map.DiscoverAttributes;
    Should(Map.Properties.TryGetValue('User', PropMap)).BeTrue;
    Should(PropMap.Relationship).Be(rtManyToOne); // BelongsTo
    Should(PropMap.DeleteBehavior).Be(caCascade);
  finally
    Map.Free;
  end;
end;

procedure TEntityRelationshipTests.TestHasPrincipalKey_Fluent;
var
  Map: TEntityMap;
  PropMap: TPropertyMap;
begin
  Map := TEntityMap.Create(TypeInfo(TFluentBook));
  try
    TEntityBuilder<TFluentBook>.Create(Map)
      .BelongsTo('Author')
        .WithMany('Books')
        .HasForeignKey('AuthorId')
        .HasPrincipalKey('Id')
        .OnDelete(caSetNull);

    Should(Map.Properties.TryGetValue('Author', PropMap)).BeTrue;
    Should(PropMap.PrincipalKey).Be('Id');
    Should(PropMap.DeleteBehavior).Be(caSetNull);
  finally
    Map.Free;
  end;
end;

type
  // Many-to-Many relationship: Student <-> Course via StudentCourses join table
  [Table('Students')]
  TStudent = class
  private
    FId: Integer;
    FName: string;
    FCourses: IList<TObject>;
  public
    [PK] property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    [ManyToMany('StudentCourses', 'student_id', 'course_id'), InverseProperty('Students')]
    property Courses: IList<TObject> read FCourses write FCourses;
  end;

  [Table('Students')]
  TStudentLazy = class
  private
    FId: Integer;
    FCourses: Lazy<IList<TObject>>;
  public
    [PK] property Id: Integer read FId write FId;
    [ManyToMany('StudentCourses', 'student_id', 'course_id')]
    property Courses: Lazy<IList<TObject>> read FCourses write FCourses;
  end;

  [Table('Courses')]
  TCourse = class
  private
    FId: Integer;
    FTitle: string;
    FStudents: IList<TObject>;
  public
    [PK] property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    [ManyToMany('StudentCourses', 'course_id', 'student_id'), InverseProperty('Courses')]
    property Students: IList<TObject> read FStudents write FStudents;
  end;

procedure TEntityRelationshipTests.TestManyToMany_Discovery;
var
  Map: TEntityMap;
  PropMap: TPropertyMap;
begin
  // Test attribute-based Many-to-Many
  Map := TEntityMap.Create(TypeInfo(TStudent));
  try
    Map.DiscoverAttributes;
    Should(Map.Properties.TryGetValue('Courses', PropMap)).BeTrue;
    Should(PropMap.IsNavigation).BeTrue;
    Should(PropMap.Relationship).Be(rtManyToMany);
    Should(PropMap.JoinTableName).Be('StudentCourses');
    Should(PropMap.LeftKeyColumn).Be('student_id');
    Should(PropMap.RightKeyColumn).Be('course_id');
    Should(PropMap.InverseProperty).Be('Students');
  finally
    Map.Free;
  end;

  // Test fluent API Many-to-Many
  Map := TEntityMap.Create(TypeInfo(TCourse));
  try
    TEntityBuilder<TCourse>.Create(Map)
      .HasManyToMany('Students')
        .WithMany('Courses')
        .UsingEntity('StudentCourses', 'course_id', 'student_id')
        .OnDelete(caCascade);

    Should(Map.Properties.TryGetValue('Students', PropMap)).BeTrue;
    Should(PropMap.IsNavigation).BeTrue;
    Should(PropMap.Relationship).Be(rtManyToMany);
    Should(PropMap.JoinTableName).Be('StudentCourses');
    Should(PropMap.LeftKeyColumn).Be('course_id');
    Should(PropMap.RightKeyColumn).Be('student_id');
    Should(PropMap.DeleteBehavior).Be(caCascade);
  finally
    Map.Free;
  end;
end;

procedure TEntityRelationshipTests.TestJoinTableSQL_Insert;
var
  Dialect: ISQLDialect;
  SQL: string;
begin
  Dialect := TSQLiteDialect.Create;
  
  SQL := TJoinTableSQLHelper.GenerateInsert(Dialect,
    'StudentCourses', 'student_id', 'course_id');
    
  Should(SQL).Contain('INSERT INTO');
  Should(SQL).Contain('"StudentCourses"');
  Should(SQL).Contain('"student_id"');
  Should(SQL).Contain('"course_id"');
  Should(SQL).Contain('VALUES');
  Should(SQL).Contain(':p1');
  Should(SQL).Contain(':p2');
end;

procedure TEntityRelationshipTests.TestJoinTableSQL_Delete;
var
  Dialect: ISQLDialect;
  SQL: string;
begin
  Dialect := TSQLiteDialect.Create;
  
  SQL := TJoinTableSQLHelper.GenerateDelete(Dialect,
    'StudentCourses', 'student_id', 'course_id');
    
  Should(SQL).Contain('DELETE FROM');
  Should(SQL).Contain('"StudentCourses"');
  Should(SQL).Contain('WHERE');
  Should(SQL).Contain('"student_id" = :p1');
  Should(SQL).Contain('AND');
  Should(SQL).Contain('"course_id" = :p2');
end;

procedure TEntityRelationshipTests.TestJoinTableSQL_DeleteByLeft;
var
  Dialect: ISQLDialect;
  SQL: string;
begin
  Dialect := TSQLiteDialect.Create;
  
  SQL := TJoinTableSQLHelper.GenerateDeleteByLeft(Dialect,
    'StudentCourses', 'student_id');
    
  Should(SQL).Contain('DELETE FROM');
  Should(SQL).Contain('"StudentCourses"');
  Should(SQL).Contain('WHERE');
  Should(SQL).Contain('"student_id" = :p1');
  // Should NOT contain course_id
  Should(SQL).NotContain('course_id');
end;

procedure TEntityRelationshipTests.TestManyToMany_AttributeDetection;
var
  Map: TEntityMap;
  PropMap: TPropertyMap;
begin
  // Test TStudent side
  Map := TEntityMap.Create(TypeInfo(TStudent));
  try
    Map.DiscoverAttributes;
    Should(Map.Properties.TryGetValue('Courses', PropMap)).BeTrue;
    Should(PropMap.Relationship).Be(rtManyToMany);
    Should(PropMap.JoinTableName).Be('StudentCourses');
    Should(PropMap.LeftKeyColumn).Be('student_id');
    Should(PropMap.RightKeyColumn).Be('course_id');
    Should(PropMap.InverseProperty).Be('Students');
  finally
    Map.Free;
  end;
  
  // Test TCourse side (reverse direction)
  Map := TEntityMap.Create(TypeInfo(TCourse));
  try
    Map.DiscoverAttributes;
    Should(Map.Properties.TryGetValue('Students', PropMap)).BeTrue;
    Should(PropMap.Relationship).Be(rtManyToMany);
    Should(PropMap.JoinTableName).Be('StudentCourses');
    // Note: Course side uses course_id as left key
    Should(PropMap.LeftKeyColumn).Be('course_id');
    Should(PropMap.RightKeyColumn).Be('student_id');
    Should(PropMap.InverseProperty).Be('Courses');
  finally
    Map.Free;
  end;
end;

procedure TEntityRelationshipTests.TestManyToMany_LazyConfiguration;
var
  Student: TStudentLazy;
  Ctx: TRttiContext;
  Typ: TRttiType;
  Field: TRttiField;
begin
  Student := TStudentLazy.Create;
  try
    // In Dext, TLazyInjector.Inject is called during hydration.
    // Here we can test if it correctly identifies and injects the loader.
    // Since we don't have a real DBContext for this unit test, 
    // we just check if the attribute discovery works for Lazy properties.
    
    Ctx := TRttiContext.Create;
    try
      Typ := Ctx.GetType(TStudentLazy);
      Field := Typ.GetField('FCourses');
      
      Should(Field).NotBeNil;
      Should(Field.FieldType.Name).Contain('Lazy<');
      
      // Test if ManyToMany attribute is present on the property
      var Prop := Typ.GetProperty('Courses');
      Should(Prop).NotBeNil;
      Should(Prop.HasAttribute<ManyToManyAttribute>).BeTrue;
    finally
      Ctx.Free;
    end;
  finally
    Student.Free;
  end;
end;

end.
