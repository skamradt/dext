unit Dext.Entity.FluentMapping.Tests;

interface

uses
  Dext.Testing.Attributes,
  Dext.Assertions,
  Dext.Entity,
  Dext.Entity.Mapping,
  Dext.Entity.Core,
  Dext.Entity.Dialects,
  Dext.Entity.Attributes,
  Dext.Entity.ProxyFactory,
  System.SysUtils;

type
  TFluentOrder = class;

  TFluentCustomer = class
  private
    FId: Integer;
    FName: string;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
  end;

  TFluentOrder = class
  private
    FId: Integer;
    FCustomerId: Integer;
    FDescription: string;
    FCreatedAt: TDateTime;
    FVersion: Integer;
    FCustomer: TFluentCustomer;
  public
    property Id: Integer read FId write FId;
    property CustomerId: Integer read FCustomerId write FCustomerId;
    property Description: string read FDescription write FDescription;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property Version: Integer read FVersion write FVersion;
    
    // Auto-Proxy requirement: virtual getter
    function GetCustomer: TFluentCustomer; virtual;
    property Customer: TFluentCustomer read GetCustomer;
  end;

  TFluentMappingConfig = class(TEntityTypeConfiguration<TFluentOrder>)
  public
    procedure Configure(Builder: IEntityTypeBuilder<TFluentOrder>); override;
  end;

  [TestFixture]
  TFluentMappingTests = class
  public
    [Test]
    [Description('Verify typed property selectors in Fluent API')]
    procedure TestTypedSelectors;

    [Test]
    [Description('Verify Auto-Proxy creation for IsLazy properties')]
    procedure TestAutoProxyDetection;

    [Test]
    [Description('Verify Audit and Version markers in Mapping')]
    procedure TestMappingMarkers;

    [Test]
    [Description('Verify real proxy interception and loading')]
    procedure TestFunctionalAutoProxy;
  end;

implementation

{ TFluentOrder }

function TFluentOrder.GetCustomer: TFluentCustomer;
begin
  Result := FCustomer;
end;

{ TFluentMappingConfig }

procedure TFluentMappingConfig.Configure(Builder: IEntityTypeBuilder<TFluentOrder>);
var
  o: TFluentOrder;
begin
  o := Prototype.Entity<TFluentOrder>;

  Builder.ToTable('Orders');
  Builder.HasKey(o.Id);
  
  Builder.Property(o.Description).HasColumnName('Desc');
  Builder.Property(o.CreatedAt).IsCreatedAt;
  Builder.Property(o.Version).IsVersion;
  Builder.Property(o.Customer).IsLazy;
end;

{ TFluentMappingTests }

procedure TFluentMappingTests.TestTypedSelectors;
var
  Model: TModelBuilder;
  Map: TEntityMap;
  Prop: TPropertyMap;
begin
  Model := TModelBuilder.Create;
  try
    Model.ApplyConfiguration<TFluentOrder>(TFluentMappingConfig.Create);
    Map := TEntityMap(Model.GetMap(TypeInfo(TFluentOrder)));
    
    Should(Map.TableName).Be('Orders');
    
    // Check property override by name
    if Map.Properties.TryGetValue('Description', Prop) then
      Should(Prop.ColumnName).Be('Desc')
    else
      Assert.Fail('Property Description not found in Map');
  finally
    Model.Free;
  end;
end;

procedure TFluentMappingTests.TestAutoProxyDetection;
var
  Model: TModelBuilder;
begin
  Model := TModelBuilder.Create;
  try
    Model.ApplyConfiguration<TFluentOrder>(TFluentMappingConfig.Create);
    
    // Manual check for NeedsProxy
    // In a real scenario, this is called by TDbSet via TEntityProxyFactory
    // But here we can't easily mock IDbContext perfectly without more code, 
    // so we verify the map flag which is the trigger.
    
    var Map := TEntityMap(Model.GetMap(TypeInfo(TFluentOrder)));
    var Prop: TPropertyMap;
    Map.Properties.TryGetValue('Customer', Prop);
    
    Should(Prop.IsLazy).BeTrue;
  finally
    Model.Free;
  end;
end;

procedure TFluentMappingTests.TestMappingMarkers;
var
  Model: TModelBuilder;
  Map: TEntityMap;
  Prop: TPropertyMap;
begin
  Model := TModelBuilder.Create;
  try
    Model.ApplyConfiguration<TFluentOrder>(TFluentMappingConfig.Create);
    Map := TEntityMap(Model.GetMap(TypeInfo(TFluentOrder)));
    
    Map.Properties.TryGetValue('CreatedAt', Prop);
    Should(Prop.IsCreatedAt).BeTrue;
    
    Map.Properties.TryGetValue('Version', Prop);
    Should(Prop.IsVersion).BeTrue;
  finally
    Model.Free;
  end;
end;

procedure TFluentMappingTests.TestFunctionalAutoProxy;
var
  Ctx: TDbContext;
  Order: TFluentOrder;
begin
  // Set up an in-memory context with the configuration
  Ctx := TDbContext.Create('sqlite::memory:', TSQLiteDialect.Create);
  try
    // 1. Configure Mapping
    Ctx.ModelBuilder.ApplyConfiguration<TFluentOrder>(TFluentMappingConfig.Create);
    
    // 2. Create Schema
    Ctx.DataSet<TFluentOrder>.CreateTable;
    Ctx.DataSet<TFluentCustomer>.CreateTable;
    
    // 3. Seed data
    var Cust := TFluentCustomer.Create;
    Cust.Id := 10;
    Cust.Name := 'Cesar';
    Ctx.Add(Cust);
    
    var Ord := TFluentOrder.Create;
    Ord.Id := 1;
    Ord.CustomerId := 10;
    Ord.Description := 'Test Auto Proxy';
    Ctx.Add(Ord);
    Ctx.SaveChanges;
    
    // 4. Test Lazy Load
    Ctx.DetachAll; // Clear cache
    
    // This should return a Proxy
    Order := Ctx.DataSet<TFluentOrder>.Find(1);
    
    Should(Order).Not.BeNil;
    Should(Order.ClassName).Contain('Proxy'); // Verify it is indeed a proxy
    
    // Pre-access check
    // Direct field access (if we could) would be empty, but we use the property
    
    // Act: Access the virtual property
    var LoadedCustomer := Order.Customer;
    
    // Assert
    Should(LoadedCustomer).Not.BeNil;
    Should(LoadedCustomer.Id).Be(10);
    Should(LoadedCustomer.Name).Be('Cesar');
    
  finally
    Ctx.Free;
  end;
end;

end.
