unit MultiTenancy.Entities;

interface

uses
  Dext.Entity,
  Dext.Core.SmartTypes;

type
  /// <summary>
  ///   Represents a Tenant (Company) in the multi-tenant system.
  /// </summary>
  [Table('tenants')]
  TTenant = class
  private
    FId: StringType;
    FName: StringType;
    FSubdomain: StringType;
    FDatabaseName: StringType;
    FCreatedAt: DateTimeType;
    FIsActive: BoolType;
  public
    [PK]
    [Column('id')]
    property Id: StringType read FId write FId;
    
    [Column('name'), Required, MaxLength(100)]
    property Name: StringType read FName write FName;
    
    [Column('subdomain'), MaxLength(50)]
    property Subdomain: StringType read FSubdomain write FSubdomain;
    
    [Column('database_name'), MaxLength(100)]
    property DatabaseName: StringType read FDatabaseName write FDatabaseName;
    
    [Column('created_at')]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;
    
    [Column('is_active')]
    property IsActive: BoolType read FIsActive write FIsActive;
  end;

  /// <summary>
  ///   Product entity - exists per tenant (tenant-isolated data)
  /// </summary>
  [Table('products')]
  TProduct = class
  private
    FId: Int64;
    FName: StringType;
    FDescription: StringType;
    FPrice: CurrencyType;
    FStock: IntType;
    FTenantId: StringType;
    FCreatedAt: DateTimeType;
  public
    [PK, AutoInc]
    [Column('id')]
    property Id: Int64 read FId write FId;
    
    [Column('tenant_id'), Required]
    property TenantId: StringType read FTenantId write FTenantId;
    
    [Column('name'), Required, MaxLength(100)]
    property Name: StringType read FName write FName;
    
    [Column('description'), MaxLength(500)]
    property Description: StringType read FDescription write FDescription;
    
    [Column('price'), Precision(18, 2)]
    property Price: CurrencyType read FPrice write FPrice;
    
    [Column('stock')]
    property Stock: IntType read FStock write FStock;
    
    [Column('created_at')]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;
  end;

  /// <summary>
  ///   DTO for creating a new tenant
  /// </summary>
  TCreateTenantDto = record
    Name: string;
    Subdomain: string;
  end;

  /// <summary>
  ///   DTO for creating a new product
  /// </summary>
  TCreateProductDto = record
    Name: string;
    Description: string;
    Price: Currency;
    Stock: Integer;
  end;

implementation

end.
