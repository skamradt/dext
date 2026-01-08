unit MultiTenancy.Service;

interface

uses
  System.SysUtils,
  Dext.Entity,
  Dext.Entity.Core,
  Dext.Core.SmartTypes,
  Dext.Collections,
  MultiTenancy.Entities,
  MultiTenancy.DbContext;

type
  /// <summary>
  ///   Service for managing tenants (create, list, get)
  /// </summary>
  ITenantService = interface
    ['{A1B2C3D4-E5F6-7890-1234-567890ABCDEF}']
    function CreateTenant(const Dto: TCreateTenantDto): TTenant;
    function GetTenant(const TenantId: string): TTenant;
    function GetAllTenants: IList<TTenant>;
    function TenantExists(const TenantId: string): Boolean;
  end;

  TTenantService = class(TInterfacedObject, ITenantService)
  private
    FDbContext: TTenantDbContext;
  public
    constructor Create(ADbContext: TTenantDbContext);
    function CreateTenant(const Dto: TCreateTenantDto): TTenant;
    function GetTenant(const TenantId: string): TTenant;
    function GetAllTenants: IList<TTenant>;
    function TenantExists(const TenantId: string): Boolean;
  end;

  /// <summary>
  ///   Service for managing products within a tenant
  /// </summary>
  IProductService = interface
    ['{B2C3D4E5-F6A7-8901-2345-67890ABCDEF1}']
    function CreateProduct(const TenantId: string; const Dto: TCreateProductDto): TProduct;
    function GetProducts(const TenantId: string): IList<TProduct>;
    function GetProductById(const TenantId: string; ProductId: Int64): TProduct;
  end;

  TProductService = class(TInterfacedObject, IProductService)
  private
    FDbContext: TTenantDbContext;
  public
    constructor Create(ADbContext: TTenantDbContext);
    function CreateProduct(const TenantId: string; const Dto: TCreateProductDto): TProduct;
    function GetProducts(const TenantId: string): IList<TProduct>;
    function GetProductById(const TenantId: string; ProductId: Int64): TProduct;
  end;

implementation

{ TTenantService }

constructor TTenantService.Create(ADbContext: TTenantDbContext);
begin
  inherited Create;
  FDbContext := ADbContext;
end;

function TTenantService.CreateTenant(const Dto: TCreateTenantDto): TTenant;
var
  TenantId: string;
begin
  TenantId := TGUID.NewGuid.ToString.Replace('{', '').Replace('}', '').Substring(0, 8);
  
  Result := TTenant.Create;
  Result.Id := TenantId;
  Result.Name := Dto.Name;
  Result.Subdomain := Dto.Subdomain;
  Result.DatabaseName := 'tenant_' + TenantId + '.db';
  Result.CreatedAt := Now;
  Result.IsActive := True;
  
  FDbContext.Tenants.Add(Result);
  FDbContext.SaveChanges;
  
  WriteLn(Format('[Tenant] Created: %s (%s)', [string(Result.Name), string(Result.Id)]));
end;

function TTenantService.GetTenant(const TenantId: string): TTenant;
begin
  Result := FDbContext.Tenants.Find(TenantId);
end;

function TTenantService.GetAllTenants: IList<TTenant>;
begin
  Result := FDbContext.Tenants.ToList;
end;

function TTenantService.TenantExists(const TenantId: string): Boolean;
begin
  Result := GetTenant(TenantId) <> nil;
end;

{ TProductService }

constructor TProductService.Create(ADbContext: TTenantDbContext);
begin
  inherited Create;
  FDbContext := ADbContext;
end;

function TProductService.CreateProduct(const TenantId: string; const Dto: TCreateProductDto): TProduct;
begin
  Result := TProduct.Create;
  Result.TenantId := TenantId;
  Result.Name := Dto.Name;
  Result.Description := Dto.Description;
  Result.Price := Dto.Price;
  Result.Stock := Dto.Stock;
  Result.CreatedAt := Now;

  FDbContext.Products.Add(Result);
  FDbContext.SaveChanges;
  
  WriteLn(Format('[Product] Created for tenant %s: %s', [TenantId, Dto.Name]));
end;

function TProductService.GetProducts(const TenantId: string): IList<TProduct>;
var
  AllProducts: IList<TProduct>;
  Product: TProduct;
begin
  Result := TCollections.CreateList<TProduct>;
  AllProducts := FDbContext.Products.ToList;
  
  for Product in AllProducts do
  begin
    if string(Product.TenantId) = TenantId then
      Result.Add(Product);
  end;
  
  WriteLn(Format('[Product] Listing products for tenant: %s (%d found)', [TenantId, Result.Count]));
end;

function TProductService.GetProductById(const TenantId: string; ProductId: Int64): TProduct;
begin
  Result := FDbContext.Products.Find(ProductId);
  
  // Verify tenant ownership
  if (Result <> nil) and (string(Result.TenantId) <> TenantId) then
    Result := nil;
end;

end.
