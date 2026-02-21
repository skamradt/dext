unit MultiTenancy.Endpoints;

interface

uses
  System.SysUtils,
  Dext.Web,
  Dext.Web.Results,
  Dext.Json,
  Dext.Collections,
  MultiTenancy.Entities,
  MultiTenancy.Service;

type
  /// <summary>
  ///   Request DTO with TenantId from header binding.
  ///   Model binding will automatically populate TenantId from X-Tenant-Id header.
  /// </summary>
  TTenantRequest = record
    [FromHeader('X-Tenant-Id')]
    TenantId: string;
  end;

  /// <summary>
  ///   Request DTO for creating products with tenant context.
  /// </summary>
  TProductCreateRequest = record
    [FromHeader('X-Tenant-Id')]
    TenantId: string;
    
    // These come from body
    Name: string;
    Description: string;
    Price: Currency;
    Stock: Integer;
  end;

  TMultiTenancyEndpoints = class
  public
    class procedure Map(const App: TDextAppBuilder);
  end;

implementation

uses
  Dext.Logging.Global;

{ TMultiTenancyEndpoints }

class procedure TMultiTenancyEndpoints.Map(const App: TDextAppBuilder);
begin
  // ============================================================
  // TENANT MANAGEMENT ENDPOINTS (Public - no tenant required)
  // ============================================================
  
  // POST /api/tenants - Create a new tenant
  App.MapPost<ITenantService, TCreateTenantDto, IResult>('/api/tenants',
    function(Service: ITenantService; Dto: TCreateTenantDto): IResult
    var
      Tenant: TTenant;
    begin
      Tenant := Service.CreateTenant(Dto);
      Result := Results.Created('/api/tenants/' + string(Tenant.Id), Tenant);
    end);

  // GET /api/tenants - List all tenants
  App.MapGet<ITenantService, IResult>('/api/tenants',
    function(Service: ITenantService): IResult
    var
      Tenants: IList<TTenant>;
    begin
      Tenants := Service.GetAllTenants;
      Result := Results.Ok<IList<TTenant>>(Tenants);
    end);

  // GET /api/tenants/{id} - Get tenant by ID
  App.MapGet<ITenantService, string, IResult>('/api/tenants/{id}',
    function(Service: ITenantService; Id: string): IResult
    var
      Tenant: TTenant;
    begin
      Tenant := Service.GetTenant(Id);
      
      if Tenant = nil then
        Result := Results.NotFound('Tenant not found')
      else
        Result := Results.Ok(Tenant);
    end);

  // ============================================================
  // PRODUCT ENDPOINTS (Tenant-scoped)
  // Uses TProductCreateRequest for header+body injection
  // ============================================================

  // GET /api/products - List products for current tenant
  App.MapGet<IProductService, TTenantRequest, IResult>('/api/products',
    function(Service: IProductService; Request: TTenantRequest): IResult
    var
      Products: IList<TProduct>;
    begin
      if Request.TenantId = '' then
        Exit(Results.BadRequest('X-Tenant-Id header is required'));
        
      Products := Service.GetProducts(Request.TenantId);
      Result := Results.Ok<IList<TProduct>>(Products);
    end);

  // POST /api/products - Create product for current tenant
  App.MapPost<IProductService, TProductCreateRequest, IResult>('/api/products',
    function(Service: IProductService; Request: TProductCreateRequest): IResult
    var
      Dto: TCreateProductDto;
    begin
      // Debug log to see what we are receiving
      Log.Debug('[DEBUG] Product Request - TenantId: "{TenantId}", Name: "{Name}"', [Request.TenantId, Request.Name]);

      if Request.TenantId = '' then
        Exit(Results.BadRequest('X-Tenant-Id header is required'));
        
      Dto.Name := Request.Name;
      Dto.Description := Request.Description;
      Dto.Price := Request.Price;
      Dto.Stock := Request.Stock;
        
      var Product := Service.CreateProduct(Request.TenantId, Dto);
      Result := Results.Created('/api/products/' + IntToStr(Product.Id), Product);
    end);

  // GET /api/products/{id} - Get product by ID for current tenant
  App.MapGet<IProductService, TTenantRequest, Int64, IResult>('/api/products/{id}',
    function(Service: IProductService; Request: TTenantRequest; Id: Int64): IResult
    var
      Product: TProduct;
    begin
      if Request.TenantId = '' then
        Exit(Results.BadRequest('X-Tenant-Id header is required'));
        
      Product := Service.GetProductById(Request.TenantId, Id);
      
      if Product = nil then
        Result := Results.NotFound('Product not found')
      else
        Result := Results.Ok(Product);
    end);
end;

end.
