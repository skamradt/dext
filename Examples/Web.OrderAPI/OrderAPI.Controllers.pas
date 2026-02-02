unit OrderAPI.Controllers;

{***************************************************************************}
{  Order API - Controllers                                                  }
{  Demonstra uso de Swagger attrs e StatusCodes                            }
{***************************************************************************}

{$M+}

interface

uses
  System.SysUtils,
  System.Classes,
  Dext,
  Dext.Web,
  OrderAPI.Entities,
  OrderAPI.Services;

type
  // ==========================================================================
  // Categories Controller
  // ==========================================================================
  [ApiController('/api/categories')]
  [Authorize]
  [SwaggerTag('Categories')]
  TCategoriesController = class
  private
    FService: ICategoryService;
  public
    constructor Create(Service: ICategoryService);
    
    [HttpGet('')]
    [SwaggerOperation('List all categories', 'Returns all product categories')]
    [SwaggerResponse(HttpStatus.OK, 'Categories list')]
    procedure GetAll(Ctx: IHttpContext); virtual;
    
    [HttpGet('/{id}')]
    [SwaggerOperation('Get category by ID')]
    [SwaggerParam('id', 'Category unique identifier')]
    [SwaggerResponse(HttpStatus.OK, TCategory, 'Category found')]
    [SwaggerResponse(HttpStatus.NotFound, 'Category not found')]
    procedure GetById(Ctx: IHttpContext; [FromRoute] Id: Int64); virtual;
    
    [HttpPost('')]
    [SwaggerOperation('Create new category')]
    [SwaggerResponse(HttpStatus.Created, TCategory, 'Category created')]
    procedure Add(Ctx: IHttpContext; const Request: TCreateCategoryRequest); virtual;
    
    [HttpDelete('/{id}')]
    [SwaggerOperation('Delete category')]
    [SwaggerParam('id', 'Category ID to delete')]
    [SwaggerResponse(HttpStatus.NoContent, 'Category deleted')]
    procedure Delete(Ctx: IHttpContext; [FromRoute] Id: Int64); virtual;
  end;

  // ==========================================================================
  // Products Controller
  // ==========================================================================
  [ApiController('/api/products')]
  [Authorize]
  [SwaggerTag('Products')]
  TProductsController = class
  private
    FService: IProductService;
  public
    constructor Create(Service: IProductService);
    
    [HttpGet('')]
    [SwaggerOperation('List all products', 'Returns all menu products')]
    [SwaggerResponse(HttpStatus.OK, 'Products list')]
    procedure GetAll(Ctx: IHttpContext); virtual;
    
    [HttpGet('/{id}')]
    [SwaggerOperation('Get product by ID')]
    [SwaggerParam('id', 'Product unique identifier')]
    [SwaggerResponse(HttpStatus.OK, TProduct, 'Product found')]
    [SwaggerResponse(HttpStatus.NotFound, 'Product not found')]
    procedure GetById(Ctx: IHttpContext; [FromRoute] Id: Int64); virtual;
    
    [HttpGet('/category/{categoryId}')]
    [SwaggerOperation('Get products by category')]
    [SwaggerParam('categoryId', 'Category ID')]
    procedure GetByCategory(Ctx: IHttpContext; [FromRoute] CategoryId: Int64); virtual;
    
    [HttpPost('')]
    [SwaggerOperation('Create new product')]
    [SwaggerResponse(HttpStatus.Created, TProduct, 'Product created')]
    procedure Add(Ctx: IHttpContext; const Request: TCreateProductRequest); virtual;
    
    [HttpPatch('/{id}/availability')]
    [SwaggerOperation('Update product availability')]
    [SwaggerParam('id', 'Product ID')]
    procedure UpdateAvailability(Ctx: IHttpContext; [FromRoute] Id: Int64; 
      [FromQuery] Available: Boolean); virtual;
  end;

  // ==========================================================================
  // Tables Controller
  // ==========================================================================
  [ApiController('/api/tables')]
  [Authorize]
  [SwaggerTag('Tables')]
  TTablesController = class
  private
    FService: ITableService;
  public
    constructor Create(Service: ITableService);
    
    [HttpGet('')]
    [SwaggerOperation('List all tables')]
    [SwaggerResponse(HttpStatus.OK, 'Tables list')]
    procedure GetAll(Ctx: IHttpContext); virtual;
    
    [HttpGet('/available')]
    [SwaggerOperation('Get available tables')]
    [SwaggerResponse(HttpStatus.OK, 'List of available tables')]
    procedure GetAvailable(Ctx: IHttpContext); virtual;
    
    [HttpGet('/{id}')]
    [SwaggerOperation('Get table by ID')]
    [SwaggerParam('id', 'Table unique identifier')]
    [SwaggerResponse(HttpStatus.OK, 'Table found')]
    [SwaggerResponse(HttpStatus.NotFound, 'Table not found')]
    procedure GetById(Ctx: IHttpContext; [FromRoute] Id: Int64); virtual;
    
    [HttpPatch('/{id}/status')]
    [SwaggerOperation('Update table status')]
    [SwaggerParam('id', 'Table ID')]
    procedure UpdateStatus(Ctx: IHttpContext; [FromRoute] Id: Int64; 
      const Request: TUpdateTableStatusRequest); virtual;
  end;

  // ==========================================================================
  // Orders Controller
  // ==========================================================================
  [ApiController('/api/orders')]
  [Authorize]
  [SwaggerTag('Orders')]
  TOrdersController = class
  private
    FOrderService: IOrderService;
  public
    constructor Create(OrderService: IOrderService);
    
    [HttpGet('')]
    [SwaggerOperation('List all orders')]
    [SwaggerResponse(HttpStatus.OK, 'Orders list')]
    procedure GetAll(Ctx: IHttpContext); virtual;
    
    [HttpGet('/open')]
    [SwaggerOperation('Get open orders')]
    [SwaggerResponse(HttpStatus.OK, 'List of open orders')]
    procedure GetOpen(Ctx: IHttpContext); virtual;
    
    [HttpGet('/{id}')]
    [SwaggerOperation('Get order by ID')]
    [SwaggerParam('id', 'Order unique identifier')]
    [SwaggerResponse(HttpStatus.OK, 'Order found')]
    [SwaggerResponse(HttpStatus.NotFound, 'Order not found')]
    procedure GetById(Ctx: IHttpContext; [FromRoute] Id: Int64); virtual;
    
    [HttpGet('/table/{tableId}')]
    [SwaggerOperation('Get open order by table', 'Returns the current open order for a table')]
    [SwaggerParam('tableId', 'Table ID')]
    procedure GetByTable(Ctx: IHttpContext; [FromRoute] TableId: Int64); virtual;
    
    [HttpPost('')]
    [SwaggerOperation('Create new order', 'Opens a new order/tab for a table')]
    [SwaggerResponse(HttpStatus.Created, 'Order created')]
    procedure Add(Ctx: IHttpContext; const Request: TCreateOrderRequest); virtual;
    
    [HttpPost('/{id}/items')]
    [SwaggerOperation('Add item to order')]
    [SwaggerParam('id', 'Order ID')]
    [SwaggerResponse(HttpStatus.Created, 'Item added')]
    procedure AddItem(Ctx: IHttpContext; [FromRoute] Id: Int64; 
      const Request: TAddOrderItemRequest); virtual;
    
    [HttpGet('/{id}/items')]
    [SwaggerOperation('Get order items')]
    [SwaggerParam('id', 'Order ID')]
    [SwaggerResponse(HttpStatus.OK, 'Order items')]
    procedure GetItems(Ctx: IHttpContext; [FromRoute] Id: Int64); virtual;
    
    [HttpPost('/{id}/close')]
    [SwaggerOperation('Close order', 'Marks order as paid and closes it')]
    [SwaggerParam('id', 'Order ID')]
    [SwaggerResponse(HttpStatus.OK, 'Order closed')]
    procedure CloseOrder(Ctx: IHttpContext; [FromRoute] Id: Int64); virtual;
    
    [HttpPost('/{id}/cancel')]
    [SwaggerOperation('Cancel order')]
    [SwaggerParam('id', 'Order ID')]
    [SwaggerResponse(HttpStatus.OK, 'Order cancelled')]
    procedure CancelOrder(Ctx: IHttpContext; [FromRoute] Id: Int64); virtual;
  end;

  // ==========================================================================
  // Reports Controller
  // ==========================================================================
  [ApiController('/api/reports')]
  [Authorize]
  [SwaggerTag('Reports')]
  TReportsController = class
  private
    FService: TReportService;
  public
    constructor Create(Service: TReportService);
    destructor Destroy; override;
    
    [HttpGet('/stats')]
    [SwaggerOperation('Get general statistics')]
    procedure GetStats(Ctx: IHttpContext); virtual;
    
    [HttpGet('/daily-summary')]
    [SwaggerOperation('Get daily summary')]
    procedure GetDailySummary(Ctx: IHttpContext); virtual;
  end;

implementation

{ TCategoriesController }

constructor TCategoriesController.Create(Service: ICategoryService);
begin
  inherited Create;
  FService := Service;
end;

procedure TCategoriesController.GetAll(Ctx: IHttpContext);
begin
  Ctx.Response.Json(TDextJson.Serialize(FService.GetAll));
end;

procedure TCategoriesController.GetById(Ctx: IHttpContext; Id: Int64);
var
  Entity: TCategory;
begin
  Entity := FService.GetById(Id);
  if Entity = nil then
    Ctx.Response.Status(HttpStatus.NotFound).Json('{"message": "Category not found"}')
  else
    Ctx.Response.Json(TDextJson.Serialize(Entity));
end;

procedure TCategoriesController.Add(Ctx: IHttpContext; const Request: TCreateCategoryRequest);
var
  Entity: TCategory;
begin
  Entity := FService.Add(Request);
  Ctx.Response.Status(HttpStatus.Created).Json(TDextJson.Serialize(Entity));
end;

procedure TCategoriesController.Delete(Ctx: IHttpContext; Id: Int64);
begin
  FService.Delete(Id);
  Ctx.Response.Status(HttpStatus.NoContent);
end;

{ TProductsController }

constructor TProductsController.Create(Service: IProductService);
begin
  inherited Create;
  FService := Service;
end;

procedure TProductsController.GetAll(Ctx: IHttpContext);
begin
  Ctx.Response.Json(TDextJson.Serialize(FService.GetAll));
end;

procedure TProductsController.GetById(Ctx: IHttpContext; Id: Int64);
var
  Entity: TProduct;
begin
  Entity := FService.GetById(Id);
  if Entity = nil then
    Ctx.Response.Status(HttpStatus.NotFound).Json('{"message": "Product not found"}')
  else
    Ctx.Response.Json(TDextJson.Serialize(Entity));
end;

procedure TProductsController.GetByCategory(Ctx: IHttpContext; CategoryId: Int64);
begin
  Ctx.Response.Json(TDextJson.Serialize(FService.GetByCategory(CategoryId)));
end;

procedure TProductsController.Add(Ctx: IHttpContext; const Request: TCreateProductRequest);
var
  Entity: TProduct;
begin
  Entity := FService.Add(Request);
  Ctx.Response.Status(HttpStatus.Created).Json(TDextJson.Serialize(Entity));
end;

procedure TProductsController.UpdateAvailability(Ctx: IHttpContext; Id: Int64; Available: Boolean);
begin
  FService.UpdateAvailability(Id, Available);
  Ctx.Response.Status(HttpStatus.NoContent);
end;

{ TTablesController }

constructor TTablesController.Create(Service: ITableService);
begin
  inherited Create;
  FService := Service;
end;

procedure TTablesController.GetAll(Ctx: IHttpContext);
begin
  Ctx.Response.Json(TDextJson.Serialize(FService.GetAll));
end;

procedure TTablesController.GetAvailable(Ctx: IHttpContext);
begin
  Ctx.Response.Json(TDextJson.Serialize(FService.GetAvailable));
end;

procedure TTablesController.GetById(Ctx: IHttpContext; Id: Int64);
var
  Entity: TRestaurantTable;
begin
  Entity := FService.GetById(Id);
  if Entity = nil then
    Ctx.Response.Status(HttpStatus.NotFound).Json('{"message": "Table not found"}')
  else
    Ctx.Response.Json(TDextJson.Serialize(Entity));
end;

procedure TTablesController.UpdateStatus(Ctx: IHttpContext; Id: Int64; const Request: TUpdateTableStatusRequest);
begin
  FService.UpdateStatus(Id, Request.Status);
  Ctx.Response.Status(HttpStatus.NoContent);
end;

{ TOrdersController }

constructor TOrdersController.Create(OrderService: IOrderService);
begin
  inherited Create;
  FOrderService := OrderService;
end;

procedure TOrdersController.GetAll(Ctx: IHttpContext);
begin
  Ctx.Response.Json(TDextJson.Serialize(FOrderService.GetAll));
end;

procedure TOrdersController.GetOpen(Ctx: IHttpContext);
begin
  Ctx.Response.Json(TDextJson.Serialize(FOrderService.GetOpen));
end;

procedure TOrdersController.GetById(Ctx: IHttpContext; Id: Int64);
var
  Entity: TOrder;
begin
  Entity := FOrderService.GetById(Id);
  if Entity = nil then
    Ctx.Response.Status(HttpStatus.NotFound).Json('{"message": "Order not found"}')
  else
    Ctx.Response.Json(TDextJson.Serialize(Entity));
end;

procedure TOrdersController.GetByTable(Ctx: IHttpContext; TableId: Int64);
var
  Entity: TOrder;
begin
  Entity := FOrderService.GetByTable(TableId);
  if Entity = nil then
    Ctx.Response.Status(HttpStatus.NotFound).Json('{"message": "No open order for this table"}')
  else
    Ctx.Response.Json(TDextJson.Serialize(Entity));
end;

procedure TOrdersController.Add(Ctx: IHttpContext; const Request: TCreateOrderRequest);
var
  Entity: TOrder;
begin
  try
    Entity := FOrderService.Add(Request);
    Ctx.Response.Status(HttpStatus.Created).Json(TDextJson.Serialize(Entity));
  except
    on E: Exception do
      Ctx.Response.Status(HttpStatus.BadRequest).Json('{"error": "' + E.Message + '"}');
  end;
end;

procedure TOrdersController.AddItem(Ctx: IHttpContext; Id: Int64; const Request: TAddOrderItemRequest);
var
  Entity: TOrderItem;
begin
  try
    Entity := FOrderService.AddItem(Id, Request);
    Ctx.Response.Status(HttpStatus.Created).Json(TDextJson.Serialize(Entity));
  except
    on E: Exception do
      Ctx.Response.Status(HttpStatus.BadRequest).Json('{"error": "' + E.Message + '"}');
  end;
end;

procedure TOrdersController.GetItems(Ctx: IHttpContext; Id: Int64);
begin
  Ctx.Response.Json(TDextJson.Serialize(FOrderService.GetItems(Id)));
end;

procedure TOrdersController.CloseOrder(Ctx: IHttpContext; Id: Int64);
begin
  try
    FOrderService.CloseOrder(Id);
    Ctx.Response.Status(HttpStatus.OK).Json('{"message": "Order closed"}');
  except
    on E: Exception do
      Ctx.Response.Status(HttpStatus.BadRequest).Json('{"error": "' + E.Message + '"}');
  end;
end;

procedure TOrdersController.CancelOrder(Ctx: IHttpContext; Id: Int64);
begin
  try
    FOrderService.CancelOrder(Id);
    Ctx.Response.Status(HttpStatus.OK).Json('{"message": "Order cancelled"}');
  except
    on E: Exception do
      Ctx.Response.Status(HttpStatus.BadRequest).Json('{"error": "' + E.Message + '"}');
  end;
end;

{ TReportsController }

constructor TReportsController.Create(Service: TReportService);
begin
  inherited Create;
  FService := Service;
end;

destructor TReportsController.Destroy;
begin
  FService.Free;
  inherited;
end;

procedure TReportsController.GetStats(Ctx: IHttpContext);
begin
  Ctx.Response.Json(TDextJson.Serialize(FService.GetStats));
end;

procedure TReportsController.GetDailySummary(Ctx: IHttpContext);
begin
  Ctx.Response.Json(TDextJson.Serialize(FService.GetDailySummary));
end;

initialization
  // Force linker to include these classes
  TCategoriesController.ClassName;
  TProductsController.ClassName;
  TTablesController.ClassName;
  TOrdersController.ClassName;
  TReportsController.ClassName;

end.

