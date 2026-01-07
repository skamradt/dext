unit OrderAPI.Services;

{***************************************************************************}
{  Order API - Business Services                                            }
{  Services são Transient com interfaces para que ARC gerencie lifecycle    }
{***************************************************************************}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Dext.Entity,
  Dext.Collections,
  OrderAPI.Entities,
  OrderAPI.Database;

type
  // ==========================================================================
  // Interfaces (Int64 IDs)
  // ==========================================================================

  ICategoryService = interface
    ['{A1B2C3D1-1111-1111-1111-111111111111}']
    function GetAll: IList<TCategory>;
    function GetById(Id: Int64): TCategory;
    function Add(const Request: TCreateCategoryRequest): TCategory;
    procedure Delete(Id: Int64);
  end;

  IProductService = interface
    ['{A1B2C3D2-2222-2222-2222-222222222222}']
    function GetAll: IList<TProduct>;
    function GetById(Id: Int64): TProduct;
    function GetByCategory(CategoryId: Int64): IList<TProduct>;
    function Add(const Request: TCreateProductRequest): TProduct;
    procedure UpdateAvailability(Id: Int64; Available: Boolean);
  end;

  ITableService = interface
    ['{A1B2C3D3-3333-3333-3333-333333333333}']
    function GetAll: IList<TRestaurantTable>;
    function GetById(Id: Int64): TRestaurantTable;
    function GetAvailable: IList<TRestaurantTable>;
    procedure UpdateStatus(Id: Int64; Status: TTableStatus);
  end;

  IOrderService = interface
    ['{A1B2C3D4-4444-4444-4444-444444444444}']
    function GetAll: IList<TOrder>;
    function GetById(Id: Int64): TOrder;
    function GetOpen: IList<TOrder>;
    function GetByTable(TableId: Int64): TOrder;
    function Add(const Request: TCreateOrderRequest): TOrder;
    function AddItem(OrderId: Int64; const Request: TAddOrderItemRequest): TOrderItem;
    function GetItems(OrderId: Int64): IList<TOrderItem>;
    procedure CloseOrder(Id: Int64);
    procedure CancelOrder(Id: Int64);
  end;

  // ==========================================================================
  // Services Declarations
  // ==========================================================================

  TCategoryService = class(TInterfacedObject, ICategoryService)
  private
    FDbContext: TOrderDbContext;
  public
    constructor Create(DbContext: TOrderDbContext);
    function GetAll: IList<TCategory>;
    function GetById(Id: Int64): TCategory;
    function Add(const Request: TCreateCategoryRequest): TCategory;
    procedure Delete(Id: Int64);
  end;

  TProductService = class(TInterfacedObject, IProductService)
  private
    FDbContext: TOrderDbContext;
  public
    constructor Create(DbContext: TOrderDbContext);
    function GetAll: IList<TProduct>;
    function GetById(Id: Int64): TProduct;
    function GetByCategory(CategoryId: Int64): IList<TProduct>;
    function Add(const Request: TCreateProductRequest): TProduct;
    procedure UpdateAvailability(Id: Int64; Available: Boolean);
  end;

  TTableService = class(TInterfacedObject, ITableService)
  private
    FDbContext: TOrderDbContext;
  public
    constructor Create(DbContext: TOrderDbContext);
    function GetAll: IList<TRestaurantTable>;
    function GetById(Id: Int64): TRestaurantTable;
    function GetAvailable: IList<TRestaurantTable>;
    procedure UpdateStatus(Id: Int64; Status: TTableStatus);
  end;

  TOrderService = class(TInterfacedObject, IOrderService)
  private
    FDbContext: TOrderDbContext;
  public
    constructor Create(DbContext: TOrderDbContext);
    function GetAll: IList<TOrder>;
    function GetById(Id: Int64): TOrder;
    function GetOpen: IList<TOrder>;
    function GetByTable(TableId: Int64): TOrder;
    function Add(const Request: TCreateOrderRequest): TOrder;
    function AddItem(OrderId: Int64; const Request: TAddOrderItemRequest): TOrderItem;
    function GetItems(OrderId: Int64): IList<TOrderItem>;
    procedure CloseOrder(Id: Int64);
    procedure CancelOrder(Id: Int64);
  end;

  TReportService = class
  private
    FDbContext: TOrderDbContext;
    FInstanceId: Integer;
    class var FInstanceCounter: Integer;
  public
    constructor Create(DbContext: TOrderDbContext);
    destructor Destroy; override;
    function GetStats: TReportStats;
    function GetDailySummary: TDailySummary;
  end;

implementation

{ TCategoryService }

constructor TCategoryService.Create(DbContext: TOrderDbContext);
begin
  inherited Create;
  FDbContext := DbContext;
end;

function TCategoryService.GetAll: IList<TCategory>;
begin
  Result := FDbContext.Entities<TCategory>.ToList;
end;

function TCategoryService.GetById(Id: Int64): TCategory;
begin
  Result := FDbContext.Entities<TCategory>.Find(Id);
end;

function TCategoryService.Add(const Request: TCreateCategoryRequest): TCategory;
begin
  Result := TCategory.Create;
  Result.Name := Request.Name;
  Result.Description := Request.Description;
  Result.Active := True;
  
  FDbContext.Entities<TCategory>.Add(Result);
  FDbContext.SaveChanges;
end;

procedure TCategoryService.Delete(Id: Int64);
var
  Entity: TCategory;
begin
  Entity := GetById(Id);
  if Assigned(Entity) then
  begin
    FDbContext.Entities<TCategory>.Remove(Entity);
    FDbContext.SaveChanges;
  end;
end;

{ TProductService }

constructor TProductService.Create(DbContext: TOrderDbContext);
begin
  inherited Create;
  FDbContext := DbContext;
end;

function TProductService.GetAll: IList<TProduct>;
begin
  Result := FDbContext.Entities<TProduct>.ToList;
end;

function TProductService.GetById(Id: Int64): TProduct;
begin
  Result := FDbContext.Entities<TProduct>.Find(Id);
end;

function TProductService.GetByCategory(CategoryId: Int64): IList<TProduct>;
begin
  var u := Prototype.Entity<TProduct>;
  Result := FDbContext.Entities<TProduct>
    .Where(u.CategoryId = CategoryId)
    .ToList;
end;

function TProductService.Add(const Request: TCreateProductRequest): TProduct;
begin
  Result := TProduct.Create;
  Result.Name := Request.Name;
  Result.Description := Request.Description;
  Result.Price := Request.Price;
  Result.CategoryId := Request.CategoryId; // Int64 -> Int64Type (Implicit)
  Result.Available := True;
  Result.ImageUrl := Request.ImageUrl;
  
  FDbContext.Entities<TProduct>.Add(Result);
  FDbContext.SaveChanges;
end;

procedure TProductService.UpdateAvailability(Id: Int64; Available: Boolean);
var
  Product: TProduct;
begin
  Product := GetById(Id);
  if Assigned(Product) then
  begin
    Product.Available := Available;
    FDbContext.Entities<TProduct>.Update(Product);
    FDbContext.SaveChanges;
  end;
end;

{ TTableService }

constructor TTableService.Create(DbContext: TOrderDbContext);
begin
  inherited Create;
  FDbContext := DbContext;
end;

function TTableService.GetAll: IList<TRestaurantTable>;
begin
  Result := FDbContext.Entities<TRestaurantTable>.ToList;
end;

function TTableService.GetById(Id: Int64): TRestaurantTable;
begin
  Result := FDbContext.Entities<TRestaurantTable>.Find(Id);
end;

function TTableService.GetAvailable: IList<TRestaurantTable>;
var
  t: TRestaurantTable;
begin
  t := Prototype.Entity<TRestaurantTable>;
  Result := FDbContext.Entities<TRestaurantTable>
    .Where(t.Status = TTableStatus.tsAvailable)
    .ToList;
end;

procedure TTableService.UpdateStatus(Id: Int64; Status: TTableStatus);
var
  Table: TRestaurantTable;
begin
  Table := GetById(Id);
  if Assigned(Table) then
  begin
    Table.Status := Status;
    FDbContext.Entities<TRestaurantTable>.Update(Table);
    FDbContext.SaveChanges;
  end;
end;

{ TOrderService }

constructor TOrderService.Create(DbContext: TOrderDbContext);
begin
  inherited Create;
  FDbContext := DbContext;
end;

function TOrderService.GetAll: IList<TOrder>;
begin
  Result := FDbContext.Entities<TOrder>.ToList;
end;

function TOrderService.GetById(Id: Int64): TOrder;
begin
  Result := FDbContext.Entities<TOrder>.Find(Id);
end;

function TOrderService.GetOpen: IList<TOrder>;
var
  o: TOrder;
begin
  o := Prototype.Entity<TOrder>;
  Result := FDbContext.Entities<TOrder>
    .Where(o.Status = TOrderStatus.osOpen)
    .ToList;
end;

function TOrderService.GetByTable(TableId: Int64): TOrder;
var
  o: TOrder;
  Orders: IList<TOrder>;
begin
  o := Prototype.Entity<TOrder>;
  Orders := FDbContext.Entities<TOrder>
    .Where((o.TableId = TableId) and (o.Status = TOrderStatus.osOpen))
    .ToList;
    
  if Orders.Count > 0 then
    Result := Orders[0]
  else
    Result := nil;
end;

function TOrderService.Add(const Request: TCreateOrderRequest): TOrder;
var
  Table: TRestaurantTable;
begin
  Table := FDbContext.Entities<TRestaurantTable>.Find(Request.TableId); // Auto-boxing Integer->Variant or Int64 find
  if Table = nil then
    raise Exception.Create('Table not found');
    
  if Table.Status.Value <> TTableStatus.tsAvailable then // Explicit .Value check
    raise Exception.Create('Table is not available');
  
  Table.Status := TTableStatus.tsOccupied;
  FDbContext.Entities<TRestaurantTable>.Update(Table);
  
  Result := TOrder.Create;
  Result.TableId := Request.TableId;
  Result.Status := TOrderStatus.osOpen;
  Result.OpenedAt := Now;
  Result.CustomerName := Request.CustomerName;
  Result.Notes := Request.Notes;
  Result.TotalAmount := 0;
  
  FDbContext.Entities<TOrder>.Add(Result);
  FDbContext.SaveChanges;
end;

function TOrderService.AddItem(OrderId: Int64; const Request: TAddOrderItemRequest): TOrderItem;
var
  Order: TOrder;
  Product: TProduct;
begin
  Order := GetById(OrderId);
  if Order = nil then
    raise Exception.Create('Order not found');
    
  if Order.Status.Value <> TOrderStatus.osOpen then // Explicit .Value check
    raise Exception.Create('Order is not open');
  
  Product := FDbContext.Entities<TProduct>.Find(Request.ProductId);
  if Product = nil then
    raise Exception.Create('Product not found');
    
  if not Product.Available.Value then // Explicit .Value check
    raise Exception.Create('Product is not available');
  
  Result := TOrderItem.Create;
  Result.OrderId := OrderId;
  Result.ProductId := Request.ProductId;
  Result.Quantity := Request.Quantity;
  Result.UnitPrice := Product.Price;
  Result.Notes := Request.Notes;
  Result.CreatedAt := Now;
  
  FDbContext.Entities<TOrderItem>.Add(Result);
  
  // Implicit conversion from CurrencyType to Currency and back works
  Order.TotalAmount := Order.TotalAmount.Value + (Product.Price.Value * Request.Quantity);
  FDbContext.Entities<TOrder>.Update(Order);
  
  FDbContext.SaveChanges;
end;

function TOrderService.GetItems(OrderId: Int64): IList<TOrderItem>;
var
  i: TOrderItem;
begin
  i := Prototype.Entity<TOrderItem>;
  Result := FDbContext.Entities<TOrderItem>
    .Where(i.OrderId = OrderId)
    .ToList;
end;

procedure TOrderService.CloseOrder(Id: Int64);
var
  Order: TOrder;
  Table: TRestaurantTable;
begin
  Order := GetById(Id);
  if Order = nil then
    raise Exception.Create('Order not found');
    
  if Order.Status.Value <> TOrderStatus.osOpen then // Explicit .Value
    raise Exception.Create('Order is not open');
  
  Order.Status := TOrderStatus.osClosed;
  Order.ClosedAt := Now;
  FDbContext.Entities<TOrder>.Update(Order);
  
  Table := FDbContext.Entities<TRestaurantTable>.Find(Order.TableId.Value);
  if Assigned(Table) then
  begin
    Table.Status := TTableStatus.tsAvailable;
    FDbContext.Entities<TRestaurantTable>.Update(Table);
  end;
    
  FDbContext.SaveChanges;
end;

procedure TOrderService.CancelOrder(Id: Int64);
var
  Order: TOrder;
  Table: TRestaurantTable;
begin
  Order := GetById(Id);
  if Order = nil then
    raise Exception.Create('Order not found');
    
  if Order.Status.Value <> TOrderStatus.osOpen then // Explicit .Value
    raise Exception.Create('Order is not open');
  
  Order.Status := TOrderStatus.osCancelled;
  Order.ClosedAt := Now;
  FDbContext.Entities<TOrder>.Update(Order);
  
  Table := FDbContext.Entities<TRestaurantTable>.Find(Order.TableId.Value);
  if Assigned(Table) then
  begin
    Table.Status := TTableStatus.tsAvailable;
    FDbContext.Entities<TRestaurantTable>.Update(Table);
  end;
    
  FDbContext.SaveChanges;
end;

{ TReportService }

constructor TReportService.Create(DbContext: TOrderDbContext);
begin
  inherited Create;
  FDbContext := DbContext;
  Inc(FInstanceCounter);
  FInstanceId := FInstanceCounter;
  WriteLn(Format('[TReportService] Created instance #%d', [FInstanceId]));
end;

destructor TReportService.Destroy;
begin
  WriteLn(Format('[TReportService] Destroyed instance #%d', [FInstanceId]));
  inherited;
end;

function TReportService.GetStats: TReportStats;
var
  Tables: IList<TRestaurantTable>;
  Tbl: TRestaurantTable;
  AvailCount, OccupiedCount: Integer;
begin
  Tables := FDbContext.Entities<TRestaurantTable>.ToList;
  
  AvailCount := 0;
  OccupiedCount := 0;
  for Tbl in Tables do
  begin
    if Tbl.Status.Value = TTableStatus.tsAvailable then Inc(AvailCount); // Explicit .Value
    if Tbl.Status.Value = TTableStatus.tsOccupied then Inc(OccupiedCount); // Explicit .Value
  end;
  
  var o := Prototype.Entity<TOrder>;
  // Forçando o uso de ToList.Count devido ao erro de overload de Count
  var OpenCount := FDbContext.Entities<TOrder>
    .Where(o.Status = TOrderStatus.osOpen)
    .ToList.Count;
  
  Result.TotalTables := Tables.Count;
  Result.AvailableTables := AvailCount;
  Result.OccupiedTables := OccupiedCount;
  // Usando ToList.Count para evitar erro de overload
  Result.TotalProducts := FDbContext.Entities<TProduct>.ToList.Count;
  Result.TotalCategories := FDbContext.Entities<TCategory>.ToList.Count;
  Result.OpenOrders := OpenCount;
  Result.InstanceId := FInstanceId;
end;

function TReportService.GetDailySummary: TDailySummary;
var
  Orders: IList<TOrder>;
  Order: TOrder;
  TotalRevenue: Currency;
  ClosedCount: Integer;
begin
  Result.Date := Date;
  
  var o := Prototype.Entity<TOrder>;
  Orders := FDbContext.Entities<TOrder>
    .Where(o.Status = TOrderStatus.osClosed)
    .ToList;
  
  TotalRevenue := 0;
  ClosedCount := 0;
  for Order in Orders do
  begin
    if Trunc(Order.ClosedAt.Value) = Trunc(Date) then // Explicit .Value
    begin
      Inc(ClosedCount);
      TotalRevenue := TotalRevenue + Order.TotalAmount.Value;
    end;
  end;
  
  Result.TotalOrders := ClosedCount;
  Result.TotalRevenue := TotalRevenue;
  if ClosedCount > 0 then
    Result.AverageOrderValue := TotalRevenue / ClosedCount
  else
    Result.AverageOrderValue := 0;
  Result.InstanceId := FInstanceId;
end;

end.
