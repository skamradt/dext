unit ComplexQuerying.Service;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.DateUtils,
  Dext.Entity,
  Dext.Entity.Core,
  Dext.Core.SmartTypes,
  Dext.Collections,
  Dext.Json,
  ComplexQuerying.Entities,
  ComplexQuerying.DbContext;

type
  /// <summary>
  ///   Service for order-related queries
  /// </summary>
  IOrderService = interface
    ['{C1D2E3F4-A5B6-7890-1234-567890ABCDEF}']
    function GetAllOrders: IList<TOrder>;
    function GetOrderById(Id: Int64): TOrder;
    function GetOrdersByStatus(const Status: string): IList<TOrder>;
    function GetOrdersByCustomer(CustomerId: Int64): IList<TOrder>;
    procedure SeedSampleData;
  end;

  TOrderService = class(TInterfacedObject, IOrderService)
  private
    FDbContext: TQueryDbContext;
  public
    constructor Create(ADbContext: TQueryDbContext);
    function GetAllOrders: IList<TOrder>;
    function GetOrderById(Id: Int64): TOrder;
    function GetOrdersByStatus(const Status: string): IList<TOrder>;
    function GetOrdersByCustomer(CustomerId: Int64): IList<TOrder>;
    procedure SeedSampleData;
  end;

  /// <summary>
  ///   Service for generating reports with aggregations
  /// </summary>
  IReportService = interface
    ['{D2E3F4A5-B6C7-8901-2345-67890ABCDEF1}']
    function GetSalesReport: TList<TSalesReportItem>;
    function GetTopCustomers(Top: Integer): TList<TTopCustomerItem>;
  end;

  TReportService = class(TInterfacedObject, IReportService)
  private
    FDbContext: TQueryDbContext;
  public
    constructor Create(ADbContext: TQueryDbContext);
    function GetSalesReport: TList<TSalesReportItem>;
    function GetTopCustomers(Top: Integer): TList<TTopCustomerItem>;
  end;

implementation

{ TOrderService }

constructor TOrderService.Create(ADbContext: TQueryDbContext);
begin
  inherited Create;
  FDbContext := ADbContext;
end;

function TOrderService.GetAllOrders: IList<TOrder>;
begin
  Result := FDbContext.Orders.ToList;
end;

function TOrderService.GetOrderById(Id: Int64): TOrder;
begin
  Result := FDbContext.Orders.Find(Id);
end;

function TOrderService.GetOrdersByStatus(const Status: string): IList<TOrder>;
var
  AllOrders: IList<TOrder>;
  Order: TOrder;
begin
  Result := TCollections.CreateList<TOrder>;
  AllOrders := FDbContext.Orders.ToList;
  
  for Order in AllOrders do
  begin
    if string(Order.Status) = Status then
      Result.Add(Order);
  end;
end;

function TOrderService.GetOrdersByCustomer(CustomerId: Int64): IList<TOrder>;
var
  AllOrders: IList<TOrder>;
  Order: TOrder;
begin
  Result := TCollections.CreateList<TOrder>;
  AllOrders := FDbContext.Orders.ToList;
  
  for Order in AllOrders do
  begin
    if Order.CustomerId = CustomerId then
      Result.Add(Order);
  end;
end;

procedure TOrderService.SeedSampleData;
var
  Customer: TCustomer;
  Order: TOrder;
  Product: TProduct;
begin
  WriteLn('[Seed] Creating sample data...');
  
  // Create sample customers
  Customer := TCustomer.Create;
  Customer.Name := 'Acme Corporation';
  Customer.Email := 'contact@acme.com';
  Customer.Tags := '["enterprise", "vip"]';
  Customer.Metadata := '{"industry": "manufacturing", "size": "large"}';
  Customer.CreatedAt := Now;
  Customer.TotalSpent := 15000;
  FDbContext.Customers.Add(Customer);
  
  Customer := TCustomer.Create;
  Customer.Name := 'TechStart Inc';
  Customer.Email := 'hello@techstart.io';
  Customer.Tags := '["startup", "tech"]';
  Customer.Metadata := '{"industry": "software", "size": "small"}';
  Customer.CreatedAt := Now;
  Customer.TotalSpent := 3500;
  FDbContext.Customers.Add(Customer);
  
  Customer := TCustomer.Create;
  Customer.Name := 'Global Retail';
  Customer.Email := 'sales@globalretail.com';
  Customer.Tags := '["retail", "enterprise"]';
  Customer.Metadata := '{"industry": "retail", "size": "enterprise"}';
  Customer.CreatedAt := Now;
  Customer.TotalSpent := 42000;
  FDbContext.Customers.Add(Customer);
  
  // Create sample products
  Product := TProduct.Create;
  Product.Name := 'Widget Pro';
  Product.Category := 'Electronics';
  Product.Price := 299.99;
  Product.Stock := 150;
  Product.Attributes := '{"color": "black", "warranty": "2 years"}';
  FDbContext.Products.Add(Product);
  
  Product := TProduct.Create;
  Product.Name := 'Super Gadget';
  Product.Category := 'Electronics';
  Product.Price := 149.99;
  Product.Stock := 300;
  Product.Attributes := '{"color": "silver", "wireless": true}';
  FDbContext.Products.Add(Product);
  
  // Create sample orders
  Order := TOrder.Create;
  Order.CustomerId := 1;
  Order.OrderNumber := 'ORD-2024-001';
  Order.Status := 'delivered';
  Order.TotalAmount := 599.98;
  Order.Items := '[{"productId": 1, "qty": 2, "price": 299.99}]';
  Order.ShippingAddress := '{"street": "123 Main St", "city": "New York", "zip": "10001"}';
  Order.CreatedAt := IncDay(Now, -30);
  Order.UpdatedAt := IncDay(Now, -25);
  FDbContext.Orders.Add(Order);
  
  Order := TOrder.Create;
  Order.CustomerId := 2;
  Order.OrderNumber := 'ORD-2024-002';
  Order.Status := 'shipped';
  Order.TotalAmount := 449.97;
  Order.Items := '[{"productId": 2, "qty": 3, "price": 149.99}]';
  Order.ShippingAddress := '{"street": "456 Tech Ave", "city": "San Francisco", "zip": "94102"}';
  Order.CreatedAt := IncDay(Now, -5);
  Order.UpdatedAt := IncDay(Now, -1);
  FDbContext.Orders.Add(Order);
  
  Order := TOrder.Create;
  Order.CustomerId := 3;
  Order.OrderNumber := 'ORD-2024-003';
  Order.Status := 'pending';
  Order.TotalAmount := 1499.95;
  Order.Items := '[{"productId": 1, "qty": 5, "price": 299.99}]';
  Order.ShippingAddress := '{"street": "789 Retail Blvd", "city": "Chicago", "zip": "60601"}';
  Order.CreatedAt := Now;
  Order.UpdatedAt := Now;
  FDbContext.Orders.Add(Order);
  
  Order := TOrder.Create;
  Order.CustomerId := 1;
  Order.OrderNumber := 'ORD-2024-004';
  Order.Status := 'processing';
  Order.TotalAmount := 749.97;
  Order.Items := '[{"productId": 2, "qty": 5, "price": 149.99}]';
  Order.ShippingAddress := '{"street": "123 Main St", "city": "New York", "zip": "10001"}';
  Order.CreatedAt := IncDay(Now, -2);
  Order.UpdatedAt := Now;
  FDbContext.Orders.Add(Order);
  
  FDbContext.SaveChanges;
  
  WriteLn('[Seed] Created 3 customers, 2 products, 4 orders');
end;

{ TReportService }

constructor TReportService.Create(ADbContext: TQueryDbContext);
begin
  inherited Create;
  FDbContext := ADbContext;
end;

function TReportService.GetSalesReport: TList<TSalesReportItem>;
var
  Orders: IList<TOrder>;
  Order: TOrder;
  Item: TSalesReportItem;
  StatusMap: TDictionary<string, TSalesReportItem>;
  Status: string;
begin
  // Manual aggregation (demonstration - in production use SQL aggregations)
  Result := TList<TSalesReportItem>.Create;
  StatusMap := TDictionary<string, TSalesReportItem>.Create;
  
  try
    Orders := FDbContext.Orders.ToList;
    
    for Order in Orders do
    begin
      Status := string(Order.Status);
      if StatusMap.ContainsKey(Status) then
      begin
        Item := StatusMap[Status];
        Item.OrderCount := Item.OrderCount + 1;
        Item.TotalAmount := Item.TotalAmount + Currency(Order.TotalAmount);
        StatusMap[Status] := Item;
      end
      else
      begin
        Item.Status := Status;
        Item.OrderCount := 1;
        Item.TotalAmount := Currency(Order.TotalAmount);
        StatusMap.Add(Status, Item);
      end;
    end;
    
    for Item in StatusMap.Values do
      Result.Add(Item);
  finally
    StatusMap.Free;
  end;
end;

function TReportService.GetTopCustomers(Top: Integer): TList<TTopCustomerItem>;
var
  Customers: IList<TCustomer>;
  Customer: TCustomer;
  Item: TTopCustomerItem;
  Count: Integer;
begin
  Result := TList<TTopCustomerItem>.Create;
  
  // Get all customers (in production, use ORDER BY and LIMIT in SQL)
  Customers := FDbContext.Customers.ToList;
  
  Count := 0;
  for Customer in Customers do
  begin
    if Count >= Top then Break;
    
    Item.CustomerId := Customer.Id;
    Item.CustomerName := string(Customer.Name);
    Item.TotalSpent := Currency(Customer.TotalSpent);
    Item.OrderCount := 0; // Would need join to calculate
    Result.Add(Item);
    
    Inc(Count);
  end;
end;

end.
