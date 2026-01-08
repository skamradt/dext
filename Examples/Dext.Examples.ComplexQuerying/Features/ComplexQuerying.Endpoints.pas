unit ComplexQuerying.Endpoints;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Dext.Web,
  Dext.Web.Results,
  Dext.Json,
  Dext.Collections,
  ComplexQuerying.Entities,
  ComplexQuerying.Service;

type
  /// <summary>
  ///   Request DTO for order ID route parameter
  /// </summary>
  TOrderIdRequest = record
    [FromRoute('id')]
    Id: Int64;
  end;

  /// <summary>
  ///   Request DTO for status route parameter
  /// </summary>
  TStatusRequest = record
    [FromRoute('status')]
    Status: string;
  end;

  /// <summary>
  ///   Request DTO for customer ID route parameter
  /// </summary>
  TCustomerIdRequest = record
    [FromRoute('customerId')]
    CustomerId: Int64;
  end;

  /// <summary>
  ///   Request DTO for top customers query parameter
  /// </summary>
  TTopCustomersRequest = record
    [FromQuery('top')]
    Top: Integer;
  end;

  TComplexQueryingEndpoints = class
  public
    class procedure Map(const App: TDextAppBuilder);
  end;

implementation

{ TComplexQueryingEndpoints }

class procedure TComplexQueryingEndpoints.Map(const App: TDextAppBuilder);
begin
  // ============================================================
  // ORDER ENDPOINTS
  // ============================================================

  // GET /api/orders - List all orders
  App.MapGet<IOrderService, IResult>('/api/orders',
    function(Service: IOrderService): IResult
    var
      Orders: IList<TOrder>;
    begin
      Orders := Service.GetAllOrders;
      Result := Results.Ok<IList<TOrder>>(Orders);
    end);

  // GET /api/orders/{id} - Get order by ID
  App.MapGet<IOrderService, TOrderIdRequest, IResult>('/api/orders/{id}',
    function(Service: IOrderService; Request: TOrderIdRequest): IResult
    var
      Order: TOrder;
    begin
      Order := Service.GetOrderById(Request.Id);
      
      if Order = nil then
        Result := Results.NotFound('Order not found')
      else
        Result := Results.Ok(Order);
    end);

  // GET /api/orders/status/{status} - Get orders by status
  App.MapGet<IOrderService, TStatusRequest, IResult>('/api/orders/status/{status}',
    function(Service: IOrderService; Request: TStatusRequest): IResult
    var
      Orders: IList<TOrder>;
    begin
      Orders := Service.GetOrdersByStatus(Request.Status);
      Result := Results.Ok<IList<TOrder>>(Orders);
    end);

  // GET /api/orders/customer/{customerId} - Get orders by customer
  App.MapGet<IOrderService, TCustomerIdRequest, IResult>('/api/orders/customer/{customerId}',
    function(Service: IOrderService; Request: TCustomerIdRequest): IResult
    var
      Orders: IList<TOrder>;
    begin
      Orders := Service.GetOrdersByCustomer(Request.CustomerId);
      Result := Results.Ok<IList<TOrder>>(Orders);
    end);

  // ============================================================
  // REPORT ENDPOINTS
  // ============================================================

  // GET /api/reports/sales - Sales report by status
  App.MapGet<IReportService, IResult>('/api/reports/sales',
    function(Service: IReportService): IResult
    var
      Report: TList<TSalesReportItem>;
    begin
      Report := Service.GetSalesReport;
      try
        Result := Results.Ok<TList<TSalesReportItem>>(Report);
      finally
        Report.Free;
      end;
    end);

  // GET /api/reports/top-customers - Top customers by spending
  App.MapGet<IReportService, TTopCustomersRequest, IResult>('/api/reports/top-customers',
    function(Service: IReportService; Request: TTopCustomersRequest): IResult
    var
      Top: Integer;
      Report: TList<TTopCustomerItem>;
    begin
      Top := Request.Top;
      if Top <= 0 then Top := 10;
      
      Report := Service.GetTopCustomers(Top);
      try
        Result := Results.Ok<TList<TTopCustomerItem>>(Report);
      finally
        Report.Free;
      end;
    end);

  // ============================================================
  // SEED ENDPOINT
  // ============================================================

  // POST /api/seed - Seed sample data
  App.MapPost<IOrderService, IResult>('/api/seed',
    function(Service: IOrderService): IResult
    begin
      Service.SeedSampleData;
      Result := Results.Ok('Sample data seeded successfully');
    end);
end;

end.
