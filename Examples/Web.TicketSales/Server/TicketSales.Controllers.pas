unit TicketSales.Controllers;

{***************************************************************************}
{                                                                           }
{           Web.TicketSales - API Controllers                               }
{                                                                           }
{           REST API Controllers following ASP.NET Core patterns            }
{                                                                           }
{***************************************************************************}

interface

uses
  System.SysUtils,
  System.JSON,
  Dext.Collections,
  Dext,
  Dext.Web,
  Dext.Web.Results,
  Dext.Json,
  TicketSales.Domain.Entities,
  TicketSales.Domain.Enums,
  TicketSales.Domain.Models,
  TicketSales.Services;

type
  // ===========================================================================
  // 🎭 Events Controller
  // ===========================================================================
  [ApiController, Route('/api/events')]
  TEventsController = class
  private
    FService: IEventService;
    FTicketTypeService: ITicketTypeService;
    
    function MapEventToResponse(Event: TEvent): TEventResponse;
  public
    constructor Create(EventService: IEventService; TicketTypeService: ITicketTypeService);

    /// <summary>Get all events</summary>
    [HttpGet]
    function GetAll: IResult;

    /// <summary>Get only available events (on sale, future)</summary>
    [HttpGet, Route('/available')]
    function GetAvailable: IResult;

    /// <summary>Get event by ID</summary>
    [HttpGet, Route('/{id}')]
    function GetById(Id: Integer): IResult;

    /// <summary>Get ticket types for an event</summary>
    [HttpGet, Route('/{id}/ticket-types')]
    function GetTicketTypes(Id: Integer): IResult;

    /// <summary>Create a new event</summary>
    [HttpPost]
    [Authorize]
    [ValidateModel]
    function CreateEvent([FromBody] Request: TCreateEventRequest): IResult;

    /// <summary>Update an event</summary>
    [HttpPut('/{id}')]
    [Authorize]
    function UpdateEvent(Id: Integer; [FromBody] Request: TUpdateEventRequest): IResult;

    /// <summary>Delete an event</summary>
    [HttpDelete, Route('/{id}')]
    [Authorize]
    function DeleteEvent(Id: Integer): IResult;

    /// <summary>Open ticket sales for an event</summary>
    [HttpPost, Route('/{id}/open-sales')]
    [Authorize]
    function OpenSales(Id: Integer): IResult;

    /// <summary>Close ticket sales for an event</summary>
    [HttpPost, Route('/{id}/close-sales')]
    [Authorize]
    function CloseSales(Id: Integer): IResult;
  end;

  // ===========================================================================
  // 🎫 Ticket Types Controller
  // ===========================================================================
  [ApiController('/api/ticket-types')]
  TTicketTypesController = class
  private
    FService: ITicketTypeService;
    
    function MapTicketTypeToResponse(TicketType: TTicketType): TTicketTypeResponse;
  public
    constructor Create(Service: ITicketTypeService);

    /// <summary>Get ticket type by ID</summary>
    [HttpGet('/{id}')]
    function GetById(Id: Integer): IResult;

    /// <summary>Create a new ticket type</summary>
    [HttpPost]
    [Authorize]
    [ValidateModel]
    function CreateTicketType([FromBody] Request: TCreateTicketTypeRequest): IResult;

    /// <summary>Delete a ticket type</summary>
    [HttpDelete('/{id}')]
    [Authorize]
    function DeleteTicketType(Id: Integer): IResult;
  end;

  // ===========================================================================
  // 👤 Customers Controller
  // ===========================================================================
  [ApiController('/api/customers')]
  TCustomersController = class
  private
    FService: ICustomerService;
    
    function MapCustomerToResponse(Customer: TCustomer): TCustomerResponse;
  public
    constructor Create(Service: ICustomerService);

    /// <summary>Get all customers</summary>
    [HttpGet]
    [Authorize]
    function GetAll: IResult;

    /// <summary>Get customer by ID</summary>
    [HttpGet('/{id}')]
    function GetById(Id: Integer): IResult;

    /// <summary>Register a new customer</summary>
    [HttpPost]
    [AllowAnonymous]
    [ValidateModel]
    function CreateCustomer([FromBody] Request: TCreateCustomerRequest): IResult;
  end;

  // ===========================================================================
  // 🛒 Orders Controller
  // ===========================================================================
  [ApiController('/api/orders')]
  TOrdersController = class
  private
    FService: IOrderService;
    FTicketService: ITicketService;
    
    function MapOrderToResponse(Order: TOrder): TOrderResponse;
  public
    constructor Create(OrderService: IOrderService; TicketService: ITicketService);

    /// <summary>Get all orders (admin)</summary>
    [HttpGet]
    [Authorize]
    function GetAll: IResult;

    /// <summary>Get order by ID</summary>
    [HttpGet('/{id}')]
    function GetById(Id: Integer): IResult;

    /// <summary>Get orders for a customer</summary>
    [HttpGet('/customer/{customerId}')]
    function GetByCustomerId(CustomerId: Integer): IResult;

    /// <summary>Create a new order</summary>
    [HttpPost]
    [ValidateModel]
    function CreateOrder([FromBody] Request: TCreateOrderRequest): IResult;

    /// <summary>Pay for an order</summary>
    [HttpPost('/{id}/pay')]
    function PayOrder(Id: Integer): IResult;

    /// <summary>Cancel an order</summary>
    [HttpPost('/{id}/cancel')]
    function CancelOrder(Id: Integer): IResult;

    /// <summary>Get tickets for an order</summary>
    [HttpGet('/{id}/tickets')]
    function GetOrderTickets(Id: Integer): IResult;
  end;

  // ===========================================================================
  // 🎟️ Tickets Controller
  // ===========================================================================
  [ApiController('/api/tickets')]
  TTicketsController = class
  private
    FService: ITicketService;
  public
    constructor Create(Service: ITicketService);

    /// <summary>Get ticket by code</summary>
    [HttpGet('/{code}')]
    function GetByCode(Code: string): IResult;

    /// <summary>Validate a ticket (mark as used)</summary>
    [HttpPost('/validate')]
    [ValidateModel]
    function ValidateTicket([FromBody] Request: TValidateTicketRequest): IResult;
  end;

  // ===========================================================================
  // ❤️ Health Controller
  // ===========================================================================
  [ApiController('/api/health')]
  THealthController = class
  public
    [HttpGet]
    [AllowAnonymous]
    function Check: IResult;
  end;

implementation

uses
  Dext.Entity,
  TicketSales.Data.Context;

// =============================================================================
// TEventsController
// =============================================================================

constructor TEventsController.Create(EventService: IEventService; TicketTypeService: ITicketTypeService);
begin
  inherited Create;
  FService := EventService;
  FTicketTypeService := TicketTypeService;
end;

function TEventsController.MapEventToResponse(Event: TEvent): TEventResponse;
begin
  Result.Id := Event.Id;
  Result.Name := Event.Name;
  Result.Description := Event.Description;
  Result.Venue := Event.Venue;
  Result.EventDate := Event.EventDate;
  Result.Capacity := Event.Capacity;
  Result.SoldCount := Event.SoldCount;
  Result.AvailableTickets := Event.AvailableTickets;
  
  case TEventStatus(Event.Status) of
    esScheduled: Result.Status := 'Scheduled';
    esOnSale: Result.Status := 'OnSale';
    esSoldOut: Result.Status := 'SoldOut';
    esCancelled: Result.Status := 'Cancelled';
    esCompleted: Result.Status := 'Completed';
  end;
end;

function TEventsController.GetAll: IResult;
var
  Events: IList<TEvent>;
  Response: TArray<TEventResponse>;
  I: Integer;
begin
  Events := FService.GetAll;
  SetLength(Response, Events.Count);
  
  for I := 0 to Events.Count - 1 do
    Response[I] := MapEventToResponse(Events[I]);
    
  Result := Results.Json(TDextJson.Serialize(Response));
end;

function TEventsController.GetAvailable: IResult;
var
  Events: IList<TEvent>;
  Response: TArray<TEventResponse>;
  I: Integer;
begin
  Events := FService.GetAvailable;
  SetLength(Response, Events.Count);
  
  for I := 0 to Events.Count - 1 do
    Response[I] := MapEventToResponse(Events[I]);
    
  Result := Results.Json(TDextJson.Serialize(Response));
end;

function TEventsController.GetById(Id: Integer): IResult;
var
  Event: TEvent;
begin
  try
    Event := FService.GetById(Id);
    Result := Results.Json(TDextJson.Serialize(MapEventToResponse(Event)));
  except
    on E: EEventNotFoundException do
      Result := Results.NotFound;
  end;
end;

function TEventsController.GetTicketTypes(Id: Integer): IResult;
var
  TicketTypes: IList<TTicketType>;
  Response: TArray<TTicketTypeResponse>;
  I: Integer;
begin
  try
    // Validate event exists first
    FService.GetById(Id);
    
    TicketTypes := FTicketTypeService.GetByEventId(Id);
    SetLength(Response, TicketTypes.Count);
    
    for I := 0 to TicketTypes.Count - 1 do
    begin
      Response[I].Id := TicketTypes[I].Id;
      Response[I].EventId := TicketTypes[I].EventId;
      Response[I].Name := TicketTypes[I].Name;
      Response[I].Description := TicketTypes[I].Description;
      Response[I].Price := TicketTypes[I].Price;
      Response[I].Quantity := TicketTypes[I].Quantity;
      Response[I].SoldCount := TicketTypes[I].SoldCount;
      Response[I].AvailableQuantity := TicketTypes[I].AvailableQuantity;
      Response[I].IsHalfPrice := TicketTypes[I].IsHalfPrice;
    end;
    
    Result := Results.Json(TDextJson.Serialize(Response));
  except
    on E: EEventNotFoundException do
      Result := Results.NotFound;
  end;
end;

function TEventsController.CreateEvent(Request: TCreateEventRequest): IResult;
var
  Event: TEvent;
begin
  try
    Event := FService.CreateEvent(Request);
    Result := Results.Created<TEventResponse>('/api/events/' + IntToStr(Integer(Event.Id)), MapEventToResponse(Event));
  except
    on E: ETicketSalesException do
      Result := Results.BadRequest(E.Message);
  end;
end;

function TEventsController.UpdateEvent(Id: Integer; Request: TUpdateEventRequest): IResult;
var
  Event: TEvent;
begin
  try
    Event := FService.Update(Id, Request);
    Result := Results.Json(TDextJson.Serialize(MapEventToResponse(Event)));
  except
    on E: EEventNotFoundException do
      Result := Results.NotFound;
    on E: ETicketSalesException do
      Result := Results.BadRequest(E.Message);
  end;
end;

function TEventsController.DeleteEvent(Id: Integer): IResult;
begin
  try
    FService.Delete(Id);
    Result := Results.NoContent;
  except
    on E: EEventNotFoundException do
      Result := Results.NotFound;
    on E: ETicketSalesException do
      Result := Results.BadRequest(E.Message);
  end;
end;

function TEventsController.OpenSales(Id: Integer): IResult;
begin
  try
    FService.OpenSales(Id);
    Result := Results.Ok;
  except
    on E: EEventNotFoundException do
      Result := Results.NotFound;
    on E: ETicketSalesException do
      Result := Results.BadRequest(E.Message);
  end;
end;

function TEventsController.CloseSales(Id: Integer): IResult;
begin
  try
    FService.CloseSales(Id);
    Result := Results.Ok;
  except
    on E: EEventNotFoundException do
      Result := Results.NotFound;
  end;
end;

// =============================================================================
// TTicketTypesController
// =============================================================================

constructor TTicketTypesController.Create(Service: ITicketTypeService);
begin
  inherited Create;
  FService := Service;
end;

function TTicketTypesController.MapTicketTypeToResponse(TicketType: TTicketType): TTicketTypeResponse;
begin
  Result.Id := TicketType.Id;
  Result.EventId := TicketType.EventId;
  Result.Name := TicketType.Name;
  Result.Description := TicketType.Description;
  Result.Price := TicketType.Price;
  Result.Quantity := TicketType.Quantity;
  Result.SoldCount := TicketType.SoldCount;
  Result.AvailableQuantity := TicketType.AvailableQuantity;
  Result.IsHalfPrice := TicketType.IsHalfPrice;
end;

function TTicketTypesController.GetById(Id: Integer): IResult;
var
  TicketType: TTicketType;
begin
  try
    TicketType := FService.GetById(Id);
    Result := Results.Json(MapTicketTypeToResponse(TicketType));
  except
    on E: ETicketTypeNotFoundException do
      Result := Results.NotFound;
  end;
end;

function TTicketTypesController.CreateTicketType(Request: TCreateTicketTypeRequest): IResult;
var
  TicketType: TTicketType;
begin
  try
    TicketType := FService.CreateTicketType(Request);
    Result := Results.Created<TTicketTypeResponse>('/api/ticket-types/' + IntToStr(Integer(TicketType.Id)), MapTicketTypeToResponse(TicketType));
  except
    on E: EEventNotFoundException do
      Result := Results.BadRequest(E.Message);
    on E: ETicketSalesException do
      Result := Results.BadRequest(E.Message);
  end;
end;

function TTicketTypesController.DeleteTicketType(Id: Integer): IResult;
begin
  try
    FService.Delete(Id);
    Result := Results.NoContent;
  except
    on E: ETicketTypeNotFoundException do
      Result := Results.NotFound;
    on E: ETicketSalesException do
      Result := Results.BadRequest(E.Message);
  end;
end;

// =============================================================================
// TCustomersController
// =============================================================================

constructor TCustomersController.Create(Service: ICustomerService);
begin
  inherited Create;
  FService := Service;
end;

function TCustomersController.MapCustomerToResponse(Customer: TCustomer): TCustomerResponse;
begin
  Result.Id := Customer.Id;
  Result.Name := Customer.Name;
  Result.Email := Customer.Email;
  Result.CPF := Customer.CPF;
  Result.IsHalfPriceEligible := Customer.IsHalfPriceEligible;
  
  case TCustomerType(Customer.CustomerType) of
    ctRegular: Result.CustomerType := 'Regular';
    ctStudent: Result.CustomerType := 'Student';
    ctSenior: Result.CustomerType := 'Senior';
    ctChild: Result.CustomerType := 'Child';
  end;
end;

function TCustomersController.GetAll: IResult;
var
  Customers: IList<TCustomer>;
  Response: TArray<TCustomerResponse>;
  I: Integer;
begin
  Customers := FService.GetAll;
  SetLength(Response, Customers.Count);
  
  for I := 0 to Customers.Count - 1 do
    Response[I] := MapCustomerToResponse(Customers[I]);
    
  Result := Results.Json(TDextJson.Serialize(Response));
end;

function TCustomersController.GetById(Id: Integer): IResult;
var
  Customer: TCustomer;
begin
  try
    Customer := FService.GetById(Id);
    Result := Results.Json(MapCustomerToResponse(Customer));
  except
    on E: ECustomerNotFoundException do
      Result := Results.NotFound;
  end;
end;

function TCustomersController.CreateCustomer(Request: TCreateCustomerRequest): IResult;
var
  Customer: TCustomer;
begin
  try
    Customer := FService.CreateCustomer(Request);
    Result := Results.Created<TCustomerResponse>('/api/customers/' + IntToStr(Integer(Customer.Id)), MapCustomerToResponse(Customer));
  except
    on E: ETicketSalesException do
      Result := Results.BadRequest(E.Message);
  end;
end;

// =============================================================================
// TOrdersController
// =============================================================================

constructor TOrdersController.Create(OrderService: IOrderService; TicketService: ITicketService);
begin
  inherited Create;
  FService := OrderService;
  FTicketService := TicketService;
end;

function TOrdersController.MapOrderToResponse(Order: TOrder): TOrderResponse;
var
  I: Integer;
begin
  Result.Id := Order.Id;
  Result.CustomerId := Order.CustomerId;
  Result.CustomerName := ''; // Would need customer lookup
  Result.Total := Order.Total;
  Result.CreatedAt := Order.CreatedAt;
  
  case TOrderStatus(Order.Status) of
    osPending: Result.Status := 'Pending';
    osPaid: Result.Status := 'Paid';
    osCompleted: Result.Status := 'Completed';
    osCancelled: Result.Status := 'Cancelled';
    osRefunded: Result.Status := 'Refunded';
  end;
  
  SetLength(Result.Items, Order.Items.Count);
  for I := 0 to Order.Items.Count - 1 do
  begin
    Result.Items[I].Id := Order.Items[I].Id;
    Result.Items[I].TicketTypeName := ''; // Would need lookup
    Result.Items[I].Quantity := Order.Items[I].Quantity;
    Result.Items[I].UnitPrice := Order.Items[I].UnitPrice;
    Result.Items[I].IsHalfPrice := Order.Items[I].IsHalfPrice;
    Result.Items[I].Total := Order.Items[I].Total;
  end;
end;

function TOrdersController.GetAll: IResult;
var
  Orders: IList<TOrder>;
  Response: TArray<TOrderResponse>;
  I: Integer;
begin
  Orders := FService.GetAll;
  SetLength(Response, Orders.Count);
  
  for I := 0 to Orders.Count - 1 do
    Response[I] := MapOrderToResponse(Orders[I]);
    
  Result := Results.Json(TDextJson.Serialize(Response));
end;

function TOrdersController.GetById(Id: Integer): IResult;
var
  Order: TOrder;
begin
  try
    Order := FService.GetById(Id);
    Result := Results.Json(MapOrderToResponse(Order));
  except
    on E: EOrderNotFoundException do
      Result := Results.NotFound;
  end;
end;

function TOrdersController.GetByCustomerId(CustomerId: Integer): IResult;
var
  Orders: IList<TOrder>;
  Response: TArray<TOrderResponse>;
  I: Integer;
begin
  Orders := FService.GetByCustomerId(CustomerId);
  SetLength(Response, Orders.Count);
  
  for I := 0 to Orders.Count - 1 do
    Response[I] := MapOrderToResponse(Orders[I]);
    
  Result := Results.Json(TDextJson.Serialize(Response));
end;

function TOrdersController.CreateOrder(Request: TCreateOrderRequest): IResult;
var
  Order: TOrder;
begin
  try
    Order := FService.CreateOrder(Request);
    Result := Results.Created<TOrderResponse>('/api/orders/' + IntToStr(Integer(Order.Id)), MapOrderToResponse(Order));
  except
    on E: ETicketSalesException do
      Result := Results.BadRequest(E.Message);
  end;
end;

function TOrdersController.PayOrder(Id: Integer): IResult;
begin
  try
    FService.Pay(Id);
    Result := Results.Ok;
  except
    on E: EOrderNotFoundException do
      Result := Results.NotFound;
    on E: ETicketSalesException do
      Result := Results.BadRequest(E.Message);
  end;
end;

function TOrdersController.CancelOrder(Id: Integer): IResult;
begin
  try
    FService.Cancel(Id);
    Result := Results.Ok;
  except
    on E: EOrderNotFoundException do
      Result := Results.NotFound;
    on E: ETicketSalesException do
      Result := Results.BadRequest(E.Message);
  end;
end;

function TOrdersController.GetOrderTickets(Id: Integer): IResult;
var
  Tickets: IList<TTicket>;
  Response: TArray<TTicketResponse>;
  I: Integer;
begin
  try
    // Validate order exists
    FService.GetById(Id);
    
    Tickets := FTicketService.GetByOrderId(Id);
    SetLength(Response, Tickets.Count);
    
    for I := 0 to Tickets.Count - 1 do
    begin
      Response[I].Id := Tickets[I].Id;
      Response[I].Code := Tickets[I].Code;
      Response[I].EventName := ''; // Would need lookup
      Response[I].TicketTypeName := '';
      Response[I].CustomerName := '';
      
      case TTicketStatus(Tickets[I].Status) of
        tsValid: Response[I].Status := 'Valid';
        tsUsed: Response[I].Status := 'Used';
        tsCancelled: Response[I].Status := 'Cancelled';
        tsExpired: Response[I].Status := 'Expired';
      end;
    end;
    
    Result := Results.Json(TDextJson.Serialize(Response));
  except
    on E: EOrderNotFoundException do
      Result := Results.NotFound;
  end;
end;

// =============================================================================
// TTicketsController
// =============================================================================

constructor TTicketsController.Create(Service: ITicketService);
begin
  inherited Create;
  FService := Service;
end;

function TTicketsController.GetByCode(Code: string): IResult;
var
  Ticket: TTicket;
  Response: TTicketResponse;
begin
  Ticket := FService.GetByCode(Code);
  if Ticket = nil then
    Exit(Results.NotFound);
  
  Response.Id := Ticket.Id;
  Response.Code := Ticket.Code;
  
  case TTicketStatus(Ticket.Status) of
    tsValid: Response.Status := 'Valid';
    tsUsed: Response.Status := 'Used';
    tsCancelled: Response.Status := 'Cancelled';
    tsExpired: Response.Status := 'Expired';
  end;
  
  Result := Results.Json(TDextJson.Serialize(Response));
end;

function TTicketsController.ValidateTicket(Request: TValidateTicketRequest): IResult;
var
  Response: TValidateTicketResponse;
begin
  Response := FService.Validate(Request.Code);
  
  if Response.Valid then
    Result := Results.Json(Response)
  else
    Result := Results.BadRequest(Response);
end;

// =============================================================================
// THealthController
// =============================================================================

function THealthController.Check: IResult;
var
  Json: System.JSON.TJSONObject;
begin
  Json := System.JSON.TJSONObject.Create;
  try
    Json.AddPair('status', 'healthy');
    Json.AddPair('service', 'Web.TicketSales');
    Json.AddPair('version', '1.0.0');
    Result := Results.Json(Json.ToString);
  finally
    Json.Free;
  end;
end;

initialization
  // Force RTTI linkage
  TEventsController.ClassName;
  TTicketTypesController.ClassName;
  TCustomersController.ClassName;
  TOrdersController.ClassName;
  TTicketsController.ClassName;
  THealthController.ClassName;

end.
