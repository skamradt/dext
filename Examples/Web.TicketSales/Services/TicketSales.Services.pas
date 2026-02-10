unit TicketSales.Services;

{***************************************************************************}
{                                                                           }
{           Web.TicketSales - Business Services                             }
{                                                                           }
{           Business logic layer with validation and rules                  }
{                                                                           }
{***************************************************************************}

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Dext,
  Dext.Collections,
  TicketSales.Domain.Entities,
  TicketSales.Domain.Enums,
  TicketSales.Domain.Models,
  TicketSales.Data.Context;

type
  // ===========================================================================
  // Exception Types for Business Rules
  // ===========================================================================
  
  ETicketSalesException = class(Exception);
  EEventNotFoundException = class(ETicketSalesException);
  ETicketTypeNotFoundException = class(ETicketSalesException);
  ECustomerNotFoundException = class(ETicketSalesException);
  EOrderNotFoundException = class(ETicketSalesException);
  ETicketNotFoundException = class(ETicketSalesException);
  EInsufficientStockException = class(ETicketSalesException);
  EEventNotAvailableException = class(ETicketSalesException);
  EHalfPriceNotAllowedException = class(ETicketSalesException);
  EOrderAlreadyPaidException = class(ETicketSalesException);
  EOrderCannotBeCancelledException = class(ETicketSalesException);
  ETicketAlreadyUsedException = class(ETicketSalesException);
  EMaxTicketsPerCustomerException = class(ETicketSalesException);

  // ===========================================================================
  // Service Interfaces
  // ===========================================================================

  {$M+}
  IEventService = interface
    ['{A1B2C3D4-E5F6-1234-5678-9ABCDEF01234}']
    function GetAll: IList<TEvent>;
    function GetById(Id: Integer): TEvent;
    function GetAvailable: IList<TEvent>;
    function CreateEvent(const Request: TCreateEventRequest): TEvent;
    function Update(Id: Integer; const Request: TUpdateEventRequest): TEvent;
    procedure Delete(Id: Integer);
    procedure OpenSales(Id: Integer);
    procedure CloseSales(Id: Integer);
  end;

  ITicketTypeService = interface
    ['{B2C3D4E5-F6A7-2345-6789-ABCDEF012345}']
    function GetByEventId(EventId: Integer): IList<TTicketType>;
    function GetById(Id: Integer): TTicketType;
    function CreateTicketType(const Request: TCreateTicketTypeRequest): TTicketType;
    procedure Delete(Id: Integer);
  end;

  ICustomerService = interface
    ['{C3D4E5F6-A7B8-3456-789A-BCDEF0123456}']
    function GetAll: IList<TCustomer>;
    function GetById(Id: Integer): TCustomer;
    function GetByEmail(const Email: string): TCustomer;
    function CreateCustomer(const Request: TCreateCustomerRequest): TCustomer;
  end;

  IOrderService = interface
    ['{D4E5F6A7-B8C9-4567-89AB-CDEF01234567}']
    function GetAll: IList<TOrder>;
    function GetById(Id: Integer): TOrder;
    function GetByCustomerId(CustomerId: Integer): IList<TOrder>;
    function CreateOrder(const Request: TCreateOrderRequest): TOrder;
    procedure Pay(Id: Integer);
    procedure Cancel(Id: Integer);
  end;

  ITicketService = interface
    ['{E5F6A7B8-C9D0-5678-9ABC-DEF012345678}']
    function GetByOrderId(OrderId: Integer): IList<TTicket>;
    function GetByCode(const Code: string): TTicket;
    function Validate(const Code: string): TValidateTicketResponse;
  end;
  {$M-}

  // ===========================================================================
  // Service Implementations
  // ===========================================================================

  TEventService = class(TInterfacedObject, IEventService)
  private
    FDb: TTicketSalesDbContext;
  public
    constructor Create(Db: TTicketSalesDbContext);
    
    function GetAll: IList<TEvent>;
    function GetById(Id: Integer): TEvent;
    function GetAvailable: IList<TEvent>;
    function CreateEvent(const Request: TCreateEventRequest): TEvent;
    function Update(Id: Integer; const Request: TUpdateEventRequest): TEvent;
    procedure Delete(Id: Integer);
    procedure OpenSales(Id: Integer);
    procedure CloseSales(Id: Integer);
  end;

  TTicketTypeService = class(TInterfacedObject, ITicketTypeService)
  private
    FDb: TTicketSalesDbContext;
  public
    constructor Create(Db: TTicketSalesDbContext);
    
    function GetByEventId(EventId: Integer): IList<TTicketType>;
    function GetById(Id: Integer): TTicketType;
    function CreateTicketType(const Request: TCreateTicketTypeRequest): TTicketType;
    procedure Delete(Id: Integer);
  end;

  TCustomerService = class(TInterfacedObject, ICustomerService)
  private
    FDb: TTicketSalesDbContext;
  public
    constructor Create(Db: TTicketSalesDbContext);
    
    function GetAll: IList<TCustomer>;
    function GetById(Id: Integer): TCustomer;
    function GetByEmail(const Email: string): TCustomer;
    function CreateCustomer(const Request: TCreateCustomerRequest): TCustomer;
  end;

  TOrderService = class(TInterfacedObject, IOrderService)
  private
    FDb: TTicketSalesDbContext;
    const MAX_TICKETS_PER_CUSTOMER = 10;
    
    procedure ValidateOrder(const Request: TCreateOrderRequest; Customer: TCustomer);
    procedure UpdateStock(Order: TOrder; Increment: Boolean);
    procedure GenerateTickets(Order: TOrder);
    function GenerateTicketCode: string;
  public
    constructor Create(Db: TTicketSalesDbContext);
    
    function GetAll: IList<TOrder>;
    function GetById(Id: Integer): TOrder;
    function GetByCustomerId(CustomerId: Integer): IList<TOrder>;
    function CreateOrder(const Request: TCreateOrderRequest): TOrder;
    procedure Pay(Id: Integer);
    procedure Cancel(Id: Integer);
  end;

  TTicketService = class(TInterfacedObject, ITicketService)
  private
    FDb: TTicketSalesDbContext;
  public
    constructor Create(Db: TTicketSalesDbContext);
    
    function GetByOrderId(OrderId: Integer): IList<TTicket>;
    function GetByCode(const Code: string): TTicket;
    function Validate(const Code: string): TValidateTicketResponse;
  end;

implementation

uses
  System.DateUtils;

// =============================================================================
// TEventService
// =============================================================================

constructor TEventService.Create(Db: TTicketSalesDbContext);
begin
  inherited Create;
  FDb := Db;
end;

function TEventService.GetAll: IList<TEvent>;
begin
  Result := FDb.Events.QueryAll.ToList;
end;

function TEventService.GetById(Id: Integer): TEvent;
begin
  Result := FDb.Events.Find(Id);
  if Result = nil then
    raise EEventNotFoundException.CreateFmt('Event with ID %d not found', [Id]);
end;

function TEventService.GetAvailable: IList<TEvent>;
var
  e: TEvent;
begin
  e := TEvent.Props;
  Result := FDb.Events
    .Where((e.Status = TEventStatus.esOnSale) and (e.EventDate > Now))
    .OrderBy(e.EventDate.Asc)
    .ToList;
end;

function TEventService.CreateEvent(const Request: TCreateEventRequest): TEvent;
begin
  // Validate event date is in the future
  if Request.EventDate <= Now then
    raise EEventNotAvailableException.Create('Event date must be in the future');

  Result := TEvent.Create;
  Result.Name := Request.Name;
  Result.Description := Request.Description;
  Result.Venue := Request.Venue;
  Result.EventDate := Request.EventDate;
  Result.Capacity := Request.Capacity;
  Result.SoldCount := 0;
  Result.Status := esScheduled;

  FDb.Events.Add(Result);
  FDb.SaveChanges;
end;

function TEventService.Update(Id: Integer; const Request: TUpdateEventRequest): TEvent;
begin
  Result := GetById(Id);

  if Request.Name <> '' then
    Result.Name := Request.Name;
  if Request.Description <> '' then
    Result.Description := Request.Description;
  if Request.Venue <> '' then
    Result.Venue := Request.Venue;
  if Request.EventDate > 0 then
    Result.EventDate := Request.EventDate;

  FDb.Events.Update(Result);
  FDb.SaveChanges;
end;

procedure TEventService.Delete(Id: Integer);
var
  Event: TEvent;
begin
  Event := GetById(Id);
  
  // Cannot delete if tickets were sold
  if Integer(Event.SoldCount) > 0 then
    raise ETicketSalesException.Create('Cannot delete event with sold tickets');

  FDb.Events.Remove(Event);
  FDb.SaveChanges;
end;

procedure TEventService.OpenSales(Id: Integer);
var
  Event: TEvent;
begin
  Event := GetById(Id);
  
  if TEventStatus(Event.Status) <> esScheduled then
    raise EEventNotAvailableException.Create('Can only open sales for scheduled events');
  
  Event.Status := esOnSale;
  FDb.Events.Update(Event);
  FDb.SaveChanges;
end;

procedure TEventService.CloseSales(Id: Integer);
var
  Event: TEvent;
begin
  Event := GetById(Id);
  Event.Status := esSoldOut;
  FDb.Events.Update(Event);
  FDb.SaveChanges;
end;

// =============================================================================
// TTicketTypeService
// =============================================================================

constructor TTicketTypeService.Create(Db: TTicketSalesDbContext);
begin
  inherited Create;
  FDb := Db;
end;

function TTicketTypeService.GetByEventId(EventId: Integer): IList<TTicketType>;
var
  tt: TTicketType;
begin
  tt := TTicketType.Props;
  Result := FDb.TicketTypes
    .Where(tt.EventId = EventId)
    .ToList;
end;

function TTicketTypeService.GetById(Id: Integer): TTicketType;
begin
  Result := FDb.TicketTypes.Find(Id);
  if Result = nil then
    raise ETicketTypeNotFoundException.CreateFmt('Ticket type with ID %d not found', [Id]);
end;

function TTicketTypeService.CreateTicketType(const Request: TCreateTicketTypeRequest): TTicketType;
begin
  // Validate event exists
  var Event := FDb.Events.Find(Request.EventId);
  if Event = nil then
    raise EEventNotFoundException.CreateFmt('Event with ID %d not found', [Request.EventId]);

  Result := TTicketType.Create;
  Result.EventId := Request.EventId;
  Result.Name := Request.Name;
  Result.Description := Request.Description;
  Result.Price := Request.Price;
  Result.Quantity := Request.Quantity;
  Result.SoldCount := 0;
  Result.IsHalfPrice := Request.IsHalfPrice;

  FDb.TicketTypes.Add(Result);
  FDb.SaveChanges;
end;

procedure TTicketTypeService.Delete(Id: Integer);
var
  TicketType: TTicketType;
begin
  TicketType := GetById(Id);
  
  if Integer(TicketType.SoldCount) > 0 then
    raise ETicketSalesException.Create('Cannot delete ticket type with sold tickets');

  FDb.TicketTypes.Remove(TicketType);
  FDb.SaveChanges;
end;

// =============================================================================
// TCustomerService
// =============================================================================

constructor TCustomerService.Create(Db: TTicketSalesDbContext);
begin
  inherited Create;
  FDb := Db;
end;

function TCustomerService.GetAll: IList<TCustomer>;
begin
  Result := FDb.Customers.QueryAll.ToList;
end;

function TCustomerService.GetById(Id: Integer): TCustomer;
begin
  Result := FDb.Customers.Find(Id);
  if Result = nil then
    raise ECustomerNotFoundException.CreateFmt('Customer with ID %d not found', [Id]);
end;

function TCustomerService.GetByEmail(const Email: string): TCustomer;
var
  c: TCustomer;
begin
  c := TCustomer.Props;
  Result := FDb.Customers
    .Where(c.Email = Email)
    .FirstOrDefault;
end;

function TCustomerService.CreateCustomer(const Request: TCreateCustomerRequest): TCustomer;
begin
  // Check if email already exists
  if GetByEmail(Request.Email) <> nil then
    raise ETicketSalesException.Create('Customer with this email already exists');

  Result := TCustomer.Create;
  Result.Name := Request.Name;
  Result.Email := Request.Email;
  Result.CPF := Request.CPF;
  Result.CustomerType := Request.CustomerType;

  FDb.Customers.Add(Result);
  FDb.SaveChanges;
end;

// =============================================================================
// TOrderService
// =============================================================================

constructor TOrderService.Create(Db: TTicketSalesDbContext);
begin
  inherited Create;
  FDb := Db;
end;

function TOrderService.GetAll: IList<TOrder>;
begin
  Result := FDb.Orders.QueryAll.ToList;
end;

function TOrderService.GetById(Id: Integer): TOrder;
begin
  Result := FDb.Orders.Find(Id);
  if Result = nil then
    raise EOrderNotFoundException.CreateFmt('Order with ID %d not found', [Id]);
end;

function TOrderService.GetByCustomerId(CustomerId: Integer): IList<TOrder>;
var
  o: TOrder;
begin
  o := TOrder.Props;
  Result := FDb.Orders
    .Where(o.CustomerId = CustomerId)
    .OrderBy(o.CreatedAt.Desc)
    .ToList;
end;

procedure TOrderService.ValidateOrder(const Request: TCreateOrderRequest; Customer: TCustomer);
var
  ItemReq: TOrderItemRequest;
  TicketType: TTicketType;
  Event: TEvent;
  TotalQuantity: Integer;
begin
  TotalQuantity := 0;

  for ItemReq in Request.Items do
  begin
    TicketType := FDb.TicketTypes.Find(ItemReq.TicketTypeId);
    if TicketType = nil then
      raise ETicketTypeNotFoundException.CreateFmt('Ticket type %d not found', [ItemReq.TicketTypeId]);

    Event := FDb.Events.Find(Integer(TicketType.EventId));
    if Event = nil then
      raise EEventNotFoundException.Create('Event not found for ticket type');

    // Rule 1: Event must be on sale
    if TEventStatus(Event.Status) <> esOnSale then
      raise EEventNotAvailableException.CreateFmt('Event "%s" is not available for sale', [string(Event.Name)]);

    // Rule 2: Event must be in the future
    if TDateTime(Event.EventDate) <= Now then
      raise EEventNotAvailableException.Create('Cannot purchase tickets for past events');

    // Rule 3: Check stock availability
    if ItemReq.Quantity > TicketType.AvailableQuantity then
      raise EInsufficientStockException.CreateFmt(
        'Insufficient stock for "%s". Available: %d, Requested: %d',
        [string(TicketType.Name), TicketType.AvailableQuantity, ItemReq.Quantity]);

    // Rule 4: Check half-price eligibility
    if Customer.IsHalfPriceEligible and not Boolean(TicketType.IsHalfPrice) then
      raise EHalfPriceNotAllowedException.CreateFmt(
        'Ticket type "%s" does not allow half-price discount', [string(TicketType.Name)]);

    TotalQuantity := TotalQuantity + ItemReq.Quantity;
  end;

  // Rule 5: Max tickets per customer
  if TotalQuantity > MAX_TICKETS_PER_CUSTOMER then
    raise EMaxTicketsPerCustomerException.CreateFmt(
      'Maximum %d tickets per order. Requested: %d', [MAX_TICKETS_PER_CUSTOMER, TotalQuantity]);
end;

function TOrderService.CreateOrder(const Request: TCreateOrderRequest): TOrder;
var
  Customer: TCustomer;
  ItemReq: TOrderItemRequest;
  TicketType: TTicketType;
  OrderItem: TOrderItem;
begin
  // Get and validate customer
  Customer := FDb.Customers.Find(Request.CustomerId);
  if Customer = nil then
    raise ECustomerNotFoundException.CreateFmt('Customer with ID %d not found', [Request.CustomerId]);

  // Validate all business rules
  ValidateOrder(Request, Customer);

  // Create order
  Result := TOrder.Create;
  Result.CustomerId := Request.CustomerId;
  Result.Status := osPending;
  Result.Total := 0;

  FDb.Orders.Add(Result);
  FDb.SaveChanges;

  // Create order items
  for ItemReq in Request.Items do
  begin
    TicketType := FDb.TicketTypes.Find(ItemReq.TicketTypeId);

    OrderItem := TOrderItem.Create;
    OrderItem.OrderId := Result.Id;
    OrderItem.TicketTypeId := ItemReq.TicketTypeId;
    OrderItem.Quantity := ItemReq.Quantity;
    OrderItem.CalculatePricing(
      Currency(TicketType.Price),
      Customer.IsHalfPriceEligible and Boolean(TicketType.IsHalfPrice)
    );

    FDb.OrderItems.Add(OrderItem);
    Result.Items.Add(OrderItem);
  end;

  // Calculate total
  Result.CalculateTotal;
  FDb.Orders.Update(Result);
  FDb.SaveChanges;

  // Update stock (reserve)
  UpdateStock(Result, False);
end;

procedure TOrderService.UpdateStock(Order: TOrder; Increment: Boolean);
var
  Item: TOrderItem;
  oi: TOrderItem;
  TicketType: TTicketType;
  Event: TEvent;
  Items: IList<TOrderItem>;
  Delta: Integer;
begin
  // Load items if not loaded
  oi := TOrderItem.Props;
  Items := FDb.OrderItems
    .Where(oi.OrderId = Integer(Order.Id))
    .ToList;

  for Item in Items do
  begin
    TicketType := FDb.TicketTypes.Find(Integer(Item.TicketTypeId));
    Event := FDb.Events.Find(Integer(TicketType.EventId));

    if Increment then
      Delta := -Integer(Item.Quantity)  // Returning stock
    else
      Delta := Integer(Item.Quantity);  // Reserving stock

    TicketType.SoldCount := Integer(TicketType.SoldCount) + Delta;
    Event.SoldCount := Integer(Event.SoldCount) + Delta;

    FDb.TicketTypes.Update(TicketType);
    FDb.Events.Update(Event);
  end;

  FDb.SaveChanges;
end;

procedure TOrderService.GenerateTickets(Order: TOrder);
var
  Item: TOrderItem;
  oi: TOrderItem;
  Items: IList<TOrderItem>;
  Ticket: TTicket;
  I: Integer;
begin
  oi := TOrderItem.Props;
  Items := FDb.OrderItems
    .Where(oi.OrderId = Integer(Order.Id))
    .ToList;

  for Item in Items do
  begin
    for I := 1 to Integer(Item.Quantity) do
    begin
      Ticket := TTicket.Create;
      Ticket.OrderItemId := Item.Id;
      Ticket.Code := GenerateTicketCode;
      Ticket.Status := tsValid;
      FDb.Tickets.Add(Ticket);
    end;
  end;

  FDb.SaveChanges;
end;

function TOrderService.GenerateTicketCode: string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result := 'TKT-' + Copy(GUIDToString(Guid), 2, 8);
end;

procedure TOrderService.Pay(Id: Integer);
var
  Order: TOrder;
begin
  Order := GetById(Id);

  if TOrderStatus(Order.Status) <> osPending then
    raise EOrderAlreadyPaidException.Create('Order is not pending payment');

  Order.Status := osPaid;
  FDb.Orders.Update(Order);
  FDb.SaveChanges;

  // Generate tickets
  GenerateTickets(Order);

  // Mark as completed
  Order.Status := osCompleted;
  FDb.Orders.Update(Order);
  FDb.SaveChanges;
end;

procedure TOrderService.Cancel(Id: Integer);
var
  Order: TOrder;
begin
  Order := GetById(Id);

  if not (TOrderStatus(Order.Status) in [osPending, osPaid]) then
    raise EOrderCannotBeCancelledException.Create('Only pending or paid orders can be cancelled');

  // Return stock
  UpdateStock(Order, True);

  Order.Status := osCancelled;
  FDb.Orders.Update(Order);
  FDb.SaveChanges;
end;

// =============================================================================
// TTicketService
// =============================================================================

constructor TTicketService.Create(Db: TTicketSalesDbContext);
begin
  inherited Create;
  FDb := Db;
end;

function TTicketService.GetByOrderId(OrderId: Integer): IList<TTicket>;
var
  t: TTicket;
  oi: TOrderItem;
  OrderItemIds: IList<TOrderItem>;
  Tickets: IList<TTicket>;
  OrderItem: TOrderItem;
begin
  Result := TCollections.CreateList<TTicket>;
  
  oi := TOrderItem.Props;
  OrderItemIds := FDb.OrderItems
    .Where(oi.OrderId = OrderId)
    .ToList;

  for OrderItem in OrderItemIds do
  begin
    t := TTicket.Props;
    Tickets := FDb.Tickets
      .Where(t.OrderItemId = Integer(OrderItem.Id))
      .ToList;
    
    for var Ticket in Tickets do
      Result.Add(Ticket);
  end;
end;

function TTicketService.GetByCode(const Code: string): TTicket;
var
  t: TTicket;
begin
  t := TTicket.Props;
  Result := FDb.Tickets
    .Where(t.Code = Code)
    .FirstOrDefault;
end;

function TTicketService.Validate(const Code: string): TValidateTicketResponse;
var
  Ticket: TTicket;
  OrderItem: TOrderItem;
  TicketType: TTicketType;
  Event: TEvent;
  Order: TOrder;
  Customer: TCustomer;
begin
  Result.Valid := False;
  Result.Message := '';
  
  Ticket := GetByCode(Code);
  if Ticket = nil then
  begin
    Result.Message := 'Ticket not found';
    Exit;
  end;

  // Load related data
  OrderItem := FDb.OrderItems.Find(Integer(Ticket.OrderItemId));
  TicketType := FDb.TicketTypes.Find(Integer(OrderItem.TicketTypeId));
  Event := FDb.Events.Find(Integer(TicketType.EventId));
  Order := FDb.Orders.Find(Integer(OrderItem.OrderId));
  Customer := FDb.Customers.Find(Integer(Order.CustomerId));

  // Build response
  Result.Ticket.Id := Ticket.Id;
  Result.Ticket.Code := Ticket.Code;
  Result.Ticket.EventName := Event.Name;
  Result.Ticket.TicketTypeName := TicketType.Name;
  Result.Ticket.CustomerName := Customer.Name;
  Result.Ticket.EventDate := Event.EventDate;

  case TTicketStatus(Ticket.Status) of
    tsUsed:
      begin
        Result.Ticket.Status := 'Used';
        Result.Message := 'Ticket has already been used';
      end;
    tsCancelled:
      begin
        Result.Ticket.Status := 'Cancelled';
        Result.Message := 'Ticket was cancelled';
      end;
    tsExpired:
      begin
        Result.Ticket.Status := 'Expired';
        Result.Message := 'Ticket has expired';
      end;
    tsValid:
      begin
        // Mark as used
        if Ticket.Use then
        begin
          FDb.Tickets.Update(Ticket);
          FDb.SaveChanges;
          
          Result.Valid := True;
          Result.Ticket.Status := 'Valid';
          Result.Message := 'Ticket validated successfully. Welcome!';
        end
        else
        begin
          Result.Ticket.Status := 'Error';
          Result.Message := 'Failed to validate ticket';
        end;
      end;
  end;
end;

end.
