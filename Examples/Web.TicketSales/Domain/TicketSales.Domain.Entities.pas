unit TicketSales.Domain.Entities;

{***************************************************************************}
{                                                                           }
{           Web.TicketSales - Domain Entities                               }
{                                                                           }
{           ORM Entities with Smart Properties for type-safe queries        }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Collections,
  Dext.Entity,
  Dext.Core.SmartTypes,
  TicketSales.Domain.Enums;

type
  { Forward Declarations }
  TEvent = class;
  TTicketType = class;
  TCustomer = class;
  TOrder = class;
  TOrderItem = class;
  TTicket = class;

  { -------------------------------------------------------------------------- }
  { TEvent - Represents an event (concert, theater, etc.)                      }
  { -------------------------------------------------------------------------- }
  [Table('Events')]
  TEvent = class
  private
    FId: IntType;
    FName: StringType;
    FDescription: StringType;
    FVenue: StringType;
    FEventDate: DateTimeType;
    FCapacity: IntType;
    FSoldCount: IntType;
    FStatus: TEventStatusType;
    FCreatedAt: DateTimeType;
    FUpdatedAt: DateTimeType;
  public
    [PK, AutoInc]
    property Id: IntType read FId write FId;
    
    [Required, MaxLength(200)]
    property Name: StringType read FName write FName;
    
    [MaxLength(2000)]
    property Description: StringType read FDescription write FDescription;
    
    [Required, MaxLength(300)]
    property Venue: StringType read FVenue write FVenue;
    
    [Required]
    property EventDate: DateTimeType read FEventDate write FEventDate;
    
    /// <summary>Maximum capacity for the event</summary>
    [Required]
    property Capacity: IntType read FCapacity write FCapacity;
    
    /// <summary>Number of tickets already sold</summary>
    property SoldCount: IntType read FSoldCount write FSoldCount;
    
    property Status: TEventStatusType read FStatus write FStatus;
    
    [CreatedAt]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;
    
    [UpdatedAt]
    property UpdatedAt: DateTimeType read FUpdatedAt write FUpdatedAt;

    /// <summary>Returns available tickets count</summary>
    function AvailableTickets: Integer;
    
    /// <summary>Smart Props accessor for type-safe queries</summary>
    class function Props: TEvent; static;
  end;

  { -------------------------------------------------------------------------- }
  { TTicketType - Types of tickets for an event (VIP, Standard, etc.)          }
  { -------------------------------------------------------------------------- }
  [Table('TicketTypes')]
  TTicketType = class
  private
    FId: IntType;
    FEventId: IntType;
    FName: StringType;
    FDescription: StringType;
    FPrice: CurrencyType;
    FQuantity: IntType;
    FSoldCount: IntType;
    FIsHalfPrice: BoolType;
    
    // Navigation
    FEvent: TEvent;
  public
    [PK, AutoInc]
    property Id: IntType read FId write FId;
    
    [ForeignKey('Event')]
    property EventId: IntType read FEventId write FEventId;
    
    [Required, MaxLength(100)]
    property Name: StringType read FName write FName;
    
    [MaxLength(500)]
    property Description: StringType read FDescription write FDescription;
    
    /// <summary>Base price (full price)</summary>
    [Required]
    property Price: CurrencyType read FPrice write FPrice;
    
    /// <summary>Total quantity available for this type</summary>
    [Required]
    property Quantity: IntType read FQuantity write FQuantity;
    
    /// <summary>Number sold</summary>
    property SoldCount: IntType read FSoldCount write FSoldCount;
    
    /// <summary>If true, this ticket type is eligible for half-price</summary>
    property IsHalfPrice: BoolType read FIsHalfPrice write FIsHalfPrice;
    
    // Navigation property
    property Event: TEvent read FEvent write FEvent;
    
    /// <summary>Returns available tickets for this type</summary>
    function AvailableQuantity: Integer;
    
    class function Props: TTicketType; static;
  end;

  { -------------------------------------------------------------------------- }
  { TCustomer - Customer/buyer information                                     }
  { -------------------------------------------------------------------------- }
  [Table('Customers')]
  TCustomer = class
  private
    FId: IntType;
    FName: StringType;
    FEmail: StringType;
    FCPF: StringType;
    FCustomerType: TCustomerTypeType;
    FCreatedAt: DateTimeType;
  public
    [PK, AutoInc]
    property Id: IntType read FId write FId;
    
    [Required, MaxLength(200)]
    property Name: StringType read FName write FName;
    
    [Required, MaxLength(200)]
    property Email: StringType read FEmail write FEmail;
    
    /// <summary>Brazilian CPF (taxpayer ID)</summary>
    [Required, MaxLength(14)]
    property CPF: StringType read FCPF write FCPF;
    
    /// <summary>Customer category for pricing (Student, Senior, etc.)</summary>
    property CustomerType: TCustomerTypeType read FCustomerType write FCustomerType;
    
    [CreatedAt]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;
    
    /// <summary>Returns true if customer is eligible for half-price</summary>
    function IsHalfPriceEligible: Boolean;
    
    class function Props: TCustomer; static;
  end;

  { -------------------------------------------------------------------------- }
  { TOrder - Purchase order                                                    }
  { -------------------------------------------------------------------------- }
  [Table('Orders')]
  TOrder = class
  private
    FId: IntType;
    FCustomerId: IntType;
    FStatus: TOrderStatusType;
    FTotal: CurrencyType;
    FCreatedAt: DateTimeType;
    FUpdatedAt: DateTimeType;
    FItems: IList<TOrderItem>;
    
    // Navigation
    FCustomer: TCustomer;
  public
    constructor Create;
    destructor Destroy; override;

    [PK, AutoInc]
    property Id: IntType read FId write FId;
    
    [ForeignKey('Customer')]
    property CustomerId: IntType read FCustomerId write FCustomerId;
    
    property Status: TOrderStatusType read FStatus write FStatus;
    
    /// <summary>Total order amount</summary>
    property Total: CurrencyType read FTotal write FTotal;
    
    [CreatedAt]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;
    
    [UpdatedAt]
    property UpdatedAt: DateTimeType read FUpdatedAt write FUpdatedAt;

    /// <summary>Order line items</summary>
    property Items: IList<TOrderItem> read FItems;
    
    // Navigation
    property Customer: TCustomer read FCustomer write FCustomer;
    
    /// <summary>Calculates and updates the total from items</summary>
    procedure CalculateTotal;
    
    class function Props: TOrder; static;
  end;

  { -------------------------------------------------------------------------- }
  { TOrderItem - Line item in an order                                         }
  { -------------------------------------------------------------------------- }
  [Table('OrderItems')]
  TOrderItem = class
  private
    FId: IntType;
    FOrderId: IntType;
    FTicketTypeId: IntType;
    FQuantity: IntType;
    FUnitPrice: CurrencyType;
    FIsHalfPrice: BoolType;
    FTotal: CurrencyType;
    
    // Navigation
    FTicketType: TTicketType;
  public
    [PK, AutoInc]
    property Id: IntType read FId write FId;
    
    [ForeignKey('Order')]
    property OrderId: IntType read FOrderId write FOrderId;
    
    [ForeignKey('TicketType')]
    property TicketTypeId: IntType read FTicketTypeId write FTicketTypeId;
    
    [Required]
    property Quantity: IntType read FQuantity write FQuantity;
    
    /// <summary>Price per unit (may be half if IsHalfPrice)</summary>
    property UnitPrice: CurrencyType read FUnitPrice write FUnitPrice;
    
    /// <summary>If true, this was sold at half-price</summary>
    property IsHalfPrice: BoolType read FIsHalfPrice write FIsHalfPrice;
    
    /// <summary>Line total (Quantity * UnitPrice)</summary>
    property Total: CurrencyType read FTotal write FTotal;
    
    // Navigation
    property TicketType: TTicketType read FTicketType write FTicketType;
    
    /// <summary>Calculates and sets UnitPrice and Total</summary>
    procedure CalculatePricing(BasePrice: Currency; ApplyHalfPrice: Boolean);
    
    class function Props: TOrderItem; static;
  end;

  { -------------------------------------------------------------------------- }
  { TTicket - Individual ticket issued after purchase                          }
  { -------------------------------------------------------------------------- }
  [Table('Tickets')]
  TTicket = class
  private
    FId: IntType;
    FOrderItemId: IntType;
    FCode: StringType;
    FStatus: TTicketStatusType;
    FUsedAt: DateTimeType;
    FCreatedAt: DateTimeType;
    
    // Navigation
    FOrderItem: TOrderItem;
  public
    [PK, AutoInc]
    property Id: IntType read FId write FId;
    
    [ForeignKey('OrderItem')]
    property OrderItemId: IntType read FOrderItemId write FOrderItemId;
    
    /// <summary>Unique ticket code (for QR code)</summary>
    [Required, MaxLength(50)]
    property Code: StringType read FCode write FCode;
    
    property Status: TTicketStatusType read FStatus write FStatus;
    
    /// <summary>When the ticket was scanned/used</summary>
    property UsedAt: DateTimeType read FUsedAt write FUsedAt;
    
    [CreatedAt]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;
    
    // Navigation
    property OrderItem: TOrderItem read FOrderItem write FOrderItem;
    
    /// <summary>Marks ticket as used</summary>
    function Use: Boolean;
    
    class function Props: TTicket; static;
  end;

implementation

uses
  System.SysUtils;

{ TEvent }

function TEvent.AvailableTickets: Integer;
begin
  Result := Integer(FCapacity) - Integer(FSoldCount);
  if Result < 0 then
    Result := 0;
end;

class function TEvent.Props: TEvent;
begin
  Result := Prototype.Entity<TEvent>;
end;

{ TTicketType }

function TTicketType.AvailableQuantity: Integer;
begin
  Result := Integer(FQuantity) - Integer(FSoldCount);
  if Result < 0 then
    Result := 0;
end;

class function TTicketType.Props: TTicketType;
begin
  Result := Prototype.Entity<TTicketType>;
end;

{ TCustomer }

function TCustomer.IsHalfPriceEligible: Boolean;
begin
  Result := TCustomerType(FCustomerType) in [ctStudent, ctSenior, ctChild];
end;

class function TCustomer.Props: TCustomer;
begin
  Result := Prototype.Entity<TCustomer>;
end;

{ TOrder }

constructor TOrder.Create;
begin
  inherited;
  FItems := TCollections.CreateList<TOrderItem>(False);
end;

destructor TOrder.Destroy;
begin
  inherited;
end;

procedure TOrder.CalculateTotal;
var
  Item: TOrderItem;
  Sum: Currency;
begin
  Sum := 0;
  for Item in FItems do
    Sum := Sum + Currency(Item.Total);
  FTotal := Sum;
end;

class function TOrder.Props: TOrder;
begin
  Result := Prototype.Entity<TOrder>;
end;

{ TOrderItem }

procedure TOrderItem.CalculatePricing(BasePrice: Currency; ApplyHalfPrice: Boolean);
begin
  FIsHalfPrice := ApplyHalfPrice;
  if ApplyHalfPrice then
    FUnitPrice := BasePrice / 2
  else
    FUnitPrice := BasePrice;
  FTotal := Currency(FUnitPrice) * Integer(FQuantity);
end;

class function TOrderItem.Props: TOrderItem;
begin
  Result := Prototype.Entity<TOrderItem>;
end;

{ TTicket }

function TTicket.Use: Boolean;
begin
  if TTicketStatus(FStatus) <> tsValid then
  begin
    Result := False;
    Exit;
  end;
  
  FStatus := tsUsed;
  FUsedAt := Now;
  Result := True;
end;

class function TTicket.Props: TTicket;
begin
  Result := Prototype.Entity<TTicket>;
end;

end.
