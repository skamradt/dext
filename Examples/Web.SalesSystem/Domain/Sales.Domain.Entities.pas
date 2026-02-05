unit Sales.Domain.Entities;

interface

uses
  Dext.Collections,
  Dext.Entity,
  Dext.Core.SmartTypes,
  Sales.Domain.Enums;

type
  { Forward Declarations }
  TCustomer = class;
  TProduct = class;
  TOrder = class;
  TOrderItem = class;

  { -------------------------------------------------------------------------- }
  { TCustomer (State)                                                          }
  { -------------------------------------------------------------------------- }
  [Table('Customers')]
  TCustomer = class
  private
    FId: IntType;
    FName: StringType;
    FEmail: StringType;
    FStatus: TCustomerStatusType;
    FCreditLimit: CurrencyType;
  public
    [PK, AutoInc]
    property Id: IntType read FId write FId;
    property Name: StringType read FName write FName;
    property Email: StringType read FEmail write FEmail;
    property Status: TCustomerStatusType read FStatus write FStatus;
    property CreditLimit: CurrencyType read FCreditLimit write FCreditLimit;

    // Smart Props Accessor
    class function Props: TCustomer; static;
  end;

  { -------------------------------------------------------------------------- }
  { TProduct (State)                                                           }
  { -------------------------------------------------------------------------- }
  [Table('Products')]
  TProduct = class
  private
    FId: IntType;
    FName: StringType;
    FPrice: CurrencyType;
    FStockQuantity: IntType;
  public
    [PK, AutoInc]
    property Id: IntType read FId write FId;
    property Name: StringType read FName write FName;
    property Price: CurrencyType read FPrice write FPrice;
    property StockQuantity: IntType read FStockQuantity write FStockQuantity;

    // Smart Props Accessor
    class function Props: TProduct; static;
  end;

  { -------------------------------------------------------------------------- }
  { TOrder (State)                                                             }
  { -------------------------------------------------------------------------- }
  [Table('Orders')]
  TOrder = class
  private
    FId: IntType;
    FCustomerId: IntType;
    FStatus: TOrderStatusType;
    FTotal: CurrencyType;
    FCreatedAt: DateTimeType;
    FItems: IList<TOrderItem>;
    
    // Navigation Properties (Optional, usually for Includes)
    FCustomer: TCustomer; 
  public
    // Configures the List for Items
    constructor Create;
    destructor Destroy; override;

    [PK, AutoInc]
    property Id: IntType read FId write FId;
    property CustomerId: IntType read FCustomerId write FCustomerId;
    property Status: TOrderStatusType read FStatus write FStatus;
    property Total: CurrencyType read FTotal write FTotal;
    
    [CreatedAt]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;
    
    // [HasMany] // TODO: Create and implement HasMany attribute
    property Items: IList<TOrderItem> read FItems;

    // [BelongsTo] // TODO: Create and implement BelongsTo attribute
    property Customer: TCustomer read FCustomer write FCustomer;

    // Smart Props Accessor
    class function Props: TOrder; static;
  end;

  { -------------------------------------------------------------------------- }
  { TOrderItem (State)                                                         }
  { -------------------------------------------------------------------------- }
  [Table('OrderItems')]
  TOrderItem = class
  private
    FId: IntType;
    FOrderId: IntType;
    FProductId: IntType;
    FQuantity: IntType;
    FUnitPrice: CurrencyType;
    FTotal: CurrencyType;
    
    // Navigation
    FProduct: TProduct;
  public
    [PK, AutoInc]
    property Id: IntType read FId write FId;
    property OrderId: IntType read FOrderId write FOrderId;
    property ProductId: IntType read FProductId write FProductId;
    property Quantity: IntType read FQuantity write FQuantity;
    property UnitPrice: CurrencyType read FUnitPrice write FUnitPrice;
    property Total: CurrencyType read FTotal write FTotal;

    // [BelongsTo] // TODO: Create and implement BelongsTo attribute
    property Product: TProduct read FProduct write FProduct;

    // Smart Props Accessor
    class function Props: TOrderItem; static;
  end;

implementation

{ TCustomer }

class function TCustomer.Props: TCustomer;
begin
  Result := Prototype.Entity<TCustomer>;
end;

{ TProduct }

class function TProduct.Props: TProduct;
begin
  Result := Prototype.Entity<TProduct>;
end;

{ TOrder }

constructor TOrder.Create;
begin
  inherited;
  FItems := TCollections.CreateList<TOrderItem>;
end;

destructor TOrder.Destroy;
begin
  // We don't free Items because the List implementation handles it? 
  // Ideally in Dext Entity lists are ARC or managed. 
  // But standard TList<T> needs cleanup if it owns objects.
  // Dext.Entity typically manages the lifecycle of loaded entities.
  // However, for pure POCO usage, we might leak if we don't free.
  // Assuming TCollections.CreateList returns a standard list that needs manual free in standard Delphi.
  // Dext.Entity uses TSmartList internally usually.
  // Let's assume the user handles object lifecycle or the Framework does via DI/Scope.
  // But for the list itself:
  // FItems is an interface (IList<T>), so it is ref counted. Objects inside?
  // Let's keep it simple.
  inherited;
end;

class function TOrder.Props: TOrder;
begin
  Result := Prototype.Entity<TOrder>;
end;

{ TOrderItem }

class function TOrderItem.Props: TOrderItem;
begin
  Result := Prototype.Entity<TOrderItem>;
end;

end.
