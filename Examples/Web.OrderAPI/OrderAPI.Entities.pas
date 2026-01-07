unit OrderAPI.Entities;

{***************************************************************************}
{  Sistema de Comandas para Bares e Restaurantes                            }
{***************************************************************************}

{$M+}

interface

uses
  System.SysUtils,
  Dext.Entity,
  Dext.Core.SmartTypes;

type
  /// <summary>
  ///   Status possíveis para uma mesa
  /// </summary>
  TTableStatus = (
    tsAvailable,   // Disponível
    tsOccupied,    // Ocupada
    tsReserved     // Reservada
  );

  /// <summary>
  ///   Status possíveis para um pedido
  /// </summary>
  TOrderStatus = (
    osOpen,        // Aberto
    osClosed,      // Fechado/Pago
    osCancelled    // Cancelado
  );

  /// <summary>
  ///   Categoria de produtos (bebidas, pratos, sobremesas, etc.)
  /// </summary>
  [Table('categories')]
  TCategory = class
  private
    FId: Int64; // Dext prefere Int64 para IDs
    FName: StringType;
    FDescription: StringType;
    FActive: BoolType;
  public
    [PK, AutoInc]
    property Id: Int64 read FId write FId;
    
    [Column('name')]
    property Name: StringType read FName write FName;
    
    [Column('description')]
    property Description: StringType read FDescription write FDescription;
    
    [Column('active')]
    property Active: BoolType read FActive write FActive;
  end;

  /// <summary>
  ///   Produto disponível no cardápio
  /// </summary>
  [Table('products')]
  TProduct = class
  private
    FId: Int64;
    FName: StringType;
    FDescription: StringType;
    FPrice: CurrencyType;
    FCategoryId: Int64Type;
    FAvailable: BoolType;
    FImageUrl: StringType;
  public
    [PK, AutoInc]
    property Id: Int64 read FId write FId;
    
    [Column('name')]
    property Name: StringType read FName write FName;
    
    [Column('description')]
    property Description: StringType read FDescription write FDescription;
    
    [Column('price')]
    property Price: CurrencyType read FPrice write FPrice;
    
    [Column('category_id')]
    property CategoryId: Int64Type read FCategoryId write FCategoryId;
    
    [Column('available')]
    property Available: BoolType read FAvailable write FAvailable;
    
    [Column('image_url')]
    property ImageUrl: StringType read FImageUrl write FImageUrl;
  end;

  /// <summary>
  ///   Mesa do restaurante
  /// </summary>
  [Table('tables')]
  TRestaurantTable = class
  private
    FId: Int64;
    FNumber: IntType;
    FSeats: IntType;
    FStatus: Prop<TTableStatus>; // Usando Prop genérico para Enum
    FLocation: StringType;
  public
    [PK, AutoInc]
    property Id: Int64 read FId write FId;
    
    [Column('number')]
    property Number: IntType read FNumber write FNumber;
    
    [Column('seats')]
    property Seats: IntType read FSeats write FSeats;
    
    [Column('status')]
    property Status: Prop<TTableStatus> read FStatus write FStatus;
    
    [Column('location')]
    property Location: StringType read FLocation write FLocation;
  end;

  /// <summary>
  ///   Pedido/Comanda
  /// </summary>
  [Table('orders')]
  TOrder = class
  private
    FId: Int64;
    FTableId: Int64Type;
    FStatus: Prop<TOrderStatus>;
    FOpenedAt: DateTimeType;
    FClosedAt: DateTimeType;
    FTotalAmount: CurrencyType;
    FCustomerName: StringType;
    FNotes: StringType;
  public
    [PK, AutoInc]
    property Id: Int64 read FId write FId;
    
    [Column('table_id')]
    property TableId: Int64Type read FTableId write FTableId;
    
    [Column('status')]
    property Status: Prop<TOrderStatus> read FStatus write FStatus;
    
    [Column('opened_at')]
    property OpenedAt: DateTimeType read FOpenedAt write FOpenedAt;
    
    [Column('closed_at')]
    property ClosedAt: DateTimeType read FClosedAt write FClosedAt;
    
    [Column('total_amount')]
    property TotalAmount: CurrencyType read FTotalAmount write FTotalAmount;
    
    [Column('customer_name')]
    property CustomerName: StringType read FCustomerName write FCustomerName;
    
    [Column('notes')]
    property Notes: StringType read FNotes write FNotes;
  end;

  /// <summary>
  ///   Item de um pedido
  /// </summary>
  [Table('order_items')]
  TOrderItem = class
  private
    FId: Int64;
    FOrderId: Int64Type;
    FProductId: Int64Type;
    FQuantity: IntType;
    FUnitPrice: CurrencyType;
    FNotes: StringType;
    FCreatedAt: DateTimeType;
  public
    [PK, AutoInc]
    property Id: Int64 read FId write FId;
    
    [Column('order_id')]
    property OrderId: Int64Type read FOrderId write FOrderId;
    
    [Column('product_id')]
    property ProductId: Int64Type read FProductId write FProductId;
    
    [Column('quantity')]
    property Quantity: IntType read FQuantity write FQuantity;
    
    [Column('unit_price')]
    property UnitPrice: CurrencyType read FUnitPrice write FUnitPrice;
    
    [Column('notes')]
    property Notes: StringType read FNotes write FNotes;
    
    [Column('created_at')]
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;
  end;

  // ==========================================================================
  // DTOs para requests/responses
  // DTOs mantêm tipos primitivos para facilidade de uso
  // ==========================================================================
  
  TCreateCategoryRequest = record
    Name: string;
    Description: string;
  end;
  
  TCreateProductRequest = record
    Name: string;
    Description: string;
    Price: Currency;
    CategoryId: Integer;
    ImageUrl: string;
  end;
  
  TCreateOrderRequest = record
    TableId: Integer;
    CustomerName: string;
    Notes: string;
  end;
  
  TAddOrderItemRequest = record
    ProductId: Integer;
    Quantity: Integer;
    Notes: string;
  end;
  
  TUpdateTableStatusRequest = record
    Status: TTableStatus;
  end;
  
  // ==========================================================================
  // DTOs para relatórios (TReportService - classe pura Transient)
  // ==========================================================================
  
  TReportStats = record
    TotalTables: Integer;
    AvailableTables: Integer;
    OccupiedTables: Integer;
    TotalProducts: Integer;
    TotalCategories: Integer;
    OpenOrders: Integer;
    InstanceId: Integer;
  end;
  
  TDailySummary = record
    Date: TDateTime;
    TotalOrders: Integer;
    TotalRevenue: Currency;
    AverageOrderValue: Currency;
    InstanceId: Integer;
  end;

implementation

end.
