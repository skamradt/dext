# 📊 Complex Querying Example

Demonstrates **advanced ORM queries**, **JSON fields**, **aggregations**, and **reporting** with Dext.Entity.

> 📦 Example: `Dext.Examples.ComplexQuerying`

## Features

- **JSON Fields** - Entities with JSON columns (arrays and objects)
- **Fluent Queries** - Chain filters with `.Where()`, `.OrderBy()`, etc.
- **Search Criteria** - Dynamic filtering based on user input
- **Aggregations** - Sales reports and top customers
- **Date Range Queries** - Filter by date ranges

## Running the Example

```bash
# Compile and run
msbuild Dext.Examples.ComplexQuerying.dproj
.\..\..\Output\Dext.Examples.ComplexQuerying.exe
```

## API Endpoints

### Orders

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/orders` | List all orders |
| GET | `/api/orders/{id}` | Get order by ID |
| GET | `/api/orders/status/{status}` | Filter by status |
| GET | `/api/orders/customer/{id}` | Filter by customer |
| POST | `/api/orders/search` | Advanced search |

### Reports

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/reports/sales` | Sales by status |
| GET | `/api/reports/top-customers?top=5` | Top customers |

### Utilities

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/api/seed` | Seed sample data |

## Usage Examples

### 1. Seed Sample Data

```bash
curl -X POST http://localhost:8080/api/seed
```

### 2. List All Orders

```bash
curl http://localhost:8080/api/orders
```

### 3. Filter by Status

```bash
curl http://localhost:8080/api/orders/status/pending
```

### 4. Advanced Search

```bash
curl -X POST http://localhost:8080/api/orders/search \
  -H "Content-Type: application/json" \
  -d '{"status": "shipped", "minAmount": 100}'
```

### 5. Get Sales Report

```bash
curl http://localhost:8080/api/reports/sales
```

### 6. Get Top Customers

```bash
curl "http://localhost:8080/api/reports/top-customers?top=5"
```

## Entity Design with Smart Properties

```pascal
uses
  Dext.Entity,
  Dext.Core.SmartTypes;

[Table('customers')]
TCustomer = class
private
  FId: Int64;
  FName: StringType;
  FTags: StringType;      // JSON array as string
  FMetadata: StringType;  // JSON object as string
  FTotalSpent: CurrencyType;
public
  [PK, AutoInc]
  property Id: Int64 read FId write FId;
  
  [Column('name'), Required]
  property Name: StringType read FName write FName;
  
  // JSON stored as string
  property Tags: StringType read FTags write FTags;
  property Metadata: StringType read FMetadata write FMetadata;
  
  [Column('total_spent'), Precision(18, 2)]
  property TotalSpent: CurrencyType read FTotalSpent write FTotalSpent;
end;
```

## DbContext Pattern

```pascal
TQueryDbContext = class(TDbContext)
private
  function GetOrders: IDbSet<TOrder>;
public
  property Orders: IDbSet<TOrder> read GetOrders;
end;

function TQueryDbContext.GetOrders: IDbSet<TOrder>;
begin
  Result := Entities<TOrder>;  // Use Entities<T>, not TDbSet
end;
```

## Service Layer with IList<T>

```pascal
uses
  Dext.Collections;

function TOrderService.GetAllOrders: IList<TOrder>;
begin
  Result := FDbContext.Orders.ToList;  // Returns IList<TOrder>
end;

function TOrderService.GetOrdersByStatus(const Status: string): IList<TOrder>;
var
  AllOrders: IList<TOrder>;
  Order: TOrder;
begin
  Result := TCollections.CreateList<TOrder>;  // Factory method
  AllOrders := FDbContext.Orders.ToList;
  
  for Order in AllOrders do
    if string(Order.Status) = Status then  // Cast SmartType to string
      Result.Add(Order);
end;
```

## Architecture

```
Dext.Examples.ComplexQuerying/
├── Dext.Examples.ComplexQuerying.dpr
├── Domain/
│   ├── ComplexQuerying.Entities.pas   # TCustomer, TOrder, TProduct
│   └── ComplexQuerying.DbContext.pas
└── Features/
    ├── ComplexQuerying.Service.pas    # Query & Report services
    └── ComplexQuerying.Endpoints.pas
```

## See Also

- [ORM Entities](../../Docs/Book/05-orm/entities.md)
- [Querying](../../Docs/Book/05-orm/querying.md)
