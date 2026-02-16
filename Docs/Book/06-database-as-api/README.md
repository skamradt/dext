# 6. Database as API

Generate REST APIs automatically from your entities - zero code required.

> 📦 **Example**: [Web.DatabaseAsApi](../../../Examples/Web.DatabaseAsApi/)

## Quick Start

```pascal
type
  [Table('products')]
  TProduct = class
  private
    FId: Integer;
    FName: string;
    FPrice: Double;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Price: Double read FPrice write FPrice;
  end;

// One line to expose full CRUD!
App.Configure(procedure(App: IApplicationBuilder)
  begin
    TDataApiHandler<TProduct>.Map(App, '/api/products');
  end);
```

## Generated Endpoints

| Method | URL | Description |
|--------|-----|-------------|
| GET | `/api/products` | List all (with pagination) |
| GET | `/api/products/{id}` | Get by ID |
| POST | `/api/products` | Create new |
| PUT | `/api/products/{id}` | Update |
| DELETE | `/api/products/{id}` | Delete |

## Features

- **Automatic Pagination**: `?_limit=20&_offset=40`
- **Sorting**: `?_orderby=price desc,name asc`
- **Dynamic Specification (Filtering)**: Smart mapping via QueryString:

### Filter Operators

| Suffix | SQL Operator | Example | Description |
|--------|--------------|---------|-------------|
| `_eq`  | `=`          | `?status_eq=1` | Equal to (default) |
| `_neq` | `<>`         | `?type_neq=2` | Not equal to |
| `_gt`  | `>`          | `?price_gt=50` | Greater than |
| `_gte` | `>=`         | `?age_gte=18` | Greater or equal |
| `_lt`  | `<`          | `?stock_lt=5` | Less than |
| `_lte` | `<=`         | `?date_lte=2025-01-01` | Less or equal |
| `_cont`| `LIKE %x%`   | `?name_cont=Dext` | Contains |
| `_sw`  | `LIKE x%`    | `?code_sw=ABC` | Starts with |
| `_ew`  | `LIKE %x`    | `?mail_ew=gmail.com` | Ends with |
| `_in`  | `IN (...)`   | `?cat_in=1,2,5` | List of values |

---

## Security Policies

You can restrict access by operation or by role:

```pascal
App.Builder.MapDataApi<TProduct>('/api/products', DataApiOptions
  .RequireAuth
  .RequireRole('Admin')
  .Allow([amGet, amGetList]) // Read-only access
);
```


## With Security

```pascal
TDataApiHandler<TProduct>.Map(App, '/api/products',
  TDataApiOptions.Create
    .AllowRead
    .AllowCreate
    .DenyDelete  // No DELETE allowed
    .RequireAuth // Require authentication
);
```

---

[← ORM](../05-orm/README.md) | [Next: Real-Time →](../07-real-time/README.md)
