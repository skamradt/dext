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
| GET | `/api/products/:id` | Get by ID |
| POST | `/api/products` | Create new |
| PUT | `/api/products/:id` | Update |
| DELETE | `/api/products/:id` | Delete |

## Features

- **Automatic Pagination**: `?page=1&pageSize=20`
- **Filtering**: `?name=Widget&price_gt=100`
- **Sorting**: `?orderBy=price&desc=true`
- **Security Policies**: Restrict access per operation

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
