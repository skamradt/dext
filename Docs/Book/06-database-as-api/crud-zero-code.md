# Zero-Code CRUD

Generate full REST APIs from your entities automatically - without writing repetitive code.

> 📦 **Example**: [Web.DatabaseAsApi](../../../Examples/Web.DatabaseAsApi/)

## Quick Start

Expose a complete CRUD for an entity with just one line:

```pascal
type
  [Table('products')]
  TProduct = class
  public
    [PK, AutoInc] property Id: Integer;
    property Name: string;
    property Price: Double;
  end;

// Configure in the pipeline
App.Configure(procedure(App: IApplicationBuilder)
  begin
    // Maps GET, POST, PUT, DELETE to /api/products
    TDataApiHandler<TProduct>.Map(App, '/api/products');
  end);
```

## Generated Endpoints

| Method | URL | Description |
|--------|-----|-----------|
| GET | `/api/products` | List all (supports pagination/filters) |
| GET | `/api/products/:id` | Find by unique ID |
| POST | `/api/products` | Create new record |
| PUT | `/api/products/:id` | Update existing record |
| DELETE | `/api/products/:id` | Delete record |

## Advanced Features

### Pagination and Ordering

Use query parameters to control returned data:
- `?page=1&pageSize=20`
- `?orderBy=Name&desc=true`

### Automatic Filtering

Filter records directly via URL:
- `?Name=Keyboard` (Exact match)
- `?Price_gt=100` (Price greater than 100)
- `?Status_in=Active,Pending` (IN filter)

### Security and Policies

You can restrict which operations are available:

```pascal
TDataApiHandler<TProduct>.Map(App, '/api/products', 
  procedure(Options: TDataApiOptions)
  begin
    Options.AllowedOperations := [ToRead, ToCreate]; // Read and Create only
    Options.RequireAuthorization := True;           // Requires JWT
  end);
```

---

[← Database as API](README.md) | [Next: Real-Time →](../07-real-time/README.md)
