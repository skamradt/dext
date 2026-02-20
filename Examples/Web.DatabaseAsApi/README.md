# Database as API Example

This example demonstrates the **Database as API** feature - automatically exposing database entities as REST endpoints with zero boilerplate code.

## 🚀 Features Demonstrated

| Feature | Description |
|---------|-------------|
| **Zero-Code REST** | `TDataApiHandler<T>.Map` creates full CRUD endpoints automatically |
| **Snake Case JSON** | `UseSnakeCase` transforms `CreatedAt` → `created_at` in outputs |
| **OpenAPI/Swagger** | `UseSwagger` generates API documentation automatically |
| **Field Exclusion** | `[NotMapped]` hides fields from both API and database |
| **Auto PK/AutoInc** | `[PK, AutoInc]` for automatic primary key handling |

## 🛠️ Getting Started

1. **Compile** `Web.DatabaseAsApi.dproj`
2. **Run** `Web.DatabaseAsApi.exe`
   - SQLite in-memory database is created automatically
   - Server starts on **http://localhost:5000**
3. **Test**:
   ```powershell
   .\Test.Web.DatabaseAsApi.ps1
   ```

## 📍 Auto-Generated Endpoints

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/api/customers` | List all customers |
| `GET` | `/api/customers/{id}` | Get customer by ID |
| `POST` | `/api/customers` | Create new customer |
| `PUT` | `/api/customers/{id}` | Update customer |
| `DELETE` | `/api/customers/{id}` | Delete customer |
| `GET` | `/swagger` | Swagger UI |
| `GET` | `/swagger.json` | OpenAPI spec |

## 💡 Code Highlights

### Minimal Entity Definition
```delphi
[Table('Customers')]
TCustomer = class
private
  FId: Integer;
  FName: string;
  FEmail: string;
  FActive: Boolean;
  FInternalCode: string;
  FCreatedAt: TDateTime;
public
  [PK, AutoInc]
  property Id: Integer read FId write FId;
  property Name: string read FName write FName;
  property Email: string read FEmail write FEmail;
  property Active: Boolean read FActive write FActive;
  
  [NotMapped]  // Excluded from API and DB
  property InternalCode: string read FInternalCode write FInternalCode;
  
  property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
end;
```

### One-Line API Setup
```delphi
TDataApiHandler<TCustomer>.Map(ABuilder, '/api/customers', FDbContext,
  TDataApiOptions<TCustomer>.Create
    .UseSnakeCase       // JSON: created_at instead of CreatedAt
    .UseSwagger         // Enable OpenAPI documentation
    .Tag('Customers')   // Custom Swagger tag
);
```

### JSON Output (Snake Case)
```json
{
  "id": 1,
  "name": "John Doe",
  "email": "john@example.com",
  "active": true,
  "created_at": "2025-01-02T10:30:00"
}
```

Note: `InternalCode` is not present in output due to `[NotMapped]`.

## 🔧 Configuration Options

| Option | Description |
|--------|-------------|
| `UseSnakeCase` | Converts property names to snake_case in JSON |
| `UseSwagger` | Enables OpenAPI documentation generation |
| `Tag(name)` | Sets custom tag name in Swagger UI |

## 🔗 See Also

- [Database as API Guide](../../docs/database-as-api.md)
- [ORM Mapping Attributes](../../docs/orm-mapping-attributes.md)
- [Swagger Integration](../../docs/swagger.md)
