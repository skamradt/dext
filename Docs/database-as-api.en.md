# Database as API

> **Zero-Code REST API** - Exposes database entities directly as REST endpoints with minimal configuration.

## Overview

The **Database as API** module allows you to create full CRUD endpoints for your entities with a single line of code. It supports:

- ✅ All HTTP methods (GET, POST, PUT, DELETE)
- ✅ Automatic JSON parsing to entities
- ✅ Dynamic filters via query string
- ✅ Integrated pagination
- ✅ Security based on roles/claims
- ✅ **Mapping/Ignore Support** (Attributes & Fluent)
- ✅ **Naming Strategies** (CamelCase, SnakeCase)

---

## Quick Start

### 1. Minimal Configuration

```pascal
uses
  Dext.Web.DataApi;

// Exposes TCustomer with all CRUD endpoints
TDataApiHandler<TCustomer>.Map(App, '/api/customers', DbContext);
```

This code automatically generates:

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/api/customers` | List all (with filters) |
| `GET` | `/api/customers/{id}` | Find by ID |
| `POST` | `/api/customers` | Create new record |
| `PUT` | `/api/customers/{id}` | Update record |
| `DELETE` | `/api/customers/{id}` | Remove record |

---

## Step-by-Step Guide: Creating an API from Scratch

### Step 1: Define your Entity

```pascal
unit Entities.Customer;

interface

uses
  Dext.Entity;

type
  [Table('customers')]
  TCustomer = class(TEntity)
  private
    FId: Integer;
    FName: string;
    FEmail: string;
    FActive: Boolean;
    FCreatedAt: TDateTime;
  published
    [PrimaryKey, AutoIncrement]
    property Id: Integer read FId write FId;
    
    [Column('name'), Required, MaxLength(100)]
    property Name: string read FName write FName;
    
    [Column('email'), MaxLength(255)]
    property Email: string read FEmail write FEmail;
    
    [Column('active')]
    property Active: Boolean read FActive write FActive;
    
    [Column('created_at')]
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  end;

implementation

end.
```

### Step 2: Configuration & Naming Strategies

By default, the API uses **camelCase** (e.g., `createdAt`). You can configure it to **snake_case** (e.g., `created_at`) if preferred.

```pascal
procedure TStartup.ConfigureRoutes;
begin
  // Default Configuration (CamelCase)
  // JSON: { "id": 1, "name": "...", "createdAt": "..." }
  TDataApiHandler<TCustomer>.Map(FApp, '/api/customers', FDbContext);

  // SnakeCase Configuration
  // JSON: { "id": 1, "name": "...", "created_at": "..." }
  TDataApiHandler<TCustomer>.Map(FApp, '/api/customers_snake', FDbContext,
    TDataApiOptions<TCustomer>.Create
      .UseSnakeCase
  );
end;
```

---

## Endpoint Configuration

### Read-Only (GET)

```pascal
TDataApiHandler<TCustomer>.Map(App, '/api/customers', DbContext,
  TDataApiOptions<TCustomer>.Create
    .Allow([amGet, amGetList])  // Only GET and GET List
);
```

### Write-Only (POST)

```pascal
TDataApiHandler<TLog>.Map(App, '/api/logs', DbContext,
  TDataApiOptions<TLog>.Create
    .Allow([amPost])  // Only POST
);
```

### Available Methods

| Constant | HTTP Method | Description |
|-----------|-------------|-----------|
| `amGet` | GET /{id} | Find by ID |
| `amGetList` | GET | List all |
| `amPost` | POST | Create new |
| `amPut` | PUT /{id} | Update |
| `amDelete` | DELETE /{id} | Remove |
| `AllApiMethods` | All | Default value |

---

## Filters via Query String

### Basic Syntax

```
GET /api/customers?{property}={value}
```

### Filter Examples

```bash
# Filter by boolean
GET /api/customers?active=true

# Filter by string
GET /api/customers?name=John

# Filter by number
GET /api/customers?id=5

# Multiple filters (AND)
GET /api/customers?active=true&name=John
```

### Supported Types

| Pascal Type | Query Example | Conversion |
|-------------|---------------|-----------|
| `Integer` | `?id=123` | `StrToInt` |
| `Int64` | `?code=999999999` | `StrToInt64` |
| `String` | `?name=John` | Direct |
| `Boolean` | `?active=true` | `true/false` or `1/0` |

> **Note**: Properties not found in entity are silently ignored.

---

## Mapping Support (Ignore)

DataApi uses Dext ORM mapping configurations via `TEntityMap`. Properties ignored via Attributes (`[NotMapped]`) or Fluent API (`Ignore`) will be automatically excluded from all API operations.

### Example (Attribute)
```pascal
type
  TCustomer = class
  published
    property Name: string ...;
    
    [NotMapped] // Ignored in API
    property InternalCode: string ...;
  end;
```

### Example (Fluent API)
```pascal
ModelBuilder.Entity<TCustomer>
  .Ignore('InternalCode');
```

---

## JSON Naming Strategies

The parser avoids exposing database column names, using Class Property names with the configured conversion strategy:

| Pascal Property | CamelCase (Default) | SnakeCase (`UseSnakeCase`) |
|-------------------|--------------------|----------------------------|
| `Name` | `name` | `name` |
| `EmailAddress` | `emailAddress` | `email_address` |
| `IsActive` | `isActive` | `is_active` |
| `CreatedAt` | `createdAt` | `created_at` |

---

## API Reference

### TDataApiOptions\<T\>

```pascal
TDataApiOptions<T> = class
  // Allowed Methods
  function Allow(AMethods: TApiMethods): TDataApiOptions<T>;
  
  // Multi-tenancy
  function RequireTenant: TDataApiOptions<T>;
  
  // Authentication
  function RequireAuth: TDataApiOptions<T>;
  
  // Roles
  function RequireRole(const ARoles: string): TDataApiOptions<T>;       // All ops
  function RequireReadRole(const ARoles: string): TDataApiOptions<T>;   // GET only
  function RequireWriteRole(const ARoles: string): TDataApiOptions<T>;  // POST/PUT/DELETE
  
  // Naming
  function UseCamelCase: TDataApiOptions<T>;  // Default
  function UseSnakeCase: TDataApiOptions<T>;
end;
```
