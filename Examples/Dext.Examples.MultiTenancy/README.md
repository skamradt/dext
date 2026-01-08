# 🏢 Multi-Tenancy Example

Demonstrates how to build a **SaaS-style multi-tenant application** with Dext, featuring tenant isolation and per-tenant data access.

> 📦 Example: `Dext.Examples.MultiTenancy`

## Features

- **Tenant Resolution Middleware** - Extracts tenant ID from `X-Tenant-Id` header
- **Tenant Management** - Create and list tenants
- **Tenant-Scoped Data** - Products are isolated per tenant
- **Schema-per-Tenant Pattern** - Conceptual pattern for database isolation

## Running the Example

```bash
# Compile and run
msbuild Dext.Examples.MultiTenancy.dproj
.\..\..\Output\Dext.Examples.MultiTenancy.exe
```

## API Endpoints

### Tenant Management (Public)

| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/api/tenants` | Create a new tenant |
| GET | `/api/tenants` | List all tenants |
| GET | `/api/tenants/{id}` | Get tenant by ID |

### Product Management (Tenant-Scoped)

> **Requires Header:** `X-Tenant-Id: <tenant-id>`

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/products` | List products for tenant |
| POST | `/api/products` | Create product for tenant |
| GET | `/api/products/{id}` | Get product by ID |

## Usage Examples

### 1. Create a Tenant

```bash
curl -X POST http://localhost:8080/api/tenants \
  -H "Content-Type: application/json" \
  -d '{"name": "Acme Corp", "subdomain": "acme"}'
```

### 2. Create a Product for Tenant

```bash
curl -X POST http://localhost:8080/api/products \
  -H "Content-Type: application/json" \
  -H "X-Tenant-Id: abc123" \
  -d '{"name": "Widget", "description": "A great widget", "price": 29.99, "stock": 100}'
```

### 3. List Products for Tenant

```bash
curl http://localhost:8080/api/products \
  -H "X-Tenant-Id: abc123"
```

## Multi-Tenancy Strategies

This example demonstrates the **Tenant ID Column** approach, where all tenants share a database but data is filtered by `tenant_id`.

Other strategies (not implemented in this demo):
- **Separate Database per Tenant** - Each tenant has their own database file
- **Schema per Tenant** - Each tenant has their own schema in a shared database (PostgreSQL)

## Architecture

```
Dext.Examples.MultiTenancy/
├── Dext.Examples.MultiTenancy.dpr    # Main program
├── Domain/
│   ├── MultiTenancy.Entities.pas     # TTenant, TProduct entities
│   └── MultiTenancy.DbContext.pas    # Database contexts
├── Middleware/
│   └── MultiTenancy.Middleware.pas   # Tenant resolution
└── Features/
    ├── MultiTenancy.Service.pas      # Tenant & Product services
    └── MultiTenancy.Endpoints.pas    # API endpoints
```

## See Also

- [Configuration](../../Docs/Book/10-advanced/configuration.md) - Environment-based config
- [Middleware](../../Docs/Book/02-web-framework/middleware.md) - Custom middleware
