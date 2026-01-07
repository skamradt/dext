# Multi-Tenancy

Implement SaaS applications with transparent data isolation.

## Multi-Tenancy Strategies

Dext supports three main strategies:

1. **Shared Database (Column-based isolation)**
2. **Separate Schema (PostgreSQL/SQL Server)**
3. **Separate Database**

## Shared Database (Column-based)

Add a `TenantId` column to your tables, and Dext will automatically apply filters to all queries.

```pascal
type
  [Table('orders'), MultiTenant]
  TOrder = class
  public
    [PK] property Id: Integer;
    property TenantId: string; // Isolation column
    property Description: string;
  end;
```

## Configuring Tenant via Middleware

The framework resolves the current tenant through the request (Header, Host, Query, etc.):

```pascal
App.UseMultiTenancy(procedure(Options: TMultiTenancyOptions)
  begin
    // Resolve tenant from 'X-Tenant' header
    Options.ResolveFromHeader('X-Tenant');
  end);
```

## Schema Isolation

For higher security and performance, use separate schemas (e.g., PostgreSQL `search_path`):

```pascal
App.UseMultiTenancy(procedure(Options: TMultiTenancyOptions)
  begin
    Options.Strategy := TTenancyStrategy.Schema;
    Options.ResolveFromHost; // e.g., customer1.myapp.com
  end);
```

When a connection is opened, Dext automatically executes the command to switch the default schema based on the resolved tenant.

## Advantages of Multi-Tenancy in Dext

- **Transparency**: You write `Context.Users.ToList` and the framework adds `WHERE TenantId = 'abc'` automatically.
- **Security**: Prevents data leakage between customers at the architectural level.
- **Migrations**: The `dext migrate:up` CLI can apply migrations across all tenant schemas/databases.

---

[← Scaffolding](scaffolding.md) | [Next: Database as API →](../06-database-as-api/README.md)
