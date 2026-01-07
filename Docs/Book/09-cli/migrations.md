# CLI: Migrations

Manage your database lifecycle via command line.

## Available Commands

### migrate:up
Applies all pending migrations to the database.
```bash
dext migrate:up
```

### migrate:down
Reverts the last applied migration. Useful for undoing quick changes during development.
```bash
dext migrate:down
```

### migrate:list
Shows a table of all registered migrations and their status (applied/pending).
```bash
dext migrate:list
```

### migrate:generate
Creates a new hollow migration unit file with the correct naming pattern and timestamp.
```bash
dext migrate:generate --name AddProductsTable
```

## Multi-Tenancy in Migrations

If your application is Multi-Tenant, migration commands can traverse all tenants automatically:

```bash
# Applies migrations across all schemas for all customers
dext migrate:up --all-tenants
```

## Tips

- **Commit always**: Always commit your migrations along with the code that depends on them.
- **Don't alter old migrations**: If you need to change something already in production, create a new migration.

---

[← Commands](commands.md) | [Next: Scaffolding →](scaffolding.md)
