# Database Dialects

Dext abstracts differences between database engines using **Dialects**.

## Supported Dialects

### PostgreSQL
Optimized for performance with native JSONB, Arrays, and UUIDs.
```pascal
Dialect := TPostgreSQLDialect.Create;
```

### SQL Server
Full T-SQL support including TOP, offsets, and modern date types.
```pascal
Dialect := TMSSQLDialect.Create;
```

### SQLite
Ideal for local development, mobile, and unit testing.
```pascal
Dialect := TSQLiteDialect.Create;
```

### Firebird
Support for versions 2.5 to 5.0, correctly handling pagination differences (ROWS vs OFFSET).
```pascal
Dialect := TFirebirdDialect.Create;
```

## Features per Dialeto

| Feature | PG | SQL Server | SQLite | Firebird |
|---------|----|------------|--------|----------|
| Pagination | ✅ | ✅ | ✅ | ✅ |
| Native UUID | ✅ | ✅ | ❌ | ❌ |
| JSON Support | ✅ | ✅ | ❌ | ❌ |
| Bulk Insert | ✅ | ✅ | ✅ | 🟡 |
| Multi-Tenancy (Schema) | ✅ | ✅ | ❌ | ❌ |

## Creating your own Dialect

If you need support for a database not listed, you can implement the `TSQLDialect` class:

```pascal
type
  TMyCustomDialect = class(TSQLDialect)
  public
    function GetLimitTemplate: string; override;
    function MapType(Field: TField): string; override;
  end;
```

---

[← Type System](type-system.md) | [Next: Troubleshooting →](troubleshooting.md)
