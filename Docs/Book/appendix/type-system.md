# Appendix: Type System Reference

Quick reference for Dext types and ORM mappings.

## Smart Types

| Type | Description | SQL Mapping |
|------|-------------|-------------|
| `Prop<T>` | Smart property wrapper | N/A (compile-time) |
| `Nullable<T>` | Nullable value | `NULL` columns |
| `ILazy<T>` | Lazy-loaded reference | Foreign keys |
| `TUUID` | RFC 9562 UUID | `UUID` / `GUID` |

## Entity Attributes

| Attribute | Description |
|-----------|-------------|
| `[Table('name')]` | Map class to table |
| `[Column('name')]` | Map property to column |
| `[PK]` | Primary key |
| `[AutoInc]` | Auto-increment |
| `[ForeignKey('col')]` | Foreign key reference |
| `[NotMapped]` | Exclude from mapping |
| `[SoftDelete]` | Soft delete support |
| `[Version]` | Optimistic concurrency |

## SQL Type Mappings

| Delphi Type | PostgreSQL | SQL Server | SQLite |
|-------------|------------|------------|--------|
| `Integer` | `INTEGER` | `INT` | `INTEGER` |
| `Int64` | `BIGINT` | `BIGINT` | `INTEGER` |
| `Double` | `DOUBLE PRECISION` | `FLOAT` | `REAL` |
| `string` | `VARCHAR/TEXT` | `NVARCHAR` | `TEXT` |
| `TDateTime` | `TIMESTAMP` | `DATETIME2` | `TEXT` |
| `Boolean` | `BOOLEAN` | `BIT` | `INTEGER` |
| `TGUID` | `UUID` | `UNIQUEIDENTIFIER` | `TEXT` |
| `TBytes` | `BYTEA` | `VARBINARY` | `BLOB` |

---

[← Advanced](../10-advanced/README.md)
