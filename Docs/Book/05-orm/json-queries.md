# JSON Queries

Query data inside JSON/JSONB columns using the fluent expression API.

## Overview

Dext ORM supports querying JSON data stored in database columns. This is useful when you have semi-structured data that doesn't fit a fixed schema.

## Setup

### 1. Mark Column as JSON

Use the `[JsonColumn]` attribute on string properties that store JSON:

```pascal
type
  [Table('UserMetadata')]
  TUserMetadata = class
  private
    FId: Integer;
    FName: string;
    FSettings: string;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    
    [JsonColumn]  // or [JsonColumn(True)] for JSONB on PostgreSQL
    property Settings: string read FSettings write FSettings;
  end;
```

### 2. Database Column Types

| Database | Recommended Column Type |
|----------|------------------------|
| PostgreSQL | `JSONB` (indexed) or `JSON` |
| SQLite | `TEXT` (requires JSON1 extension) |
| MySQL | `JSON` |
| SQL Server | `NVARCHAR(MAX)` |

## Querying JSON Properties

Use the `.Json('path')` method on property expressions:

### Simple Property Access

```pascal
// Data: {"role": "admin", "theme": "dark"}

var Admins := Context.UserMetadata
  .Where(Prop('Settings').Json('role') = 'admin')
  .ToList;
```

**Generated SQL (PostgreSQL):**
```sql
SELECT * FROM "UserMetadata" 
WHERE "Settings" #>> '{role}' = :p1
```

### Nested Property Access

Access nested JSON structures using dot notation:

```pascal
// Data: {"profile": {"details": {"level": 5}}}

var Result := Context.UserMetadata
  .Where(Prop('Settings').Json('profile.details.level') = 5)
  .ToList;
```

**Generated SQL (PostgreSQL):**
```sql
SELECT * FROM "UserMetadata" 
WHERE "Settings" #>> '{profile,details,level}' = :p1::text
```

> 💡 Numeric values are automatically cast to TEXT for comparison when querying JSON.

### Check for NULL/Missing Keys

Query records where a JSON key doesn't exist or is null:

```pascal
// Find records without the "nonexistent" key
var Result := Context.UserMetadata
  .Where(Prop('Settings').Json('nonexistent').IsNull)
  .ToList;
```

**Generated SQL (PostgreSQL):**
```sql
SELECT * FROM "UserMetadata" 
WHERE ("Settings" #>> '{nonexistent}' IS NULL)
```

## Database-Specific Behavior

### PostgreSQL

- Uses `#>>` operator for text extraction
- Supports `JSONB` type with indexing and optimization
- Automatic `::text` cast when comparing with non-string values
- Automatic `::jsonb` cast on INSERT for `[JsonColumn]` properties

### SQLite

- Uses `json_extract()` function
- **Requires** SQLite compiled with `SQLITE_ENABLE_JSON1`
- Enable in `Dext.inc`: `{$DEFINE DEXT_ENABLE_SQLITE_JSON}`

### MySQL

- Uses `JSON_EXTRACT()` and `JSON_UNQUOTE()` functions
- Native `JSON` column type

### SQL Server

- Uses `JSON_VALUE()` function
- Store in `NVARCHAR(MAX)` columns

## JSONB INSERT (PostgreSQL)

When using `[JsonColumn(True)]` (UseJsonB = True), the ORM automatically casts string values to `jsonb` during INSERT:

```pascal
var Meta := TUserMetadata.Create;
Meta.Name := 'Admin';
Meta.Settings := '{"role": "admin"}';  // String with JSON content

Context.UserMetadata.Add(Meta);
Context.SaveChanges;
```

**Generated SQL:**
```sql
INSERT INTO "UserMetadata" ("Name", "Settings") 
VALUES (:p1, :p2::jsonb)
```

## Complete Example

```pascal
procedure TestJsonQueries(Context: TMyDbContext);
var
  User: TUserMetadata;
  Results: IList<TUserMetadata>;
begin
  // Insert test data
  User := TUserMetadata.Create;
  User.Name := 'Admin';
  User.Settings := '{"role": "admin", "permissions": ["read", "write"]}';
  Context.UserMetadata.Add(User);
  
  User := TUserMetadata.Create;
  User.Name := 'Guest';
  User.Settings := '{"role": "guest", "permissions": ["read"]}';
  Context.UserMetadata.Add(User);
  
  Context.SaveChanges;
  Context.DetachAll;
  
  // Query by JSON property
  Results := Context.UserMetadata
    .Where(Prop('Settings').Json('role') = 'admin')
    .ToList;
    
  Assert(Results.Count = 1);
  Assert(Results[0].Name = 'Admin');
end;
```

## Limitations

1. **Type Coercion**: JSON extraction returns TEXT; numeric comparisons require cast
2. **Indexing**: Only PostgreSQL JSONB supports native indexing
3. **Complex Queries**: Array indexing and advanced operators not yet supported
4. **SQLite**: Requires custom-compiled sqlite3.dll with JSON support

---

[← Querying](querying.md) | [Next: Relationships →](relationships.md)
