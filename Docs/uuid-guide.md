# 🆔 UUID Support in Dext

Dext Framework provides **first-class support** for UUIDs (Universally Unique Identifiers), strictly adhering to **RFC 9562**. This ensures compatibility with modern databases (especially PostgreSQL) and Web APIs, resolving common endianness issues found in standard Delphi implementations.

## 🚀 Why TUUID?

Standard Delphi `TGUID` has a historical limitation:
*   **Delphi `TGUID`**: Little-Endian for the first 3 parts (`D1`, `D2`, `D3`).
*   **Network/Database (RFC)**: Big-Endian (Network Byte Order).

This mismatch often leads to "shuffled" GUIDs when reading/writing to PostgreSQL or external APIs. **Dext `TUUID`** solves this by using Big-Endian storage internally, while providing seamless conversion to `TGUID` for Delphi compatibility.

---

## 🛠️ Key Features

*   **RFC 9562 Compliant**: Correct Big-Endian storage.
*   **UUID v7 Support**: Time-ordered UUIDs, optimized for database primary keys (reduced fragmentation).
*   **Native ORM Support**: Maps directly to `uuid` columns in PostgreSQL/CockroachDB and `UNIQUEIDENTIFIER` in SQL Server.
*   **Web Model Binding**: Automatically bind UUIDs from URL parameters, Query Strings, and JSON Bodies.
*   **JSON Serialization**: Serializes as clean, canonical strings (lowercase, hyphenated).
*   **FireDAC Integration**: Automatic byte-order correction when reading UUIDs from PostgreSQL.

---

## 💻 Usage

### 1. Basic Usage (Dext.Types.UUID)

`TUUID` is a value type (record) with implicit operators for `string` and `TGUID`.

```pascal
uses
  Dext.Types.UUID;

var
  Id: TUUID;
begin
  // Generate random UUID (v4)
  Id := TUUID.NewV4;
  
  // Generate time-ordered UUID (v7) - Recommended for DB Keys
  Id := TUUID.NewV7;
  
  // Implicit String Conversion
  WriteLn('UUID: ' + Id); // Standard format: a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11
  
  // Implicit TGUID Conversion
  var Guid: TGUID := Id;
end;
```

### 2. Web API (Model Binding)

You can use `TUUID` directly in your Controller Actions or Minimal API endpoints.

```pascal
// Function with TUUID parameter
App.MapGet('/products/:id',
  function(Context: IHttpContext): IResult
  begin
    // Automatically binds from URL segment
    var Id := Context.Request.BindRoute<TUUID>('id');
    
    // ... lookup product ...
  end);

// DTO with TUUID
type
  TCreateUserCmd = record
    Id: TUUID;
    Name: string;
  end;

App.MapPost('/users',
  function(Context: IHttpContext): IResult
  begin
    // Automatically deserializes from JSON body
    var Cmd := Context.Request.BodyAs<TCreateUserCmd>;
    
    // ... use Cmd.Id ...
  end);
```

### 3. ORM (Dext.Entity)

Use `TUUID` as your Primary Key type. The ORM handles the correct mapping for each database.

```pascal
type
  [Table('orders')]
  TOrder = class
  private
    FId: TUUID;
    // ...
  public
    [PK]
    property Id: TUUID read FId write FId;
  end;

// Usage
var Order := TOrder.Create;
Order.Id := TUUID.NewV7; // Client-side generation (good for distributed systems)
DbContext.Orders.Add(Order);
DbContext.SaveChanges;

// Find by UUID
var Found := DbContext.Orders.Find('a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11');
```

#### Database Mapping
*   **PostgreSQL**: `uuid` (Native type)
*   **SQL Server**: `uniqueidentifier`
*   **SQLite/MySQL**: `char(36)`

### 4. JSON Serialization

`TUUID` serializes to a standard canonical string.

```json
{
  "id": "0193e4a9-7f30-746b-9c79-123456789abc",
  "name": "Example Item"
}
```

Standard `TGUID` serializes with braces in Delphi (`{...}`), but **Dext.Json enforces standard UUID format** for both `TUUID` and `TGUID` to ensuring web compatibility.

---

## 🔧 Technical: FireDAC and PostgreSQL

When reading UUID columns from PostgreSQL via FireDAC, there's an important byte-order consideration:

### The Problem

PostgreSQL stores UUIDs in **Big-Endian format** (RFC 9562). When FireDAC reads this data, it interprets the raw bytes as a Windows `TGUID` structure, which uses **Little-Endian** for the first three fields (D1, D2, D3).

This causes the UUID to appear "shuffled" when formatted as a string:
- **Database**: `830c3664-027d-4b87-8c98-76fb0aac08ec`
- **FireDAC TGUID.AsString**: `64360C83-7D02-874B-8C98-76FB0AAC08EC`

### Dext's Solution

Dext implements a dual-mode approach to ensure both Delphi compatibility and RFC compliance:

1.  **Driver Level**: The FireDAC driver returns the raw `TGUID` value directly from the database field. This preserves native Delphi behavior for properties typed as `TGUID`.
2.  **Converter Level**: The `TUuidConverter` (used for properties typed as `TUUID`) detects when it receives a `TGUID` from the driver. It then extracts the **raw bytes** directly from the memory structure. Since these bytes were stored in Big-Endian order by PostgreSQL, they are correctly mapped to `TUUID` without any "shuffling" or bit-flipping.

```pascal
// Internal implementation in Dext.Entity.TypeConverters
function TUuidConverter.FromDatabase(const AValue: TValue; ...): TValue;
begin
  if AValue.TypeInfo = TypeInfo(TGUID) then
  begin
    // PostgreSQL bytes are Big-Endian, but FireDAC thinks it's a Little-Endian TGUID.
    // We extract the bytes directly from memory to get the correct RFC format.
    G := AValue.AsType<TGUID>;
    Move(G, Bytes[0], 16);
    U := TUUID.FromRawBytes(Bytes);
    Result := TValue.From<TUUID>(U);
  end;
end;
```

This approach allows **TGUID and TUUID to coexist** in the same application while ensuring that `TUUID` always adheres to the correct string representation expected by Web APIs.

### 🆔 Identity Map & Primary Keys

Dext ORM includes a specialized conversion logic for internal **Identity Map** keys. When a `TUUID` or `TGUID` is used as a primary key, it is converted to a canonical, hyphenated, lowercase string for tracking. This ensures that lookups via `DbContext.Find(Id)` work reliably regardless of the input format.

### For Other Database Drivers

If you're implementing a custom database driver, ensure that:
1. UUID values are read as raw bytes or strings from the database
2. Avoid using `GUIDToString()` on values read from PostgreSQL
3. Use the byte-order-aware methods in `TUUID` for conversions

---

## ⚡ Performance Note: UUID v7

For high-volume databases, prefer **UUID v7** (`TUUID.NewV7`). It includes a timestamp component ensuring that new IDs are (mostly) increasing. This significantly reduces B-Tree index fragmentation compared to random v4 UUIDs, improving insert performance and cache locality.

---

## 📖 See Also

- [RFC 9562 - UUIDs](https://www.rfc-editor.org/rfc/rfc9562)
- [PostgreSQL UUID Type](https://www.postgresql.org/docs/current/datatype-uuid.html)
- [Model Binding Guide](model-binding.md) - TUUID/TGUID binding from route and body
- Examples: `Examples/Web.UUIDExample`
- Tests: `Tests/Entity/TestTypeConvertersDb.dpr`
