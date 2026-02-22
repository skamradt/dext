# Concurrency & Locking

Dext supports both Optimistic and Pessimistic locking strategies to ensure data integrity in multi-user environments.

## Optimistic Concurrency

Optimistic concurrency assumes that conflicts are rare. It uses a version column to detect if a record has been modified by another process since it was loaded.

### Usage
Add the `[Version]` attribute to an integer property in your entity:

```pascal
type
  [Table('Products')]
  TProduct = class
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    [Version]
    property Version: Integer read FVersion write FVersion;
  end;
```

When you call `SaveChanges`, Dext automatically checks if the `Version` in the database matches the one in memory. If it doesn't, a `DbUpdateConcurrencyException` is raised.

## Pessimistic Locking

Pessimistic locking is used when conflicts are expected. It locks the record at the database level when it is read, preventing others from modifying it until your transaction is complete.

### Usage
Use the `Lock` method in your fluent query:

```pascal
var Product := Db.Products
  .Where(Prop('Id') = 1)
  .Lock(TLockMode.Update) // FOR UPDATE (PostgreSQL/Oracle) or UPDLOCK (SQL Server)
  .FirstOrDefault;

try
  Product.Price := Product.Price * 1.1;
  Db.SaveChanges;
finally
  // The lock is automatically released when the transaction ends
end;
```

### Supported Lock Modes

| Mode | SQL Equivalent | Description |
|------|----------------|-------------|
| `TLockMode.None` | (None) | Default behavior. |
| `TLockMode.Update` | `FOR UPDATE` / `UPDLOCK` | Request an exclusive lock for updating. |
| `TLockMode.Shared` | `FOR SHARE` / `HOLDLOCK` | Request a shared lock (allows others to read but not update). |

## Offline Locking

For long-running tasks where a database transaction cannot be kept open (e.g., a user editing a form for 10 minutes), Dext provides an **Offline Locking** mechanism.

### Usage
You can request a token for a specific entity and tenant:

```pascal
var Token := Db.LockManager.AcquireLock('TProduct', ProductId, TenantId);
if Token <> '' then
begin
  // Record is locked for this user
end;
```

To release the lock:
```pascal
Db.LockManager.ReleaseLock(Token);
```

> **Note**: Offline locks are typically stored in a dedicated `DextLocks` table and have an expiration time.
