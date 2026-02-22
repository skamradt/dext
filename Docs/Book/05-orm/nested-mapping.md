# Multi-Mapping (Nested Objects)

Dext supports **Multi-Mapping** (similar to Dapper's multi-mapping), allowing you to hydrate complex object graphs from a single SQL query with multiple joins. This is achieved using the `[Nested]` attribute.

## The [Nested] Attribute

The `[Nested]` attribute tells the ORM that a property represents a nested object that should be hydrated from the columns of the current result set, rather than being loaded via a separate query (Lazy Loading) or an `Include` join.

### Example

```pascal
type
  TAddress = class
  public
    property Street: string read FStreet write FStreet;
    property City: string read FCity write FCity;
  end;

  [Table('Users')]
  TUser = class
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;

    [Nested]
    property Address: TAddress read FAddress write FAddress;
  end;
```

## Hydration Logic

When you execute a query for `TUser`, Dext expects the result set to contain columns for both the User and the Address.

```pascal
// SQL generated or manual
// SELECT Id, Name, Street, City FROM Users

var Users := Db.Users.ToList;
// Each User object will have its Address property automatically instantiated and populated.
```

## Advanced Multi-Mapping

You can use `[Nested]` with a prefix if your columns have specific naming conventions:

```pascal
type
  TUser = class
  public
    [Nested('addr_')]
    property Address: TAddress read FAddress write FAddress;
  end;

// Expects columns: addr_Street, addr_City
```

## When to use Multi-Mapping vs Include

*   **Use `Include`**: For standard relationships (1:1, 1:N) where the related entity has its own table and ID. This is the "Standard ORM" way.
*   **Use `[Nested]`**:
    *   For **Value Objects** (DDD pattern) that don't have their own identity and are stored in the same table as the owner.
    *   To manually optimize complex joins when using `FromSql`.
    *   When you want to avoid the overhead of tracking multiple separate entities and just want a single flat hydration.

## Recursive Nesting

Dext supports recursive nesting. You can have a `[Nested]` property inside another `[Nested]` class, and the hydration engine will traverse the tree as long as matching columns are found in the result set.
