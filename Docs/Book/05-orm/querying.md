# Querying

Fluent query API for retrieving data.

## Return Types: IList\<T\>

> [!IMPORTANT]
> Dext uses `IList<T>` from `Dext.Collections`, not `TObjectList<T>` from `System.Generics.Collections`.

```pascal
uses
  Dext.Collections;

var Users: IList<TUser>;
begin
  Users := Context.Users.ToList;  // Returns IList<TUser>
  
  for var User in Users do
    WriteLn(User.Name);
end;
```

### Creating New Lists

Use the `TCollections.CreateList<T>` factory:

```pascal
// Create a new list
var MyList := TCollections.CreateList<TUser>;
MyList.Add(User1);
MyList.Add(User2);

// Create with ownership (auto-free objects)
var OwnedList := TCollections.CreateList<TUser>(True);
```

> [!TIP]
> `IList<T>` is an interface, so no `try/finally Free` is needed!

## Basic Queries

### Get All

```pascal
var Users := Context.Users.ToList;
```

### Find by ID

```pascal
var User := Context.Users.Find(1);
if User <> nil then
  WriteLn('Found: ', User.Name);
```

### First / FirstOrDefault

```pascal
// Throws if not found
var User := Context.Users.First;

// Returns nil if not found
var User := Context.Users.FirstOrDefault;
```

## Filtering (Where)

### Smart Properties (Recommended)

Use `var` inline for Props (simulating C# lambda variables):

```pascal
// ✅ CORRECT: var inline camelCase, close to usage scope
var u := TUser.Props;
var ActiveUsers := Context.Users
  .Where(u.IsActive = True)
  .ToList;

// Multiple conditions
var Results := Context.Users
  .Where((u.Age >= 18) and (u.Status = 'active'))
  .ToList;
```

> [!IMPORTANT]
> **Why camelCase?** To differentiate from PascalCase method variables.  
> In multi-table queries with Include/Join, each table has its own inline var (`var u`, `var o`, `var p`).

```pascal
// ❌ WRONG: Verbose, repeats TUser.Props for every field
ExistingUser := Context.Users
  .Where((TUser.Props.Email = Email) and
         (TUser.Props.Status <> usDeleted))
  .FirstOrDefault;

// ✅ CORRECT: var inline, concise
var u := TUser.Props;
ExistingUser := Context.Users
  .Where((u.Email = Email) and (u.Status <> usDeleted))
  .FirstOrDefault;
```

### Lambda Expression

```pascal
var ActiveUsers := Context.Users
  .Where(function(U: TUser): Boolean
    begin
      Result := U.IsActive;
    end)
  .ToList;
```

## Ordering

> [!WARNING]
> `OrderBy` requires `.Asc` or `.Desc` — passing a Prop directly causes `E2010 Incompatible types`.

```pascal
var u := TUser.Props;

// Ascending
var Users := Context.Users
  .QueryAll
  .OrderBy(u.Name.Asc)
  .ToList;

// Descending
var Users := Context.Users
  .QueryAll
  .OrderBy(u.CreatedAt.Desc)
  .ToList;

// Multiple columns
var Users := Context.Users
  .QueryAll
  .OrderBy(u.LastName.Asc)
  .OrderBy(u.FirstName.Asc)
  .ToList;
```

```pascal
// ❌ WRONG: Causes E2010
.OrderBy(TUser.Props.CreatedAt)

// ✅ CORRECT: Use .Asc or .Desc
var u := TUser.Props;
.OrderBy(u.CreatedAt.Asc)
.OrderBy(u.CreatedAt.Desc)
```

## Pagination

```pascal
var u := TUser.Props;
var Page := Context.Users
  .QueryAll
  .OrderBy(u.Id.Asc)
  .Skip(20)   // Skip first 20
  .Take(10)   // Take next 10
  .ToList;
```

## Projection (Select)

```pascal
type
  TUserDto = record
    Name: string;
    Email: string;
  end;

var Dtos := Context.Users
  .Select<TUserDto>(function(U: TUser): TUserDto
    begin
      Result.Name := U.Name;
      Result.Email := U.Email;
    end)
  .ToList;
```

## Aggregates

```pascal
var u := TUser.Props;

// Count
var Total := Context.Users.Count;
var ActiveCount := Context.Users
  .Where(u.IsActive = True)
  .Count;

// Any / Exists (preferred for seeder checks)
var HasAdmin := Context.Users
  .Where(u.Role = 'admin')
  .Any;
```

## Custom Types (Enums)

If a specific type (like an Enum) doesn't have a pre-defined alias, create one using `Prop<T>`:

```pascal
type
  StatusType = Prop<TOrderStatus>;  // Custom smart type
```

Use in queries:
```pascal
var o := TOrder.Props;
var PaidOrders := Context.Orders
  .Where(o.Status = osPaid)
  .OrderBy(o.Total.Desc)
  .ToList;
```

## Ghost Entities (Prototype)

Use `Prototype.Entity<T>` from `Dext.Entity.Prototype` for type-safe metadata in queries:

```pascal
var p := Prototype.Entity<TProduct>;
var Expensive := Context.Products
  .Where(p.Price > 100)
  .ToList;
```

## Query Execution

Queries are lazy — executed only when you:

| Method | Effect |
|--------|--------|
| `.ToList` | Execute and return list |
| `.First` / `.FirstOrDefault` | Execute and return one |
| `.Count` | Execute and return count |
| `.Any` | Execute and return boolean |
| `.Find(id)` | Execute and return by PK |

```pascal
// Query NOT executed yet
var Query := Context.Users.Where(TUser.Props.Age > 18);

// Query executed HERE
var Users := Query.ToList;
```


## Performance & Caching

### SQL Generation Cache
Dext includes a singleton `TSQLCache` that caches the generated SQL for queries based on their structure (AST Signature). This significantly improves performance for repetitive queries by skipping the SQL generation phase.

The cache is **enabled by default** and is thread-safe.

#### Disabling the Cache
If you need to disable caching (e.g., for debugging or specific dynamic scenarios), you can toggle it globally:

```pascal
uses
  Dext.Entity.Cache;

// Disable globally
TSQLCache.Instance.Enabled := False;
```

---

[← Entities](entities.md) | [Next: Smart Properties →](smart-properties.md)
