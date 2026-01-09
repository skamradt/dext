# Querying

Fluent query API for retrieving data.

## Return Types: IList<T>

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

### Lambda Expression

```pascal
var ActiveUsers := Context.Users
  .Where(function(U: TUser): Boolean
    begin
      Result := U.IsActive;
    end)
  .ToList;
```

### Smart Properties (Recommended)

```pascal
var ActiveUsers := Context.Users
  .Where(TUser.Props.IsActive = True)
  .ToList;

// Multiple conditions
var Results := Context.Users
  .Where(
    (TUser.Props.Age >= 18) and 
    (TUser.Props.Status = 'active')
  )
  .ToList;
```

## Ordering

```pascal
// Ascending
var Users := Context.Users
  .OrderBy(TUser.Props.Name)
  .ToList;

// Descending
var Users := Context.Users
  .OrderByDescending(TUser.Props.CreatedAt)
  .ToList;

// Multiple columns
var Users := Context.Users
  .OrderBy(TUser.Props.LastName)
  .ThenBy(TUser.Props.FirstName)
  .ToList;
```

## Pagination

```pascal
var Page := Context.Users
  .OrderBy(TUser.Props.Id)
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
// Count
var Total := Context.Users.Count;
var ActiveCount := Context.Users
  .Where(TUser.Props.IsActive = True)
  .Count;

// Any / Exists
var HasAdmin := Context.Users
  .Where(TUser.Props.Role = 'admin')
  .Any;
```

## Raw SQL

For complex queries:

```pascal
var Users := Context.FromSql<TUser>(
  'SELECT * FROM users WHERE created_at > $1',
  [DateToISO(Yesterday)]
).ToList;
```

## Query Execution

Queries are lazy - executed only when you:

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
