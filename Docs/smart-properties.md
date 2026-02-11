# Smart Properties (Type-Safe Queries)

Smart Properties enable type-safe query expressions in Dext ORM without magic strings.

> ⚠️ **Update**: This guide reflects the modern **Record Helper** approach. You do **NOT** need to change your Entity types to `Prop<T>`. Use standard Delphi types.

## Overview

Instead of strings:
```pascal
Users.Where('Age > 18'); // ❌ Unsafe, no compile-time check
```

Use typed expressions:
```pascal
var u := TUser.Props;    // ✅ Type-safe helper
Users.Where(u.Age > 18);
```

## How it Works

The Dext Compiler Plugin (or Record Helpers) auto-generates a `Props` static property for your entities. This returns a proxy object where every property returns a `Prop<T>` metadata record instead of the actual value.

### 1. Define Entity (Standard Types)

```pascal
type
  [Table('users')]
  TUser = class
  private
    FName: string;
    FAge: Integer;
  public
    property Name: string read FName write FName;
    property Age: Integer read FAge write FAge;
  end;
```

### 2. Query with Props

```pascal
// "var inline" is recommended for conciseness
var u := TUser.Props;

var Results := Context.Users
  .Where(u.Age >= 18)
  .Where(u.Name.StartsWith('John'))
  .OrderBy(u.Age.Desc)
  .ToList;
```

## Capabilities

### Comparisons
```pascal
u.Age = 18
u.Age <> 18
u.Age > 18
u.Age <= 18
```

### String Operations
```pascal
u.Name.StartsWith('A')
u.Name.EndsWith('z')
u.Name.Contains('elm')
u.Name.Like('A%')
```

### Null Handling
```pascal
u.Email.IsNull
u.Email.IsNotNull
```

### Logical Operations
```pascal
// Implicit AND
.Where(u.Age > 18)
.Where(u.Active = True)

// Explicit Boolean Logic
.Where((u.Age > 18) and (u.Active = True))
.Where((u.Status = 'Pending') or (u.Status = 'Failed'))
```

## Advanced: Custom Types (Enums)

For enums, use `Prop<TEnum>` to enable smart comparisons if the helper doesn't support them auto-magically:

```pascal
var o := TOrder.Props;
Context.Orders.Where(o.Status.Equals(osPaid));
```

---

> 📚 **Reference**: See [Book: Querying](Book/05-orm/querying.md) for full documentation.
