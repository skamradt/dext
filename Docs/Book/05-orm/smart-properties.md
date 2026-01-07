# Smart Properties

Type-safe query expressions using `Prop<T>`.

> 📦 **Example**: [Web.SmartPropsDemo](../../../Examples/Web.SmartPropsDemo/)

## The Problem

Traditional string-based queries are error-prone:

```pascal
// Typo won't be caught at compile time!
Query.Where('usre_name = ?', ['John']);  // Bug: "usre_name"
```

## The Solution: Smart Properties

```pascal
type
  [Table('users')]
  TUser = class
  public
    class var Props: TUserProps;  // Smart properties
    
    property Id: Integer;
    property Name: string;
    property Age: Integer;
  end;
  
  TUserProps = record
    Id: Prop<Integer>;
    Name: Prop<string>;
    Age: Prop<Integer>;
  end;

// Type-safe querying!
var Adults := Context.Users
  .Where(TUser.Props.Age >= 18)  // Compile-time checked!
  .OrderBy(TUser.Props.Name)
  .ToList;
```

## Defining Smart Properties

### Option 1: Inline Record

```pascal
type
  TUser = class
  public
    class var Props: record
      Id: Prop<Integer>;
      Name: Prop<string>;
      Email: Prop<string>;
    end;
  end;
```

### Option 2: Separate Record

```pascal
type
  TUserProps = record
    Id: Prop<Integer>;
    Name: Prop<string>;
    Email: Prop<string>;
  end;
  
  TUser = class
  public
    class var Props: TUserProps;
  end;
```

## Query Operations

### Comparison

```pascal
TUser.Props.Age = 25       // Equal
TUser.Props.Age <> 25      // Not equal
TUser.Props.Age > 18       // Greater than
TUser.Props.Age >= 18      // Greater or equal
TUser.Props.Age < 65       // Less than
TUser.Props.Age <= 65      // Less or equal
```

### String Operations

```pascal
TUser.Props.Name.Contains('John')
TUser.Props.Name.StartsWith('J')
TUser.Props.Name.EndsWith('son')
TUser.Props.Email.IsNull
TUser.Props.Email.IsNotNull
```

### Logical Operators

```pascal
// AND
(TUser.Props.Age >= 18) and (TUser.Props.Age <= 65)

// OR
(TUser.Props.Status = 'active') or (TUser.Props.IsAdmin = True)

// NOT
not TUser.Props.IsDeleted
```

### IN Clause

```pascal
TUser.Props.Status.In(['active', 'pending', 'review'])
TUser.Props.Id.In([1, 2, 3, 4, 5])
```

## Full Example

```pascal
var
  ActiveAdults: IList<TUser>;
begin
  ActiveAdults := Context.Users
    .Where(
      (TUser.Props.Age >= 18) and 
      (TUser.Props.Status = 'active') and
      (TUser.Props.Email.IsNotNull)
    )
    .OrderBy(TUser.Props.Name)
    .Take(10)
    .ToList;
end;
```

## Generated SQL

The smart properties generate optimized SQL:

```sql
SELECT * FROM users 
WHERE age >= 18 
  AND status = 'active' 
  AND email IS NOT NULL 
ORDER BY name 
LIMIT 10
```

---

[← Querying](querying.md) | [Next: Specifications →](specifications.md)
