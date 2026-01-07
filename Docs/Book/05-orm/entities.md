# Entities & Mapping

Configure how classes map to database tables.

## Mapping Styles

Dext supports two mapping styles:

1. **Attribute-based** (recommended for most cases)
2. **Fluent Mapping** (for POCO classes)

## Attribute Mapping

### Basic Entity

```pascal
type
  [Table('users')]
  TUser = class
  private
    FId: Integer;
    FName: string;
    FEmail: string;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    [Column('full_name')]
    property Name: string read FName write FName;
    
    [Column('email')]
    property Email: string read FEmail write FEmail;
  end;
```

## Available Attributes

### Table Mapping

| Attribute | Description |
|-----------|-------------|
| `[Table('name')]` | Map class to table |
| `[Schema('schema')]` | Specify schema |

### Column Mapping

| Attribute | Description |
|-----------|-------------|
| `[Column('name')]` | Map to specific column |
| `[PK]` | Primary key |
| `[AutoInc]` | Auto-increment |
| `[NotMapped]` | Exclude from mapping |
| `[Version]` | Optimistic concurrency |

### Relationships

| Attribute | Description |
|-----------|-------------|
| `[ForeignKey('col')]` | Foreign key column |
| `[InverseProperty('prop')]` | Navigation link |

### Type Hints

| Attribute | Description |
|-----------|-------------|
| `[StringLength(100)]` | Max string length |
| `[Required]` | NOT NULL |
| `[Default('value')]` | Default value |

## Nullable Columns

Use `Nullable<T>` for nullable database columns:

```pascal
uses
  Dext.Types.Nullable;

type
  [Table('products')]
  TProduct = class
  private
    FId: Integer;
    FDescription: Nullable<string>;  // Can be NULL
    FDiscount: Nullable<Double>;      // Can be NULL
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    property Description: Nullable<string> read FDescription write FDescription;
    property Discount: Nullable<Double> read FDiscount write FDiscount;
  end;
```

Using nullable values:

```pascal
// Check if has value
if Product.Discount.HasValue then
  WriteLn('Discount: ', Product.Discount.Value);

// Get value with default
var Disc := Product.Discount.GetValueOrDefault(0);

// Set to null
Product.Discount := Nullable<Double>.Null;
```

## Fluent Mapping

For POCO classes without attributes:

```pascal
type
  // Clean POCO
  TUser = class
  public
    Id: Integer;
    Name: string;
    Email: string;
  end;

// Register mapping
procedure RegisterMappings(Builder: TModelBuilder);
begin
  Builder.Entity<TUser>
    .Table('users')
    .HasKey('Id')
    .Prop('Id').AutoIncrement
    .Prop('Name').Column('full_name').MaxLength(100)
    .Prop('Email').Required;
end;
```

## Naming Conventions

By default, Dext uses:
- **Table**: Class name without 'T' prefix → `users`
- **Column**: Property name → `id`, `name`

Override with `[Table]` and `[Column]` attributes.

---

[← Getting Started](getting-started.md) | [Next: Querying →](querying.md)
