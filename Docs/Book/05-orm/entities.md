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
| `[Version]` | Optimistic concurrency versioning |
| `[SoftDelete('deleted_col', 1, 0)]` | Logical deletion with DeletedValue and NotDeletedValue |
| `[CreatedAt]` | Automatic timestamp on insertion |
| `[UpdatedAt]` | Automatic timestamp on update |

### Relationships

| Attribute | Description |
|-----------|-------------|
| `[ForeignKey('col')]` | Foreign key column |
| `[InverseProperty('prop')]` | Navigation link |

### Entity Collections & Ownership

When defining `IList<T>` properties that are also managed by the `DbContext` (e.g., in a parent-child relationship):

1. Use `FItems: IList<TChild>` as a private field.
2. Initialize it in the constructor using `TCollections.CreateList<TChild>(False)`.
3. **Crucial**: Pass `False` for `OwnsObjects`.
   - **Reason**: The `DbContext` already manages the lifecycle of tracked entities. If the list also owns them (`True`), you will encounter an **Invalid Pointer Operation** (Double Free) during shutdown.
4. **Unit Tests**: Since no DbContext exists in unit tests, you **must manually free** the child items in your test's `finally` block.

### Type Hints

| Attribute | Description |
|-----------|-------------|
| `[StringLength(100)]` | Max string length |
| `[MaxLength(100)]` | Same as StringLength (Alias) |
| `[Precision(18, 2)]` | Precision and Scale for numeric/decimal |
| `[Required]` | NOT NULL constraint |
| `[Default('value')]` | Default value in database |
| `[JsonColumn]` | Treat column as JSON (converts to object/list) |
| `[DbType(ftGuid)]` | Force a specific TFieldType for database parameter |

### Type Conversion

| Attribute | Description |
|-----------|-------------|
| `[TypeConverter(TMyConverter)]` | Custom converter for this property |

Use `[TypeConverter]` to override how a specific property is converted to/from the database:

```pascal
type
  // Custom converter: stores TDateTime as Unix timestamp
  TUnixTimestampConverter = class(TTypeConverterBase)
  public
    function CanConvert(ATypeInfo: PTypeInfo): Boolean; override;
    function ToDatabase(const AValue: TValue; ADialect: TDatabaseDialect): TValue; override;
    function FromDatabase(const AValue: TValue; ATypeInfo: PTypeInfo): TValue; override;
  end;

  [Table('events')]
  TEvent = class
  private
    FId: Integer;
    FName: string;
    FCreatedAt: TDateTime;
    FScheduledAt: TDateTime;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    
    // Uses custom converter - stored as Unix timestamp (Integer)
    [TypeConverter(TUnixTimestampConverter)]
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    
    // Uses default TDateTime converter (ISO format)
    property ScheduledAt: TDateTime read FScheduledAt write FScheduledAt;
  end;
```

> 💡 The property-level converter takes priority over global type converters.

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

> 💡 **Reference**: See the [Orm.EntityStyles](../../../Examples/Orm.EntityStyles/) example for a side-by-side comparison between Classic and Smart entities.

---

[← Getting Started](getting-started.md) | [Next: Querying →](querying.md)
