# Entities & Mapping

Configure how classes map to database tables.

## Mapping Styles

Dext supports two mapping models:

1. **Explicit Mapping** (strings in attributes) — Use when the database already exists or names don't follow a pattern.
2. **Naming Strategies** (automatic mapping) — Recommended for new projects.

## Attribute Mapping

### Basic Entity

```pascal
uses
  Dext.Entity; // Facade: Table, Column, PK, AutoInc, Required, MaxLength

type
  [Table('users')]
  TUser = class
  private
    FId: Integer;
    FName: string;
    FEmail: string;
    FCreatedAt: TDateTime;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;

    [Required, MaxLength(100)]
    property Name: string read FName write FName;

    [Required, MaxLength(200)]
    property Email: string read FEmail write FEmail;

    [CreatedAt]
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  end;
```

> [!IMPORTANT]
> **Attribute declaration style**: Place attributes on the same line, separated by commas.  
> - ✅ `[Required, MaxLength(50), JSONName('code')]`  
> - ❌ `[Required]` on one line, `[MaxLength(50)]` on the next.

## Available Attributes

### Table Mapping

| Attribute | Description |
|-----------|-------------|
| `[Table('name')]` | Map class to table |
| `[Table]` | Map using Naming Strategy |
| `[Schema('schema')]` | Specify schema |

### Column Mapping

| Attribute | Description |
|-----------|-------------|
| `[Column('name')]` | Map to specific column |
| `[Column]` | Map using Naming Strategy |
| `[PK]` | Primary key |
| `[AutoInc]` | Auto-increment |
| `[NotMapped]` | Exclude from mapping AND JSON |
| `[Version]` | Optimistic concurrency versioning |
| `[SoftDelete('deleted_col', 1, 0)]` | Logical deletion |
| `[CreatedAt]` | Automatic timestamp on insertion |
| `[UpdatedAt]` | Automatic timestamp on update |

### Validation Attributes

| Attribute | Description |
|-----------|-------------|
| `[Required]` | NOT NULL constraint (validated on SaveChanges) |
| `[MaxLength(N)]` | Maximum string length |
| `[MinLength(N)]` | Minimum string length |

> [!WARNING]
> **`[StringLength]` does NOT exist in Dext!** Use `[MaxLength(N)]` instead.

These attributes require `Dext.Entity` in uses. Validation runs automatically on `SaveChanges`.

### Relationships

| Attribute | Description |
|-----------|-------------|
| `[ForeignKey('col')]` | Foreign key column |
| `[InverseProperty('prop')]` | Navigation link |

### Entity Collections & Ownership

When defining `IList<T>` properties that are also managed by the `DbContext`:

1. Use `FItems: IList<TChild>` as a private field.
2. Initialize in constructor with `TCollections.CreateList<TChild>(False)`.
3. **Crucial**: Pass `False` for `OwnsObjects`.
   - **Reason**: The DbContext already manages entity lifecycle. If the list also owns them (`True`), you get **Invalid Pointer Operation** (Double Free) during shutdown.
4. **Unit Tests**: Since no DbContext exists, you **must manually free** child items in your test's `finally` block.

### Type Hints

| Attribute | Description |
|-----------|-------------|
| `[MaxLength(100)]` | Max string length |
| `[Precision(18, 2)]` | Precision and Scale for numeric/decimal |
| `[Default('value')]` | Default value in database |
| `[JsonColumn]` | Treat column as JSON |
| `[DbType(ftGuid)]` | Force a specific TFieldType |

### Type Conversion

| Attribute | Description |
|-----------|-------------|
| `[TypeConverter(TMyConverter)]` | Custom converter for this property |

```pascal
type
  TUnixTimestampConverter = class(TTypeConverterBase)
  public
    function CanConvert(ATypeInfo: PTypeInfo): Boolean; override;
    function ToDatabase(const AValue: TValue; ADialect: TDatabaseDialect): TValue; override;
    function FromDatabase(const AValue: TValue; ATypeInfo: PTypeInfo): TValue; override;
  end;

  [Table('events')]
  TEvent = class
  private
    FCreatedAt: TDateTime;
    FScheduledAt: TDateTime;
  public
    // Custom converter - stored as Unix timestamp
    [TypeConverter(TUnixTimestampConverter)]
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;

    // Default TDateTime converter (ISO format)
    property ScheduledAt: TDateTime read FScheduledAt write FScheduledAt;
  end;
```

## Nullable Columns

Use `Nullable<T>` for nullable database columns:

```pascal
uses
  Dext.Types.Nullable;  // Required for Nullable<T>

type
  [Table('tickets')]
  TTicket = class
  private
    FId: Integer;
    FAssigneeId: Nullable<Integer>;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;

    [ForeignKey('Assignee')]
    property AssigneeId: Nullable<Integer> read FAssigneeId write FAssigneeId;
  end;
```

**Implicit conversion** works automatically:
```pascal
// Integer → Nullable<Integer>
Ticket.AssigneeId := AgentId;   // Works without casts

// Check if has value
if Ticket.AssigneeId.HasValue then
  WriteLn('Assigned to: ', Ticket.AssigneeId.Value);

// Get value with default
var AssignId := Ticket.AssigneeId.GetValueOrDefault(0);

// Set to null
Ticket.AssigneeId := Nullable<Integer>.Null;
```

> [!WARNING]
> **`NavType<T>` does NOT exist in Dext!** Always use `Nullable<T>`.

## Change Tracking

The `ChangeTracker` may not automatically detect changes if the entity is detached. **Always** call `Update` explicitly before `SaveChanges`:

```pascal
// ❌ WRONG: Trusting auto-tracking can fail silently
Event.Status := esPublished;
FDb.SaveChanges;

// ✅ CORRECT: Force explicit update
Event.Status := esPublished;
FDb.Events.Update(Event);  // Ensures State = Modified
FDb.SaveChanges;
```

## Auto-Generated IDs

`SaveChanges` automatically populates IDs for inserted entities (`[AutoInc]`).

```pascal
var User := TUser.Create;
User.Name := 'Alice';
FDb.Users.Add(User);
FDb.SaveChanges;

// ✅ User.Id is already populated — no need to query the DB again!
WriteLn('New ID: ', User.Id);
```

> [!WARNING]
> ⛔ **NEVER** query the database again to retrieve the ID after saving. The object is already updated.

## Detach (Memory Management)

`FDb.Detach(Entity)` only removes the entity from the IdentityMap. It does **NOT** free memory.

```pascal
// ❌ WRONG: Memory Leak (detached entity becomes orphan)
FDb.Detach(Entity);
Entity := FDb.Find(ID);

// ✅ CORRECT: Free memory explicitly
FDb.Detach(Entity);
Entity.Free;
Entity := FDb.Find(ID);
```

## Naming Conventions

By default, Dext uses the property name as column name. For new projects, configure a Naming Strategy:

```pascal
// In DbContext
procedure TAppDbContext.OnModelCreating(Builder: TModelBuilder);
begin
  Builder.UseNamingStrategy(TSnakeCaseNamingStrategy);
end;
```

With `TSnakeCaseNamingStrategy`:
- Table `TUser` → `user`
- Column `CreatedAt` → `created_at`

Override with `[Table('name')]` and `[Column('name')]` when needed.

> 💡 **Reference**: See the [Orm.EntityStyles](../../../Examples/Orm.EntityStyles/) example for a side-by-side comparison.

---

[← Getting Started](getting-started.md) | [Next: Querying →](querying.md)
