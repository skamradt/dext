# 🗄️ Orm.EntityDemo - Dext ORM Showcase

A comprehensive demonstration of the **Dext Entity ORM** capabilities. This project showcases modern ORM features including strongly-typed queries, lazy loading, soft delete, and multi-database support.

---

## ✨ Features Demonstrated

### Core ORM Features
- **CRUD Operations** - Create, Read, Update, Delete with automatic change tracking
- **Relationships** - Foreign keys, navigation properties, cascade actions
- **Lazy Loading** - Automatic loading of related entities on access
- **Explicit Loading** - Manual loading with `.Include()`
- **No Tracking** - Read-only queries for performance

### Advanced Queries
- **Strongly-Typed Expressions** - `Where(TUserType.Age > 18)`
- **Fluent Query Builder** - Chainable methods: `.Where().OrderBy().Take()`
- **Specifications Pattern** - Reusable query criteria
- **Complex Filters** - AND/OR conditions, LIKE, StartsWith, IsNull

### Data Integrity
- **Composite Keys** - Multi-column primary keys (Integer + String)
- **Soft Delete** - Logical deletion with `[SoftDelete]` attribute
- **Concurrency Control** - Optimistic locking with `[Version]`
- **Nullable Types** - Full `Nullable<T>` support

### Schema Management
- **Migrations** - Database schema versioning
- **Scaffolding** - Reverse-engineer entities from existing DB
- **Fluent Mapping** - Alternative to attributes

### Performance
- **Bulk Operations** - Batch insert/update/delete
- **Lazy Query Execution** - Deferred SQL generation
- **Async Support** - Non-blocking database operations

---

## 🚀 Getting Started

### Prerequisites
- Delphi 11+ (Alexandria or later)
- Dext Framework in Library Path

### Running the Tests

1. Open `Orm.EntityDemo.dproj` in Delphi
2. Build the project (F9)
3. Run the executable

The tests use **SQLite In-Memory** by default - no database setup required!

### Switch Database Provider

Edit `Orm.EntityDemo.dpr` and change the provider:

```pascal
// Default: SQLite Memory (no setup required)
ConfigureDatabase(dpSQLiteMemory);

// SQLite File (persisted)
ConfigureDatabase(dpSQLite);

// PostgreSQL
ConfigureDatabase(dpPostgreSQL);

// Firebird
ConfigureDatabase(dpFirebird);

// SQL Server
ConfigureDatabase(dpSQLServer);
```

---

## 📋 Test Suite

The demo includes **18 comprehensive test suites**:

| # | Test | Description |
|---|------|-------------|
| 1 | **TCRUDTest** | Basic Create, Read, Update, Delete |
| 2 | **TRelationshipTest** | Foreign Keys and navigation properties |
| 3 | **TAdvancedQueryTest** | Complex queries with filters and projections |
| 4 | **TCompositeKeyTest** | Multi-column primary keys |
| 5 | **TExplicitLoadingTest** | Manual loading of related entities |
| 6 | **TLazyLoadingTest** | Automatic loading on property access |
| 7 | **TFluentAPITest** | Query builder and LINQ-style operations |
| 8 | **TLazyExecutionTest** | Deferred query execution |
| 9 | **TBulkTest** | Batch insert/update/delete operations |
| 10 | **TConcurrencyTest** | Optimistic concurrency control |
| 11 | **TScaffoldingTest** | Reverse-engineering from database |
| 12 | **TMigrationsTest** | Schema versioning and migrations |
| 13 | **TCollectionsTest** | IList<T> integration |
| 14 | **TNoTrackingTest** | Read-only queries for performance |
| 15 | **TMixedCompositeKeyTest** | Integer + String composite keys |
| 16 | **TSoftDeleteTest** | Logical deletion with filters |
| 17 | **TAsyncTest** | Async database operations |
| 18 | **TTypeSystemTest** | Strongly-typed property expressions |

---

## 📖 Code Examples

### Basic CRUD

```pascal
// Create
var User := TUser.Create;
User.Name := 'Alice';
User.Age := 25;
Context.Entities<TUser>.Add(User);
Context.SaveChanges;

// Read
var Found := Context.Entities<TUser>.Find(1);

// Update
Found.Age := 26;
Context.Entities<TUser>.Update(Found);
Context.SaveChanges;

// Delete
Context.Entities<TUser>.Remove(Found);
Context.SaveChanges;
```

### Strongly-Typed Queries

```pascal
// Using TypeSystem for compile-time safety
var Adults := Context.Entities<TUser>.QueryAll
  .Where(TUserType.Age >= 18)
  .OrderBy(TUserType.Name.Asc)
  .ToList;

// Complex filters
var NYAdults := Context.Entities<TUser>.QueryAll
  .Where((TUserType.Age > 21) and (TUserType.City = 'NY'))
  .ToList;

// String operations
var AliceUsers := Context.Entities<TUser>.QueryAll
  .Where(TUserType.Name.StartsWith('Ali'))
  .ToList;
```

### Entity Definition

```pascal
[Table('users')]
TUser = class
private
  FId: Integer;
  FName: string;
  FAddressId: Nullable<Integer>;
  FAddress: Lazy<TAddress>;
public
  [PK, AutoInc]
  property Id: Integer read FId write FId;

  [Column('full_name')]
  property Name: string read FName write FName;

  [ForeignKey('AddressId'), NotMapped]
  property Address: TAddress read GetAddress write SetAddress;
end;
```

### Soft Delete

```pascal
// Entity with soft delete
[Table('tasks'), SoftDelete('IsDeleted')]
TTask = class
  // ...
  property IsDeleted: Boolean read FIsDeleted write FIsDeleted;
end;

// Normal query excludes deleted
var ActiveTasks := Context.Entities<TTask>.QueryAll.ToList;

// Include deleted
var AllTasks := Context.Entities<TTask>.QueryAll
  .IgnoreQueryFilters
  .ToList;

// Only deleted
var Trash := Context.Entities<TTask>.QueryAll
  .OnlyDeleted
  .ToList;
```

---

## ⚙️ Database Configuration

### SQLite (Default)
No setup required! Uses in-memory database.

### PostgreSQL
```pascal
TDbConfig.ConfigurePostgreSQL('localhost', 5432, 'postgres', 'postgres', 'password');
```

### Firebird
```pascal
TDbConfig.ConfigureFirebird('C:\temp\test.fdb', 'SYSDBA', 'masterkey');
```

### SQL Server
```pascal
// Windows Authentication
TDbConfig.ConfigureSQLServerWindowsAuth('localhost', 'dext_test');

// SQL Authentication
TDbConfig.ConfigureSQLServer('localhost', 'dext_test', 'sa', 'password');
```

---

## 🔧 Project Structure

```
Orm.EntityDemo/
├── Orm.EntityDemo.dpr              # Main program
├── EntityDemo.DbConfig.pas         # Database configuration
├── EntityDemo.Entities.pas         # Entity definitions
├── EntityDemo.Entities.Info.pas    # TypeSystem metadata
├── EntityDemo.Tests.Base.pas       # Base test class
├── EntityDemo.Tests.*.pas          # Individual test suites
└── README.md                       # This file
```

---

## 🐛 Troubleshooting

### "Driver not found"
Ensure FireDAC drivers are linked in your uses clause:
- SQLite: `FireDAC.Phys.SQLite`
- PostgreSQL: `FireDAC.Phys.PG`
- Firebird: `FireDAC.Phys.FB`
- SQL Server: `FireDAC.Phys.MSSQL`

### "Table already exists"
The tests automatically drop tables before running. If you see this:
1. Delete `test.db` file (if using SQLite file)
2. Or manually drop tables in your database

### Memory Leaks Reported
Some leaks are expected with FDConnection singletons. The framework uses FastMM4 for leak detection.

---

## 📚 Related Documentation

- [Dext ORM Guide](../../Docs/orm-guide.md)
- [Nullable Types Support](../../Docs/NULLABLE_SUPPORT.md)
- [Database Configuration](../../Docs/DATABASE_CONFIG.md)
- [Portuguese Version](README.pt-br.md)

---

## 📄 License

This example is part of the Dext Framework and is licensed under the Apache License 2.0.

---

*Happy Coding! 🚀*
