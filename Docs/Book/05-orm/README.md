# 5. ORM (Dext.Entity)

Dext.Entity is a full-featured ORM for Delphi with support for multiple databases.

## Chapters

1. [Getting Started](getting-started.md) - First entity & context
2. [Entities & Mapping](entities.md) - Attributes and configuration
3. [Querying](querying.md) - Fluent query API
4. [Smart Properties](smart-properties.md) - Type-safe expressions
5. [Specifications](specifications.md) - Reusable query patterns
6. [Relationships](relationships.md) - 1:1, 1:N, Lazy Loading
7. [Migrations](migrations.md) - Schema versioning
8. [Scaffolding](scaffolding.md) - DB-first code generation
9. [Multi-Tenancy](multi-tenancy.md) - Schema/DB/Column isolation

## Quick Start

```pascal
// 1. Define Entity
type
  [Table('users')]
  TUser = class
  private
    FId: Integer;
    FName: string;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    [Column('name')]
    property Name: string read FName write FName;
  end;

// 2. Create Context
type
  TAppContext = class(TDbContext)
  public
    function Users: IDbSet<TUser>;
  end;

// 3. Use It!
var
  Ctx: TAppContext;
  User: TUser;
begin
  Ctx := TAppContext.Create(Connection, Dialect);
  
  // Create
  User := TUser.Create;
  User.Name := 'John';
  Ctx.Users.Add(User);
  Ctx.SaveChanges;
  
  // Read
  User := Ctx.Users.Find(1);
  
  // Query
  var ActiveUsers := Ctx.Users
    .Where(function(U: TUser): Boolean
      begin
        Result := U.Name.Contains('John');
      end)
    .ToList;
end;
```

## Supported Databases

| Database | Status |
|----------|--------|
| PostgreSQL | ✅ Stable |
| SQL Server | ✅ Stable |
| SQLite | ✅ Stable |
| Firebird | ✅ Stable |
| MySQL | 🟡 Beta |
| Oracle | 🟡 Beta |

---

[← API Features](../04-api-features/README.md) | [Next: Getting Started →](getting-started.md)
