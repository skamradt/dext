# Migrations

Version control for your database schema.

## Overview

Migrations let you:
- Track schema changes over time
- Apply/revert changes reproducibly
- Share schema between team members

## Creating a Migration

```pascal
unit Migration_001_CreateUsers;

interface

uses
  Dext.Entity.Migrations;

type
  [Migration(1, 'CreateUsers')]
  TMigration_001_CreateUsers = class(TMigration)
  public
    procedure Up; override;
    procedure Down; override;
  end;

implementation

procedure TMigration_001_CreateUsers.Up;
begin
  CreateTable('users', procedure(T: TTableBuilder)
    begin
      T.AddColumn('id').AsInteger.PrimaryKey.AutoIncrement;
      T.AddColumn('name').AsString(100).NotNull;
      T.AddColumn('email').AsString(255).NotNull.Unique;
      T.AddColumn('created_at').AsDateTime.Default('CURRENT_TIMESTAMP');
    end);
end;

procedure TMigration_001_CreateUsers.Down;
begin
  DropTable('users');
end;

end.
```

## CLI Commands

```bash
# Apply pending migrations
dext migrate:up

# Rollback last migration
dext migrate:down

# Check status
dext migrate:list

# Generate skeleton
dext migrate:generate --name AddOrdersTable
```

## Table Builder API

### Columns

```pascal
T.AddColumn('id').AsInteger.PrimaryKey.AutoIncrement;
T.AddColumn('name').AsString(100).NotNull;
T.AddColumn('email').AsString(255).Nullable;
T.AddColumn('price').AsDecimal(10, 2).Default('0.00');
T.AddColumn('is_active').AsBoolean.Default('true');
T.AddColumn('created_at').AsDateTime;
T.AddColumn('data').AsText;  // CLOB/TEXT
T.AddColumn('binary').AsBlob;  // BLOB
T.AddColumn('uuid').AsGuid;
```

### Constraints

```pascal
T.AddColumn('email').AsString(255).Unique;
T.AddColumn('status').AsString(20).Check('status IN (''active'', ''inactive'')');
T.AddForeignKey('user_id', 'users', 'id').OnDeleteCascade;
T.AddIndex('idx_email', 'email');
T.AddUniqueIndex('idx_email_unique', 'email');
```

### Alter Table

```pascal
procedure TMigration_002.Up;
begin
  AlterTable('users', procedure(T: TTableBuilder)
    begin
      T.AddColumn('phone').AsString(20).Nullable;
      T.DropColumn('legacy_field');
      T.RenameColumn('name', 'full_name');
    end);
end;
```

## Raw SQL

For complex operations:

```pascal
procedure TMigration_003.Up;
begin
  Execute('CREATE INDEX CONCURRENTLY idx_users_email ON users(email)');
end;
```

## Registering Migrations

Add to your program's uses clause:

```pascal
uses
  Migration_001_CreateUsers,
  Migration_002_AddEmailIndex,
  Migration_003_CreateOrders;
```

Dext auto-discovers `[Migration]` attributed classes.

---

[← Relationships](relationships.md) | [Next: Scaffolding →](scaffolding.md)
