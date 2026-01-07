# Scaffolding

Generate entity classes from an existing database schema.

## Quick Start

```bash
dext scaffold -c "mydb.db" -d sqlite -o Entities.pas
```

## Options

| Option | Shortcut | Description |
|--------|-------|-------------|
| `--connection` | `-c` | Connection string or database file path |
| `--driver` | `-d` | Driver: `sqlite`, `pg`, `mssql`, `firebird` |
| `--output` | `-o` | Output file (default: `Entities.pas`) |
| `--unit` | `-u` | Unit name (default: based on filename) |
| `--fluent` | | Generate fluent mapping instead of attributes |
| `--tables` | `-t` | Specific tables (comma-separated) |

## Examples

### SQLite

```bash
dext scaffold -c "myapp.db" -d sqlite -o Models/Entities.pas
```

### PostgreSQL

```bash
dext scaffold \
  -c "Server=localhost;Port=5432;Database=myapp;User_Name=postgres;Password=secret" \
  -d pg \
  -o Entities.pas
```

## Generated Code

### Attribute-Based Mapping (Default)

```pascal
unit Entities;

interface

uses
  Dext.Entity.Attributes;

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

implementation

end.
```

---

[← Migrations](migrations.md) | [Next: Multi-Tenancy →](multi-tenancy.md)
