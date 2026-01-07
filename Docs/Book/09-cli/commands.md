# CLI Commands

Overview of all `dext` CLI commands.

## Available Commands

| Command | Description |
|---------|-------------|
| `help` | Show all commands |
| `migrate:up` | Run pending migrations |
| `migrate:down` | Rollback last migration |
| `migrate:list` | List migration status |
| `migrate:generate` | Generate new migration |
| `test` | Run test suite |
| `scaffold` | Generate entities from DB |
| `ui` | Start web dashboard |
| `config:init` | Initialize configuration |
| `env:scan` | Scan for Delphi environments |

## Migration Commands

### migrate:up

Run all pending migrations:

```bash
dext migrate:up
```

### migrate:down

Rollback the last applied migration:

```bash
dext migrate:down
```

### migrate:list

Show migration status:

```bash
dext migrate:list
```

Output:
```
Migration Status
================
[✓] 001_CreateUsers       Applied: 2026-01-05 10:30:00
[✓] 002_AddEmailToUsers   Applied: 2026-01-06 14:15:00
[ ] 003_CreateOrders      Pending
```

### migrate:generate

Create a new migration class:

```bash
dext migrate:generate --name CreateProducts
```

## Test Commands

### Basic test run

```bash
dext test
```

### With code coverage

```bash
dext test --coverage
```

### Generate HTML report

```bash
dext test --html --output TestReport.html
```

### All options

```bash
dext test --coverage --html --xml --json --output ./reports/
```

## Scaffold Command

Generate entities from database:

```bash
dext scaffold -c "mydb.db" -d sqlite -o Entities.pas
```

Options:
- `-c, --connection` - Connection string
- `-d, --driver` - Database driver (sqlite, pg, mssql, firebird)
- `-o, --output` - Output file
- `--fluent` - Use fluent mapping
- `-t, --tables` - Specific tables (comma-separated)

## Dashboard

Start the web monitoring UI:

```bash
dext ui
dext ui --port 8080
```

Visit `http://localhost:3000` (or specified port).

---

[← CLI](README.md) | [Next: Migrations →](migrations.md)
