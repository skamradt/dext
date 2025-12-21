# 🗄️ Dext Entity - Migrations

## 📖 Introduction to Migrations

### What are Migrations?
Database schema migrations are like version control for your database. They allow your team to define and share database schema changes over time. Instead of sharing loose SQL scripts or relying on a centralized DBA to manually apply changes, migrations provide a structured, automated, and repeatable way to evolve your database schema alongside your application code.

### The Theory Behind Migrations
The core concept is that the state of your database structure (schema) should be defined by code, not by manual changes in a database management tool. Each change (creating a table, adding a column, creating an index) is encapsulated in a discrete "Migration" file or class.

These migrations are ordered usually by a timestamp or version number. A "Migration Runner" (like `dext.exe` or `TMigrator`) is responsible for checking which migrations satisfy the current state of the database and applying the pending ones in the correct order.

### Advantages
1.  **Version Control**: Schema changes are committed to your VCS (Git), providing history, blame, and rollback capabilities.
2.  **Consistency**: Ensures all environments (Dev, QA, Staging, Production) possess the exact same database structure.
3.  **Automation**: Deployments can be fully automated. The application or a CI/CD pipeline can apply pending migrations automatically.
4.  **Collaboration**: Multiple developers can work on different features requiring schema changes without stepping on each other's toes, provided they create separate migration files.
5.  **Database Agnostic**: Dext Migrations use a fluent API (`TSchemaBuilder`) that abstracts the SQL dialect. You write the migration once, and it works on SQLite, PostgreSQL, SQL Server, etc.

---

## 🛠️ Components & Architecture

### Core Classes

#### `IMigration`
The interface that every migration must implement. It has two primary methods:
*   `Up(Builder: TSchemaBuilder)`: Defines the changes to apply (e.g., Create Table).
*   `Down(Builder: TSchemaBuilder)`: Defines how to revert the changes (e.g., Drop Table).

#### `TSchemaBuilder`
A fluent API used within the `Up` and `Down` methods to define schema operations without writing raw SQL.
*   `CreateTable('Users', ...)`
*   `AddColumn('Users', 'Email', 'VARCHAR', 100)`
*   `CreateIndex(...)`

#### `TMigrator`
The core engine that orchestrates the migration process.
*   Checks the `__DextMigrations` history table in the database.
*   Compares applied migrations with available migrations in the registry.
*   Executes pending migrations within a transaction.

#### `TDextCLI` & `dext.exe`
The command-line interface tool that allows you to manage migrations without running your main application.

---

## 🚀 How to Use

### 1. Creating a Migration

Currently, you can define a migration manually by implementing `IMigration`.

```pascal
unit Migrations.M20231025_CreateUsers;

interface

uses
  Dext.Entity.Migrations,
  Dext.Entity.Migrations.Builder;

type
  TMigration_20231025_CreateUsers = class(TInterfacedObject, IMigration)
  public
    function GetId: string;
    procedure Up(Builder: TSchemaBuilder);
    procedure Down(Builder: TSchemaBuilder);
  end;

implementation

function TMigration_20231025_CreateUsers.GetId: string;
begin
  Result := '20231025_CreateUsers'; // Unique ID (usually YYYYMMDD_Name)
end;

procedure TMigration_20231025_CreateUsers.Up(Builder: TSchemaBuilder);
begin
  Builder.CreateTable('Users', procedure(T: TTableBuilder)
  begin
    T.Column('Id', 'INTEGER').PrimaryKey.Identity;
    T.Column('Username', 'VARCHAR', 50).NotNull;
    T.Column('Email', 'VARCHAR', 100).Nullable;
    T.Column('CreatedAt', 'TIMESTAMP').Default('CURRENT_TIMESTAMP');
  end);
  
  Builder.CreateIndex('Users', 'IX_Users_Username', ['Username'], True); // Unique Index
end;

procedure TMigration_20231025_CreateUsers.Down(Builder: TSchemaBuilder);
begin
  Builder.DropTable('Users');
end;

initialization
  RegisterMigration(TMigration_20231025_CreateUsers.Create);

end.
```

### 2. Loading Configuration (`appsettings.json`)

The CLI tool (`dext.exe`) looks for an `appsettings.json` file in the same directory to determine which database to connect to.

```json
{
  "ConnectionStrings": {
    "DefaultConnection": "Data Source=MyDatabase.db;Mode=ReadWriteCreate"
  },
  "Database": {
    "Driver": "SQLite"
  }
}
```

### 3. Running Migrations via CLI

The `dext.exe` tool provides commands to manage your database.

#### `migrate:list`
Displays all registered migrations and their status (Applied or Pending).

```bash
dext.exe migrate:list
```

**Output:**
```
Migration Status:
-----------------
[Applied]   20231025_CreateUsers
[Pending]   20231026_AddPhoneNumber
```

#### `migrate:up`
Applies all pending migrations to the database configured in `appsettings.json`.

```bash
dext.exe migrate:up
```

**Output:**
```
Starting migration update...
   🚀 Applying migration: 20231026_AddPhoneNumber
database is up to date.
```

### 4. Running Migrations in Code

You can also run migrations automatically at application startup (useful for desktop apps or simple servers).

```pascal
var
  Migrator: TMigrator;
begin
  // Assume Context is your IDbContext
  Migrator := TMigrator.Create(Context);
  try
    Migrator.Migrate;
  finally
    Migrator.Free;
  end;
end;
```

---

## 🔮 Roadmap / Future Features

*   **Scaffolding**: Automatically generate Migration classes by comparing your Delphi Entity classes with the current database schema.
*   **Down Migrations**: CLI support for reverting the last migration (`migrate:down`).
*   **SQL Export**: Generate a SQL script instead of applying changes directly (`migrate:script`).
*   **Seed Data**: Support for data seeding migrations alongside schema migrations.
