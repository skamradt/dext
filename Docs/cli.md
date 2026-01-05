# 🛠️ Dext CLI Tool Documentation

The `Dext.Hosting.CLI` (also referred to as `dext.exe` or `DextTool.exe`) is the command-line interface for the Dext Framework. It provides essential utilities for project management, testing, and database migrations.

> 📝 **Note**: The CLI tool is usually embedded within your application if you use `Dext.Hosting`, but it can also be compiled as a standalone tool.

## 🚀 Usage Syntax

```bash
dext <command> [arguments] [options]
```

To see available commands:

```bash
dext help
```

---

## 🖥️ Dashboard UI

### `ui`
Launches the web-based dashboard for visual Dext management, configuration, and environment handling.

**Syntax:**
```bash
dext ui [--port <number>]
```

**Features:**
- **Projects**: View recent projects and their statuses.
- **Tests**: View test results, code coverage metrics, and access full HTML reports.
- **Settings**: Configure global paths (Dext CLI, Code Coverage) and manage Delphi environments.
- **Tools**: Auto-install tools like CodeCoverage (via Settings).

---

## 🌍 Environment Commands

Manage detected Delphi installations and configure which version to use for compilation.

### `env scan`
Scans the Windows Registry for available Delphi installations and updates the global `config.yaml`.

**Syntax:**
```bash
dext env scan
```

### `env list`
Lists all configured Delphi installations and indicates the default one.

**Syntax:**
```bash
dext env list
```

---

## 🧪 Testing Commands

### `test`
Runs the project's test suite. It automatically detects your `.dproj` (must contain "Test" in the name), builds it, and executes the resulting binary.

**Syntax:**
```bash
dext test [options]
```

**Options:**
- `--project=<path>`: Specifies the Delphi project file (`.dproj`) to build and test. If omitted, it searches for a `*Test*.dproj` in the current directory.
- `--coverage`: Enables code coverage analysis.
  - Builds the project with debug information (`-map` file).
  - Runs tests using `CodeCoverage.exe`.
  - Generates HTML and XML reports in `TestOutput/report`.
  - **Quality Gate**: Checks `coverage.threshold` from `dext.json` and fails the build if not met.

**Configuration (`dext.json`):**
Values in `dext.json` serve as defaults if CLI flags are not provided.

```json
{
  "test": {
    "project": "Tests/MyProjectTests.dproj",
    "reportDir": "build/reports",
    "coverageThreshold": 80.0,
    "coverageExclude": [
      "*Dext.*",
      "*ThirdParty*"
    ]
  }
}
```

---

## 🗄️ Migration Commands

The CLI integrates with `Dext.Entity` to manage comprehensive database schema migrations.

### `migrate:up`
Applies all pending migrations to the database.

**Syntax:**
```bash
dext migrate:up [--source <path>]
```

**Options:**
- `--source <path>` (alias `-s`): Directory containing migration JSON files. Defaults to internal registry if omitted.

### `migrate:down`
Reverts migrations. By default, it reverts the last applied migration.

**Syntax:**
```bash
dext migrate:down [--target <id>]
```

**Options:**
- `--target <id>` (alias `-t`): Reverts migrations sequentially until the specified Migration ID is reached (inclusive). If omitted, reverts only the last one.

### `migrate:list`
Lists the status of all known migrations (Applied vs. Pending).

**Syntax:**
```bash
dext migrate:list
```

**Output Example:**
```text
Migration Status:
-----------------
[Applied]   202501010000_InitialSchema
[Pending]   202501021230_AddUsers
```

### `migrate:generate`
Creates a new empty JSON migration file with a timestamped ID.

**Syntax:**
```bash
dext migrate:generate <name> [--path <dir>]
```

**Arguments:**
- `<name>`: A descriptive name for the migration (e.g., `AddCustomerTable`).

**Options:**
- `--path <dir>` (alias `-p`): Directory to save the file. Defaults to current directory.

**Output:**
Generates a file like `20260104223000_AddCustomerTable.json`.

---

## ⚙️ Global Options

- `--help` / `-h` / `help`: Displays the help screen with a list of available commands.

---

## 📦 Installation

If compiling from source:

1. Open `Sources/DextFramework.groupproj`.
2. Build the `DextTool` project (found in `Apps/CLI`).
3. Add the output directory to your system `PATH`.
