# DextFacadeGenerator

**Status: 🚧 Alpha - Requires manual review after generation**

A command-line tool that automatically generates type aliases and uses clauses for the Dext framework facade units (`Dext.pas`, `Dext.Entity.pas`, `Dext.Web.pas`).

## Purpose

The Dext framework uses "facade" units that re-export types from internal units, providing a simplified API for developers. Manually maintaining these facades is error-prone and time-consuming. This tool automates the generation of:

- **`.Uses.inc`** - Contains the `uses` clause with all internal units
- **`.Aliases.inc`** - Contains type aliases and constants

## Usage

### Quick Start

Run the batch script from the repository root:

```batch
cd C:\dev\Dext\DextRepository
Tools\UpdateFacades.bat
```

### Manual Execution

```batch
DextFacadeGenerator.exe <SourcePath> <OutputPath> <BaseName> [Options]
```

**Parameters:**
| Parameter | Description |
|-----------|-------------|
| `SourcePath` | Directory containing `.pas` files to scan |
| `OutputPath` | Directory where `.inc` files will be written |
| `BaseName` | Prefix for generated files (e.g., `Dext` → `Dext.Uses.inc`) |

**Options:**
| Option | Description |
|--------|-------------|
| `-rtti` | Use RTTI-based extraction (experimental, not recommended) |
| `-x:UnitName` | Exclude a unit from processing |

### Example

```batch
DextFacadeGenerator.exe ^
  "C:\dev\Dext\DextRepository\Sources\Core" ^
  "C:\dev\Dext\DextRepository\Sources\Core" ^
  "Dext" ^
  -x:Dext -x:Dext.Collections.SmartList
```

## Architecture

### AST-Based Parsing (Default)

The tool uses the [DelphiAST](https://github.com/SkliarOleksandr/DelphiAST) library to parse Pascal source files. It extracts:

- **Types**: Classes, Records, Interfaces, Enums, Type Aliases
- **Constants**: Typed and untyped constants

### Resilient Parsing Mode

The parser includes error recovery for complex syntax:

```pascal
try
  // Normal parsing
except
  // On failure: Register type name as dummy alias
  // Skip to semicolon and continue
end;
```

This allows the tool to continue processing even when encountering unsupported syntax like complex generics.

## Generated Files Structure

### `.Uses.inc`

```pascal
  Dext.Collections,
  Dext.Configuration.Binder,
  Dext.DI.Core,
  // ... more units
```

### `.Aliases.inc`

```pascal
// Auto-generated Aliases

  // Dext.Collections
  IList = Dext.Collections.IList;
  
  // Dext.DI.Interfaces
  IServiceCollection = Dext.DI.Interfaces.IServiceCollection;

const

  // Dext.DI.Interfaces
  Singleton = Dext.DI.Interfaces.Singleton;
```

## Integration with Facade Units

After generation, the facade units use the includes:

```pascal
unit Dext;

interface

uses
  {$I Dext.Uses.inc}

type
  {$I Dext.Aliases.inc}

// ... additional manual declarations
```

## Known Limitations

> ⚠️ **Manual review is required after every generation**

### 1. Generic Types
When a generic type is encountered, the parser may fail to extract subsequent types in the same unit.

**Workaround:** Add missing types manually to the `.inc` file.

### 2. Generic Class Fields
Fields from generic classes may be incorrectly extracted as type aliases (e.g., `FValue`, `FItems`).

**Workaround:** Remove field names from the generated aliases.

### 3. Forward Declarations
Forward type declarations are not properly handled and may prevent extraction of related types.

**Workaround:** Manually add the forward-declared types.

### 4. Visibility Keywords as Types
Parser may occasionally capture keywords like `public`, `protected`, `override` as type names.

**Workaround:** The generator filters common keywords, but review for edge cases.

### 5. External Dependencies
Units from RTL, VCL, or third-party libraries are not included in `.Uses.inc`.

**By Design:** External units must be added manually to the facade.

## Filtering

The generator automatically filters:

- Private and protected declarations
- Generic types (types with `<T>` parameters)
- Reserved keywords mistakenly parsed as types
- Duplicate declarations

## Rebuilding the Tool

```batch
cd C:\dev\Dext\DextRepository
call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
msbuild Tools\DextFacadeGenerator\DextFacadeGenerator.dproj /p:Config=Release
```

## File Structure

```
Tools/DextFacadeGenerator/
├── DextFacadeGenerator.dpr      # Main project file
├── DextFacadeGenerator.dproj    # Delphi project
├── FacadeGenerator.pas          # AST-based generator
├── FacadeGenerator.RTTI.pas     # RTTI-based generator (experimental)
└── README.md                    # This file
```

## Dependencies

- Delphi 11+ (tested on Delphi 12)
- DelphiAST library (included in `Libs\DelphiAST`)

## Future Improvements

- [ ] Better handling of generic types
- [ ] Support for forward declarations
- [ ] Incremental generation (only regenerate changed units)
- [ ] Validation mode (compare generated vs manual)
- [ ] IDE plugin integration

## Changelog

### v0.1.0 (2024-12-24)
- Initial release
- AST-based parsing with resilient error recovery
- RTTI mode (experimental, not recommended)
- Keyword filtering
- UpdateFacades.bat script

---

**Author:** Cesar Romero  
**License:** Apache 2.0
