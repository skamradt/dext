# Installation

## Requirements

- **Delphi**: 11.x Alexandria or 12.x Athens
- **Target**: Win32 or Win64
- **FireDAC**: Required for ORM (included with Delphi)

## Installation Methods

### Option 1: Clone from GitHub (Recommended)

```bash
git clone https://github.com/ArmyOfPirates/Dext.git
cd Dext
```

### Option 2: Download ZIP

Download the latest release from [GitHub Releases](https://github.com/ArmyOfPirates/Dext/releases).

## IDE Configuration

### 1. Add Library Paths

In Delphi, go to **Tools → Options → Language → Delphi → Library**:

Add these paths to the **Library Path**:

```
<DextPath>\Sources
<DextPath>\Sources\Core
<DextPath>\Sources\Data
<DextPath>\Sources\Testing
<DextPath>\Sources\Web
<DextPath>\Sources\Hosting
```

### 2. Install Design-Time Packages (Optional)

For IDE integration, install the packages in:
```
<DextPath>\Packages\
```

## Verify Installation

Create a new Console Application and add this code:

```pascal
program VerifyDext;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Dext.Web;

begin
  WriteLn('Dext is installed correctly!');
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
```

If it compiles, you're ready! 🎉

---

[← Back to Getting Started](README.md) | [Next: Hello World →](hello-world.md)
