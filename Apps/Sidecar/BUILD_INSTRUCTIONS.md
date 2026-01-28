### Build Instructions for Dext Sidecar

When building `DextSidecar.dpr` (or any app using the Dext Framework), **DO NOT add Framework source paths** (`Sources\Core`, `Sources\Web`, etc.) to the project's Search Path or Command Line.

**Why?**
- The framework is already compiled into DCU/BPL files in the `Output` directory.
- Adding source paths causes the compiler to recompile framework units, creating "duplicate" DCUs incompatible with the pre-compiled ones (different timestamps/settings).
- This leads to `E2010 Incompatible types` and `F2063 Could not compile used unit` errors.

**Correct Way:**
1. Ensure the Framework is built first (run `build_framework.bat`).
2. Point the project **ONLY** to the `Output` directory (e.g., `..\..\Output\$(Platform)_$(Config)` or similar).
3. For MSBuild scripts, ensure `DCC_UnitSearchPath` includes the Output folder, NOT the Source folders.

**Example (Correct):**
```xml
<DCC_UnitSearchPath>..\..\Output\22.0_Win32_Debug;$(DCC_UnitSearchPath)</DCC_UnitSearchPath>
```

**Example (Incorrect - DO NOT DO THIS):**
```xml
<DCC_UnitSearchPath>..\..\Sources\Core;..\..\Sources\Web;...</DCC_UnitSearchPath>
```
