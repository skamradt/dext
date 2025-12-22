# Dext Framework Installation and Setup

This guide outlines the steps required to compile the framework and configure Delphi to use Dext.

## 1. Source Compilation

The Dext Framework is designed so that its compiled binaries (`.dcu`, `.bpl`, `.dcp`) are generated in a centralized output folder, simplifying configuration.

1.  Open the main project group:
    *   `Sources\DextFramework.groupproj`
2.  In the Project Manager, right-click on the root node (**ProjectGroup**) and select **Build All**.
3.  Wait for all packages to compile.

The compiled files will be automatically generated in the folder:
*   `Output\$(Platform)\$(Config)`
*   *Example:* `Output\Win32\Debug`

## 2. Library Path Configuration (DCUs)

For the IDE to locate the framework's compiled files:

1.  In Delphi, go to **Tools** > **Options** > **Language** > **Delphi** > **Library**.
2.  Select the desired **Platform** (e.g., Windows 32-bit).
3.  In the **Library Path** field, add the absolute path to the output folder generated in the previous step.
    *   Example: `C:\dev\Dext\DextRepository\Output\Win32\Debug`

> **Note:** If you switch between Debug and Release configurations or Platforms (Win32/Win64), remember to adjust this path or add both.

## 3. Source Configuration (Browsing/Library Path)

To allow source code navigation (Ctrl+Click) and detailed debugging, add the following directories to your IDE's **Library Path** (or Browsing Path).

Replace `[Root]` with the path where you cloned the repository (e.g., `C:\dev\Dext\DextRepository\`).

```text
[Root]\Sources\Core
[Root]\Sources\Core\Base
[Root]\Sources\Core\Json
[Root]\Sources\Data
[Root]\Sources\Hosting\CLI
[Root]\Sources\Hosting\CLI\Commands
[Root]\Sources\Web
[Root]\Sources\Web\Caching
[Root]\Sources\Web\Hosting
[Root]\Sources\Web\Indy
[Root]\Sources\Web\Middleware
[Root]\Sources\Web\Mvc
```

### Ready-to-Copy List (Example Based on `C:\dev\Dext\DextRepository`)

```text
C:\dev\Dext\DextRepository\Sources\Core
C:\dev\Dext\DextRepository\Sources\Core\Base
C:\dev\Dext\DextRepository\Sources\Core\Json
C:\dev\Dext\DextRepository\Sources\Data
C:\dev\Dext\DextRepository\Sources\Hosting\CLI
C:\dev\Dext\DextRepository\Sources\Hosting\CLI\Commands
C:\dev\Dext\DextRepository\Sources\Web
C:\dev\Dext\DextRepository\Sources\Web\Caching
C:\dev\Dext\DextRepository\Sources\Web\Hosting
C:\dev\Dext\DextRepository\Sources\Web\Indy
C:\dev\Dext\DextRepository\Sources\Web\Middleware
C:\dev\Dext\DextRepository\Sources\Web\Mvc
```

*Note: Folders like `Http` and `Expressions` mentioned in previous versions have been renamed or reorganized into `Web` and other modules.*

## 4. Verification

To confirm the installation is correct:

1.  Close the framework project group.
2.  Open the examples group:
    *   `Examples\DextExamples.groupproj`
3.  Execute **Build All**.
4.  If all projects compile successfully, the environment is correctly configured.
