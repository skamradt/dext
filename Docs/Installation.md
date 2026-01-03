# Dext Framework Installation Guide

This guide covers the installation of the Dext Framework. You can choose between the **Automated Setup** (recommended) or the **Manual Setup**.

## Prerequisites
- Delphi 11 Alexandria or newer.
- Git (to clone the repository).

---

## Option 1: Automated Setup (Recommended)

We use **TMS Smart Setup** to automate the build process and manage dependencies. A PowerShell script is provided to handle this for you.

1.  Open PowerShell in the `DextRepository` folder.
2.  Run the setup script:
    ```powershell
    ./setup.ps1
    ```
    *This script will download the necessary build tool locally and compile all Dext packages (Core, EF, Web) in the correct order for Windows and Linux.*

    > **Note:** If this is your first time using TMS Smart Setup, the script might fail asking for configuration. If so, run `.\.tms_tool\tms.exe config` manually to initialize the environment.
    > The config can be empty, just running the command is enough to create the default config file in your user folder.

3.  Once completed, the compiled files will be in the `.tmssetup` hidden folder.

We recommend adding the `.tmssetup` folder to your Delphi Library Path, or let Smart Setup manage it via `tms install` if you have it installed globally.

---

## Option 2: Manual Setup

If you prefer to compile manually using the IDE, follow these steps.

### 1. Environment Variable Configuration (Best Practice)

Using an environment variable simplifies your Library Paths and allows you to switch between different versions/forks of Dext easily.

1.  In Delphi, go to **Tools** > **Options** > **IDE** > **Environment Variables**.
2.  Click **New...**
3.  **Variable Name**: `DEXT`
4.  **Value**: The full path to the `Sources` directory inside your cloned repository.
    *   *Example*: `C:\dev\Dext\DextRepository\Sources`
    *   *Note*: Ensure it points to the `Sources` folder, not the root, to match the paths below.
    
    ![DEXT Environment Variable](Images/ide-env-var.png)

### 2. Configure Library Paths

Add the following paths to your **Library Path** (Tools > Options > Language > Delphi > Library) for your target platforms (Win32, Win64, Linux64). 

If you set up the `$(DEXT)` variable as described above:

```text
$(DEXT)
$(DEXT)\Core
$(DEXT)\Core\Base
$(DEXT)\Core\Json
$(DEXT)\Data
$(DEXT)\Hosting
$(DEXT)\Hosting\Cli
$(DEXT)\Hosting\Cli\Commands
$(DEXT)\Web
$(DEXT)\Web\Caching
$(DEXT)\Web\Hosting
$(DEXT)\Web\Indy
$(DEXT)\Web\Middleware
$(DEXT)\Web\MVC
```

### 3. Build

1.  Open `Sources\DextFramework.groupproj`.
2.  Right-click **ProjectGroup** > **Build All**.

---

## Troubleshooting

- **"File not found" during Manual Build**: Ensure all subdirectories in `Sources` are covered by your Library Path or the `$(DEXT)` expansion.
- **TMS Smart Setup download fails**: Check your internet connection or manually download `tmssmartsetup.zip` from GitHub and extract it to `.tms_tool`.
