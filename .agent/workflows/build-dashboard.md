---
description: Build and Deploy Dext Dashboard V2 (Vue.js)
---

# Dext Dashboard V2 - Build & Deploy Workflow

This workflow describes how to compile the Vue.js frontend and deploy it to the Dext CLI's reliable path.

## 1. Build Vue Application
Navigate to the Vue App source directory and build the project using Vite.

```powershell
# Navigate to source
cd c:\dev\Dext\DextRepository\Sources\Hosting\Dashboard\vue-app

# Install dependencies (if needed)
npm install

# Build for production (output to ./dist)
npm run build
```

## 2. Deploy to Dext Runtime
The Dext CLI looks for UI assets in the current user's `.dext\ui\wwwroot` directory.
We must copy the build artifacts from the `dist` folder to this location.

```powershell
# Define paths
$Source = "c:\dev\Dext\DextRepository\Sources\Hosting\Dashboard\vue-app\dist"
$Dest = "$env:USERPROFILE\.dext\ui\wwwroot"

# Ensure destination exists and clean it
if (!(Test-Path $Dest)) { New-Item -ItemType Directory -Path $Dest -Force }
Remove-Item -Path "$Dest\*" -Recurse -Force -ErrorAction SilentlyContinue

# Copy assets
Copy-Item -Path "$Source\*" -Destination $Dest -Recurse -Force

Write-Host "Dashboard Deployed Successfully to $Dest"
```

## 3. Run Dext CLI
Ensure you are running the latest build of Dext CLI.

```powershell
# Run the UI command
dext ui
```

**Verify Version:**
The console output should read:
`Starting Dext Dashboard V2 (Vue) on port 3000...`

**Troubleshooting:**
- If you see the "Old Dashboard", ensure you don't have zombie `dext.exe` processes running (`taskkill /F /IM dext.exe`).
- If you see "404 Not Found", ensure the step #2 (Copy) completed successfully and `index.html` exists in `C:\Users\<User>\.dext\ui\wwwroot`.
