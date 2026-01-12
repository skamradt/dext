
# Script to update all facade units using Dext Facade Generator
# Usage: .\Scripts\update_facades.ps1

$ErrorActionPreference = "Stop"
$Root = Resolve-Path "$PSScriptRoot\.."
$DextTool = "$Root\Apps\dext.exe" # Assumption: dext tool is compiled and available here or on path
# Fallback to path if not found in bin
if (-not (Test-Path $DextTool)) {
    $DextTool = "dext"
}

Write-Host "Updating Facade Units..." -ForegroundColor Cyan

# 1. Dext.pas (Core)
Write-Host "Updating Dext.pas (Core)..." -ForegroundColor Yellow
& $DextTool facade -p "$Root\Sources\Core" -t "$Root\Sources\Core\Dext.pas" -x "Dext"

# 2. Dext.Entity.pas (EF.Core)
Write-Host "Updating Dext.Entity.pas (Data)..." -ForegroundColor Yellow
& $DextTool facade -p "$Root\Sources\Data" -t "$Root\Sources\Data\Dext.Entity.pas" -x "Dext.Entity"
            
# 3. Dext.Web.pas (Web.Core)
Write-Host "Updating Dext.Web.pas (Web)..." -ForegroundColor Yellow
& $DextTool facade -p "$Root\Sources\Web" -t "$Root\Sources\Web\Dext.Web.pas" -x "Dext.Web"
    
# 4. Dext.Testing.pas (Testing)
Write-Host "Updating Dext.Testing.pas (Testing)..." -ForegroundColor Yellow
& $DextTool facade -p "$Root\Sources\Testing" -t "$Root\Sources\Testing\Dext.Testing.pas" -x "Dext.Testing"

# 5. Dext.Hosting.pas (Hosting)
Write-Host "Updating Dext.Hosting.pas..." -ForegroundColor Yellow
& $DextTool facade -p "$Root\Sources\Hosting" -t "$Root\Sources\Hosting\Dext.Hosting.pas" -x "Dext.Hosting"

# 6. Dext.Hubs.pas (Web.Hubs)
Write-Host "Updating Dext.Hubs.pas..." -ForegroundColor Yellow
& $DextTool facade -p "$Root\Sources\Hubs" -t "$Root\Sources\Hubs\Dext.Hubs.pas" -x "Dext.Hubs"

Write-Host "All facades updated successfully!" -ForegroundColor Green
