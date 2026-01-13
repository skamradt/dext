<#
.SYNOPSIS
    Generates/Updates the Dext Framework API documentation.
.DESCRIPTION
    This script runs the 'dext doc' command to generate API documentation from the source code.
    It expects the 'dext' executable to be available in the repository root, Output folder, or system PATH.
#>
param (
    [string]$Title = "Dext Framework 1.0",
    [string]$RepoRoot = "$PSScriptRoot\.."
)

$ErrorActionPreference = "Stop"

# Resolve absolute path for RepoRoot
$RepoRoot = Resolve-Path $RepoRoot
$SourceDir = Join-Path $RepoRoot "Sources"
$OutputDir = Join-Path $RepoRoot "Docs\API"

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "  Dext API Documentation Generator" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Repository Root: $RepoRoot"
Write-Host "Source Input:    $SourceDir"
Write-Host "Output Dir:      $OutputDir"
Write-Host "------------------------------------------"

# Locate 'dext' executable
$DextExe = "dext" # Default to PATH

# Check common locations
$PossibleLocations = @(
    "$RepoRoot\Apps\dext.exe",
)

foreach ($loc in $PossibleLocations) {
    if (Test-Path $loc) {
        $DextExe = $loc
        Write-Host "Found executable: $DextExe" -ForegroundColor Green
        break
    }
}

if ($DextExe -eq "dext") {
    Write-Host "Executable 'dext.exe' not found in common locations. Assuming it is in system PATH." -ForegroundColor Yellow
}

# Ensure Output directory exists
if (-not (Test-Path $OutputDir)) {
    New-Item -ItemType Directory -Force -Path $OutputDir | Out-Null
}

# Construct arguments
$ArgsList = @(
    "doc",
    "--title", "`"$Title`"",
    "--input", "`"$SourceDir`"",
    "--output", "`"$OutputDir`""
)

# Execute
$CmdLine = "& '$DextExe' $ArgsList"
Write-Host "Executing: $DextExe doc --title `"$Title`" --input `"$SourceDir`" --output `"$OutputDir`"" -ForegroundColor Gray

try {
    & $DextExe doc --title "$Title" --input "$SourceDir" --output "$OutputDir"
    if ($LASTEXITCODE -eq 0) {
        Write-Host "`nDocumentation generated successfully!" -ForegroundColor Green
    }
    else {
        Write-Host "`nFailed to generate documentation. Exit Code: $LASTEXITCODE" -ForegroundColor Red
        exit $LASTEXITCODE
    }
}
catch {
    Write-Error "An unexpected error occurred: $_"
    exit 1
}
