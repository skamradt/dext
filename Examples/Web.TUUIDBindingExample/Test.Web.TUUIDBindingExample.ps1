$ErrorActionPreference = "Stop"
$baseUrl = "http://localhost:8080"

Write-Host "🚀 Testing Web.TUUIDBindingExample on $baseUrl" -ForegroundColor Cyan

function Invoke-DextRequest {
    param (
        [string]$Uri,
        [string]$Method = "GET",
        [string]$Body = $null
    )
    try {
        $params = @{
            Uri             = $Uri
            Method          = $Method
            UseBasicParsing = $true
            ContentType     = "application/json"
        }
        if ($Body) { $params.Body = $Body }
        
        $resp = Invoke-WebRequest @params
        try { return ($resp.Content | ConvertFrom-Json) } catch { return $resp.Content }
    }
    catch {
        throw "Request to $Uri failed: $($_.Exception.Message)"
    }
}

try {
    # 1. Generate v7
    Write-Host "`n1. Generate v7..." -ForegroundColor Yellow
    $gen = Invoke-DextRequest "$baseUrl/api/products/generate-v7" "POST"
    Write-Host "   Generated: $($gen.id)"
    $uuid = $gen.id

    # 2. Get by ID
    Write-Host "`n2. Get by ID..." -ForegroundColor Yellow
    $get = Invoke-DextRequest "$baseUrl/api/products/$uuid"
    Write-Host "   Name: $($get.name)"
    if ($get.id -ne $uuid) { throw "ID mismatch" }

    # 3. Formats (No Braces)
    Write-Host "`n3. Testing Format (No Braces)..." -ForegroundColor Yellow
    # Remove hyphens to test raw hex parsing
    $cleanUuid = $uuid -replace "-", ""
    $cleanUuid = $cleanUuid -replace "{", ""
    $cleanUuid = $cleanUuid -replace "}", ""
    
    $fmt = Invoke-DextRequest "$baseUrl/api/uuid/formats/$cleanUuid"
    Write-Host "   Input: $($fmt.input)"
    Write-Host "   Canonical: $($fmt.canonical)"
    
    if ($fmt.canonical -ne $uuid) { throw "Canonical mismatch" }

    Write-Host "`nSUCCESS: ALL TUUID BINDING TESTS PASSED!" -ForegroundColor Green

}
catch {
    Write-Error "TEST FAILED: $_"
    exit 1
}
