$ErrorActionPreference = "Stop"
$baseUrl = "http://localhost:8080"

Write-Host "🚀 Testing Web.UUIDExample on $baseUrl" -ForegroundColor Cyan

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
        Write-Error "Request to $Uri failed: $($_.Exception.Message)"
        throw
    }
}

try {
    # 1. Generate (POST /api/products/generate)
    Write-Host "`n1. Generating via API..." -ForegroundColor Yellow
    $gen = Invoke-DextRequest "$baseUrl/api/products/generate" "POST"
    Write-Host "   Generated: $($gen.id) (Name: $($gen.name))"
    $uuid = $gen.id

    # 2. Get by UUID (GET /api/products/{id})
    Write-Host "`n2. Getting Product by UUID ($uuid)..." -ForegroundColor Yellow
    $get = Invoke-DextRequest "$baseUrl/api/products/$uuid"
    Write-Host "   Found: $($get.name)"
    
    # Simple string comparison
    if ($get.id -ne $uuid) { throw "ID mismatch" }

    # 3. Create Manual (POST /api/products)
    Write-Host "`n3. Creating with Manual UUID..." -ForegroundColor Yellow
    $newId = [Guid]::NewGuid().ToString()
    $body = "{""id"":""$newId"", ""name"":""Manual Product"", ""price"":10.5}"
    $create = Invoke-DextRequest "$baseUrl/api/products" "POST" $body
    Write-Host "   Created: $($create.id)"

    # 4. Formats Test
    Write-Host "`n4. Testing UUID Formats..." -ForegroundColor Yellow
    $fmt = Invoke-DextRequest "$baseUrl/api/uuid/test"
    Write-Host "   v7: $($fmt.uuid_v7)"
    Write-Host "   braces: $($fmt.with_braces)"

    Write-Host "`nSUCCESS: ALL UUID TESTS PASSED!" -ForegroundColor Green

}
catch {
    Write-Error "TEST FAILED: $_"
    exit 1
}
