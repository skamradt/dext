# Test.Web.UUIDExample.ps1
$ErrorActionPreference = "Stop"
$baseUrl = "http://localhost:8080"

Write-Host "Testing Web.UUIDExample" -ForegroundColor Cyan
Write-Host "========================" -ForegroundColor Cyan
Write-Host

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
    # 1. Generate UUID v7 (POST /api/products/generate)
    Write-Host "1. POST /api/products/generate" -ForegroundColor Yellow
    Write-Host "   Testing UUID v7 generation..."
    $gen = Invoke-DextRequest "$baseUrl/api/products/generate" "POST"
    Write-Host "   [OK] Generated ID: $($gen.id)" -ForegroundColor Green
    Write-Host "   Name: $($gen.name), Price: $($gen.price)"
    $uuid = $gen.id
    Write-Host

    # 2. Get by UUID - Manual Binding (GET /api/products/{id})
    Write-Host "2. GET /api/products/{id} [MANUAL]" -ForegroundColor Yellow
    Write-Host "   Fetching product by UUID: $uuid"
    $get = Invoke-DextRequest "$baseUrl/api/products/$uuid"
    Write-Host "   [OK] Found: $($get.name)" -ForegroundColor Green
    
    if ($get.id -ne $uuid) { throw "ID mismatch in GET response" }
    Write-Host

    # 3. Get by UUID - Auto Binding (GET /api/uuid/lookup/{id})
    Write-Host "3. GET /api/uuid/lookup/{id} [AUTO-BOUND]" -ForegroundColor Yellow
    Write-Host "   Testing automatic TUUID model binding..."
    $lookup = Invoke-DextRequest "$baseUrl/api/uuid/lookup/$uuid"
    Write-Host "   [OK] Found: $($lookup.name)" -ForegroundColor Green
    Write-Host "   Price: $($lookup.price)"
    Write-Host

    # 4. Create with Manual UUID (POST /api/products)
    Write-Host "4. POST /api/products" -ForegroundColor Yellow
    $newId = [Guid]::NewGuid().ToString()
    Write-Host "   Creating product with UUID: $newId"
    $body = "{`"id`":`"$newId`", `"name`":`"Manual Product`", `"price`":29.99}"
    $create = Invoke-DextRequest "$baseUrl/api/products" "POST" $body
    Write-Host "   [OK] Created: $($create.id)" -ForegroundColor Green
    Write-Host "   Name: $($create.name), Price: $($create.price)"
    Write-Host

    # 5. Update Product (PUT /api/products/{id})
    Write-Host "5. PUT /api/products/{id}" -ForegroundColor Yellow
    Write-Host "   Updating product: $newId"
    $updateBody = "{`"id`":`"$newId`", `"name`":`"Updated Product`", `"price`":49.99}"
    $update = Invoke-DextRequest "$baseUrl/api/products/$newId" "PUT" $updateBody
    Write-Host "   [OK] Updated: $($update.name)" -ForegroundColor Green
    Write-Host "   New Price: $($update.price)"
    Write-Host

    # 6. Test UUID Formats (GET /api/uuid/test)
    Write-Host "6. GET /api/uuid/test" -ForegroundColor Yellow
    Write-Host "   Testing UUID format conversions..."
    $fmt = Invoke-DextRequest "$baseUrl/api/uuid/test"
    Write-Host "   [OK] UUID v7:     $($fmt.uuid_v7)" -ForegroundColor Green
    Write-Host "   With braces: $($fmt.with_braces)"
    Write-Host "   As TGUID:    $($fmt.tguid)"
    Write-Host "   Timestamp:   $($fmt.timestamp_ms) ms"
    Write-Host

    Write-Host "==========================================" -ForegroundColor Cyan
    Write-Host "SUCCESS: ALL UUID TESTS PASSED!" -ForegroundColor Green
    Write-Host "==========================================" -ForegroundColor Cyan
}
catch {
    Write-Host
    Write-Host "==========================================" -ForegroundColor Red
    Write-Host "TEST FAILED: $_" -ForegroundColor Red
    Write-Host "==========================================" -ForegroundColor Red
    exit 1
}
