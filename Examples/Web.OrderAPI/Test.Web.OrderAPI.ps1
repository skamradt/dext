# Test.Web.OrderAPI.ps1
# Comprehensive API test script for Order API
# Tests all endpoints for the restaurant ordering system

param(
    [string]$BaseUrl = "http://localhost:5000",
    [switch]$Verbose
)

$ErrorActionPreference = "Stop"
$script:PassCount = 0
$script:FailCount = 0

# Basic Auth Credentials
$creds = "admin:admin"
$encodedCreds = [Convert]::ToBase64String([System.Text.Encoding]::ASCII.GetBytes($creds))
$authHeader = @{ Authorization = "Basic $encodedCreds" }

Write-Host "Authentication Configured: Basic Auth ($creds)" -ForegroundColor Magenta
Write-Host ""

function Write-TestHeader {
    param([string]$Title)
    Write-Host ""
    Write-Host ("=========================================================================================") -ForegroundColor Cyan
    Write-Host "  $Title" -ForegroundColor Cyan
    Write-Host ("=========================================================================================") -ForegroundColor Cyan
}

function Write-TestResult {
    param(
        [string]$TestName,
        [bool]$Passed,
        [string]$Details = ""
    )
    
    if ($Passed) {
        Write-Host "  [PASS] $TestName" -ForegroundColor Green
        $script:PassCount++
    }
    else {
        Write-Host "  [FAIL] $TestName" -ForegroundColor Red
        if ($Details) {
            Write-Host "         $Details" -ForegroundColor Yellow
        }
        $script:FailCount++
    }
}

function Invoke-ApiRequest {
    param(
        [string]$Method,
        [string]$Endpoint,
        [object]$Body = $null,
        [int]$ExpectedStatus = 200
    )
    
    $uri = "$BaseUrl$Endpoint"
    $params = @{
        Method          = $Method
        Uri             = $uri
        ContentType     = "application/json"
        Headers         = $authHeader
        ErrorAction     = "Stop"
        UseBasicParsing = $true
    }
    
    if ($Body) {
        $params.Body = ($Body | ConvertTo-Json -Depth 10)
    }
    
    try {
        $response = Invoke-WebRequest @params
        $statusCode = $response.StatusCode
        $content = $response.Content | ConvertFrom-Json -ErrorAction SilentlyContinue
        
        return @{
            Success    = ($statusCode -eq $ExpectedStatus)
            StatusCode = $statusCode
            Content    = $content
            Raw        = $response.Content
        }
    }
    catch {
        $resp = $_.Exception.Response
        $statusCode = if ($resp) { [int]$resp.StatusCode } else { 0 }
        $errMessage = $_.Exception.Message
        if ($_.Exception.InnerException) {
            $errMessage += " | " + $_.Exception.InnerException.Message
        }

        return @{
            Success    = ($statusCode -eq $ExpectedStatus)
            StatusCode = $statusCode
            Content    = $null
            Error      = $errMessage
        }
    }
}

# ============================================================================
# Test Authentication Enforcement
# ============================================================================

Write-TestHeader "Testing Authentication Enforcement"

# Test 401 Unauthorized (No Headers)
try {
    Invoke-WebRequest "$BaseUrl/api/categories" -Method GET -ErrorAction Stop -UseBasicParsing | Out-Null
    Write-TestResult -TestName "Enforce Auth (No Credentials)" -Passed $false -Details "Expected 401, got 200 OK"
}
catch {
    $resp = $_.Exception.Response
    $status = if ($resp) { [int]$resp.StatusCode } else { 0 }
    Write-TestResult -TestName "Enforce Auth (No Credentials)" -Passed ($status -eq 401) -Details "Status: $status (Expected 401)"
}

# ============================================================================
# Test Categories
# ============================================================================

Write-TestHeader "Testing Order API - Categories"

# GET /api/categories
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/categories"
Write-TestResult -TestName "GET /api/categories" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), Count: $($result.Content.Count)"

# POST /api/categories
$newCategory = @{
    name        = "Drinks Test"
    description = "Test category for drinks"
}
$result = Invoke-ApiRequest -Method POST -Endpoint "/api/categories" -Body $newCategory -ExpectedStatus 201
Write-TestResult -TestName "POST /api/categories" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), ID: $($result.Content.id)"
$testCategoryId = $result.Content.id

# GET /api/categories/{id}
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/categories/$testCategoryId"
Write-TestResult -TestName "GET /api/categories/$testCategoryId" -Passed ($result.Success -and $result.Content.name -eq "Drinks Test") `
    -Details "Status: $($result.StatusCode), Name: $($result.Content.name)"

# GET /api/categories/{id} - Not Found
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/categories/99999" -ExpectedStatus 404
Write-TestResult -TestName "GET /api/categories/99999 (Not Found)" -Passed $result.Success `
    -Details "Status: $($result.StatusCode)"

# ============================================================================
# Test Products
# ============================================================================

Write-TestHeader "Testing Order API - Products"

# GET /api/products
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/products"
Write-TestResult -TestName "GET /api/products" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), Count: $($result.Content.Count)"

# POST /api/products
$newProduct = @{
    name        = "Test Beer"
    description = "A test beer product"
    price       = 15.50
    categoryId  = 1
    imageUrl    = ""
}
$result = Invoke-ApiRequest -Method POST -Endpoint "/api/products" -Body $newProduct -ExpectedStatus 201
Write-TestResult -TestName "POST /api/products" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), ID: $($result.Content.id)"
$testProductId = $result.Content.id

# GET /api/products/{id}
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/products/$testProductId"
Write-TestResult -TestName "GET /api/products/$testProductId" -Passed ($result.Success -and $result.Content.name -eq "Test Beer") `
    -Details "Status: $($result.StatusCode), Name: $($result.Content.name)"

# GET /api/products/category/{categoryId}
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/products/category/1"
Write-TestResult -TestName "GET /api/products/category/1" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), Count: $($result.Content.Count)"

# PATCH /api/products/{id}/availability
$result = Invoke-ApiRequest -Method PATCH -Endpoint "/api/products/$testProductId/availability?available=false" -ExpectedStatus 204
Write-TestResult -TestName "PATCH /api/products/$testProductId/availability" -Passed $result.Success `
    -Details "Status: $($result.StatusCode)"

# ============================================================================
# Test Tables
# ============================================================================

Write-TestHeader "Testing Order API - Tables"

# GET /api/tables
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/tables"
Write-TestResult -TestName "GET /api/tables" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), Count: $($result.Content.Count)"

# GET /api/tables/available
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/tables/available"
Write-TestResult -TestName "GET /api/tables/available" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), Available: $($result.Content.Count)"
$availableTable = $result.Content[0]

# GET /api/tables/{id}
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/tables/1"
Write-TestResult -TestName "GET /api/tables/1" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), Number: $($result.Content.number)"

# ============================================================================
# Test Orders (Full Workflow)
# ============================================================================

Write-TestHeader "Testing Order API - Orders (Full Workflow)"

# POST /api/orders - Create new order
$newOrder = @{
    tableId      = $availableTable.id
    customerName = "Test Customer"
    notes        = "Automated test order"
}
$result = Invoke-ApiRequest -Method POST -Endpoint "/api/orders" -Body $newOrder -ExpectedStatus 201
Write-TestResult -TestName "POST /api/orders" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), OrderID: $($result.Content.id)"
$testOrderId = $result.Content.id

# GET /api/orders
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/orders"
Write-TestResult -TestName "GET /api/orders" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), Count: $($result.Content.Count)"

# GET /api/orders/open
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/orders/open"
Write-TestResult -TestName "GET /api/orders/open" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), Open: $($result.Content.Count)"

# GET /api/orders/{id}
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/orders/$testOrderId"
Write-TestResult -TestName "GET /api/orders/$testOrderId" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), Status: $($result.Content.status)"

# GET /api/orders/table/{tableId}
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/orders/table/$($availableTable.id)"
Write-TestResult -TestName "GET /api/orders/table/$($availableTable.id)" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), OrderID: $($result.Content.id)"

# POST /api/orders/{id}/items - Add item to order
$newItem = @{
    productId = 1
    quantity  = 2
    notes     = "No ice"
}
$result = Invoke-ApiRequest -Method POST -Endpoint "/api/orders/$testOrderId/items" -Body $newItem -ExpectedStatus 201
Write-TestResult -TestName "POST /api/orders/$testOrderId/items" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), ItemID: $($result.Content.id)"

# Add another item
$newItem2 = @{
    productId = 3
    quantity  = 1
    notes     = ""
}
$result = Invoke-ApiRequest -Method POST -Endpoint "/api/orders/$testOrderId/items" -Body $newItem2 -ExpectedStatus 201
Write-TestResult -TestName "POST /api/orders/$testOrderId/items (2nd)" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), ItemID: $($result.Content.id)"

# GET /api/orders/{id}/items
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/orders/$testOrderId/items"
Write-TestResult -TestName "GET /api/orders/$testOrderId/items" -Passed ($result.Success -and $result.Content.Count -ge 2) `
    -Details "Status: $($result.StatusCode), Items: $($result.Content.Count)"

# POST /api/orders/{id}/close
$result = Invoke-ApiRequest -Method POST -Endpoint "/api/orders/$testOrderId/close"
Write-TestResult -TestName "POST /api/orders/$testOrderId/close" -Passed $result.Success `
    -Details "Status: $($result.StatusCode)"

# Verify order is closed
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/orders/$testOrderId"
Write-TestResult -TestName "Verify order closed (status=osClosed)" -Passed ($result.Success -and $result.Content.status -eq "osClosed") `
    -Details "Status: $($result.StatusCode), Order Status: $($result.Content.status)"

# ============================================================================
# Test Reports (Transient Class-Only Service)
# ============================================================================

Write-TestHeader "Testing Order API - Reports (Class-Only Transient)"

# GET /api/reports/stats
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/reports/stats"
Write-TestResult -TestName "GET /api/reports/stats" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), Tables: $($result.Content.totalTables), InstanceId: $($result.Content.instanceId)"

# GET /api/reports/daily-summary
$result = Invoke-ApiRequest -Method GET -Endpoint "/api/reports/daily-summary"
Write-TestResult -TestName "GET /api/reports/daily-summary" -Passed $result.Success `
    -Details "Status: $($result.StatusCode), Revenue: $($result.Content.totalRevenue), InstanceId: $($result.Content.instanceId)"

# Verify each request creates a new instance (instanceId should differ)
$result1 = Invoke-ApiRequest -Method GET -Endpoint "/api/reports/stats"
$result2 = Invoke-ApiRequest -Method GET -Endpoint "/api/reports/stats"
$instancesDiffer = $result1.Content.instanceId -ne $result2.Content.instanceId
Write-TestResult -TestName "Verify Transient creates new instances" -Passed $instancesDiffer `
    -Details "Instance1: $($result1.Content.instanceId), Instance2: $($result2.Content.instanceId)"

# ============================================================================
# Test Swagger Documentation
# ============================================================================

Write-TestHeader "Testing Order API - Swagger Documentation"

# GET /api/swagger (Swagger UI)
try {
    $response = Invoke-WebRequest -Uri "$BaseUrl/api/swagger" -Method GET -ErrorAction Stop
    $hasSwaggerUI = $response.Content -like "*swagger*"
    Write-TestResult -TestName "GET /api/swagger (Swagger UI)" -Passed ($response.StatusCode -eq 200 -and $hasSwaggerUI) `
        -Details "Status: $($response.StatusCode), Length: $($response.Content.Length)"
}
catch {
    Write-TestResult -TestName "GET /api/swagger (Swagger UI)" -Passed $false `
        -Details $_.Exception.Message
}

# GET /api/swagger.json
try {
    $response = Invoke-WebRequest -Uri "$BaseUrl/api/swagger.json" -Method GET -ErrorAction Stop -UseBasicParsing
    Write-TestResult -TestName "GET /api/swagger.json" -Passed ($response.StatusCode -eq 200) `
        -Details "Status: $($response.StatusCode), Length: $($response.Content.Length)"
}
catch {
    Write-TestResult -TestName "GET /api/swagger.json" -Passed $false `
        -Details "Error: $($_.Exception.Message)"
}

# ============================================================================
# Summary
# ============================================================================

Write-Host ""
Write-Host ("=" * 60) -ForegroundColor Cyan
Write-Host "  TEST SUMMARY" -ForegroundColor Cyan
Write-Host ("=" * 60) -ForegroundColor Cyan
Write-Host ""
Write-Host "  Passed: $script:PassCount" -ForegroundColor Green
Write-Host "  Failed: $script:FailCount" -ForegroundColor $(if ($script:FailCount -gt 0) { "Red" } else { "Green" })
Write-Host ""

if ($script:FailCount -gt 0) {
    Write-Host "  Some tests FAILED!" -ForegroundColor Red
    exit 1
}
else {
    Write-Host "  All tests PASSED!" -ForegroundColor Green
    exit 0
}
