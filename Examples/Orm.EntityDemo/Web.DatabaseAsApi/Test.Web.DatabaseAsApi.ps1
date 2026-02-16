# Database as API Example Test Script
# Tests all auto-generated CRUD endpoints

$ErrorActionPreference = "Stop"
$baseUrl = "http://localhost:5000"

Write-Host "Testing Web.DatabaseAsApi" -ForegroundColor Cyan
Write-Host "=========================" -ForegroundColor Cyan
Write-Host ""

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
        if ($_.Exception.Response.StatusCode -eq 404) {
            return @{ error = "NotFound"; statusCode = 404 }
        }
        throw "Request to $Uri failed: $($_.Exception.Message)"
    }
}

try {
    # ═══════════════════════════════════════════════════════════════════════════
    # TEST 1: List all customers (seeded data)
    # ═══════════════════════════════════════════════════════════════════════════
    Write-Host "1. GET /api/customers" -ForegroundColor Yellow
    Write-Host "   Listing seeded customers..."
    $customers = Invoke-DextRequest "$baseUrl/api/customers"
    if ($customers.Count -lt 2) { throw "Expected at least 2 seeded customers" }
    Write-Host "   [OK] Found $($customers.Count) customers" -ForegroundColor Green
    Write-Host "   First: $($customers[0].name)"
    Write-Host ""

    # ═══════════════════════════════════════════════════════════════════════════
    # TEST 2: Get customer by ID
    # ═══════════════════════════════════════════════════════════════════════════
    Write-Host "2. GET /api/customers/1" -ForegroundColor Yellow
    $customer = Invoke-DextRequest "$baseUrl/api/customers/1"
    if (-not $customer.name) { throw "Customer not found" }
    Write-Host "   [OK] Found: $($customer.name)" -ForegroundColor Green
    Write-Host "   Email: $($customer.email)"
    Write-Host ""

    # ═══════════════════════════════════════════════════════════════════════════
    # TEST 3: Verify snake_case JSON output
    # ═══════════════════════════════════════════════════════════════════════════
    Write-Host "3. Verifying snake_case JSON" -ForegroundColor Yellow
    if ($null -eq $customer.created_at) { throw "Expected snake_case 'created_at' field" }
    Write-Host "   [OK] Field 'created_at' present: $($customer.created_at)" -ForegroundColor Green
    Write-Host ""

    # ═══════════════════════════════════════════════════════════════════════════
    # TEST 4: Verify [NotMapped] exclusion
    # ═══════════════════════════════════════════════════════════════════════════
    Write-Host "4. Verifying [NotMapped] exclusion" -ForegroundColor Yellow
    if ($null -ne $customer.internal_code -and $customer.internal_code -ne "") { 
        throw "Expected 'internal_code' to be excluded from response" 
    }
    Write-Host "   [OK] Field 'internal_code' correctly excluded" -ForegroundColor Green
    Write-Host ""

    # ═══════════════════════════════════════════════════════════════════════════
    # TEST 5: Create new customer
    # ═══════════════════════════════════════════════════════════════════════════
    Write-Host "5. POST /api/customers" -ForegroundColor Yellow
    Write-Host "   Creating new customer..."
    $newCustomer = @{
        name   = "Test Customer"
        email  = "test@example.com"
        active = $true
    } | ConvertTo-Json
    $created = Invoke-DextRequest "$baseUrl/api/customers" "POST" $newCustomer
    if (-not $created.id) { throw "Expected 'id' in created customer" }
    $newId = $created.id
    Write-Host "   [OK] Created with ID: $newId" -ForegroundColor Green
    Write-Host ""

    # ═══════════════════════════════════════════════════════════════════════════
    # TEST 6: Update customer
    # ═══════════════════════════════════════════════════════════════════════════
    Write-Host "6. PUT /api/customers/$newId" -ForegroundColor Yellow
    Write-Host "   Updating customer..."
    $updateData = @{
        id     = $newId
        name   = "Updated Customer"
        email  = "updated@example.com"
        active = $false
    } | ConvertTo-Json
    $updated = Invoke-DextRequest "$baseUrl/api/customers/$newId" "PUT" $updateData
    if ($updated.name -ne "Updated Customer") { throw "Update failed" }
    Write-Host "   [OK] Updated: $($updated.name)" -ForegroundColor Green
    Write-Host ""

    # ═══════════════════════════════════════════════════════════════════════════
    # TEST 7: Delete customer
    # ═══════════════════════════════════════════════════════════════════════════
    Write-Host "7. DELETE /api/customers/$newId" -ForegroundColor Yellow
    Write-Host "   Deleting customer..."
    Invoke-DextRequest "$baseUrl/api/customers/$newId" "DELETE" | Out-Null
    Write-Host "   [OK] Deleted" -ForegroundColor Green
    Write-Host ""

    # ═══════════════════════════════════════════════════════════════════════════
    # TEST 8: Verify deletion
    # ═══════════════════════════════════════════════════════════════════════════
    Write-Host "8. GET /api/customers/$newId (expect 404)" -ForegroundColor Yellow
    $deleted = Invoke-DextRequest "$baseUrl/api/customers/$newId"
    if ($deleted.statusCode -ne 404) { throw "Expected 404 after deletion" }
    Write-Host "   [OK] Correctly returns 404" -ForegroundColor Green
    Write-Host ""

    # ═══════════════════════════════════════════════════════════════════════════
    # TEST 9: Swagger endpoint
    # ═══════════════════════════════════════════════════════════════════════════
    Write-Host "9. GET /swagger.json" -ForegroundColor Yellow
    Write-Host "   Checking OpenAPI spec..."
    $swagger = Invoke-DextRequest "$baseUrl/swagger.json"
    if (-not $swagger.openapi) { throw "Invalid OpenAPI spec" }
    Write-Host "   [OK] OpenAPI version: $($swagger.openapi)" -ForegroundColor Green
    Write-Host "   Title: $($swagger.info.title)"
    Write-Host ""

    # ═══════════════════════════════════════════════════════════════════════════
    Write-Host "==========================================" -ForegroundColor Green
    Write-Host "SUCCESS: ALL DATABASE AS API TESTS PASSED!" -ForegroundColor Green
    Write-Host "==========================================" -ForegroundColor Green
}
catch {
    Write-Host ""
    Write-Host "==========================================" -ForegroundColor Red
    Write-Host "TEST FAILED: $_" -ForegroundColor Red
    Write-Host "==========================================" -ForegroundColor Red
    exit 1
}
