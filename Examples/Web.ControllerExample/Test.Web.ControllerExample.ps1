# ============================================================================
# Web.ControllerExample - Complete API Test Suite
# ============================================================================
# This script tests all endpoints in the Web.ControllerExample project.
# Run with: .\Test.Web.ControllerExample.ps1
# Requirements: Server must be running on port 8080
# ============================================================================

$baseUrl = "http://localhost:8080"
$token = ""
$passed = 0
$failed = 0

Write-Host ""
Write-Host "============================================================" -ForegroundColor Cyan
Write-Host "     Web.ControllerExample - Complete API Test Suite        " -ForegroundColor Cyan
Write-Host "============================================================" -ForegroundColor Cyan
Write-Host ""

# Helper function
function Test-Endpoint {
    param(
        [string]$Method = "GET", 
        [string]$Url, 
        $Body = $null, 
        $Headers = @{}, 
        [int]$ExpectedStatus = 200, 
        [string]$Description = ""
    )
    
    $displayUrl = if ($Description) { $Description } else { "$Method $Url" }
    Write-Host "  Testing $displayUrl... " -NoNewline -ForegroundColor Yellow
    
    try {
        $fullUrl = "$baseUrl$Url"
        if ($Body) {
            $jsonBody = if ($Body -is [string]) { $Body } else { $Body | ConvertTo-Json }
            $response = Invoke-RestMethod -Method $Method -Uri $fullUrl -Body $jsonBody -ContentType "application/json" -Headers $Headers -ErrorAction Stop
        }
        else {
            $response = Invoke-RestMethod -Method $Method -Uri $fullUrl -Headers $Headers -ErrorAction Stop
        }
        Write-Host "[OK]" -ForegroundColor Green
        $script:passed++
        return $response
    }
    catch {
        $statusCode = 0
        if ($_.Exception.Response) {
            $statusCode = [int]$_.Exception.Response.StatusCode
        }
        if ($statusCode -eq $ExpectedStatus) {
            Write-Host "[OK] (Expected $ExpectedStatus)" -ForegroundColor Green
            $script:passed++
        }
        else {
            Write-Host "[FAILED] Status: $statusCode" -ForegroundColor Red
            $script:failed++
            
            # Print Body if available
            if ($_.Exception.Response) {
                $stream = $_.Exception.Response.GetResponseStream()
                if ($stream) {
                    $reader = New-Object System.IO.StreamReader($stream)
                    $body = $reader.ReadToEnd()
                    if ($body) { Write-Host "    Response: $body" -ForegroundColor DarkRed }
                }
            }
        }
        return $null
    }
}

# ============================================================================
# 1. HEALTH CHECK
# ============================================================================
Write-Host "[1] Health Check" -ForegroundColor Magenta
$health = Test-Endpoint "GET" "/health" -Description "Health Check Endpoint"
if ($health) {
    Write-Host "    Status: $($health.status)" -ForegroundColor Gray
}
Write-Host ""

# ============================================================================
# 2. AUTHENTICATION
# ============================================================================
Write-Host "[2] Authentication" -ForegroundColor Magenta
$loginResponse = Test-Endpoint "POST" "/api/auth/login" -Body @{ username = "admin"; password = "admin" } -Description "Login (valid credentials)"
if ($loginResponse) {
    $token = $loginResponse.token
    Write-Host "    Token: $($token.Substring(0, 20))..." -ForegroundColor Gray
}

Test-Endpoint "POST" "/api/auth/login" -Body @{ username = "wrong"; password = "user" } -ExpectedStatus 401 -Description "Login (invalid credentials)"
Write-Host ""

$AuthHeaders = @{ "Authorization" = "Bearer $token" }

# ============================================================================
# 3. GREETING CONTROLLER
# ============================================================================
Write-Host "[3] Greeting Controller" -ForegroundColor Magenta
Test-Endpoint "GET" "/api/greet/World" -Headers $AuthHeaders -Description "GET /api/greet/{name}"
Test-Endpoint "GET" "/api/greet/negotiated" -Description "GET /api/greet/negotiated (AllowAnonymous)"
Test-Endpoint "POST" "/api/greet" -Body @{ Name = "Dext"; Title = "Framework" } -Headers $AuthHeaders -Description "POST /api/greet (Body Binding)"
Test-Endpoint "GET" "/api/greet/search?q=test&limit=10" -Headers $AuthHeaders -Description "GET /api/greet/search (Query Binding)"
Test-Endpoint "GET" "/api/greet/config" -Headers $AuthHeaders -Description "GET /api/greet/config (IOptions)"
Write-Host ""

# ============================================================================
# 4. FILTERS CONTROLLER
# ============================================================================
Write-Host "[4] Filters Controller (Action Filters)" -ForegroundColor Magenta
Test-Endpoint "GET" "/api/filters/simple" -Description "Simple Endpoint ([LogAction])"
Test-Endpoint "GET" "/api/filters/cached" -Description "Cached Endpoint ([ResponseCache])"
Test-Endpoint "POST" "/api/filters/secure" -Headers @{ "X-API-Key" = "secret" } -Description "Secure Endpoint (with X-API-Key)"
Test-Endpoint "POST" "/api/filters/secure" -ExpectedStatus 400 -Description "Secure Endpoint (missing header)"
Test-Endpoint "GET" "/api/filters/admin" -ExpectedStatus 401 -Description "Admin Endpoint (no auth)"
Test-Endpoint "GET" "/api/filters/admin" -Headers $AuthHeaders -Description "Admin Endpoint (with auth)"
Test-Endpoint "GET" "/api/filters/protected" -ExpectedStatus 400 -Description "Protected Endpoint (no auth header)"
Test-Endpoint "GET" "/api/filters/protected" -Headers $AuthHeaders -Description "Protected Endpoint (with auth)"
Write-Host ""

# ============================================================================
# 5. LIST CONTROLLER
# ============================================================================
Write-Host "[5] List Controller (IList Serialization)" -ForegroundColor Magenta
Test-Endpoint "GET" "/api/list" -Description "GET /api/list (IList<TPerson>)"
Write-Host ""

# ============================================================================
# 6. OBJECT CONTROLLER
# ============================================================================
Write-Host "[6] Object Controller (Object Serialization)" -ForegroundColor Magenta
Test-Endpoint "GET" "/api/object" -Description "GET /api/object (TPersonWithAddress)"
Test-Endpoint "GET" "/api/object/nested" -Description "GET /api/object/nested (Nested Objects)"
Test-Endpoint "GET" "/api/object/list" -Description "GET /api/object/list (List of Objects)"
Write-Host ""

# ============================================================================
# 7. API VERSIONING
# ============================================================================
Write-Host "[7] API Versioning" -ForegroundColor Magenta
Test-Endpoint "GET" "/api/versioned?api-version=1.0" -Description "Version 1.0 (Query String)"
Test-Endpoint "GET" "/api/versioned?api-version=2.0" -Description "Version 2.0 (Query String)"
Test-Endpoint "GET" "/api/versioned" -Headers @{ "X-Version" = "1.0" } -Description "Version 1.0 (X-Version Header)"
Test-Endpoint "GET" "/api/versioned" -Headers @{ "X-Version" = "2.0" } -Description "Version 2.0 (X-Version Header)"
Write-Host ""

# ============================================================================
# 8. CONTENT NEGOTIATION
# ============================================================================
Write-Host "[8] Content Negotiation" -ForegroundColor Magenta
Test-Endpoint "GET" "/api/greet/negotiated" -Headers @{ "Accept" = "application/json" } -Description "Accept: application/json"
Write-Host ""

# ============================================================================
# SUMMARY
# ============================================================================
Write-Host "============================================================" -ForegroundColor Cyan
Write-Host "                       TEST SUMMARY                         " -ForegroundColor Cyan
Write-Host "============================================================" -ForegroundColor Cyan
Write-Host "  Passed: $passed" -ForegroundColor Green
Write-Host "  Failed: $failed" -ForegroundColor $(if ($failed -gt 0) { "Red" } else { "Green" })
Write-Host "============================================================" -ForegroundColor Cyan

if ($failed -gt 0) {
    exit 1
}
