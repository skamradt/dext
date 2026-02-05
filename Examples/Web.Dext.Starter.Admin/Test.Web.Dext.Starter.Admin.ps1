# Test.Web.Dext.Starter.Admin.ps1

$baseUrl = "http://localhost:8080"

# 2. Test Compression Middleware
Write-Host "`n[TEST] Compression Middleware..."
try {
    # Force Accept-Encoding to ensure server tries to compress
    $resp = Invoke-WebRequest -Uri "$baseUrl/auth/login" -Headers @{ "Accept-Encoding" = "gzip, deflate" } -Method Get
    
    $encoding = $resp.Headers["Content-Encoding"]
    if ($encoding -match "gzip" -or $encoding -match "deflate") {
        Write-Host "PASS: Response is compressed ($encoding)." -ForegroundC
        
        olor Green
    }
    else {
        Write-Host "FAIL: Response is NOT compressed. Headers:" -ForegroundColor Red
        $resp.Headers | Out-String | Write-Host
    }
}
catch {
    Write-Host "FAIL: Could not request /auth/login. Error: $_" -ForegroundColor Red
}

# 3. Test JWT Configuration
Write-Host "`n[TEST] JWT Configuration..."

$token = $null

# 3a. Login (Get Token)
Write-Host " - Attempting Login (admin/admin)..."
try {
    $loginBody = @{
        username = "admin"
        password = "admin"
    } | ConvertTo-Json

    $loginResp = Invoke-RestMethod -Uri "$baseUrl/auth/login" -Method Post -Body $loginBody -ContentType "application/json"
    
    if ($loginResp.token) {
        $token = $loginResp.token
        Write-Host "PASS: Login successful. Token received." -ForegroundColor Green
    }
    else {
        Write-Host "FAIL: Login failed. No token in response." -ForegroundColor Red
    }
}
catch {
    Write-Host "FAIL: Login request failed. Error: $_" -ForegroundColor Red
}

# 3b. Access Protected Resource
if ($token) {
    Write-Host " - Accessing Protected Resource (WITH Token)..."
    try {
        $statsResp = Invoke-RestMethod -Uri "$baseUrl/dashboard/stats" -Headers @{ "Authorization" = "Bearer $token" } -Method Get
        Write-Host "PASS: Access granted. Data received." -ForegroundColor Green
    }
    catch {
        Write-Host "FAIL: Access denied with valid token. Error: $_" -ForegroundColor Red
    }
}

Write-Host "`nTests Completed."
