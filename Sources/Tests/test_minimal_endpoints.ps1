$ErrorActionPreference = "Stop"

function Test-Endpoint {
    param($Method, $Url, $Body = $null, $ContentType = "application/json")
    Write-Host "Calling $Method $Url..." -NoNewline
    try {
        if ($Body) {
             # Fix for PowerShell/curl quote escaping if using curl.exe, but let's use Invoke-RestMethod for reliability
             $response = Invoke-RestMethod -Method $Method -Uri $Url -Body $Body -ContentType $ContentType -ErrorAction Stop
        } else {
             $response = Invoke-RestMethod -Method $Method -Uri $Url -ErrorAction Stop
        }
        Write-Host " OK" -ForegroundColor Green
        # Write-Host ($response | ConvertTo-Json -Depth 2)
    } catch {
        Write-Host " FAILED" -ForegroundColor Red
        Write-Host $_.Exception.Message
    }
}

Write-Host "Waiting for server to start..."
Start-Sleep -Seconds 2

Test-Endpoint "GET" "http://localhost:8080/api/users/123"
Test-Endpoint "GET" "http://localhost:8080/api/users/456/name"

$userBody = '{ "name": "John Doe", "email": "john@example.com", "age": 30 }'
Test-Endpoint "POST" "http://localhost:8080/api/users" -Body $userBody

$updateBody = '{ "name": "Jane Smith", "email": "jane@example.com" }'
Test-Endpoint "PUT" "http://localhost:8080/api/users/789" -Body $updateBody

Test-Endpoint "DELETE" "http://localhost:8080/api/users/999"
Test-Endpoint "GET" "http://localhost:8080/api/posts/hello-world"
Test-Endpoint "GET" "http://localhost:8080/api/health"
Test-Endpoint "GET" "http://localhost:8080/api/cached"

# Error endpoint expects 500, Invoke-RestMethod throws on 500.
Write-Host "Calling GET http://localhost:8080/api/error..." -NoNewline
try {
    Invoke-RestMethod -Method GET -Uri "http://localhost:8080/api/error" -ErrorAction Stop
    Write-Host " UNEXPECTED OK" -ForegroundColor Red
} catch {
    if ($_.Exception.Response.StatusCode.value__ -eq 500) {
        Write-Host " OK (Expected 500)" -ForegroundColor Green
    } else {
         Write-Host " FAILED with " $_.Exception.Response.StatusCode.value__ -ForegroundColor Red
    }
}

Test-Endpoint "GET" "http://localhost:8080/index.html" -ContentType "text/html"
# Swagger might return HTML, Invoke-RestMethod might fail to parse if it expects JSON default? No, it adapts.
# Test-Endpoint "GET" "http://localhost:8080/swagger" 
Test-Endpoint "GET" "http://localhost:8080/api/request-context"

Write-Host "Tests Completed."
