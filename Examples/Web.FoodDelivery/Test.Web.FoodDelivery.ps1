### Tests for DextFood API (PowerShell Version)
$HostUrl = "http://localhost:9000"

Write-Host "🧪 Testing DextFood API..." -ForegroundColor Cyan

# 1. Health Check
$Response = Invoke-RestMethod -Uri "$HostUrl/health" -Method Get -ErrorAction Stop
Write-Host "   ✅ Health Check: $($Response.status)"

# 2. Create Order
try {
    $Response = Invoke-RestMethod -Uri "$HostUrl/api/orders?total=150.00" -Method Post -ErrorAction Stop
    Write-Host "   ✅ Create Order: $($Response.message)"
} catch {
    Write-Error "   ❌ Create Order Failed: $_"
}

# 3. Get High Value Orders
$Orders = Invoke-RestMethod -Uri "$HostUrl/api/orders/high-value" -Method Get
Write-Host "   ✅ High Value Orders Count: $($Orders.Count)"

Write-Host "`n✨ All tests completed." -ForegroundColor Green
