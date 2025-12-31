$ErrorActionPreference = "Continue"
$baseUrl = "http://localhost:8080"

Write-Host "🚀 Testing Web.RateLimitDemo on $baseUrl" -ForegroundColor Cyan
Write-Host "Sending 15 requests (Limit is 10/min)..."

$success = 0
$limited = 0

for ($i = 1; $i -le 15; $i++) {
    try {
        $resp = Invoke-WebRequest "$baseUrl/api/test" -UseBasicParsing
        $rem = $resp.Headers["X-RateLimit-Remaining"]
        
        # Se rem for nulo, maybe headers não estão vindo
        if ($null -eq $rem) { $rem = "?" }
        
        Write-Host "Req #$i: OK (Remaining: $rem)"
        $success++
    }
    catch {
        if ($_.Exception.Response.StatusCode -eq [System.Net.HttpStatusCode]::TooManyRequests) {
            # 429
            $retry = $_.Exception.Response.Headers["Retry-After"]
            Write-Host "Req #$i: BLOCKED (429) - Retry After: $retry sec" -ForegroundColor Magenta
            $limited++
        }
        else {
            Write-Error "Req #$i: Failed with $($_.Exception.Message)"
        }
    }
}

Write-Host "`nResults:"
Write-Host "Success: $success"
Write-Host "Limited: $limited"

if ($success -eq 10 -and $limited -eq 5) {
    Write-Host "`nSUCCESS: RATE LIMIT TEST PASSED (Exact Match)" -ForegroundColor Green
}
elseif ($success -gt 10) {
    Write-Error "`nFAIL: Rate limit not enforced (Success > 10)" 
    exit 1
}
elseif ($success -lt 10) {
    # Se rodar o teste 2 vezes seguido, pode falhar pq o contador não resetou.
    Write-Host "`nWARN: Less success than expected. Did you run the test twice within 60s?" -ForegroundColor Yellow
}
else {
    Write-Host "`nWARN: Counts differ." -ForegroundColor Yellow
}
