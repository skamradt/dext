# Test.Web.TicketSales.ps1
# Script to test Ticket Sales API endpoints
# Ensure server is running at http://localhost:9000

# Fix Console Encoding (CP850 for integration with User shell)
[Console]::OutputEncoding = [System.Text.Encoding]::GetEncoding(850)
$ErrorActionPreference = "Continue"

# Bypass Proxy (crucial for localhost)
[System.Net.WebRequest]::DefaultWebProxy = [System.Net.GlobalProxySelection]::GetEmptyWebProxy()

$baseUrl = "http://localhost:9000"
$jwtSecret = "ticket-sales-super-secret-key-minimum-32-characters"
$jwtIssuer = "ticket-sales-api"
$jwtAudience = "ticket-sales-clients"

# Global IDs for chaining
$global:eventId = 0
$global:ticketTypeId = 0
$global:customerId = 0
$global:orderId = 0
$global:ticketCode = ""

# Common Headers
$global:baseHeaders = @{
    "Accept" = "application/json"
}

# Helper Function
function Run-Test {
    param (
        [string]$Name,
        [scriptblock]$Block
    )
    Write-Host "`n[TEST] $Name"
    try {
        & $Block
    }
    catch {
        Write-Host "FAIL: Exception during test '$Name': $_" -ForegroundColor Red
        if ($_.Exception.Response) {
            $httpResp = $_.Exception.Response
            Write-Host "   Status: $($httpResp.StatusCode)" -ForegroundColor Yellow
            Write-Host "   URL: $($httpResp.ResponseUri.AbsoluteUri)" -ForegroundColor Yellow
        }
    }
}

function New-JwtToken {
    param ([string]$Secret, [string]$Issuer, [string]$Audience, [string]$Subject = "admin")
    $header = @{ alg = "HS256"; typ = "JWT" } | ConvertTo-Json -Compress
    $payload = @{
        iss = $Issuer; aud = $Audience; sub = $Subject
        iat = [int64](Get-Date -UFormat %s)
        exp = [int64]((Get-Date).AddHours(2).ToUniversalTime() - (Get-Date "1970-01-01").ToUniversalTime()).TotalSeconds
    } | ConvertTo-Json -Compress

    function ConvertTo-Base64Url { param ([byte[]]$Bytes) [Convert]::ToBase64String($Bytes).Split('=')[0].Replace('+', '-').Replace('/', '_') }

    $headerBytes = [System.Text.Encoding]::UTF8.GetBytes($header)
    $payloadBytes = [System.Text.Encoding]::UTF8.GetBytes($payload)
    $inputStr = "$(ConvertTo-Base64Url $headerBytes).$(ConvertTo-Base64Url $payloadBytes)"
    $hmac = New-Object System.Security.Cryptography.HMACSHA256
    $hmac.Key = [System.Text.Encoding]::UTF8.GetBytes($Secret)
    $sig = ConvertTo-Base64Url ($hmac.ComputeHash([System.Text.Encoding]::UTF8.GetBytes($inputStr)))
    return "$inputStr.$sig"
}

Write-Host ">>> Starting Ticket Sales API Tests (All Endpoints)..." -ForegroundColor Cyan

# 1. Health Check
Run-Test "Health Check (/api/health)" {
    $health = Invoke-RestMethod -Uri "$baseUrl/api/health" -Method Get -Headers $global:baseHeaders -ErrorAction Stop
    if ($health.status -eq "healthy") { Write-Host "PASS: Server Online" -ForegroundColor Green }
    else { Write-Host "FAIL: Unexpected status: $($health.status)" -ForegroundColor Red }
}

# 2. Auth Token
Run-Test "Generate Local JWT" {
    $token = New-JwtToken -Secret $jwtSecret -Issuer $jwtIssuer -Audience $jwtAudience
    $global:baseHeaders["Authorization"] = "Bearer $token"
    Write-Host "PASS: Token generated." -ForegroundColor Green
}

# 3. Events - Create
Run-Test "Create Event (POST /api/events)" {
    $date = (Get-Date).AddDays(45).ToString("yyyy-MM-ddTHH:mm:ss")
    $body = @{ name = "Tech Summit 2026"; description = "Developer Conference"; venue = "Convention Center"; eventDate = $date; capacity = 500 } | ConvertTo-Json
    $resp = Invoke-RestMethod -Uri "$baseUrl/api/events" -Headers $global:baseHeaders -Method Post -Body $body -ContentType "application/json; charset=utf-8" -ErrorAction Stop
    $global:eventId = $resp.id
    Write-Host "PASS: Event Created ID: $global:eventId" -ForegroundColor Green
}

if ($global:eventId -gt 0) {
    # Events - Get All
    Run-Test "List Events (GET /api/events)" {
        $list = Invoke-RestMethod -Uri "$baseUrl/api/events" -Headers $global:baseHeaders -Method Get
        Write-Host "PASS: Found $($list.Count) events." -ForegroundColor Green
    }
    
    # Events - Get By ID
    Run-Test "Get Event (GET /api/events/$global:eventId)" {
        $e = Invoke-RestMethod -Uri "$baseUrl/api/events/$global:eventId" -Headers $global:baseHeaders -Method Get
        if ($e.id -eq $global:eventId) { Write-Host "PASS: Event retrieved." -ForegroundColor Green }
    }

    # Events - Update
    Run-Test "Update Event (PUT /api/events/$global:eventId)" {
        $body = @{ description = "Updated Description" } | ConvertTo-Json
        $e = Invoke-RestMethod -Uri "$baseUrl/api/events/$global:eventId" -Headers $global:baseHeaders -Method Put -Body $body -ContentType "application/json"
        Write-Host "PASS: Event updated." -ForegroundColor Green
    }

    # Ticket Types - Create
    Run-Test "Create Ticket Type (POST /api/ticket-types)" {
        $body = @{ eventId = $global:eventId; name = "Early Bird"; description = "Discounted"; price = 100.00; quantity = 50; isHalfPrice = $false } | ConvertTo-Json
        $resp = Invoke-RestMethod -Uri "$baseUrl/api/ticket-types" -Headers $global:baseHeaders -Method Post -Body $body -ContentType "application/json"
        $global:ticketTypeId = $resp.id
        Write-Host "PASS: TicketType Created ID: $global:ticketTypeId" -ForegroundColor Green
    }

    # Ticket Types - Get By ID
    if ($global:ticketTypeId -gt 0) {
        Run-Test "Get Ticket Type (GET /api/ticket-types/$global:ticketTypeId)" {
            $tt = Invoke-RestMethod -Uri "$baseUrl/api/ticket-types/$global:ticketTypeId" -Headers $global:baseHeaders -Method Get
            if ($tt.id -eq $global:ticketTypeId) { Write-Host "PASS: TicketType retrieved." -ForegroundColor Green }
        }
    }

    # Events - Open Sales
    Run-Test "Open Sales (POST /api/events/.../open-sales)" {
        Invoke-RestMethod -Uri "$baseUrl/api/events/$global:eventId/open-sales" -Headers $global:baseHeaders -Method Post
        Write-Host "PASS: Sales Opened." -ForegroundColor Green
    }
    
    # Events - Get Available
    Run-Test "Get Available Events (GET /api/events/available)" {
        $list = Invoke-RestMethod -Uri "$baseUrl/api/events/available" -Headers $global:baseHeaders -Method Get
        Write-Host "PASS: Found $($list.Count) available events." -ForegroundColor Green
    }
}

# 4. Customers - Create
Run-Test "Create Customer (POST /api/customers)" {
    $email = "dev_$(Get-Random)@dext.com"
    $body = @{ name = "Dev User"; email = $email; cpf = "111.222.333-44" } | ConvertTo-Json
    $resp = Invoke-RestMethod -Uri "$baseUrl/api/customers" -Method Post -Body $body -ContentType "application/json"
    $global:customerId = $resp.id
    Write-Host "PASS: Customer Created ID: $global:customerId" -ForegroundColor Green
}

# Customers - Get All
Run-Test "List Customers (GET /api/customers)" {
    $list = Invoke-RestMethod -Uri "$baseUrl/api/customers" -Headers $global:baseHeaders -Method Get
    Write-Host "PASS: Found $($list.Count) customers." -ForegroundColor Green
}

# 5. Orders - Create
if ($global:customerId -gt 0 -and $global:ticketTypeId -gt 0) {
    Run-Test "Create Order (POST /api/orders)" {
        $body = @{ customerId = $global:customerId; items = @( @{ ticketTypeId = $global:ticketTypeId; quantity = 1 } ) } | ConvertTo-Json
        $resp = Invoke-RestMethod -Uri "$baseUrl/api/orders" -Headers $global:baseHeaders -Method Post -Body $body -ContentType "application/json"
        $global:orderId = $resp.id
        Write-Host "PASS: Order Created ID: $global:orderId Total: $($resp.total)" -ForegroundColor Green
    }

    if ($global:orderId -gt 0) {
        # Orders - Get By ID
        Run-Test "Get Order (GET /api/orders/$global:orderId)" {
            $o = Invoke-RestMethod -Uri "$baseUrl/api/orders/$global:orderId" -Headers $global:baseHeaders -Method Get
            if ($o.id -eq $global:orderId) { Write-Host "PASS: Order retrieved." -ForegroundColor Green }
        }

        # Orders - Pay
        Run-Test "Pay Order (POST /api/orders/.../pay)" {
            Invoke-RestMethod -Uri "$baseUrl/api/orders/$global:orderId/pay" -Headers $global:baseHeaders -Method Post
            Write-Host "PASS: Order Paid." -ForegroundColor Green
        }

        # Orders - Get Tickets
        Run-Test "Get Order Tickets (GET /api/orders/.../tickets)" {
            $tickets = Invoke-RestMethod -Uri "$baseUrl/api/orders/$global:orderId/tickets" -Headers $global:baseHeaders -Method Get
            if ($tickets.Count -gt 0) {
                $global:ticketCode = $tickets[0].code
                Write-Host "PASS: Found $($tickets.Count) tickets. Code: $global:ticketCode" -ForegroundColor Green
            }
        }
    }
}

# 6. Tickets
if ($global:ticketCode -ne "") {
    # Tickets - Get By Code
    Run-Test "Get Ticket (GET /api/tickets/$global:ticketCode)" {
        $t = Invoke-RestMethod -Uri "$baseUrl/api/tickets/$global:ticketCode" -Headers $global:baseHeaders -Method Get
        Write-Host "PASS: Ticket retrieved." -ForegroundColor Green
    }

    # Tickets - Validate
    Run-Test "Validate Ticket (POST /api/tickets/validate)" {
        $body = @{ code = $global:ticketCode } | ConvertTo-Json
        $val = Invoke-RestMethod -Uri "$baseUrl/api/tickets/validate" -Headers $global:baseHeaders -Method Post -Body $body -ContentType "application/json"
        if ($val.valid) { Write-Host "PASS: Ticket Validated." -ForegroundColor Green }
        else { Write-Host "FAIL: Ticket Invalid." -ForegroundColor Red }
    }
}

# 7. Lists Check
Run-Test "List All Orders (GET /api/orders)" {
    $list = Invoke-RestMethod -Uri "$baseUrl/api/orders" -Headers $global:baseHeaders -Method Get
    Write-Host "PASS: Found $($list.Count) orders." -ForegroundColor Green
}

Write-Host "`n>>> Tests Completed." -ForegroundColor Cyan
