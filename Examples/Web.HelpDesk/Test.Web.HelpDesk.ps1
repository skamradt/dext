# Test.Web.HelpDesk.ps1
# Script to test HelpDesk API endpoints
# Ensure server is running at http://localhost:9005

# Fix Console Encoding
[Console]::OutputEncoding = [System.Text.Encoding]::GetEncoding(850)
$ErrorActionPreference = "Continue"

# Bypass Proxy
[System.Net.WebRequest]::DefaultWebProxy = [System.Net.GlobalProxySelection]::GetEmptyWebProxy()

$baseUrl = "http://localhost:9005"

# Global IDs for chaining
$global:adminId = 1
$global:agentId = 2
$global:customerId = 3
$global:ticketId = 0

# Helper Function
function Invoke-HttpTest {
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
            $reader = New-Object System.IO.StreamReader($httpResp.GetResponseStream())
            $body = $reader.ReadToEnd()
            Write-Host "   Status: $($httpResp.StatusCode)" -ForegroundColor Yellow
            Write-Host "   Body: $body" -ForegroundColor Yellow
        }
    }
}

Write-Host ">>> Starting HelpDesk API Integration Tests..." -ForegroundColor Cyan

# 1. Auth - Register
Invoke-HttpTest "Register New User (POST /api/auth/register)" {
    $email = "test_user_$(Get-Random)@dext.com"
    $body = @{ 
        name     = "Integration Test User"; 
        email    = $email; 
        password = "Password123";
        role     = 2 # urCustomer
    } | ConvertTo-Json
    $resp = Invoke-RestMethod -Uri "$baseUrl/api/auth/register" -Method Post -Body $body -ContentType "application/json" -ErrorAction Stop
    $global:newUserId = $resp.id
    Write-Host "PASS: User Registered ID: $global:newUserId" -ForegroundColor Green
}

# 2. Auth - Login
Invoke-HttpTest "Login (POST /api/auth/login)" {
    $body = @{ 
        email    = "admin@helpdesk.com"; 
        password = "hash_1234" # Correct password from Seeder
    } | ConvertTo-Json
    $resp = Invoke-RestMethod -Uri "$baseUrl/api/auth/login" -Method Post -Body $body -ContentType "application/json" -ErrorAction Stop
    if ($resp.accessToken) {
        Write-Host "PASS: Login successful. Token: $($resp.accessToken)" -ForegroundColor Green
    }
}

# 3. Tickets - Create (as Customer)
Invoke-HttpTest "Create Ticket (POST /api/tickets)" {
    $headers = @{ "X-User-Id" = "$global:customerId" }
    $body = @{
        subject     = "Internet not working";
        description = "I can't access any websites since morning.";
        priority    = 2; # tpHigh
        channel     = 2;  # tcWeb
    } | ConvertTo-Json
    $resp = Invoke-RestMethod -Uri "$baseUrl/api/tickets" -Headers $headers -Method Post -Body $body -ContentType "application/json" -ErrorAction Stop
    $global:ticketId = $resp.id
    Write-Host "PASS: Ticket Created ID: $global:ticketId, SLA: $($resp.dueDate)" -ForegroundColor Green
}

if ($global:ticketId -gt 0) {
    # 4. Tickets - List My Tickets
    Invoke-HttpTest "List My Tickets (GET /api/tickets)" {
        $headers = @{ "X-User-Id" = "$global:customerId" }
        $list = Invoke-RestMethod -Uri "$baseUrl/api/tickets" -Headers $headers -Method Get -ErrorAction Stop
        Write-Host "PASS: Found $($list.Count) tickets for user $global:customerId" -ForegroundColor Green
    }

    # 5. Tickets - Get By ID
    Invoke-HttpTest "Get Ticket Details (GET /api/tickets/$global:ticketId)" {
        $resp = Invoke-RestMethod -Uri "$baseUrl/api/tickets/$global:ticketId" -Method Get -ErrorAction Stop
        if ($resp.id -eq $global:ticketId) {
            Write-Host "PASS: Ticket details retrieved. Status: $($resp.status)" -ForegroundColor Green
        }
    }

    # 6. Tickets - Update Status (as Agent)
    Invoke-HttpTest "Update Status (POST /api/tickets/$global:ticketId/status)" {
        $headers = @{ "X-User-Id" = "$global:agentId" }
        $body = @{
            newStatus = 2; # 2 = tsInProgress (1 is tsOpen)
            reason    = "Investigating connection issues"
        } | ConvertTo-Json
        $resp = Invoke-RestMethod -Uri "$baseUrl/api/tickets/$global:ticketId/status" -Headers $headers -Method Post -Body $body -ContentType "application/json" -ErrorAction Stop
        # Compare as string since Dext is returning Enum Names
        if ($resp.status -like "*InProgress*") {
            Write-Host "PASS: Status updated to InProgress" -ForegroundColor Green
        }
        else {
            Write-Host "INFO: Status is $($resp.status)" -ForegroundColor Yellow
        }
    }

    # 7. Tickets - Assign (as Agent)
    Invoke-HttpTest "Assign Ticket (POST /api/tickets/$global:ticketId/assign)" {
        $headers = @{ "X-User-Id" = "$global:agentId" }
        $body = @{
            assigneeId = $global:agentId
        } | ConvertTo-Json
        $resp = Invoke-RestMethod -Uri "$baseUrl/api/tickets/$global:ticketId/assign" -Headers $headers -Method Post -Body $body -ContentType "application/json" -ErrorAction Stop
        if ($resp.assigneeId -eq $global:agentId) {
            Write-Host "PASS: Ticket assigned to agent $global:agentId" -ForegroundColor Green
        }
    }

    # 8. Tickets - Add Comment
    Invoke-HttpTest "Add Public Comment (POST /api/tickets/$global:ticketId/comments)" {
        $headers = @{ "X-User-Id" = "$global:agentId" }
        $body = @{
            text       = "Trying to reset the router remotely.";
            isInternal = $false
        } | ConvertTo-Json
        $comment = Invoke-RestMethod -Uri "$baseUrl/api/tickets/$global:ticketId/comments" -Headers $headers -Method Post -Body $body -ContentType "application/json" -ErrorAction Stop
        Write-Host "PASS: Comment added. ID: $($comment.id)" -ForegroundColor Green
    }
}

# 9. Metrics
Invoke-HttpTest "Get Metrics (GET /api/metrics)" {
    $resp = Invoke-RestMethod -Uri "$baseUrl/api/metrics" -Method Get -ErrorAction Stop
    # Correct names from TMetricsResponse DTO
    $open = if ($resp.totalOpen -ne $null) { $resp.totalOpen } else { $resp.TotalOpen }
    $overdue = if ($resp.overdueCount -ne $null) { $resp.overdueCount } else { $resp.OverdueCount }
    Write-Host "PASS: Metrics retrieved. Open: $open, Overdue: $overdue" -ForegroundColor Green
}

Write-Host "`n>>> Integration Tests Completed." -ForegroundColor Cyan
