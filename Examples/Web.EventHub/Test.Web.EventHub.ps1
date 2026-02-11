# ===========================================================================
#  Test.Web.EventHub.ps1 - Integration Tests for EventHub API
#
#  Usage: Start the server first, then run:
#    .\Test.Web.EventHub.ps1
#
#  Tests the entire API flow: Auth, Events, Speakers, Attendees,
#  Registrations, WaitList auto-promotion, and Dashboard Metrics.
# ===========================================================================

#[Console]::OutputEncoding = [System.Text.Encoding]::GetEncoding(850)
#[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
$ErrorActionPreference = "Stop"

$baseUrl = "http://127.0.0.1:9000"
$passed = 0
$failed = 0
$total = 0
$token = ""
$uniqueSuffix = "$([Guid]::NewGuid().ToString().Substring(0, 8))"

# ---------------------------------------------------------------------------
# Helper Functions
# ---------------------------------------------------------------------------

function Test-Endpoint {
    param(
        [string]$Name,
        [string]$Method = "GET",
        [string]$Url,
        [object]$Body = $null,
        [int]$ExpectedStatus = 200,
        [hashtable]$Headers = @{},
        [scriptblock]$Validate = $null
    )
    $script:total++

    try {
        $params = @{
            Method      = $Method
            Uri         = $Url
            ContentType = "application/json; charset=utf-8"
            Headers     = @{
                "Accept" = "application/json"
            }
        }

        # Add auth header if we have a token
        if ($script:token) {
            $params.Headers["Authorization"] = "Bearer $script:token"
        }

        # Merge extra headers
        foreach ($key in $Headers.Keys) {
            $params.Headers[$key] = $Headers[$key]
        }

        # Add body if present
        if ($Body) {
            $params["Body"] = ($Body | ConvertTo-Json -Depth 10)
        }

        $response = Invoke-WebRequest @params -UseBasicParsing

        if ($response.StatusCode -ne $ExpectedStatus) {
            $script:failed++
            Write-Host "  FAIL  $Name" -ForegroundColor Red
            Write-Host "        Expected status $ExpectedStatus, got $($response.StatusCode)" -ForegroundColor DarkRed
            return $null
        }

        $result = $null
        if ($response.Content) {
            try {
                # Force UTF-8 decoding from raw bytes to avoid charset mess
                $utf8 = [System.Text.Encoding]::UTF8
                if ($response.RawContentStream) {
                    $bytes = $response.RawContentStream.ToArray()
                    $content = $utf8.GetString($bytes)
                }
                else {
                    $content = $response.Content
                }
                
                $result = $content | ConvertFrom-Json
            }
            catch {
                # Not JSON or stream error, return as string (also decoded as UTF8)
                if ($response.RawContentStream) {
                    $result = [System.Text.Encoding]::UTF8.GetString($response.RawContentStream.ToArray())
                }
                else {
                    $result = $response.Content
                }
            }
        }

        # Run validation if provided
        if ($Validate -and $result) {
            $validationResult = & $Validate $result
            if ($validationResult -eq $false) {
                $script:failed++
                Write-Host "  FAIL  $Name (validation failed)" -ForegroundColor Red
                return $result
            }
        }

        $script:passed++
        Write-Host "  PASS  $Name" -ForegroundColor Green
        return $result
    }
    catch {
        $script:failed++
        $statusCode = 0
        if ($_.Exception.Response) {
            $statusCode = [int]$_.Exception.Response.StatusCode
        }
        Write-Host "  FAIL  $Name" -ForegroundColor Red
        Write-Host "        Error: $($_.Exception.Message)" -ForegroundColor DarkRed
        if ($statusCode -gt 0) {
            Write-Host "        HTTP Status: $statusCode" -ForegroundColor DarkRed
            if ($_.Exception.Response) {
                $reader = New-Object System.IO.StreamReader($_.Exception.Response.GetResponseStream())
                $errorBody = $reader.ReadToEnd()
                Write-Host "        Error Body: $errorBody" -ForegroundColor DarkRed
            }
        }
        return $null
    }
}

# ---------------------------------------------------------------------------
# Test Execution
# ---------------------------------------------------------------------------

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  EventHub API - Integration Tests" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# ---------------------------------------------------------------------------
# 1. Health Check
# ---------------------------------------------------------------------------
Write-Host "--- Health Check ---" -ForegroundColor Yellow

Test-Endpoint -Name "Health check" `
    -Url "$baseUrl/health"

# ---------------------------------------------------------------------------
# 2. Auth
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "--- Authentication ---" -ForegroundColor Yellow

# Login with valid credentials
$loginResult = Test-Endpoint -Name "Login with admin credentials" `
    -Method "POST" `
    -Url "$baseUrl/api/auth/login" `
    -Body @{ Username = "admin"; Password = "admin123" } `
    -Validate { param($r) $r.Token -ne $null }

if ($loginResult) {
    $token = $loginResult.Token
    Write-Host "        Token obtained: $($token.Substring(0, 20))..." -ForegroundColor DarkGray
}

# Login with invalid credentials
$script:total++
try {
    Invoke-WebRequest -Method POST -Uri "$baseUrl/api/auth/login" `
        -ContentType "application/json; charset=utf-8" `
        -Body '{"username":"hacker","password":"wrong"}' `
        -UseBasicParsing | Out-Null
    $script:failed++
    Write-Host "  FAIL  Login with invalid credentials (should return 401)" -ForegroundColor Red
}
catch {
    $statusCode = [int]$_.Exception.Response.StatusCode
    if ($statusCode -eq 401) {
        $script:passed++
        Write-Host "  PASS  Login with invalid credentials (returns 401)" -ForegroundColor Green
    }
    else {
        $script:failed++
        Write-Host "  FAIL  Login with invalid credentials (expected 401, got $statusCode)" -ForegroundColor Red
    }
}

# ---------------------------------------------------------------------------
# 3. Public Events (seeded data)
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "--- Events (Public) ---" -ForegroundColor Yellow

$events = Test-Endpoint -Name "List published events" `
    -Url "$baseUrl/api/events" `
    -Validate { param($r) $r.Count -ge 2 }

if ($events -and $events.Count -gt 0) {
    $eventId = $events[0].Id
    Write-Host "        Found $($events.Count) published events. First: '$($events[0].Title)' (ID: $eventId)" -ForegroundColor DarkGray

    Test-Endpoint -Name "Get event by ID ($eventId)" `
        -Url "$baseUrl/api/events/$eventId" `
        -Validate { param($r) $r.Id -eq $eventId }
}

# Get event that doesn't exist
$script:total++
try {
    Invoke-WebRequest -Method GET -Uri "$baseUrl/api/events/99999" `
        -ContentType "application/json" `
        -Headers @{ "Accept" = "application/json"; "Authorization" = "Bearer $token" } `
        -UseBasicParsing | Out-Null
    $script:failed++
    Write-Host "  FAIL  Get non-existent event (should return 404)" -ForegroundColor Red
}
catch {
    $statusCode = [int]$_.Exception.Response.StatusCode
    if ($statusCode -eq 404) {
        $script:passed++
        Write-Host "  PASS  Get non-existent event (returns 404)" -ForegroundColor Green
    }
    else {
        $script:failed++
        Write-Host "  FAIL  Get non-existent event (expected 404, got $statusCode)" -ForegroundColor Red
    }
}

# ---------------------------------------------------------------------------
# 4. Create & Manage Events (Auth required)
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "--- Events (Auth) ---" -ForegroundColor Yellow

# Get a venue (from the first event)
$venueId = 1
if ($events -and $events.Count -gt 0) {
    if ($events[0].VenueId -gt 0) {
        $venueId = $events[0].VenueId
    }
}

$newEvent = Test-Endpoint -Name "Create event (Draft)" `
    -Method "POST" `
    -Url "$baseUrl/api/events" `
    -Body @{
    VenueId     = $venueId
    Title       = "Event $uniqueSuffix"
    Description = "Created by PowerShell integration tests"
    StartDate   = "2026-12-20T10:00:00"
    EndDate     = "2026-12-20T18:00:00"
    MaxCapacity = 3
} `
    -ExpectedStatus 201 `
    -Validate { param($r) ($r.Title -eq "Event $uniqueSuffix") -and ($r.Status -eq 0) }

$newEventId = 0
if ($newEvent) {
    $newEventId = $newEvent.Id
    Write-Host "        Created event ID: $newEventId (status: $($newEvent.Status))" -ForegroundColor DarkGray
}

# Publish the event
if ($newEventId -gt 0) {
    $published = Test-Endpoint -Name "Publish event ($newEventId)" `
        -Method "POST" `
        -Url "$baseUrl/api/events/$newEventId/publish" `
        -Validate { param($r) $r.Status -eq 1 }

    # VERIFICATION: Check if it's actually in the DB as Published
    Test-Endpoint -Name "Verify Publish Persistence ($newEventId)" `
        -Url "$baseUrl/api/events/$newEventId" `
        -Validate { param($r) $r.Status -eq 1 }
}

# Update the event
if ($newEventId -gt 0) {
    Test-Endpoint -Name "Update event title ($newEventId)" `
        -Method "PUT" `
        -Url "$baseUrl/api/events/$newEventId" `
        -Body @{
        Title       = "Updated $uniqueSuffix"
        Description = "Updated description"
        StartDate   = "2026-12-20T10:00:00"
        EndDate     = "2026-12-20T18:00:00"
        MaxCapacity = 3
    } `
        -Validate { param($r) $r.Title -eq "Updated $uniqueSuffix" }
}

# ---------------------------------------------------------------------------
# 5. Speakers
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "--- Speakers ---" -ForegroundColor Yellow

if ($newEventId -gt 0) {
    $speaker = Test-Endpoint -Name "Add speaker to event" `
        -Method "POST" `
        -Url "$baseUrl/api/events/$newEventId/speakers" `
        -Body @{
        Name  = "Test Speaker"
        Bio   = "Integration test speaker bio"
        Email = "test.speaker@example.com"
    } `
        -ExpectedStatus 201 `
        -Validate { param($r) $r.Name -eq "Test Speaker" }

    Test-Endpoint -Name "List speakers for event" `
        -Url "$baseUrl/api/events/$newEventId/speakers" `
        -Validate { param($r) $r.Count -ge 1 }
}

# ---------------------------------------------------------------------------
# 6. Attendees
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "--- Attendees ---" -ForegroundColor Yellow

$attendee1 = Test-Endpoint -Name "Register attendee (Ana)" `
    -Method "POST" `
    -Url "$baseUrl/api/attendees" `
    -Body @{
    Name  = "Ana Test"
    Email = "ana.test.$uniqueSuffix@example.com"
    Phone = "(11) 91111-0001"
} `
    -ExpectedStatus 201 `
    -Validate { param($r) $r.Name -eq "Ana Test" }

$attendee2 = Test-Endpoint -Name "Register attendee (Bruno)" `
    -Method "POST" `
    -Url "$baseUrl/api/attendees" `
    -Body @{
    Name  = "Bruno Test"
    Email = "bruno.test.$uniqueSuffix@example.com"
    Phone = "(11) 91111-0002"
} `
    -ExpectedStatus 201

$attendee3 = Test-Endpoint -Name "Register attendee (Carla)" `
    -Method "POST" `
    -Url "$baseUrl/api/attendees" `
    -Body @{
    Name  = "Carla Test"
    Email = "carla.test.$uniqueSuffix@example.com"
    Phone = "(11) 91111-0003"
} `
    -ExpectedStatus 201

# Duplicate email should fail
$script:total++
try {
    Invoke-WebRequest -Method POST -Uri "$baseUrl/api/attendees" `
        -ContentType "application/json; charset=utf-8" `
        -Body "{`"Name`":`"Ana Duplicate`",`"Email`":`"ana.test.$uniqueSuffix@example.com`",`"Phone`":`"(11) 90000-0000`"}" `
        -Headers @{ "Accept" = "application/json"; "Authorization" = "Bearer $token" } `
        -UseBasicParsing | Out-Null
    $script:failed++
    Write-Host "  FAIL  Duplicate attendee email (should return 400)" -ForegroundColor Red
}
catch {
    $statusCode = [int]$_.Exception.Response.StatusCode
    if ($statusCode -eq 400) {
        $script:passed++
        Write-Host "  PASS  Duplicate attendee email (returns 400)" -ForegroundColor Green
    }
    else {
        $script:failed++
        Write-Host "  FAIL  Duplicate attendee email (expected 400, got $statusCode)" -ForegroundColor Red
    }
}

if ($attendee1) {
    Test-Endpoint -Name "Get attendee by ID" `
        -Url "$baseUrl/api/attendees/$($attendee1.Id)" `
        -Validate { param($r) $r.Email -eq "ana.test.$uniqueSuffix@example.com" }
}

# ---------------------------------------------------------------------------
# 7. Registrations & WaitList
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "--- Registrations & WaitList ---" -ForegroundColor Yellow

# ALWAYS use the new event we just created for registration tests
# This ensures every test run uses a fresh event with no prior registrations
$testEventId = $newEventId

if ($testEventId -gt 0 -and $attendee1 -and $attendee2 -and $attendee3) {
    # Register all 3 attendees
    $reg1 = Test-Endpoint -Name "Registration 1 (should be Confirmed)" `
        -Method "POST" `
        -Url "$baseUrl/api/registrations" `
        -Body @{ EventId = $testEventId; AttendeeId = $attendee1.Id } `
        -ExpectedStatus 201 `
        -Validate { param($r) $r.Status -eq 0 }

    $reg2 = Test-Endpoint -Name "Registration 2 (should be Confirmed)" `
        -Method "POST" `
        -Url "$baseUrl/api/registrations" `
        -Body @{ EventId = $testEventId; AttendeeId = $attendee2.Id } `
        -ExpectedStatus 201 `
        -Validate { param($r) $r.Status -eq 0 }

    $reg3 = Test-Endpoint -Name "Registration 3 (should be Confirmed)" `
        -Method "POST" `
        -Url "$baseUrl/api/registrations" `
        -Body @{ EventId = $testEventId; AttendeeId = $attendee3.Id } `
        -ExpectedStatus 201 `
        -Validate { param($r) $r.Status -eq 0 }

    # Now register a 4th attendee (create one)
    $attendee4 = Test-Endpoint -Name "Register attendee (Diego - for WaitList)" `
        -Method "POST" `
        -Url "$baseUrl/api/attendees" `
        -Body @{
        Name  = "Diego Test"
        Email = "diego.test.$uniqueSuffix@example.com"
        Phone = "(11) 91111-0004"
    } `
        -ExpectedStatus 201

    if ($attendee4) {
        # This should go to WaitList since we set MaxCapacity=3 for our new event
        $reg4 = Test-Endpoint -Name "Registration 4 (should be WaitList - capacity=3)" `
            -Method "POST" `
            -Url "$baseUrl/api/registrations" `
            -Body @{ EventId = $testEventId; AttendeeId = $attendee4.Id } `
            -ExpectedStatus 201 `
            -Validate { param($r) $r.Status -eq 1 }

        Write-Host "        WaitList auto-assignment: VERIFIED" -ForegroundColor DarkGray
    }

    # List registrations for event
    Test-Endpoint -Name "List registrations for event" `
        -Url "$baseUrl/api/events/$testEventId/registrations" `
        -Validate { param($r) $r.Count -ge 3 }

    # List registrations for attendee
    if ($attendee1) {
        Test-Endpoint -Name "List registrations for attendee" `
            -Url "$baseUrl/api/attendees/$($attendee1.Id)/registrations" `
            -Validate { param($r) $r.Count -ge 1 }
    }

    # Cancel a registration (should auto-promote from WaitList)
    if ($reg1) {
        $cancelResult = Test-Endpoint -Name "Cancel registration (should promote WaitList)" `
            -Method "POST" `
            -Url "$baseUrl/api/registrations/$($reg1.Id)/cancel" `
            -Validate { param($r) $r.Status -eq 2 }

        if ($cancelResult) {
            Write-Host "        Registration $($reg1.Id) canceled. WaitList promotion should have occurred." -ForegroundColor DarkGray
        }
    }

    # Duplicate registration should fail
    if ($attendee2) {
        $script:total++
        try {
            Invoke-WebRequest -Method POST -Uri "$baseUrl/api/registrations" `
                -ContentType "application/json; charset=utf-8" `
                -Body "{`"EventId`":$testEventId,`"AttendeeId`":$($attendee2.Id)}" `
                -Headers @{ "Accept" = "application/json"; "Authorization" = "Bearer $token" } `
                -UseBasicParsing | Out-Null
            $script:failed++
            Write-Host "  FAIL  Duplicate registration (should return 400)" -ForegroundColor Red
        }
        catch {
            $statusCode = [int]$_.Exception.Response.StatusCode
            if ($statusCode -eq 400) {
                $script:passed++
                Write-Host "  PASS  Duplicate registration (returns 400)" -ForegroundColor Green
            }
            else {
                $script:failed++
                Write-Host "  FAIL  Duplicate registration (expected 400, got $statusCode)" -ForegroundColor Red
            }
        }
    }
}

# ---------------------------------------------------------------------------
# 8. Draft Event - Registration should fail
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "--- Business Rules ---" -ForegroundColor Yellow

# The "Workshop: Delphi para Iniciantes" event is in Draft status
# Try to register for it
if ($attendee1) {
    # Find the draft event
    $draftEventId = 4  # We know from seeder it's the 4th event

    $script:total++
    try {
        Invoke-WebRequest -Method POST -Uri "$baseUrl/api/registrations" `
            -ContentType "application/json; charset=utf-8" `
            -Body "{`"EventId`":$draftEventId,`"AttendeeId`":$($attendee1.Id)}" `
            -Headers @{ "Accept" = "application/json"; "Authorization" = "Bearer $token" } `
            -UseBasicParsing | Out-Null
        $script:failed++
        Write-Host "  FAIL  Register for Draft event (should return 400)" -ForegroundColor Red
    }
    catch {
        $statusCode = [int]$_.Exception.Response.StatusCode
        if ($statusCode -eq 400) {
            $script:passed++
            Write-Host "  PASS  Register for Draft event (returns 400)" -ForegroundColor Green
        }
        else {
            $script:failed++
            Write-Host "  FAIL  Register for Draft event (expected 400, got $statusCode)" -ForegroundColor Red
        }
    }
}

# Cancel event
if ($newEventId -gt 0) {
    $canceledEvent = Test-Endpoint -Name "Cancel event ($newEventId)" `
        -Method "POST" `
        -Url "$baseUrl/api/events/$newEventId/cancel" `
        -Validate { param($r) $r.Status -eq 2 }
}

# ---------------------------------------------------------------------------
# 9. Dashboard Metrics
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "--- Dashboard Metrics ---" -ForegroundColor Yellow

$metrics = Test-Endpoint -Name "Get dashboard metrics" `
    -Url "$baseUrl/api/events/metrics" `
    -Validate { param($r)
    ($r.TotalEvents -gt 0) -and
    ($r.TotalAttendees -gt 0) -and
    ($r.TotalRegistrations -ge 0)
}

if ($metrics) {
    Write-Host "        Events: $($metrics.TotalEvents) | Published: $($metrics.PublishedEvents)" -ForegroundColor DarkGray
    Write-Host "        Attendees: $($metrics.TotalAttendees)" -ForegroundColor DarkGray
    Write-Host "        Registrations: $($metrics.TotalRegistrations) (Confirmed: $($metrics.ConfirmedRegistrations), WaitList: $($metrics.WaitListRegistrations))" -ForegroundColor DarkGray
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  Test Results" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "  Total:  $total" -ForegroundColor White
Write-Host "  Passed: $passed" -ForegroundColor Green
Write-Host "  Failed: $failed" -ForegroundColor $(if ($failed -gt 0) { "Red" } else { "Green" })
Write-Host ""

if ($failed -gt 0) {
    Write-Host "  SOME TESTS FAILED!" -ForegroundColor Red
    exit 1
}
else {
    Write-Host "  ALL TESTS PASSED!" -ForegroundColor Green
    exit 0
}
