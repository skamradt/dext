# 🎪 Web.EventHub — Event Management & Registration Platform

A comprehensive **Dext Framework** example demonstrating how to build a real-world event management API with attendee registration, speaker management, and automatic waitlist promotion.

## 🎯 What This Example Teaches

| Feature | Dext Concept |
|---------|-------------|
| **Minimal API Pattern** | `Builder.MapGet/MapPost` with DI injection |
| **5 ORM Entities** | `TEvent`, `TVenue`, `TSpeaker`, `TAttendee`, `TRegistration` |
| **Smart Properties** | `Prop<TEnum>` for type-safe queries |
| **Business Rules in Entities** | `TEvent.CanRegister`, `TRegistration.Cancel` |
| **Service Layer** | Interfaces + Scoped implementations |
| **JWT Authentication** | Login with tokens, protected endpoints |
| **Fluent Middleware** | CORS, Rate Limiting, Response Cache, Swagger |
| **Database Seeder** | Realistic sample data with scoped `IServiceScope` |
| **Unit Tests** | xUnit-style with `Should()` assertions |
| **Integration Tests** | PowerShell script covering all endpoints |

## 📂 Project Structure

```
Web.EventHub/
├── Domain/                          # Core domain layer
│   ├── EventHub.Domain.Enums.pas    # TEventStatus, TRegistrationStatus
│   ├── EventHub.Domain.Entities.pas # TVenue, TEvent, TSpeaker, TAttendee, TRegistration
│   └── EventHub.Domain.Models.pas   # DTOs (Request/Response records)
├── Data/                            # Data access layer
│   ├── EventHub.Data.Context.pas    # TEventHubDbContext (5 IDbSet<T>)
│   └── EventHub.Data.Seeder.pas     # Sample data seeder
├── Services/                        # Business logic layer
│   └── EventHub.Services.pas        # IEventService, ISpeakerService, IAttendeeService, IRegistrationService
├── Server/                          # Web API entry point
│   ├── Web.EventHub.dpr             # Program entry (Console)
│   ├── Web.EventHub.dproj           # Delphi project file
│   ├── EventHub.Startup.pas         # Middleware + DI configuration
│   └── EventHub.Endpoints.pas       # 16 Minimal API routes
├── Tests/                           # Unit tests
│   ├── Web.EventHub.Tests.dpr       # Test runner
│   ├── Web.EventHub.Tests.dproj     # Test project file
│   ├── EventHub.Tests.Entities.pas  # Entity logic tests (12+ tests)
│   └── EventHub.Tests.Services.pas  # DTO/Service tests
├── WebEventHub.groupproj            # Group project (Server + Tests)
├── Test.Web.EventHub.ps1            # Integration test script
└── README.md                        # This file
```

## 🏗️ Architecture

```
┌──────────────────┐     ┌──────────────────┐     ┌──────────────────┐
│   Endpoints      │────▶│   Services       │────▶│   DbContext      │
│  (Minimal API)   │     │  (Business Logic)│     │  (ORM/SQLite)    │
│                  │     │                  │     │                  │
│  MapGet/MapPost  │     │  IEventService   │     │  TEventHubDb-    │
│  Model Binding   │     │  IRegistration-  │     │  Context         │
│  [FromRoute]     │     │  Service         │     │  IDbSet<T>       │
└──────────────────┘     └──────────────────┘     └──────────────────┘
         │                        │
         │                        ▼
    ┌────┴────┐          ┌──────────────────┐
    │  JWT    │          │   Domain         │
    │  Auth   │          │  Entities + Enums│
    │  CORS   │          │  Business Rules  │
    │  Rate   │          │  Smart Props     │
    │  Limit  │          └──────────────────┘
    └─────────┘
```

## 🚀 Quick Start

### 1. Build & Run the Server
Open `WebEventHub.groupproj` in Delphi IDE, build, and run `Web.EventHub`.

### 2. Run Unit Tests
Build and run `Web.EventHub.Tests` from the Tests project.

### 3. Run Integration Tests
With the server running on port 9000:
```powershell
.\Test.Web.EventHub.ps1
```

### 4. Explore the API
Open Swagger UI at: **http://localhost:9000/swagger**

## 📋 API Endpoints

### 🔓 Public Endpoints
| Method | Route | Description |
|--------|-------|-------------|
| `GET` | `/health` | Health check |
| `GET` | `/api/events` | List all published events |
| `GET` | `/api/events/{id}` | Get event details |
| `GET` | `/api/events/{eventId}/speakers` | List speakers for an event |
| `POST` | `/api/attendees` | Register as an attendee |
| `GET` | `/api/attendees/{id}` | Get attendee info |
| `POST` | `/api/registrations` | Register for an event |
| `POST` | `/api/registrations/{id}/cancel` | Cancel a registration |
| `GET` | `/api/events/{eventId}/registrations` | List event registrations |
| `GET` | `/api/attendees/{id}/registrations` | List attendee registrations |

### 🔐 Protected Endpoints (JWT Required)
| Method | Route | Description |
|--------|-------|-------------|
| `POST` | `/api/auth/login` | Get JWT token |
| `POST` | `/api/events` | Create event (starts as Draft) |
| `PUT` | `/api/events/{id}` | Update event |
| `POST` | `/api/events/{id}/publish` | Publish draft event |
| `POST` | `/api/events/{id}/cancel` | Cancel event |
| `GET` | `/api/events/metrics` | Dashboard metrics |

### Demo Credentials
- **Admin**: `admin` / `admin123`
- **Organizer**: `organizer` / `org123`

## 📜 Business Rules

1. **Draft Events**: New events start as `Draft` and must be `Published` before accepting registrations
2. **Auto-WaitList**: When `MaxCapacity` is reached, new registrations go to `WaitList` automatically
3. **Auto-Promote**: When a confirmed registration is canceled, the first person on the `WaitList` is automatically promoted to `Confirmed`
4. **24h Cancellation Rule**: Registrations cannot be canceled less than 24 hours before the event starts
5. **Unique Registrations**: An attendee can only have one active registration per event
6. **Capacity Validation**: `MaxCapacity` cannot exceed the Venue's `Capacity`

## 🧪 Test Coverage

### Unit Tests (17 tests)
- **TEventTests**: CanRegister for all 4 statuses, AvailableSlots, IsFullyBooked
- **TRegistrationTests**: Cancel (success, 24h rule, idempotency, status, CanceledAt)
- **TVenueTests**, **TSpeakerTests**, **TAttendeeTests**: Property assignment
- **DTO Tests**: EventResponse, RegistrationResponse, Metrics, CreateEventRequest

### Integration Tests (25+ assertions)
- Health check
- Auth (valid + invalid credentials)
- Event CRUD + Publish + Cancel
- Speaker management
- Attendee registration + duplicate rejection
- Registration flow: Confirmed → WaitList → Cancel → Auto-Promote
- Business rule enforcement (Draft event rejection)
- Dashboard metrics validation

## 🔑 Key Dext Patterns Demonstrated

### Minimal API with DI
```pascal
Builder.MapPost<TCreateRegistrationRequest, IRegistrationService, IResult>(
  '/api/registrations',
  function(Req: TCreateRegistrationRequest; Svc: IRegistrationService): IResult
  begin
    Result := Results.Created<TRegistrationResponse>(
      '/api/registrations', Svc.CreateRegistration(Req));
  end);
```

### Smart Properties for Queries
```pascal
// Type-safe ORM queries with operator overloading
FDb.Registrations
  .Where((TRegistration.Props.EventId = EventId) and
         (TRegistration.Props.Status = rsConfirmed))
  .Count;
```

### Business Logic in Entities
```pascal
function TRegistration.Cancel(EventStartDate: TDateTime): Boolean;
begin
  if TRegistrationStatus(FStatus) = rsCanceled then
    Exit(False);
  if HoursBetween(Now, EventStartDate) < 24 then
    Exit(False);
  FStatus := rsCanceled;
  FCanceledAt := Now;
  Result := True;
end;
```

---

Built with ❤️ using the [Dext Framework](https://github.com/nicollassilva/dext) for Delphi.
