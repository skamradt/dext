# 🚀 Web.ControllerExample - Complete Web API Showcase

Comprehensive demonstration of **Dext Framework Controllers** - a modern web framework for Delphi inspired by ASP.NET Core.

This folder contains **two projects** demonstrating different configuration styles:
1.  **`Web.ControllerExample.dpr`** (Standard): Standard declarative configuration.
2.  **`Web.ControllerExample.FluentAPI.dpr`** (Fluent): Modern Fluent API configuration style.

Both projects expose the **same API** and share the same controllers and services.

![Web Client Screenshot](WebClient.png)

---

## ✨ Features Demonstrated

| Feature | Description |
|---------|-------------|
| **JWT Authentication** | Bearer token auth with `[Authorize]` attribute |
| **Parameter Binding** | Route, Body, Query, Header binding |
| **Validation** | `[Required]`, `[StringLength]` with auto 400 responses |
| **Dependency Injection** | Constructor injection in controllers |
| **Action Filters** | `[LogAction]`, `[ResponseCache]`, `[RequireHeader]` |
| **API Versioning** | Query string and header-based versioning |
| **Health Checks** | `/health` endpoint with checks |
| **Content Negotiation** | Accept header-based response formatting |
| **CORS** | Configurable cross-origin support |
| **Static Files** | Serve SPA and assets |
| **Configuration** | strongly-typed settings from `appsettings.json` |

---

## 🚀 Quick Start

### 1. Run the Server

Choose your preferred configuration style:

**Option A: Standard Configuration**
```bash
Web.ControllerExample.exe
```

**Option B: Fluent API Configuration**
```bash
Web.ControllerExample.FluentAPI.exe
```

Server starts on `http://localhost:8080`.

> **Note:** The application automatically creates a default `appsettings.json` if it doesn't exist.

### 2. Run the Test Suite

We provide a comprehensive PowerShell test suite that validates all endpoints:

```powershell
.\Test.Web.ControllerExample.ps1
```

Expected output: 
```
TEST SUMMARY
============================================================
  Passed: 25
  Failed: 0
```

### 3. Frontend (WebClient)

A modern React + TypeScript + Vite frontend is included to demonstrate real-world usage.

```bash
cd WebClient
npm install
npm run dev
```

Access `http://localhost:5173`.
- **Username:** `admin`
- **Password:** `admin`

---

## 🎭 Configuration Styles Comparison

### Standard API (`Web.ControllerExample.dpr`)

Standard configuration uses builder methods with option objects:

```pascal
// CORS
var corsOptions := Builder.CreateCorsOptions;
corsOptions.AllowedOrigins := ['http://localhost:5173'];
Builder.UseCors(corsOptions);

// JWT
var AuthOptions := Builder.CreateJwtOptions('secret-key...');
AuthOptions.Issuer := 'dext-issuer';
Builder.UseJwtAuthentication(AuthOptions);
```

### Fluent API (`Web.ControllerExample.FluentAPI.dpr`)

The Fluent API offers a more concise and readable way to configure middleware:

```pascal
// CORS with Fluent API
Builder.UseCors(procedure(Cors: TCorsBuilder)
begin
  Cors.WithOrigins(['http://localhost:5173'])
      .WithMethods(['GET', 'POST', 'PUT', 'DELETE'])
      .AllowAnyHeader
      .AllowCredentials
      .WithMaxAge(3600);
end);

// JWT with Fluent API
Builder.UseJwtAuthentication('secret-key...',
  procedure(Auth: TJwtOptionsBuilder)
  begin
    Auth.WithIssuer('dext-issuer')
        .WithAudience('dext-audience');
  end
);
```

---

## 📋 Endpoints Reference

### Authentication
| Method | Endpoint | Description |
|--------|----------|-------------|
| POST | `/api/auth/login` | Login (returns JWT) |

### Greeting Controller (Protected)
| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/greet/{name}` | Route binding |
| GET | `/api/greet/negotiated` | Content negotiation (AllowAnonymous) |
| POST | `/api/greet` | Body binding with validation |
| GET | `/api/greet/search?q=&limit=` | Query binding |
| GET | `/api/greet/config` | IOptions pattern |

### Filters Controller (Action Filters Demo)
| Method | Endpoint | Filters Applied |
|--------|----------|-----------------|
| GET | `/api/filters/simple` | `[LogAction]` |
| GET | `/api/filters/cached` | `[ResponseCache(60)]`, `[AddHeader]` |
| POST | `/api/filters/secure` | `[RequireHeader("X-API-Key")]` |
| GET | `/api/filters/admin` | `[RequireAdminRole]`, `[TimingFilter]` |
| GET | `/api/filters/protected` | `[RequireHeader("Authorization")]` |

### List/Object Controllers
| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/list` | IList<TPerson> serialization |
| GET | `/api/object` | TPersonWithAddress serialization |
| GET | `/api/object/nested` | Nested object serialization |
| GET | `/api/object/list` | List of objects with null handling |

### API Versioning
| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/versioned?api-version=1.0` | Query string versioning |
| GET | `/api/versioned` + `X-Version: 2.0` | Header versioning |

### Health Checks
| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/health` | Health check status |

---

## 🏛️ Architecture

```
Web.ControllerExample/
├── Web.ControllerExample.dpr           # Standard configuration
├── Web.ControllerExample.FluentAPI.dpr # Fluent configuration
├── ControllerExample.Setup.pas         # Shared setup logic
├── ControllerExample.Controller.pas    # All controllers
├── ControllerExample.Services.pas      # Service implementations and Settings
├── Test.Web.ControllerExample.ps1      # Automated API test suite
├── WebClient.png                       # Screenshot
├── README.md                           # This file
└── WebClient/                          # React SPA frontend
```

---

## 💡 Code Examples

### Controller with DI and Configuration Injection

```pascal
[DextController('/api/greet')]
TGreetingController = class
private
  FService: IGreetingService;
  FSettings: IOptions<TMySettings>;
public
  // Dependency Injection via Constructor
  constructor Create(AService: IGreetingService; Settings: IOptions<TMySettings>);
  
  [DextGet('/config')]
  procedure GetConfig(Ctx: IHttpContext);
end;

procedure TGreetingController.GetConfig(Ctx: IHttpContext);
begin
  // Access strongly-typed settings
  var Msg := FSettings.Value.Message;
  Ctx.Response.Json(Format('{"message": "%s"}', [Msg]));
end;
```

---

## 📦 Prerequisites

- Delphi 11+ (Alexandria or later)
- Dext Framework in Library Path
- Node.js 18+ (for WebClient only)

---

## 📄 License

This example is part of the Dext Framework and is licensed under the Apache License 2.0.

---

*Modern Web Development for Delphi! 🚀*
