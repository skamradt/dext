# 🚀 Dext Admin Starter Kit

A professional, full-stack "SaaS Admin" template built with **Delphi (Dext)** and **Modern Web Technologies**.

> **Use Case:** Perfect for developers migrating from VCL/IntraWeb who want a modern look without the complexity of Node.js/Webpack build chains.

---

## ✨ Features

*   **Logic in Delphi**: Backend API, Database, and Routing all handled by Dext.
*   **Modern Frontend**: Uses **Tailwind CSS** for styling and **HTMX** for dynamic interactions.
*   **Zero Build Tools**: No `npm install`, no `webpack`. Just plain HTML files served by Delphi.
*   **Authentication**: Complete Login flow with JWT-based auth.
*   **Dashboard**: Interactive charts (Chart.js) and real-time stats.
*   **CRUD**: Complete Customer management example.
*   **API Documentation**: Auto-generated Swagger/OpenAPI documentation at `/swagger`.
*   **Error Handling**: Global exception handler with structured error responses.

---

## 🛠️ Technology Stack

*   **Backend**: Dext Framework (Minimal APIs + Service Layer + Dext.Entity ORM)
*   **Database**: SQLite (Zero config file `dext_admin.db`) - Easily switchable to PostgreSQL
*   **Frontend**: 
    *   **HTMX**: For SPA-like navigation without writing JavaScript.
    *   **Tailwind CSS**: For utility-first styling (via CDN).
    *   **Alpine.js**: For minimal client-side interactivity (Sidebar toggle).
    *   **Chart.js**: For data visualization.

---

## 🏗️ Architecture Overview

The application follows a modular, feature-based architecture (Vertical Slice Architecture), keeping related logic together.

```
Dext.Starter.Admin/
├── AppStartup.pas          # DI & Middleware Configuration
├── AppResponseConsts.pas   # Centralized HTML/JSON Templates
├── Domain/                 # Entities & Core Business Rules
│   ├── Entities/           # Database Entities (ORM)
│   ├── DbContext.pas       # Database Context
│   └── DbSeeder.pas        # Database Seeding Logic
├── Features/               # Vertical Slices (Endpoints + Services + DTOs)
│   ├── Auth/               # Login & JWT Logic
│   ├── Customers/          # Customer CRUD
│   ├── Dashboard/          # Stats & Charts
│   ├── Settings/           # User Profile
│   └── Shared/             # Shared utilities and middleware
└── wwwroot/                # Static files (CSS, JS, HTML views)
```

---

## 🚀 Getting Started

### Prerequisites
- Delphi 11+ (Alexandria or later recommended)
- Dext Framework installed and configured in Library Path

### Running the Application

1.  **Open Project**: Open `Dext.Starter.Admin.dpr` in Delphi.
2.  **Build**: Compile the project (Console Application).
3.  **Run**: Execute the binary. It will start a web server at `http://localhost:8080`.
    *   *Note*: The first run will automatically create the SQLite database and seed it with demo data.
4.  **Login**:
    *   **Username**: `admin`
    *   **Password**: `admin`
5.  **Explore**:
    *   **Dashboard**: `http://localhost:8080/dashboard`
    *   **Swagger UI**: `http://localhost:8080/swagger`

---

## 📡 API Endpoints

### Authentication
| Method | Endpoint | Description | Auth Required |
|--------|----------|-------------|---------------|
| GET | `/auth/login` | Login page (HTML) | No |
| POST | `/auth/login` | Authenticate user | No |
| POST | `/auth/logout` | Logout user | Yes |

### Dashboard
| Method | Endpoint | Description | Auth Required |
|--------|----------|-------------|---------------|
| GET | `/dashboard` | Dashboard page (HTML) | Yes |
| GET | `/dashboard/stats` | Get dashboard statistics | Yes |
| GET | `/dashboard/chart` | Get chart data | Yes |

### Customers
| Method | Endpoint | Description | Auth Required |
|--------|----------|-------------|---------------|
| GET | `/customers` | Customer list page (HTML) | Yes |
| GET | `/customers/list` | Get all customers (JSON) | Yes |
| POST | `/customers` | Create new customer | Yes |
| PUT | `/customers/{id}` | Update customer | Yes |
| DELETE | `/customers/{id}` | Delete customer | Yes |

### Settings
| Method | Endpoint | Description | Auth Required |
|--------|----------|-------------|---------------|
| GET | `/settings` | Settings page (HTML) | Yes |
| GET | `/settings/profile` | Get user profile | Yes |
| PUT | `/settings/profile` | Update user profile | Yes |

---

## 🔑 Key Concepts

### 1. Minimal API (Endpoints)
Instead of Controllers, we use **Minimal APIs** (`MapGet`, `MapPost`) defined in static `Map` methods within each Feature.

**Example (`Customer.Endpoints.pas`):**
```delphi
class procedure TCustomerEndpoints.Map(App: TDextAppBuilder);
begin
  App.MapGet<ICustomerService, IHttpContext>('/customers/',
    procedure(Service: ICustomerService; Context: IHttpContext)
    begin
       // Use Generic Parameter Injection!
       var Data := Service.GetAll;
       // Return Response...
    end);
end;
```

### 2. Service Layer Pattern
Business logic is decoupled from Endpoints using Services (`ICustomerService`, `IDashboardService`).
- **Endpoint**: Parses Request -> Calls Service -> Formats Response (HTML/JSON).
- **Service**: Business Logic -> Database Access (`TAppDbContext`).

**Example (`Customer.Service.pas`):**
```delphi
function TCustomerService.GetAll: IList<TCustomer>;
begin
  Result := FDb.Entities<TCustomer>.ToList;
end;
```

### 3. HTMX & Server-Side Rendering
The UI is dynamic but rendered on the server.
- **HTMX Attributes** (`hx-get`, `hx-target`) in HTML trigger partial updates.
- **Endpoints** return HTML snippets (defined in `AppResponseConsts.pas`) instead of full pages or JSON.

### 4. Exception Handling
Global exception handler middleware (`UseExceptionHandler`) catches all unhandled exceptions and returns:
- **JSON** error response for API endpoints
- **HTML** error page for browser requests

### 5. JWT Authentication
- Tokens are generated on login and validated on each request
- Protected endpoints automatically check for valid JWT
- Custom middleware (`TAdminAuthMiddleware`) handles authorization

---

## 🔧 Configuration

### Switching Database Provider

Edit `AppStartup.pas` line 83:

```delphi
const
  DB_PROVIDER = 'SQLITE'; // Change to 'POSTGRES' for PostgreSQL
```

For PostgreSQL, update connection string (lines 89-94):
```delphi
Options.ConnectionString := 
  'Server=localhost;' +
  'Port=5432;' +
  'Database=dext_admin;' +
  'User_Name=postgres;' +
  'Password=postgres;';
```

### Changing JWT Secret

Update the secret key in `AppStartup.pas` (lines 73 and 166):
```delphi
'dext-admin-secret-key-change-in-production-2024'
```

⚠️ **Important**: Always use a strong, unique secret in production!

---

## 🛠️ Adding New Features

To add a new feature (e.g., "Orders"):

1.  **Define Entities**: Create `TOrder` in `Domain\Entities`.
2.  **Create Service**: Define `IOrderService` and implement it using `TAppDbContext`.
3.  **Create DTOs**: Define request/response DTOs in `Features\Orders\Order.Dto.pas`.
4.  **Create Endpoints**: Create `TOrderEndpoints` injecting `IOrderService`.
5.  **Register DI**: Add the service to `AppStartup.ConfigureServices`.
6.  **Wire Up**: Call `TOrderEndpoints.Map(WebApp)` in `AppStartup.Configure`.

---

## 🧪 Testing

### Manual Testing
1. Use the web interface at `http://localhost:8080`
2. Test API endpoints using Swagger UI at `http://localhost:8080/swagger`

### Load Testing
Use the included PowerShell scripts:
```powershell
# Run load test
.\load_test.ps1

# Run load test only (no compilation)
.\load_test_only.ps1
```

---

## 🐛 Troubleshooting

### Database Issues
**Problem**: "Database is locked" error
**Solution**: Ensure WAL mode is enabled (default in this project). Check `AppStartup.pas` line 106.

**Problem**: Database file not found
**Solution**: The database is created automatically on first run. Ensure write permissions in the application directory.

### Authentication Issues
**Problem**: "Invalid token" error
**Solution**: Check that the JWT secret matches between token generation and validation.

**Problem**: Can't access protected endpoints
**Solution**: Ensure you're sending the JWT token in the `Authorization` header: `Bearer <token>`

### Port Already in Use
**Problem**: "Address already in use" error
**Solution**: Change the port in `Dext.Starter.Admin.dpr` line 54:
```delphi
App.Run(8080); // Change to another port, e.g., 8081
```

---

## 📚 Further Reading

- [Dext Framework Documentation](../../README.md)
- [Dext ORM Guide](../../Docs/orm-guide.md)
- [Dext Web Framework Guide](../../Docs/web-guide.md)
- [Portuguese Version](README.pt-br.md)

---

## 📄 License

This example is part of the Dext Framework and is licensed under the Apache License 2.0.

