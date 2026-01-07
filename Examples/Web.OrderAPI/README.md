# Web.OrderAPI (Migration Example)

A complete reference implementation demonstrating how to build a REST API with Dext Framework, specifically designed as a migration guide from DelphiMVC Framework (DMVC).

This project simulates a **Restaurant Order System**, featuring Categories, Products, Tables, and Orders.

## 🚀 Features

*   **Modern Architecture**: Uses the `IStartup` pattern for clean initialization (similar to ASP.NET Core).
*   **MVC Controllers**: Attribute-based routing (`[DextRoute]`, `[DextGet]`) and dependency injection.
*   **Swagger/OpenAPI 3.0**: Automatic documentation generation with `[SwaggerOperation]`, `[SwaggerResponse]`.
*   **Entity Framework**: SQLite database integration using `Dext.Entity` (Dext ORM).
*   **Dependency Injection**: Full usage of `AddTransient`, `AddScoped` for Services and Repositories.
*   **Testing**: Includes a comprehensive PowerShell integration test script.

## 🛠️ Project Structure

*   `Web.OrderAPI.dpr` - Main Entry Point (Thin wrapper).
*   `OrderAPI.Startup.pas` - Application Configuration (DI, Middleware, Database Seeding).
*   `OrderAPI.Controllers.pas` - API Endpoints.
*   `OrderAPI.Services.pas` - Business Logic.
*   `OrderAPI.Entities.pas` - ORM Models.
*   `OrderAPI.Database.pas` - Database Context.

## 📋 API Endpoints

| Method | Endpoint | Description |
| :--- | :--- | :--- |
| **GET** | `/api/categories` | List all categories |
| **GET** | `/api/products` | List all products |
| **GET** | `/api/tables` | List tables |
| **GET** | `/api/tables/available` | List available tables |
| **POST** | `/api/orders` | Open a new order |
| **POST** | `/api/orders/{id}/items` | Add item to order |
| **POST** | `/api/orders/{id}/close` | Close/Pay order |
| **GET** | `/api/swagger` | **Swagger UI Documentation** |

## ⚙️ How to Run

1.  **Open Project**: Open `Web.OrderAPI.dpr` in Delphi (Alexandria 11 or newer recommended).
2.  **Compile**: Build the project. Ensure Dext sources are in the Search Path.
3.  **Run**: Execute the application.
    *   Server starts at `http://localhost:5000`
    *   **Authentication**: Basic Auth (User: `admin`, Pass: `admin`)
    *   Database `orderapi.db` is automatically created and seeded.

## 🧪 Testing

A complete integration test suite is provided in PowerShell. It verifies the entire flow, including database persistence and order logic.

```powershell
.\Test.Web.OrderAPI.ps1
```

**Expected Output:**
```text
  [PASS] GET /api/categories
  [PASS] POST /api/orders
  ...
  All tests PASSED!
```

## 📚 Migration Notes

This project follows the patterns described in the **[DMVC to Dext Migration Guide](../../Docs/Migration/dmvc-to-dext.md)**.

**Key differences implemented here:**
1.  **Startup Class**: instead of configuring everything in `dpr` or `WebModule`.
2.  **Wildcard Units**: Usage of `Dext`, `Dext.Web`, and `Dext.Entity` which provide all common types and attributes in a single import.
3.  **Fluent API**: Middleware registration using `App.Builder.Use...` (e.g. `UseBasicAuthentication`, `UseSwagger`).
4.  **Controller Injection**: Services injected via Constructor automatically.
5.  **Response Helper**: Using `Ctx.Response.Json()` instead of `Render()`.
6.  **Async/Await** (Conceptual): Dext handles requests in a thread pool automatically.
7.  **Smart Properties**: Using `Prop<T>` and its aliases (`StringType`, `IntType`, etc.) for type-safe database queries.
