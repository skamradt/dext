[ 🇧🇷 Português ](README.pt-br.md)

# Dext Framework - Modern Full-Stack Development for Delphi

> ⚠️ **Status: Active Development**
> The project is currently implementing version 1.0. The public API, fluent syntax, and method names are subject to breaking changes without prior notice until the first stable release.

**Dext** is a complete ecosystem for modern Delphi development, combining a high-performance web framework (inspired by ASP.NET Core) with a robust ORM and advanced infrastructure tooling.

The goal of Dext is to bring modern development paradigms—such as Dependency Injection, Asynchronous Programming, Fluent APIs, and Code-First—to the Delphi environment while maintaining native performance.

## 🎯 Philosophy and Goals

*   **Parity with .NET Core**: The primary goal is to reach feature parity with equivalent frameworks in the .NET ecosystem (ASP.NET Core, EF Core), staying up-to-date with platform innovations.
*   **Native Performance**: After functional stabilization of v1, the focus will shift entirely to **performance optimization**, aiming to compete with high-speed frameworks.
*   **Innovation**: While inspired by .NET, Dext is not limited to it, seeking to implement solutions that specifically make sense for the Delphi language.

---

## 📄 License

This project is licensed under the **Apache License 2.0** (the same used by .NET Core). This allows free use in commercial and open-source projects, with the security of a permissive and modern license.

---

## 🚀 Main Modules

### 🌐 Dext.Web (Web Framework)
A lightweight and powerful HTTP framework for building REST APIs and microservices.
- **Minimal APIs**: Concise fluent syntax for route definition.
- **Controllers**: Traditional class-based support for complex APIs.
- **Smart Binding**: Automatic JSON serialization and validation for Records/Classes.
- **Middlewares**: Modular and extensible request pipeline.

### 🗄️ Dext.Entity (ORM)
A modern ORM focused on productivity and performance.
- **Code-First**: Define your database using Delphi classes.
- **Migrations**: Database schema version control via CLI.
- **Fluent Query API**: Strongly typed and expressive queries.
- **Change Tracking**: Automatic change tracking and optimized persistence.
- **Multi-Database**: Support for SQL Server, PostgreSQL, Firebird, MySQL, Oracle, and SQLite.

### ⚙️ Dext.Core (Infrastructure)
The foundation of the framework, usable in any type of application.
- **Dependency Injection**: Full and fast IOC container.
- **Configuration**: Flexible configuration system (JSON, Environment Variables).
- **Logging**: Structured logging abstraction.
- **Async/Await**: Primitives for real asynchronous programming.

---

## 📚 Documentation Index

### 🚀 Getting Started
- [Framework Overview](Docs/Dext%20Web%20Framework.md)
- [Project Structure](Docs/Project%20Dext.md)
- [Minimal API - Quick Guide](Docs/Dext%20Minimal%20API.md)

### 🌐 Web API
- **Routing & Endpoints**
  - [Minimal API](Docs/MinimalAPI.md)
  - [Controllers](Docs/CONTROLLERS_IMPLEMENTATION.md)
  - [Model Binding](Docs/Dext%20Model%20Binding.md)
  - [Validation](Docs/ModelBinding.md) # (Includes validation)
- **Security & Middleware**
  - [JWT Authentication](Docs/JWT-Authentication.md)
  - [CORS](Docs/CORS.md)
  - [Rate Limiting](Docs/Rate-Limiting.md)
  - [Middlewares](Docs/Dext%20-%20Middlewares.md)
- **Advanced**
  - [Background Services](Docs/BackgroundServices.md)
  - [Action Filters](Docs/ActionFilters.md)
  - [Swagger / OpenAPI](Docs/SWAGGER.md)

### 🗄️ Data Access (ORM)
- [Comparison & Features](Docs/ORM_COMPARISON_2024.md)
- [Database Configuration](Docs/DATABASE_CONFIG.md)
- [Fluent Query API](Docs/FLUENT_QUERY_API.md)
- [Migrations](Docs/MIGRATIONS_GUIDE.md)
- [Lazy Loading](Docs/LAZY_LOADING_ADVANCED.md)
- [Bulk Operations](Docs/BULK_OPERATIONS.md)
- [Soft Delete](Docs/SOFT_DELETE.md)

### ⚙️ Core & Infrastructure
- [Dependency Injection & Scopes](Docs/ScopedServices.md)
- [Configuration & Options Pattern](Docs/OptionsPattern.md)
- [Async Programming](Docs/ASYNC_API.md)
- [Caching](Docs/Caching.md)

---

## 💻 Requirements

- **Delphi**: Recommended Delphi 10.4 Sydney or higher (due to extensive use of modern language features).
- **Indy**: Uses Indy components (already included in Delphi) for the HTTP transport layer (subject to future replacement/optimization).

## 📦 Installation and Configuration

1. **Clone the repository:**
   ```bash
   git clone https://github.com/dext-framework/dext.git
   ```

2. **Configure Library Path in Delphi:**
   Add the following paths to your project or IDE:
   - `\Sources\Core`
   - `\Sources\Core\Drivers`
   - `\Sources\Entity` (if using the ORM)

3. **Dependencies:**
   - The framework uses `FastMM5` (recommended for memory debugging).
   - Native database drivers (FireDAC, etc.) are supported.

---

## ⚡ Quick Example (Minimal API)

```pascal
program MyAPI;

uses
  Dext.Core.WebApplication,
  Dext.Http.Results;

begin
  var App := TDextApplication.Create;
  var Builder := App.GetApplicationBuilder;

  // Simple Route
  Builder.MapGetR<IResult>('/hello', 
    function: IResult
    begin
      Result := Results.Ok('{"message": "Hello Dext!"}');
    end);

  // Route with parameter and binding
  Builder.MapGetR<Integer, IResult>('/users/{id}',
    function(Id: Integer): IResult
    begin
      Result := Results.Json(Format('{"userId": %d}', [Id]));
    end);

  App.Run(8080);
end.
```

## 💎 ORM Example (Fluent Query)

Dext ORM allows expressive and strongly typed queries, eliminating magical SQL strings:

```pascal
// Complex Query with Joins and Filters
// O: TOrder (Alias/Proxy)
var Orders := DbContext.Orders
  .Where((O.Status = TOrderStatus.Paid) and (O.Total > 1000))
  .Include('Customer')       // Eager Loading
  .Include('Items')
  .OrderByDescending('Date')
  .Take(50)
  .ToList;

// High-Performance Bulk Update
DbContext.Products
  .Where(P.Category = 'Outdated') // P: TProduct
  .Update                         // Starts bulk update
  .Set('Active', False)           // Set fields
  .Execute;
```

## ⚡ Async Example (Fluent Tasks)

Forget `TThread` complexity. Use a modern API based on Promises/Tasks:

```pascal
// Asynchronous Task Chaining
var Task := TAsyncTask.Run<TUserProfile>(
  function: TUserProfile
  begin
    // Runs on background thread
    Result := ExternalApi.GetUserProfile(UserId);
  end)
  .ThenBy<Boolean>( // Transforms result (Map)
    function(Profile: TUserProfile): Boolean
    begin
      Result := Profile.IsVerified and Profile.HasCredit;
    end)
  .OnComplete( // Returns to UI Thread automatically
    procedure(IsVerified: Boolean)
    begin
      if IsVerified then
        ShowSuccess('User Verified!')
      else
        ShowError('Verification Failed');
    end)
  .Start; // Starts execution
```

## 🧪 Examples and Tests

The repository contains practical example projects:

- **`Examples/TaskFlowAPI`**: A complete REST API demonstrating layered architecture, ORM, Auth, and DI.
- **`Examples/EntityDemo`**: Demo focused on ORM features (CRUD, Migrations).
- **`Examples/WebFrameworkTests`**: Integration and stability test suite.

---

## 🗺️ Roadmaps

Follow the project development:
- [ORM Roadmap](Docs/ORM_ROADMAP.md)
- [Web Framework Roadmap](Docs/WEB_ROADMAP.md)
- [Infra & IDE Roadmap](Docs/INFRA_ROADMAP.md)

---

**Dext Framework** - *Native performance, modern productivity.*
Developed with ❤️ by the Delphi community.
