# 🚀 Dext Framework - V1.0 Beta Plan

This document consolidates the work plan for the **Beta V1.0** phase. The goal is to ensure that all implemented features are documented, tested, and ready for production use.

> ⚠️ **Document Under Construction**: This roadmap is being actively updated. We are performing a complete audit of the source code and discovering already implemented functionalities that were not documented. New features may be added or moved between categories at any time.

### 🗺️ Detailed Roadmaps (Spec & Tracking)
For technical details and granular status of each module, check:
- [**Web Framework Roadmap**](../Roadmap/web-roadmap.md) (HTTP Abstractions, MVC, SignalR)
- [**ORM Roadmap**](../Roadmap/orm-roadmap.md) (Dialects, Type System, Performance)
- [**Infra & CLI Roadmap**](../Roadmap/infra-roadmap.md) (Hosting, DI, Logging)

---

## 📋 1. Feature Inventory (Feature Set)

### 🌐 Dext.Web
| Feature | Status | Notes |
|---------|--------|-------|
| **Minimal APIs** (`MapGet`, `MapPost`) | ✅ Ready | Tested in `Web.Dext.Starter.Admin` |
| **Controllers** (`TController`) | ✅ Ready | Tested in `Web.ControllerExample` |
| **Model Binding** (JSON Body -> Record/Class) | ✅ Ready | Support for nesting and lists |
| **Dependency Injection** (Scoped/Singleton/Transient) | ✅ Ready | Full integration with HttpContext |
| **Middleware Pipeline** | ✅ Ready | Custom Middlewares supported |
| **Static Files** | ✅ Ready | MIME types support and caching |
| **Cookies** | ✅ Ready | Read/Write with security options |
| **Multipart/Form-Data** | ✅ Ready | File upload via `IFormFile` |
| **Response Compression** | ✅ Ready | Native GZip |
| **HTTPS/SSL** | 🟡 Needs Validation | Testing required for all supported versions (OpenSSL 1.0.2, 1.1, 3.0) and Taurus TLS integration |
| **CORS** | ✅ Ready | Middleware with Policy Builder |
| **Rate Limiting** | ✅ Ready | Token Bucket & Fixed Window |
| **Health Checks** | ✅ Ready | Extensible `/health` endpoint |
| **API Versioning** | ✅ Ready | Via URL, Header, or Query String |
| **OpenAPI / Swagger** | ✅ Ready | Automatic documentation generation |
| **Stream Responses** | ✅ Ready | `Response.Write(TStream)` |
| **Response Caching** | ✅ Ready | `[ResponseCache]` header control |
| **Filters Pipeline** | ✅ Ready | Action & Result Filters (`LogAction`, `RequireHeader`) |
| **JWT Authentication** | ✅ Ready | Token Generation and Validation (HS256) |
| **Validation** | ✅ Ready | Validation library with Attributes (`[Required]`, `[Email]`) |
| **Options Pattern** | ✅ Ready | Configuration binding to classes (`IOptions<T>`) |
| **Zero Alloc HTTP Context** | ✅ Ready | HTTP Server/Context with zero allocations and on-demand consumption |
| **Database as API** | ✅ Ready | Zero-code REST endpoints from entities (`TDataApiHandler<T>.Map`) with filtering, pagination, security policies |
| **RegisterForDisposal** | ✅ Ready | Host-lifetime object management via `IApplicationBuilder.RegisterForDisposal` |

### 🛠️ Dext.Web Middlewares (Built-in)
| Middleware | Class | Function |
|------------|-------|----------|
| **Exception Handler** | `TExceptionHandlerMiddleware` | Captures global exceptions and returns JSON/ProblemDetails or error page. |
| **HTTP Logging** | `THttpLoggingMiddleware` | Logs requests, responses, headers, and body (configurable). |
| **CORS** | `TCorsMiddleware` | Manages Cross-Origin Resource Sharing with flexible policies. |
| **Rate Limiting** | `TRateLimitMiddleware` | Limits requests by IP, route, or custom key (Token Bucket, Fixed Window). |
| **Static Files** | `TStaticFileMiddleware` | Serves static files with MIME type negotiation. |
| **Multi-Tenancy** | `TMultiTenancyMiddleware` | Resolves current Tenant and populates the context. |
| **Startup Lock** | `TStartupLockMiddleware` | Returns 503 if the application is in startup/migration state. |
| **Compression** | `TCompressionMiddleware` | Compresses responses (GZip) if supported by the client. |

### 🗄️ Dext.Entity (ORM)
| Feature | Status | Notes |
|---------|--------|-------|
| **CRUD Operations** (Add, Update, Remove, Find) | ✅ Ready | Basic functional |
| **Fluent Query API** (`Where`, `OrderBy`, `Take`) | ✅ Ready | Robust SQL translation |
| **Smart Properties** (`u.Age > 18`) | ✅ Ready | Type-safe expressions, IntelliSense, SQL Generation |
| **Relationships** (1:1, 1:N) | ✅ Ready | `Include` (Eager Loading) functional |
| **Attributes Mapping** (`[Table]`, `[Column]`) | ✅ Ready | |
| **Migrations** (CLI & Runtime) | ✅ Ready | `migrate:up`, `down`, `generate` |
| **Multi-Tenancy** | ✅ Ready | Schema-based, DB-based, Column-based |
| **Advanced Types** (UUID, JSON, Arrays) | ✅ Ready | Automatic serialization |
| **Bulk Operations** | ✅ Ready | Bulk Update/Delete |
| **Advanced Querying** | 🟡 Partial | `Join` and `GroupBy` (In-Memory ✅, SQL Pending ⚠️) |
| **Inheritance Mapping** (TPH) | ✅ Ready | Discriminator column supported |
| **Lazy Loading** | ✅ Ready | `Lazy<T>`, `IList<T>`, and `ILazy<T>` wrapper |
| **Scaffolding** (DB First) | ✅ Ready | Entity generation from Database Schema |
| **Soft Delete** | ✅ Ready | `[SoftDelete]` attribute |
| **Optimistic Concurrency** | ✅ Ready | `[Version]` attribute |
| **FireDAC Phys Driver** | ✅ Ready | Physical driver for transparent integration with FireDAC |
| **Field Mapping** | ✅ Ready | Field-based mapping (in addition to properties) to avoid triggering setters when loading from database |


### ⚙️ Infrastructure & CLI
| Feature | Status | Notes |
|---------|--------|-------|
| **CLI Tool** (`dext.exe`) | ✅ Ready | Migrations management |
| **Async Tasks** (`TAsyncTask`) | ✅ Ready | Modern concurrency primitives |
| **Logging** (`ILogger`) | ✅ Ready | Log abstraction |
| **Configuration** (`IConfiguration`) | ✅ Ready | JSON file provider |
| **Binary JSON Parser** | ✅ Ready | High-performance binary JSON parser |
| **AutoMapper** (`TMapper`) | ✅ Ready | DTO ↔ Entity mapping with RTTI, custom member mapping and collections |
| **Custom Constructors (DI)** | ✅ Ready | `[ServiceConstructor]` attribute to specify which constructor to use in DI |
| **TypeInfo Cache** | ✅ Ready | Cache RTTI metadata for performance optimization |

### 🧪 Dext.Testing
|Feature | Status | Notes |
|---------|--------|-------|
| **Mocking Engine** (`Mock<T>`) | ✅ Ready | Dynamic proxies via `TVirtualInterface` |
| **Class Mocking** (`Mock<TClass>`) | ✅ Ready | Virtual method interception |
| **Auto-Mocking** (`TAutoMocker`) | ✅ Ready | Automatic dependency injection for tests |
| **Snapshot Testing** (`MatchSnapshot`) | ✅ Ready | JSON/String snapshot verification |
| **Fluent Assertions** (`Should`) | ✅ Ready | Expressive assertion library |
| **Global Syntax** (`Should()`) | ✅ Ready | Clean API for tests |
| **Soft Asserts** (`Assert.Multiple`) | ✅ Ready | Failure aggregation for multiple checks |
| **Live Dashboard** | ✅ Ready | Real-time Web UI with Timeline & History |
| **Integration** | ✅ Ready | Works with Dext DI and Core types |
| **Attribute-Based Runner** ⭐ NEW | ✅ Ready | `[TestFixture]`, `[Test]`, `[Setup]`, `[TearDown]` - NUnit/xUnit style |
| **Assembly Lifecycle Hooks** ⭐ NEW | ✅ Ready | `[AssemblyInitialize]`, `[AssemblyCleanup]` |
| **Parameterized Tests** | ✅ Ready | `[TestCase(1, 2, 3)]` data-driven testing |
| **ITestContext Injection** ⭐ NEW | ✅ Ready | Runtime info: `Context.CurrentTest`, `Context.WriteLine` |
| **JUnit XML Report** | ✅ Ready | CI integration (Jenkins, GitHub Actions, GitLab CI) |
| **xUnit XML Report** ⭐ NEW | ✅ Ready | .NET ecosystem compatibility |
| **TRX Report** ⭐ NEW | ✅ Ready | Azure DevOps / Visual Studio integration |
| **HTML Report** ⭐ NEW | ✅ Ready | Beautiful standalone dark-themed report |
| **JSON Report** | ✅ Ready | Custom tooling integration |
| **SonarQube Report** | ✅ Ready | Quality gates integration |
| **Fluent API** | ✅ Ready | `TTest.Configure.Verbose.ExportToHtml('report.html').Run` |

### 🔄 Hosting & Lifecycle
| Feature | Status | Notes |
|---------|--------|-------|
| **Application State** (`IAppStateObserver`) | ✅ Ready | States: Starting, Seeding, Running, Stopping |
| **Graceful Shutdown** (`IHostApplicationLifetime`) | ✅ Ready | Tokens for `Started`, `Stopping`, `Stopped` |
| **Background Services** (`IHostedService`) | ✅ Ready | Asynchronous background tasks with DI |
| **Startup Lock** (`TStartupLockMiddleware`) | ✅ Ready | Blocks requests with 503 during boot |

---

## 📚 2. Documentation and Examples Plan

The focus now is to create **one example for each feature** and unify the documentation.

### Documentation
- [ ] **Create "The Dext Book"**: A single Markdown file (or Wiki) compiling all guides (similar to Laravel/Django documentation).
- [ ] **API Reference**: Generate automatic code documentation (PasDoc or similar) if possible, or focus on practical guides.

### New Examples Needed
1.  **Dext.Examples.Streaming**: Demonstrate large file download and upload (Stream Writing + Multipart).
2.  **Dext.Examples.MultiTenancy**: Demonstrate complete SaaS implementation (Schema per Tenant).
3.  **Dext.Examples.ComplexQuerying**: Demonstrate advanced ORM queries with JSON, Arrays, and reports.

### Updating Existing Examples
- [ ] Update `Web.TaskFlowAPI` to use new Cookie and Compression features.
- [ ] Review `Web.Dext.Starter.Admin` to ensure use of current best practices.

---

## 🛠️ 3. Code Quality & Maintenance

- [ ] **Installation Automation**: Automate framework installation/setup (possibly exploring Boss and TMS Smart Setup).
- [ ] **Package Versioning Strategy**: Improve installation and versioning of packages using `LIBSUFFIX AUTO` or fixed suffixes per IDE version to support side-by-side installations.
- [ ] **Generics Optimization**: Review intensive use of Generics to avoid "code bloat" and improve compilation time.
- [ ] **General Code Review**: Focused review on consistency, memory leaks, and unhandled exceptions.
- [ ] **Formatting & Style**: Standardize alignment and formatting (Object Pascal Style Guide).
- [ ] **Agent Guidelines**: Create technical documentation (`.agent/rules.md` or `CONTRIBUTING_AI.md`) detailing design patterns, architecture rules, and instructions for configuring/guiding AI agents in Dext development.

---

## 🧪 4. Testing Strategy

### Database Support Matrix
Implement integration tests running the ORM test suite against Docker containers for each database.

| Database | Dialect Implemented? | Automated Tests? | Status |
|----------|----------------------|------------------|--------|
| **SQLite** | ✅ Yes | ✅ Yes | 🟢 Stable |
| **PostgreSQL** | ✅ Yes | ✅ Yes | 🟢 Stable |
| **SQL Server** | ✅ Yes | ✅ Yes | 🟢 Stable |
| **Firebird** | ✅ Yes | ✅ Yes | 🟢 Stable |
| **MySQL / MariaDB** | ✅ Yes | ❌ No (Manual) | 🟡 Beta (Needs Validation) |
| **Oracle** | ✅ Yes | ❌ No (Manual) | 🟡 Beta (Needs Validation) |
| **InterBase** | ✅ Yes | ❌ No (Manual) | 🟡 Beta (Needs Validation) |

> **Immediate Action**: Create `Docker-Compose` environment to spin up all databases and unified test script.

### Web Test Plan
- [ ] Create HTTP integration tests (run real server and make real requests) to validate:
    - Cookie persistence/reading.
    - Binary file uploads.
    - Compression (verify Content-Encoding header).
    - Concurrency (Apache Bench / k6).

---

## 🚀 5. Benchmarks

Establish a performance baseline for V1.

1.  **Web Framework (Requests/sec)**:
    -   Hello World (Plain Text).
    -   JSON Serialization (Small and large objects).
    -   DB Read (1 simple query).
    -   *Tool*: `wrk` or `k6`.
    -   *Comparison*: vs DataSnap, vs Horse (if applicable), vs ASP.NET Core (as a target reference).

2.  **ORM (Op/sec)**:
    -   Bulk Insert (10k records).
    -   Select with Hydration (10k records).
    -   *Comparison*: vs Pure FireDAC.

---

## 🔮 6. Future Roadmap (Post-V1)

Features moved to v1.1 or v2.0:
- **MediatR Pattern**: Implementation of the Mediator pattern for CQRS (Command/Query Responsibility Segregation), facilitating separation of business logic and handlers. Support for `IRequest<TResponse>`, `IRequestHandler<TRequest, TResponse>`, and pipeline behaviors for validation, logging, and transactions.
- **WebSockets**: Native support for bidirectional real-time communication (needed for Dext Forum).
- **Server-Sent Events (SSE)**: Lightweight alternative to WebSockets for unidirectional pushes.
- **Background Jobs/Queues**: Robust queue system for asynchronous processing (Redis/RabbitMQ integration).
- **Scheduled Jobs (CRON)**: Scheduling recurrent tasks (e.g., daily reports, data cleanup).

- **Developer Experience (DevX)**:
  - **CLI REST Runner**: Support for executing `.http` / `.rest` files via `dext.exe run-requests file.http`. Compatibility with the VS Code/IntelliJ format.
  - **Fluent REST Client**: High-performance HTTP Client with a fluent API (`Client.Get('url').WithHeader('x', 'y').RetrieveJson<T>`). It will serve as the engine for the CLI Runner and integration tests.
  - **IDE Integration**: Future plugin to run requests directly from the Delphi IDE editor.

- **Docker Tooling**: `Dockerfile` templates and `dext docker init` command to simplify deployment. (Priority)
- **Telemetry & Observability**: Support for OpenTelemetry (Tracing/Metrics) and native dashboards.
- **Advanced Resilience**: Retry, Circuit Breaker, and Timeout patterns in Async API.
- **CancellationToken Timeout**: Automatic timeout support in `CancellationToken` for async operations (`CancellationToken.WithTimeout(Duration)`).
- **Immutable Data Structures**: `ImmutableList<T>`, `ImmutableDictionary<K,V>`, and `Nullable<T>` (ReadOnly) for thread-safe concurrency (Scalability).
- **Kestrel NativeAOT**: High-performance driver via bridge with .NET (Experimental).
- **View Engine**: Integration with **WebStencils** (new Delphi engine) or creation of Razor-like syntax.
- **Server Adapters**: Support deployment in **WebBroker** (ISAPI/Apache/IIS) in addition to Indy.
- **Native Integration**: Explore optional integration with **LiveBindings** for RAD scenarios and adapters for **DataSnap**.
- **JSON Columns (JSONB Support)**: Real ORM support implementation for `[JsonColumn]` attribute.
- **NoSQL Support** (MongoDB in ORM).
- **Distributed Caching** (Redis implementation - In Progress).
- **SQL Statement Caching**: Cache generated SQL strings for specifications to skip generation overhead (Compiled Queries).
- **SNI / Virtual Hosts**: Support for multiple domains and certificates on the same IP (Taurus TLS).

## 🖥️ IDE Support
- **Debug Visualizers**: Create custom visualizers for Dext types (Prop<T>, Smart Types) to show easy-to-read values (e.g., 'Text' instead of internal record structure) during debugging.
