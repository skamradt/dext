# 📚 The Dext Book

> A comprehensive guide to building modern web applications with the Dext Framework for Delphi.

---

## Quick Links

- [Getting Started](01-getting-started/README.md) - Installation & Hello World
- [Web Framework](02-web-framework/README.md) - Minimal APIs & Controllers
- [ORM](05-orm/README.md) - Database access & queries

---

## Table of Contents

### Part I: Foundations

#### [1. Getting Started](01-getting-started/README.md)
- [Installation](01-getting-started/installation.md)
- [Hello World](01-getting-started/hello-world.md)
- [Project Structure](01-getting-started/project-structure.md)

#### [2. Web Framework](02-web-framework/README.md)
- [Minimal APIs](02-web-framework/minimal-apis.md) - Route handlers with `MapGet`, `MapPost`
- [Controllers](02-web-framework/controllers.md) - MVC-style controllers
- [Model Binding](02-web-framework/model-binding.md) - JSON/Form to objects
- [Routing](02-web-framework/routing.md) - URL patterns & parameters
- [Middleware](02-web-framework/middleware.md) - Request pipeline

#### [3. Authentication & Security](03-authentication/README.md)
- [JWT Authentication](03-authentication/jwt-auth.md) - Token-based auth
- [Claims Builder](03-authentication/claims-builder.md) - User claims

#### [4. API Features](04-api-features/README.md)
- [OpenAPI / Swagger](04-api-features/openapi-swagger.md) - API documentation
- [Rate Limiting](04-api-features/rate-limiting.md) - Request throttling
- [CORS](04-api-features/cors.md) - Cross-origin requests
- [Response Caching](04-api-features/caching.md) - Cache headers
- [Health Checks](04-api-features/health-checks.md) - `/health` endpoint

---

### Part II: Data Access

#### [5. ORM (Dext.Entity)](05-orm/README.md)
- [Getting Started](05-orm/getting-started.md) - First entity & context
- [Entities & Mapping](05-orm/entities.md) - `[Table]`, `[Column]`, `[PK]`
- [Querying](05-orm/querying.md) - `Where`, `OrderBy`, `Take`
- [Smart Properties](05-orm/smart-properties.md) - Type-safe expressions
- [Specifications](05-orm/specifications.md) - Reusable query patterns
- [Relationships](05-orm/relationships.md) - 1:1, 1:N, Lazy Loading
- [Migrations](05-orm/migrations.md) - Schema versioning
- [Scaffolding](05-orm/scaffolding.md) - DB-first code generation
- [Multi-Tenancy](05-orm/multi-tenancy.md) - Schema/DB/Column isolation

#### [6. Database as API](06-database-as-api/README.md)
- [Zero-Code CRUD](06-database-as-api/zero-code-crud.md) - REST from entities

---

### Part III: Advanced Features

#### [7. Real-Time Communication](07-real-time/README.md)
- [Hubs (SignalR)](07-real-time/hubs-signalr.md) - WebSocket messaging

#### [8. Testing](08-testing/README.md)
- [Mocking](08-testing/mocking.md) - `Mock<T>` and verification
- [Assertions](08-testing/assertions.md) - Fluent `Should()` syntax
- [Snapshots](08-testing/snapshots.md) - JSON snapshot testing

#### [9. CLI Tool](09-cli/README.md)
- [Commands](09-cli/commands.md) - `dext` CLI overview
- [Migrations](09-cli/migrations.md) - `migrate:up`, `migrate:down`
- [Scaffolding](09-cli/scaffolding.md) - `dext scaffold`
- [Testing](09-cli/testing.md) - `dext test --coverage`
- [Dashboard](09-cli/dashboard.md) - `dext ui`

#### [10. Advanced Topics](10-advanced/README.md)
- [Dependency Injection](10-advanced/dependency-injection.md)
- [Background Services](10-advanced/background-services.md)
- [Configuration](10-advanced/configuration.md) - `IOptions<T>`
- [Async API](10-advanced/async-api.md) - `TAsyncTask`

---

### Appendix

- [Type System Reference](appendix/type-system.md)
- [Database Dialects](appendix/dialects.md)
- [Troubleshooting](appendix/troubleshooting.md)

---

## Examples

Each chapter references working examples from the `Examples/` directory:

| Example | Topics |
|---------|--------|
| [Web.MinimalAPI](../../Examples/Web.MinimalAPI/) | Minimal APIs, Routing |
| [Web.ControllerExample](../../Examples/Web.ControllerExample/) | Controllers, DI |
| [Web.JwtAuthDemo](../../Examples/Web.JwtAuthDemo/) | JWT, Authentication |
| [Web.SwaggerExample](../../Examples/Web.SwaggerExample/) | OpenAPI, Documentation |
| [Web.RateLimitDemo](../../Examples/Web.RateLimitDemo/) | Rate Limiting |
| [Web.DatabaseAsApi](../../Examples/Web.DatabaseAsApi/) | Zero-Code CRUD |
| [Web.DextStore](../../Examples/Web.DextStore/) | Full E-commerce API |
| [Orm.EntityDemo](../../Examples/Orm.EntityDemo/) | ORM Basics |
| [Hubs](../../Examples/Hubs/) | Real-Time SignalR |

---

## Contributing

Found an error? Want to improve the docs? Please open an issue or submit a PR!

---

*Last updated: January 2026*
