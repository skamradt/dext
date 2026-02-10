# Web.HelpDesk (Issue Tracker API)

Backend de um sistema de **Help Desk / Issue Tracker** construido com o **Dext Framework** usando **Minimal API**.

## Tema & Regras de Negocio

| Regra | Descricao |
|-------|-----------|
| **SLA por Prioridade** | Critical: 4h, High: 24h, Medium: 48h, Low: 72h |
| **State Machine** | New -> Open -> InProgress -> Resolved -> Closed (ou Rejected) |
| **Controle de Acesso** | Somente Agents/Admins podem: atribuir, resolver, e postar notas internas |
| **Metricas** | Total abertos, overdue, tempo medio de resolucao |
| **JSON Columns** | Tags (array) e CustomFields (object) em Tickets; Metadata em Users |

## Arquitetura

```
Web.HelpDesk/
+-- Server/
|   +-- Web.HelpDesk.dpr           # Entry point
|   +-- Web.HelpDesk.dproj         # Projeto Delphi
|   +-- HelpDesk.Startup.pas       # DI, Middleware, Swagger
|   +-- HelpDesk.Endpoints.pas     # Minimal API routes
+-- Domain/
|   +-- HelpDesk.Domain.Enums.pas  # TTicketStatus, TTicketPriority, TUserRole, TTicketChannel
|   +-- HelpDesk.Domain.Entities.pas # TUser, TTicket, TComment (Smart Properties)
|   +-- HelpDesk.Domain.Models.pas # DTOs (Request/Response)
+-- Data/
|   +-- HelpDesk.Data.Context.pas  # THelpDeskContext (DbContext)
|   +-- HelpDesk.Data.Seeder.pas   # Dados iniciais (Admin, Agent, Customer, Tickets)
+-- Services/
|   +-- HelpDesk.Services.pas      # ITicketService, IUserService
+-- Tests/
|   +-- Web.HelpDesk.Tests.dpr     # Test Runner
|   +-- Web.HelpDesk.Tests.dproj   # Projeto de Testes
|   +-- HelpDesk.Tests.Entities.pas # Testes de SLA e IsOverdue
|   +-- HelpDesk.Tests.Services.pas # Testes com Mock<IUserService>
```

## Endpoints

| Metodo | Rota | Descricao |
|--------|------|-----------|
| POST   | `/api/auth/login` | Login (retorna mock token) |
| POST   | `/api/auth/register` | Registro de usuario |
| GET    | `/api/tickets` | Listar meus tickets |
| POST   | `/api/tickets` | Criar ticket |
| GET    | `/api/tickets/{id}` | Detalhe do ticket |
| POST   | `/api/tickets/{id}/status` | Atualizar status |
| POST   | `/api/tickets/{id}/assign` | Atribuir a um agente |
| POST   | `/api/tickets/{id}/comments` | Adicionar comentario |
| GET    | `/api/metrics` | Dashboard de metricas |

## Como Executar

1. Compile o framework (`build_framework.bat`)
2. Abra `Server/Web.HelpDesk.dproj` no Delphi
3. Compile e execute (F9)
4. Acesse `http://localhost:9005/swagger`

## Testes

1. Abra `Tests/Web.HelpDesk.Tests.dproj` no Delphi
2. Compile e execute (F9)

## Features do Dext Demonstradas

- **Minimal API**: Definicao funcional de rotas (`MapGet`, `MapPost`)
- **Smart Properties**: Queries type-safe (`TTicket.Props.Status = tsOpen`)
- **JSON Columns**: `[JsonColumn]` para Tags e CustomFields
- **ORM**: `TDbContext`, `IDbSet<T>`, `EnsureCreated`
- **Validacao**: `[Required]`, `[MaxLength]` em DTOs
- **Mocking**: `Mock<IUserService>` com `Arg.Any<Integer>`
- **Memory Management**: `OwnsObjects=False` em colecoes de entidades
