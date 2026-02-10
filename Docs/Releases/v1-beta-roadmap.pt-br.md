# 🚀 Dext Framework - V1.0 Beta Plan

Este documento consolida o plano de trabalho para a fase **Beta V1.0**. O objetivo é garantir que todas as funcionalidades implementadas estejam documentadas, testadas e prontas para uso em produção.

> ⚠️ **Documento em Construção**: Este roteiro está sendo atualizado ativamente. Estamos realizando uma auditoria completa no código-fonte e descobrindo funcionalidades já implementadas que não estavam documentadas. Novas features podem ser adicionadas ou movidas de categoria a qualquer momento.

### 🗺️ Roadmaps Detalhados (Spec & Tracking)
Para detalhes técnicos e status granular de cada módulo, consulte:
- [**Web Framework Roadmap**](../Roadmap/web-roadmap.md) (Abstrações HTTP, MVC, SignalR)
- [**ORM Roadmap**](../Roadmap/orm-roadmap.md) (Dialetos, Type System, Performance)
- [**Infra & CLI Roadmap**](../Roadmap/infra-roadmap.md) (Hosting, DI, Logging)

---

## 📋 1. Inventário de Funcionalidades (Feature Set)

### 🌐 Dext.Web
| Feature | Status | Notas |
|---------|--------|-------|
| **Minimal APIs** (`MapGet`, `MapPost`) | ✅ Pronto | Testado em `Web.Dext.Starter.Admin` |
| **Controllers** ([ApiController] / POCO) | ✅ Pronto | Paridade funcional com ASP.NET Core. Sem necessidade de classe base. |
| **Model Binding** (JSON Body -> Record/Class) | ✅ Pronto | Suporte a aninhamento, listas e **Binding Misto** (Header/Query/Route/Body) |
| **Dependency Injection** (Scoped/Singleton/Transient) | ✅ Pronto | Integração total com HttpContext |
| **Middleware Pipeline** | ✅ Pronto | Custom Middlewares suportados |
| **Static Files** | ✅ Pronto | Suporte a MIME types e cache |
| **Cookies** | ✅ Pronto | Leitura/Escrita com opções de segurança |
| **Multipart/Form-Data** | ✅ Pronto | Upload de arquivos via `IFormFile` |
| **Response Compression** | ✅ Pronto | GZip nativo |
| **HTTPS/SSL** | 🟡 Precisa de Validação | Testes necessários para todas as versões (OpenSSL 1.0.2, 1.1, 3.0) e integração Taurus TLS |
| **CORS** | ✅ Pronto | Middleware com Policy Builder |
| **Rate Limiting** | ✅ Pronto | Token Bucket & Fixed Window |
| **Health Checks** | ✅ Pronto | Endpoint `/health` extensível |
| **API Versioning** | ✅ Pronto | Via URL, Header ou Query String |
| **OpenAPI / Swagger** | ✅ Pronto | Geração automática de documentação |
| **Stream Responses** | ✅ Pronto | `Response.Write(TStream)` |
| **Response Caching** | ✅ Pronto | `[ResponseCache]` header control |
| **Filters Pipeline** | ✅ Pronto | Action & Result Filters (`LogAction`, `RequireHeader`) |
| **JWT Authentication** | ✅ Pronto | Geração e Validação de Tokens (HS256) |
| **Validation** | ✅ Pronto | Library de validação com Atributos (`[Required]`, `[Email]`) |
| **Options Pattern** | ✅ Pronto | Binding de configuração para classes (`IOptions<T>`) |
| **Zero Alloc HTTP Context** | ✅ Pronto | HTTP Server/Context com zero allocations e consumo sob demanda |
| **Database as API** | ✅ Pronto | REST endpoints zero-code a partir de entities (`TDataApiHandler<T>.Map`) com filtros, paginação, security policies |
| **RegisterForDisposal** | ✅ Pronto | Gerenciamento de lifecycle de objetos via `IApplicationBuilder.RegisterForDisposal` |
| **Real-Time Hubs** ⭐ NOVO | ✅ Pronto | Comunicação em tempo real compatível com SignalR (`Dext.Web.Hubs`) - Grupos, Clients, Broadcast |

### 🛠️ Dext.Web Middlewares (Built-in)
| Middleware | Classe | Função |
|------------|--------|--------|
| **Exception Handler** | `TExceptionHandlerMiddleware` | Captura exceções globais e retorna JSON/ProblemDetails ou página de erro. |
| **HTTP Logging** | `THttpLoggingMiddleware` | Loga requisições, respostas, headers e body (configurável). |
| **CORS** | `TCorsMiddleware` | Gerencia Cross-Origin Resource Sharing com policies flexíveis. |
| **Rate Limiting** | `TRateLimitMiddleware` | Limita requisições por IP, rota ou chave customizada (Token Bucket, Fixed Window). |
| **Static Files** | `TStaticFileMiddleware` | Serve arquivos estáticos com negociação de MIME types. |
| **Multi-Tenancy** | `TMultiTenancyMiddleware` | Resolve o Tenant atual e popula o contexto. |
| **Startup Lock** | `TStartupLockMiddleware` | Retorna 503 se a aplicação estiver em estado de inicialização/migração. |
| **Compression** | `TCompressionMiddleware` | Comprime respostas (GZip) se suportado pelo cliente. |

### 🗄️ Dext.Entity (ORM)
| Feature | Status | Notas |
|---------|--------|-------|
| **CRUD Operations** (Add, Update, Remove, Find) | ✅ Pronto | Básico funcional |
| **Fluent Query API** (`Where`, `OrderBy`, `Take`) | ✅ Pronto | Tradução SQL robusta |
| **Smart Properties** (`u.Age > 18`) | ✅ Pronto | Expressões type-safe, IntelliSense, Geração SQL |
| **Relationships** (1:1, 1:N) | ✅ Pronto | `Include` (Eager Loading) funcional |
| **Attributes Mapping** (`[Table]`, `[Column]`) | ✅ Pronto | |
| **Migrations** (CLI & Runtime) | ✅ Pronto | `migrate:up`, `down`, `generate` |
| **Multi-Tenancy** | ✅ Pronto | Schema-based, DB-based, Column-based |
| **Advanced Types** (UUID, JSON, Arrays) | ✅ Pronto | Serialização automática |
| **Bulk Operations** | ✅ Pronto | Update/Delete em massa |
| **Advanced Querying** | ✅ Pronto | `Join` e `GroupBy` (Full SQL Support) |
| **Inheritance Mapping** (TPH) | ✅ Pronto | Discriminator column suportado |
| **Lazy Loading** | ✅ Pronto | `Lazy<T>`, `IList<T>` e `ILazy<T>` wrapper |
| **Scaffolding** (DB First) | ✅ Pronto | Geração de Entities via Schema do Banco |
| **Soft Delete** | ✅ Pronto | Atributo `[SoftDelete]` |
| **Optimistic Concurrency** | ✅ Pronto | Atributo `[Version]` |
| **FireDAC Phys Driver** | ✅ Pronto | Driver físico para integração transparente com FireDAC |
| **Auto-Detecção de Dialeto** | ✅ Pronto | Identificação determinística via Enum (`ddPostgreSQL`, etc) |
| **Field Mapping** | ✅ Pronto | Mapeamento por fields (além de properties) para evitar disparar setters ao carregar do banco |


### ⚙️ Infraestrutura & CLI
| Feature | Status | Notas |
|---------|--------|-------|
| **CLI Tool** (`dext.exe`) | ✅ Pronto | Dashboard UI Web, Gerenciamento de Ambientes, Migrations e Test Runner |
| **Test Results Dashboard** | ✅ Pronto | Visualização de cobertura e relatórios HTML integrada ao `dext ui` |
| **Async Tasks** (`TAsyncTask`) | ✅ Pronto | Primitivas modernas de concorrência |
| **Logging** (`ILogger`) | ✅ Pronto | Abstração de log |
| **Configuration** (`IConfiguration`) | ✅ Pronto | JSON file provider |
| **Binary JSON Parser** | ✅ Pronto | Parser JSON binário de alta performance |
| **AutoMapper** (`TMapper`) | ✅ Pronto | Mapeamento DTO ↔ Entity com RTTI, custom member mapping e collections |
| **Zero-Leak Record Facades** | ✅ Pronto | Uso de Records para `TDextServices` e Builders para eliminar vazamentos de memória (heap capture) |
| **TypeInfo Cache** | ✅ Pronto | Cache de metadados RTTI para otimização de performance |

### 🧪 Dext.Testing
| Feature | Status | Notas |
|---------|--------|-------|
| **Mocking Engine** (`Mock<T>`) | ✅ Pronto | Proxies dinâmicos via `TVirtualInterface` |
| **Class Mocking** (`Mock<TClass>`) | ✅ Pronto | Interceptação de métodos virtuais |
| **Auto-Mocking** (`TAutoMocker`) | ✅ Pronto | Injeção de dependência automática p/ testes |
| **Snapshot Testing** (`MatchSnapshot`) | ✅ Pronto | Verificação de snapshots JSON/String |
| **Fluent Assertions** (`Should`) | ✅ Pronto | Biblioteca de asserções expressiva |
| **Sintaxe Global** (`Should()`) | ✅ Pronto | API limpa para testes |
| **Code Coverage** | ✅ Pronto | Geração automática via `dext test --coverage` |
| **Integração** | ✅ Pronto | Funciona com Dext DI e Core types |

### 🔄 Hosting & Lifecycle
| Feature | Status | Notas |
|---------|--------|-------|
| **Application State** (`IAppStateObserver`) | ✅ Pronto | Estados: Starting, Seeding, Running, Stopping |
| **Graceful Shutdown** (`IHostApplicationLifetime`) | ✅ Pronto | Tokens para `Started`, `Stopping`, `Stopped` |
| **Background Services** (`IHostedService`) | ✅ Pronto | Tarefas assíncronas em background com DI |
| **Startup Lock** (`TStartupLockMiddleware`) | ✅ Pronto | Bloqueia requests com 503 durante o boot |

---

## 📚 2. Plano de Documentação e Exemplos

O foco agora é criar **um exemplo para cada funcionalidade** e unificar a documentação.

### Documentação
- [x] **Criar "The Dext Book"**: Documentação multi-arquivo abrangente cobrindo todos os aspectos do framework. [English](../../Docs/Book/README.md) | [Português](../../Docs/Book.pt-br/README.md)
- [x] **API Reference**: Geração automática de documentação utilizando **DextDoc** (Custom Node.js Generator + Mermaid.js).

### Novos Exemplos Necessários
- [x] **Dext.Examples.Streaming**: Demonstrar download e upload de arquivos grandes (Stream Writing + Multipart). ✅ Pronto (Testes Pendentes)
- [x] **Dext.Examples.MultiTenancy**: Demonstrar implementação completa de SaaS (Schema por Tenant). ✅ Pronto (Testes Pendentes)
- [x] **Dext.Examples.ComplexQuerying**: Demonstrar queries avançadas do ORM com JSON, Arrays e relatórios. ✅ Pronto (Testes Pendentes)

### Atualização de Exemplos Existentes
- [x] Atualizar `Web.TaskFlowAPI` para usar os novos recursos de Cookies e Compression. ✅ Pronto (Testes Pendentes)
- [x] Revisar `Web.Dext.Starter.Admin` para garantir uso das melhores práticas atuais. ✅ Pronto (Testes Pendentes)

---

## 🛠️ 3. Qualidade de Código & Manutenção

- [ ] **Automação de Instalação**: Automatizar a instalação/setup do framework (possivelmente explorando Boss e TMS Smart Setup).
- [ ] **Estratégia de Versionamento de Pacotes**: Melhorar a instalação e versionamento dos packages com `LIBSUFFIX AUTO` ou fixo por versão da IDE para permitir instalações lado a lado.
- [ ] **Otimização de Generics**: Revisar uso intensivo de Generics para evitar "code bloat" e melhorar tempo de compilação.
- [ ] **Code Review Geral**: Revisão focada em consistência, vazamento de memória e exceções não tratadas.
- [ ] **Formatação & Estilo**: Padronizar alinhamento e formatação (Object Pascal Style Guide).
- [ ] **Guia do Agente (Agent Guidelines)**: Criar documentação técnica (`.agent/rules.md` ou `CONTRIBUTING_AI.md`) detalhando padrões de projeto, regras de arquitetura e instruções para configurar/orientar agentes de IA no desenvolvimento do Dext.

---

## 🧪 4. Estratégia de Testes

### Matriz de Suporte a Bancos de Dados
Implementar testes de integração rodando a suite de testes do ORM contra containers Docker de cada banco.

| Banco de Dados | Dialeto Implementado? | Testes Automatizados? | Status |
|----------------|-----------------------|-----------------------|--------|
| **SQLite** | ✅ Sim | ✅ Sim | 🟢 Estável |
| **PostgreSQL** | ✅ Sim | ✅ Sim | 🟢 Estável |
| **SQL Server** | ✅ Sim | ✅ Sim | 🟢 Estável |
| **Firebird** | ✅ Sim | ✅ Sim | 🟢 Estável |
| **MySQL / MariaDB** | ✅ Sim | ✅ Sim | 🟢 Estável |
| **Oracle** | ✅ Sim | ❌ Não (Manual) | 🟡 Beta (Precisa de Validação) |
| **InterBase** | ✅ Sim | ❌ Não (Manual) | 🟡 Beta (Precisa de Validação) |

> **Ação Imediata**: Criar `Docker-Compose` environment para subir todos os bancos e script de teste unificado.

### Plano de Testes de Web
- [ ] Criar testes de integração HTTP (rodar servidor real e fazer requests reais) para validar:
    - Cookies persistência/leitura.
    - Upload de arquivos binários.
    - Compressão (verificar Content-Encoding header).
    - Concorrência (Apache Bench / k6).

---

## 🚀 5. Benchmarks

Estabelecer uma baseline de performance para a V1.

1.  **Web Framework (Requests/sec)**:
    -   Hello World (Plain Text).
    -   JSON Serialization (Objeto pequeno e grande).
    -   DB Read (1 query simples).
    -   *Ferramenta*: `wrk` ou `k6`.
    -   *Comparativo*: vs DataSnap, vs Horse (se aplicável), vs ASP.NET Core (como referência de alvo).

2.  **ORM (Op/sec)**:
    -   Bulk Insert (10k registros).
    -   Select com Hydration (10k registros).
    -   *Comparativo*: vs FireDAC puro.

---

## 🔮 6. Roadmap Futuro (Pós-V1)

Funcionalidades movidas para v1.1 ou v2.0:
- **MediatR Pattern**: Implementação do padrão Mediator para CQRS (Command/Query Responsibility Segregation), facilitando a separação de lógica de negócio e handlers. Suporte a `IRequest<TResponse>`, `IRequestHandler<TRequest, TResponse>`, e pipeline behaviors para validação, logging e transações.
- **WebSockets**: Suporte nativo para comunicação bidirecional em tempo real (necessário para Dext Forum).
- **Server-Sent Events (SSE)**: Alternativa leve a WebSockets para pushes unidirecionais.
- **Background Jobs/Queues**: Sistema de filas para processamento assíncrono robusto (integração Redis/RabbitMQ).
- **Scheduled Jobs (CRON)**: Agendamento de tarefas recorrentes (ex: relatórios diários, limpeza de dados).

- **Experiência do Desenvolvedor (DevX)**:
  - **CLI REST Runner**: ✅ IMPLEMENTADO - Suporte para parsing e execução de arquivos `.http` / `.rest`. Parser (`THttpRequestParser`) e Executor (`THttpExecutor`) prontos.
  - ~~**REST Client Fluente**~~: ✅ IMPLEMENTADO - Cliente HTTP de alta performance com API fluente (`TRestClient`).
  - **Integração IDE**: Plugin futuro para executar requests diretamente do editor da IDE Delphi.

- **Docker Tooling**: Templates de `Dockerfile` e comando `dext docker init` para facilitar o deployment. (Prioritário)
- **Telemetry & Observability**: Suporte a OpenTelemetry (Tracing/Metrics) e dashboards nativos.
- **Advanced Resilience**: Patterns de Retry, Circuit Breaker e Timeout na Async API.
- **CancellationToken Timeout**: Suporte a timeout automático em `CancellationToken` para operações assíncronas (`CancellationToken.WithTimeout(Duration)`).
- **Immutable Data Structures**: `ImmutableList<T>`, `ImmutableDictionary<K,V>` e `Nullable<T>` (ReadOnly) para concorrência segura (Scalability).
- **Kestrel NativeAOT**: Driver de alta performance via ponte com .NET (Experimental).
- **View Engine**: Integração com **WebStencils** (novo engine do Delphi) ou criação de sintaxe Razor-like.
- **Server Adapters**: Suportar deployment em **WebBroker** (ISAPI/Apache/IIS) além do Indy.
- **Native Integration**: Explorar integração opcional com **LiveBindings** para cenários RAD e adapters para **DataSnap**.
- **JSON Columns (JSONB Support)**: Implementação do suporte real no ORM para o atributo `[JsonColumn]`.
- **Suporte a NoSQL** (MongoDB no ORM).
- **Distributed Caching** (Redis implementation - Em Progresso).
- **Cache de Instruções SQL**: Cache de strings SQL geradas para specifications para pular overhead de geração (Compiled Queries).
- [ ] **Feature Toggle**: Sistema de gerenciamento de features (flags) para habilitar/desabilitar funcionalidades dinamicamente.
- [ ] **SNI / Virtual Hosts**: Suporte a múltiplos domínios e certificados no mesmo IP (Taurus TLS).
