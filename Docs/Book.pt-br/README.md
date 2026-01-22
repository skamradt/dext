# 📚 O Livro do Dext

> Um guia completo para construir aplicações web modernas com o Framework Dext para Delphi.

---

## Links Rápidos

- [Primeiros Passos](01-primeiros-passos/README.md) - Instalação & Hello World
- [Framework Web](02-framework-web/README.md) - Minimal APIs & Controllers
- [ORM](05-orm/README.md) - Acesso a banco de dados

---

## Sumário

### Parte I: Fundamentos

#### [1. Primeiros Passos](01-primeiros-passos/README.md)
- [Instalação](01-primeiros-passos/instalacao.md)
- [Hello World](01-primeiros-passos/hello-world.md)
- [Estrutura do Projeto](01-primeiros-passos/estrutura-projeto.md)

#### [2. Framework Web](02-framework-web/README.md)
- [Minimal APIs](02-framework-web/minimal-apis.md) - Handlers com `MapGet`, `MapPost`
- [Controllers](02-framework-web/controllers.md) - Controllers estilo MVC
- [Model Binding](02-framework-web/model-binding.md) - JSON/Form para objetos
- [Rotas](02-framework-web/rotas.md) - Padrões de URL & parâmetros
- [Middleware](02-framework-web/middleware.md) - Pipeline de requisições

#### [3. Autenticação & Segurança](03-autenticacao/README.md)
- [Autenticação JWT](03-autenticacao/jwt-auth.md) - Auth baseada em tokens
- [Claims Builder](03-autenticacao/claims-builder.md) - Claims de usuário

#### [4. Recursos da API](04-recursos-api/README.md)
- [OpenAPI / Swagger](04-recursos-api/openapi-swagger.md) - Documentação da API
- [Rate Limiting](04-recursos-api/rate-limiting.md) - Limitação de requisições
- [CORS](04-recursos-api/cors.md) - Requisições cross-origin
- [Cache de Resposta](04-recursos-api/cache.md) - Headers de cache
- [Health Checks](04-recursos-api/health-checks.md) - Endpoint `/health`

---

### Parte II: Acesso a Dados

#### [5. ORM (Dext.Entity)](05-orm/README.md)
- [Primeiros Passos](05-orm/primeiros-passos.md) - Primeira entidade & contexto
- [Entidades & Mapeamento](05-orm/entidades.md) - `[Table]`, `[Column]`, `[PK]`
- [Consultas](05-orm/consultas.md) - `Where`, `OrderBy`, `Take`
- [Smart Properties](05-orm/smart-properties.md) - Expressões type-safe
- [Specifications](05-orm/specifications.md) - Padrões de query reutilizáveis
- [Relacionamentos](05-orm/relacionamentos.md) - 1:1, 1:N, Lazy Loading
- [Migrations](05-orm/migrations.md) - Versionamento de schema
- [Scaffolding](05-orm/scaffolding.md) - Geração de código DB-first
- [Multi-Tenancy](05-orm/multi-tenancy.md) - Isolamento por Schema/DB/Coluna

#### [6. Database as API](06-database-as-api/README.md)
- [CRUD Sem Código](06-database-as-api/crud-zero-codigo.md) - REST a partir de entidades

---

### Parte III: Recursos Avançados

#### [7. Comunicação em Tempo Real](07-tempo-real/README.md)
- [Hubs (SignalR)](07-tempo-real/hubs-signalr.md) - Mensagens WebSocket

#### [8. Testes](08-testes/README.md)
- [Mocking](08-testes/mocking.md) - `Mock<T>` e verificação
- [Assertions](08-testes/assertions.md) - Sintaxe fluente `Should()`
- [Snapshots](08-testes/snapshots.md) - Teste de snapshot JSON

#### [9. Ferramenta CLI](09-cli/README.md)
- [Comandos](09-cli/comandos.md) - Visão geral do CLI `dext`
- [Migrations](09-cli/migrations.md) - `migrate:up`, `migrate:down`
- [Scaffolding](09-cli/scaffolding.md) - `dext scaffold`
- [Testes](09-cli/testes.md) - `dext test --coverage`
- [Dashboard](09-cli/dashboard.md) - `dext ui`

#### [10. Tópicos Avançados](10-avancado/README.md)
- [Injeção de Dependência](10-avancado/injecao-dependencia.md)
- [Background Services](10-avancado/background-services.md)
- [Configuração](10-avancado/configuracao.md) - `IOptions<T>`
- [API Assíncrona](10-avancado/async-api.md) - `TAsyncTask`

#### [11. Desktop UI (Dext.UI)](11-desktop-ui/README.md) ⭐ NOVO
- [Navigator Framework](11-desktop-ui/navigator.md) - Navegação Push/Pop com middlewares
- [Magic Binding](11-desktop-ui/magic-binding.md) - Binding declarativo de UI
- [Padrões MVVM](11-desktop-ui/mvvm-patterns.md) - Guia de arquitetura

#### [12. Networking (Dext.Net)](12-networking/rest-client.md) ⭐ NOVO
- [Cliente REST](12-networking/rest-client.md) - Cliente HTTP Fluente

---

### Apêndice

- [Referência do Sistema de Tipos](apendice/sistema-tipos.md)
- [Dialetos de Banco de Dados](apendice/dialetos.md)
- [Solução de Problemas](apendice/solucao-problemas.md)

---

## Exemplos

Cada capítulo referencia exemplos funcionais do diretório `Examples/`:

| Exemplo | Tópicos |
|---------|---------|
| [Web.MinimalAPI](../../Examples/Web.MinimalAPI/) | Minimal APIs, Rotas |
| [Web.ControllerExample](../../Examples/Web.ControllerExample/) | Controllers, DI |
| [Web.JwtAuthDemo](../../Examples/Web.JwtAuthDemo/) | JWT, Autenticação |
| [Web.SwaggerExample](../../Examples/Web.SwaggerExample/) | OpenAPI, Documentação |
| [Web.RateLimitDemo](../../Examples/Web.RateLimitDemo/) | Rate Limiting |
| [Web.DatabaseAsApi](../../Examples/Web.DatabaseAsApi/) | CRUD Sem Código |
| [Web.DextStore](../../Examples/Web.DextStore/) | API E-commerce Completa |
| [Orm.EntityDemo](../../Examples/Orm.EntityDemo/) | ORM Básico |
| [Hubs](../../Examples/Hubs/) | SignalR Tempo Real |
| [Desktop.MVVM.CustomerCRUD](../../Examples/Desktop.MVVM.CustomerCRUD/) | Navigator, MVVM, Testes |


---

## Contribuindo

Encontrou um erro? Quer melhorar a documentação? Por favor, abra uma issue ou envie um PR!

---

*Última atualização: Janeiro 2026*
