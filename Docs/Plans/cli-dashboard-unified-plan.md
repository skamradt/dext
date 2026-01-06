# 🎯 Dext CLI & Dashboard - Unified Experience Plan

## Visão Geral

Este documento define o plano estratégico para tornar o **CLI (`dext.exe`)** e o **Dashboard Web** experiências de primeira classe, sincronizadas e equivalentes em funcionalidades. O objetivo é que qualquer operação disponível em um, esteja disponível no outro.

> **Princípio Guia**: "CLI-First, Dashboard-Equal" - Todas as funcionalidades são implementadas primeiro como comandos CLI, depois expostas via Dashboard com a mesma capacidade.

---

## 📊 Status Atual

### Funcionalidades CLI Existentes
| Comando | Descrição | Dashboard? |
|---------|-----------|------------|
| `help` | Lista comandos disponíveis | ✅ (nav lateral) |
| `ui` | Inicia o Dashboard Web | N/A |
| `env scan` | Detecta instalações Delphi | ✅ Parcial |
| `config init` | Cria arquivo de configuração | ❌ |
| `test` | Executa testes do projeto | ❌ (só visualiza) |
| `test --coverage` | Executa com cobertura de código | ❌ (só visualiza) |
| `migrate:up` | Aplica migrações pendentes | ❌ |
| `migrate:down` | Reverte migrações | ❌ |
| `migrate:list` | Lista status das migrações | ❌ |
| `migrate:generate` | Gera nova migração vazia | ❌ |

### Funcionalidades Dashboard Existentes
| Funcionalidade | Descrição | CLI? |
|----------------|-----------|------|
| Projects | Lista projetos recentes | ❌ |
| Test Summary | Visualiza resultados de testes | ✅ Parcial |
| Coverage Report | Visualiza relatório HTML | ✅ (`--coverage`) |
| Settings | Configura paths e ambientes | ❌ |

---

## 🚀 Plano de Implementação

### Fase 1: Fundação CLI Completa (Prioridade Alta)

#### 1.1 Scaffolding & Code Generation
Inspirado em: .NET CLI, Laravel Artisan, Rails Generators

| Comando | Descrição | Prioridade |
|---------|-----------|------------|
| `dext new <project-type>` | Cria novo projeto a partir de template | 🔴 Alta |
| `dext make:entity <name>` | Gera classe Entity com atributos | 🔴 Alta |
| `dext make:controller <name>` | Gera Controller com endpoints CRUD | 🔴 Alta |
| `dext make:migration <name>` | Aliás para `migrate:generate` | 🟡 Média |
| `dext make:test <name>` | Gera fixture de teste | 🟡 Média |
| `dext make:middleware <name>` | Gera middleware customizado | 🟢 Baixa |
| `dext make:service <name>` | Gera interface + implementação de serviço | 🟢 Baixa |

**Templates de Projeto (`dext new`):**
- `webapi` - API REST minimalista
- `webapi-controller` - API com Controllers
- `console` - Aplicação console
- `library` - Package/Library BPL
- `fullstack` - Web API + Frontend SPA

#### 1.2 Database & ORM
| Comando | Descrição | Prioridade |
|---------|-----------|------------|
| `dext db:scaffold` | Gera Entities a partir do banco (existente) | ✅ Implementado |
| `dext db:seed` | Executa seeders para popular dados | 🔴 Alta |
| `dext db:drop` | Remove todas as tabelas (dev only) | 🟡 Média |
| `dext db:reset` | Drop + Migrate + Seed | 🟡 Média |
| `dext db:status` | Mostra status da conexão | 🟡 Média |
| `dext db:diff` | Compara schema vs entities (auto-migration) | 🟢 Baixa |

#### 1.3 Testing & Quality
| Comando | Descrição | Prioridade |
|---------|-----------|------------|
| `dext test` | Executa testes (existente) | ✅ Implementado |
| `dext test --coverage` | Com cobertura (existente) | ✅ Implementado |
| `dext test --filter <pattern>` | Filtra testes por nome | 🔴 Alta |
| `dext test --watch` | Modo watch (reexecuta em mudanças) | 🟡 Média |
| `dext test --parallel` | Execução paralela | 🟢 Baixa |
| `dext lint` | Análise estática de código | 🟢 Baixa |
| `dext format` | Formata código (Pascal Formatter) | 🟢 Baixa |

#### 1.4 Build & Run
| Comando | Descrição | Prioridade |
|---------|-----------|------------|
| `dext build` | Compila o projeto atual | 🔴 Alta |
| `dext build --release` | Compila em modo Release | 🔴 Alta |
| `dext run` | Compila e executa | 🔴 Alta |
| `dext run --port <n>` | Executa em porta específica | 🔴 Alta |
| `dext watch` | Hot reload em desenvolvimento | 🟡 Média |
| `dext clean` | Remove arquivos compilados | 🟡 Média |

#### 1.5 Project & Package Management
| Comando | Descrição | Prioridade |
|---------|-----------|------------|
| `dext init` | Inicializa projeto Dext em diretório existente | 🔴 Alta |
| `dext info` | Mostra informações do projeto | 🔴 Alta |
| `dext deps` | Lista dependências do projeto | 🟡 Média |
| `dext add <package>` | Adiciona dependência (Boss/GetIt) | 🟢 Baixa |
| `dext remove <package>` | Remove dependência | 🟢 Baixa |

#### 1.6 Environment & Configuration
| Comando | Descrição | Prioridade |
|---------|-----------|------------|
| `dext env scan` | Detecta instalações (existente) | ✅ Implementado |
| `dext env list` | Lista ambientes configurados | 🔴 Alta |
| `dext env use <version>` | Define versão padrão do Delphi | 🟡 Média |
| `dext config set <key> <value>` | Define configuração | 🟡 Média |
| `dext config get <key>` | Obtém configuração | 🟡 Média |
| `dext config list` | Lista todas as configurações | 🟡 Média |

#### 1.7 Utility Commands
| Comando | Descrição | Prioridade |
|---------|-----------|------------|
| `dext routes` | Lista todas as rotas da API | 🔴 Alta |
| `dext swagger` | Gera/Exporta OpenAPI spec | 🟡 Média |
| `dext http <file.http>` | Executa arquivo .http/.rest | 🟡 Média |
| `dext shell` | REPL interativo com contexto da app | 🟢 Baixa |
| `dext doctor` | Diagnóstico do ambiente | 🟡 Média |
| `dext upgrade` | Atualiza o CLI para última versão | 🟢 Baixa |

---

### Fase 2: Dashboard Como Portal Completo

#### 2.1 Arquitetura do Dashboard
```
┌─────────────────────────────────────────────────────────────────┐
│                     Dext Dashboard                              │
├─────────────────────────────────────────────────────────────────┤
│ ┌───────────┐ ┌───────────┐ ┌───────────┐ ┌───────────┐        │
│ │ Projects  │ │   Build   │ │   Tests   │ │ Database  │        │
│ └───────────┘ └───────────┘ └───────────┘ └───────────┘        │
│ ┌───────────┐ ┌───────────┐ ┌───────────┐ ┌───────────┐        │
│ │   Logs    │ │  Swagger  │ │  Routes   │ │ Settings  │        │
│ └───────────┘ └───────────┘ └───────────┘ └───────────┘        │
└─────────────────────────────────────────────────────────────────┘
              ▲
              │ REST API (JSON)
              ▼
┌─────────────────────────────────────────────────────────────────┐
│                   CLI Core / Business Logic                     │
└─────────────────────────────────────────────────────────────────┘
```

#### 2.2 Módulos do Dashboard
| Módulo | Funcionalidades | Status |
|--------|-----------------|--------|
| **Projects** | Lista, abre, cria projetos | 🟡 Parcial |
| **Build** | Compilar, limpar, executar | ❌ Novo |
| **Tests** | Executar, visualizar, filtrar | 🟡 Parcial |
| **Database** | Migrations, scaffold, REPL SQL | ❌ Novo |
| **Logs** | Stream de logs em tempo real | ❌ Novo |
| **Swagger** | Visualizar/Testar API | ❌ Novo |
| **Routes** | Lista visual de rotas | ❌ Novo |
| **Settings** | Configuração global | ✅ Existente |
| **Terminal** | Terminal integrado | ❌ Novo |

#### 2.3 APIs do Dashboard (Backend)
Cada funcionalidade do CLI deve expor uma API REST:

```
GET    /api/projects              # Lista projetos
POST   /api/projects              # Cria projeto
GET    /api/projects/:id          # Detalhes do projeto
DELETE /api/projects/:id          # Remove projeto

POST   /api/build                 # Compila projeto
POST   /api/run                   # Compila e executa
POST   /api/clean                 # Limpa build

GET    /api/tests                 # Lista testes
POST   /api/tests/run             # Executa testes
GET    /api/tests/results         # Resultados
GET    /api/tests/coverage        # Cobertura

GET    /api/migrations            # Lista migrações
POST   /api/migrations/up         # Aplica migrações
POST   /api/migrations/down       # Reverte
POST   /api/migrations/generate   # Gera nova

GET    /api/routes                # Lista rotas
GET    /api/swagger               # OpenAPI spec
GET    /api/logs                  # Stream de logs (SSE)
GET    /api/config                # Configurações
PUT    /api/config                # Atualiza config
GET    /api/env                   # Lista ambientes
POST   /api/env/scan              # Escaneia ambientes
```

---

### Fase 3: Features Avançadas

#### 3.1 Developer Experience
| Feature | Descrição | Inspiração |
|---------|-----------|------------|
| **Auto-completion** | Sugestões de comandos (Bash/ZSH/PowerShell) | .NET CLI |
| **Interactive Mode** | Wizards guiados para comandos complexos | Rails |
| **Rich Output** | Cores, spinners, progress bars | Laravel |
| **Config Profiles** | Múltiplos perfis de configuração | Docker |
| **Aliases** | Atalhos customizados para comandos | Git |

#### 3.2 DevOps & CI/CD
| Comando | Descrição | Prioridade |
|---------|-----------|------------|
| `dext docker init` | Gera Dockerfile + docker-compose | 🔴 Alta |
| `dext docker build` | Constrói imagem Docker | 🟡 Média |
| `dext ci init` | Gera config de CI (GitHub Actions, GitLab) | 🟡 Média |
| `dext publish` | Publica pacote | 🟢 Baixa |
| `dext deploy <env>` | Deploy para ambiente | 🟢 Baixa |

#### 3.3 Telemetry & Monitoring
| Feature | Descrição |
|---------|-----------|
| **Health Dashboard** | Visualização de health checks |
| **Metrics** | Gráficos de performance em tempo real |
| **Tracing** | Visualização de traces (OpenTelemetry) |
| **Logs Viewer** | Agregação e busca de logs |

---

## 📋 Comparativo com Outros Frameworks

### .NET CLI
| Feature | .NET | Dext (Atual) | Dext (Planejado) |
|---------|------|--------------|------------------|
| Project templates | ✅ `dotnet new` | ❌ | ✅ `dext new` |
| Scaffolding | ✅ `dotnet ef scaffold` | ✅ `db:scaffold` | ✅ |
| Build | ✅ `dotnet build` | ❌ | ✅ `dext build` |
| Run | ✅ `dotnet run` | ❌ | ✅ `dext run` |
| Watch | ✅ `dotnet watch` | ❌ | ✅ `dext watch` |
| Test | ✅ `dotnet test` | ✅ | ✅ |
| Migrations | ✅ `dotnet ef migrations` | ✅ | ✅ |
| Package mgmt | ✅ `dotnet add` | ❌ | 🟡 `dext add` |

### Laravel Artisan
| Feature | Laravel | Dext (Atual) | Dext (Planejado) |
|---------|---------|--------------|------------------|
| Generators | ✅ `make:*` | ❌ | ✅ `make:*` |
| Serve | ✅ `serve` | ❌ | ✅ `run` |
| REPL | ✅ `tinker` | ❌ | 🟡 `shell` |
| Routes list | ✅ `route:list` | ❌ | ✅ `routes` |
| Migrations | ✅ `migrate` | ✅ | ✅ |
| Seeders | ✅ `db:seed` | ❌ | ✅ `db:seed` |
| Queue | ✅ `queue:*` | ❌ | 🟢 Futuro |

### Rails CLI
| Feature | Rails | Dext (Atual) | Dext (Planejado) |
|---------|-------|--------------|------------------|
| Scaffold | ✅ `scaffold` | ❌ | ✅ Full CRUD |
| Console | ✅ `console` | ❌ | 🟡 `shell` |
| Server | ✅ `server` | ❌ | ✅ `run` |
| Generators | ✅ `generate` | ❌ | ✅ `make:*` |
| Destroy | ✅ `destroy` | ❌ | 🟢 Futuro |
| Routes | ✅ `routes` | ❌ | ✅ `routes` |
| DB tasks | ✅ `db:*` | 🟡 | ✅ `db:*` |

---

## 🏗️ Arquitetura Técnica

### Padrão de Comandos CLI
```pascal
type
  IConsoleCommand = interface
    function GetName: string;
    function GetDescription: string;
    function GetUsage: string;        // NOVO
    function GetExamples: TArray<string>; // NOVO
    procedure Execute(const Args: TCommandLineArgs);
  end;
```

### Padrão de API (Dashboard)
```pascal
type
  ICommandApi = interface
    function Execute(const Request: TApiRequest): TApiResponse;
    function GetCommandName: string;
  end;
```

### Sincronização CLI <-> Dashboard
```
┌────────────────┐
│   Dashboard    │
│   (Frontend)   │
└───────┬────────┘
        │ HTTP/REST
        ▼
┌────────────────┐
│  API Handler   │
│  (Middleware)  │
└───────┬────────┘
        │ Delega
        ▼
┌────────────────┐
│ IConsoleCommand│
│    (Core)      │
└────────────────┘
```

---

## 📈 Roadmap de Implementação

### Q1 2026 - Foundation
- [ ] Refatorar estrutura de comandos CLI
- [ ] Implementar `dext new` com templates
- [ ] Implementar `dext build` / `dext run`
- [ ] Implementar `dext routes`
- [ ] Expor APIs para Dashboard

### Q2 2026 - Scaffolding
- [ ] Implementar `dext make:*` (entity, controller, test)
- [ ] Implementar `dext db:seed`
- [ ] Dashboard: módulo Build
- [ ] Dashboard: módulo Routes
- [ ] Dashboard: módulo Database

### Q3 2026 - DevX
- [ ] Implementar `dext watch`
- [ ] Implementar `dext test --filter`
- [ ] Dashboard: Terminal integrado
- [ ] Dashboard: Logs em tempo real
- [ ] Auto-completion para shells

### Q4 2026 - DevOps
- [ ] Implementar `dext docker init`
- [ ] Implementar `dext ci init`
- [ ] Dashboard: Health/Metrics
- [ ] Dashboard: Swagger integrado

---

## 📁 Estrutura de Pastas Proposta

```
Sources/
├── Hosting/
│   └── CLI/
│       ├── Commands/
│       │   ├── Build/          # build, run, clean, watch
│       │   ├── Database/       # migrate:*, db:*
│       │   ├── Generate/       # make:*, new
│       │   ├── Test/           # test
│       │   ├── Project/        # init, info, routes
│       │   └── Config/         # config, env
│       ├── Templates/          # Templates para scaffolding
│       │   ├── webapi/
│       │   ├── console/
│       │   └── library/
│       ├── API/                # API handlers para Dashboard
│       └── Core/               # Infraestrutura CLI
└── Dashboard/
    └── wwwroot/
        ├── index.html
        ├── app.js
        └── styles.css
```

---

## ✅ Critérios de Sucesso

1. **Paridade de Funcionalidades**: 100% das operações disponíveis em ambas interfaces
2. **Performance**: CLI deve responder em < 100ms para comandos simples
3. **Documentação**: `dext help <command>` com exemplos para cada comando
4. **Testabilidade**: Comandos testáveis de forma isolada
5. **Extensibilidade**: Sistema de plugins para comandos customizados

---

## 🔗 Documentos Relacionados

- [CLI Documentation](../cli.md)
- [V1 Beta Roadmap](../Releases/v1-beta-roadmap.md)
- [Infrastructure Roadmap](../Roadmap/infra-roadmap.md)
- [Web Roadmap](../Roadmap/web-roadmap.md)

---

*Última atualização: 06 de Janeiro de 2026*
