# 🎯 Dext Dashboard V2 - Plano de Implementação

## Visão

Transformar o **Dext Dashboard** em uma **plataforma de desenvolvimento visual** que espelha e estende todas as funcionalidades do CLI, permitindo ao desenvolvedor trabalhar com a **IDE Delphi em um monitor** e o **Dashboard em outro**, executando comandos e visualizando resultados em tempo real.

> **Princípio:** O Dashboard NÃO substitui o CLI - ele o complementa visualmente e o executa via Actions.

---

## 📊 Inventário: O Que Já Existe

### CLI Commands (Implementados)

| Comando | Arquivo | Descrição | Status |
|---------|---------|-----------|--------|
| `migrate:up` | `Commands.MigrateUp.pas` | Aplica migrações pendentes | ✅ |
| `migrate:down` | `Commands.MigrateDown.pas` | Reverte migrações | ✅ |
| `migrate:list` | `Commands.MigrateList.pas` | Lista status das migrações | ✅ |
| `migrate:generate` | `Commands.MigrateGenerate.pas` | Gera nova migração | ✅ |
| `test` | `Commands.Test.pas` | Executa testes | ✅ |
| `test --coverage` | `Commands.Test.pas` | Testes + Code Coverage | ✅ |
| `config init` | `Commands.Configuration.pas` | Cria dext.json | ✅ |
| `env scan` | `Commands.Configuration.pas` | Detecta Delphis instalados | ✅ |
| `ui` | `Commands.UI.pas` | Inicia Dashboard | ✅ |

### Scaffolding (Existente - Precisa Integrar)

| Componente | Arquivo | Descrição | Status |
|------------|---------|-----------|--------|
| Schema Provider | `Dext.Entity.Scaffolding.pas` | Lê schema do banco (tabelas, colunas, FKs) | ✅ Core pronto |
| Entity Generator | `Dext.Entity.Scaffolding.pas` | Gera código Delphi (atributos ou fluent) | ✅ Core pronto |
| Tool CLI | `Dext.Tool.Scaffolding.CLI.pas` | CLI standalone (`dext-gen scaffold`) | ⚠️ Separado |

**Comandos a integrar no CLI principal:**

| Novo Comando | Descrição | Baseado em |
|--------------|-----------|------------|
| `db:scaffold` | Gera entities a partir do banco | `Dext.Entity.Scaffolding` |
| `db:scaffold --table <name>` | Scaffold de tabela específica | - |
| `db:scaffold --style attributes` | Usa atributos (default) | `msAttributes` |
| `db:scaffold --style fluent` | Usa fluent mapping | `msFluent` |
| `make:entity <name>` | Gera entity vazia com template | Novo |
| `make:controller <name>` | Gera controller CRUD | Novo |

### Facade Generator (Standalone - Precisa Integrar)

| Componente | Arquivo | Descrição | Status |
|------------|---------|-----------|--------|
| Generator Core | `Tools/DextFacadeGenerator` | Gera `Dext.Uses.inc` e `Dext.Aliases.inc` via AST | ✅ |
| CLI Tool | `DextFacadeGenerator.exe` | Executável separado | ⚠️ Separado |

**Comandos a integrar:**

| Novo Comando | Descrição |
|--------------|-----------|
| `dev:facades` | Regenera facades do framework (uso interno/avançado) |
| `dev:facades --watch` | Monitora mudanças e regenera |

### Dashboard APIs (Implementadas)

| Endpoint | Método | Descrição | Status |
|----------|--------|-----------|--------|
| `/api/test/summary` | GET | Sumário de testes (último resultado) | ✅ |
| `/api/projects` | GET | Lista projetos registrados | ✅ |
| `/api/config` | GET | Lê configuração global | ✅ |
| `/api/config` | POST | Salva configuração | ✅ |
| `/api/env/scan` | POST | Escaneia ambientes Delphi | ✅ |
| `/api/env/versions` | GET | Lista versões detectadas | ✅ |
| `/reports/*` | GET | Serve relatórios de coverage | ✅ |

### Dashboard Frontend (Atual - HTML/JS embarcado)

- **Home/Projects**: Lista projetos recentes
- **Test Summary**: Exibe últimos resultados
- **Coverage Report**: Exibe HTML de coverage (iframe)
- **Settings**: Configura ambientes Delphi

---

## 🚀 Plano de Implementação

### Fase 0: Preparação (1-2 dias)
- [x] Documentar todas as APIs existentes formalmente
- [x] Criar estrutura de projeto Vue.js em `/Sources/Dashboard/vue-app`
- [x] Configurar Vite para build integrado
- [x] Preservar funcionalidades existentes como fallback

### Fase 1: Dashboard Core com Vue.js (1 semana)

#### 1.1 Estrutura de Componentes
- [x] Setup Vue Router + Pinia + Tailwind 4
- [x] Layout Principal (Sidebar, Status Indicator)
- [x] HomeView (Projects List com API real/mock)
- [x] SettingsView (Configurações com API real)
- [x] Integração Dext.Hubs Client

#### 1.2 Design System (baseado na imagem)
- [x] Tema "Cyberpunk Industrial" (Slate/Neon Green) configurado no `style.css`

#### 1.3 Integração com Dext.Hubs
```typescript
// useHub.ts
import { DextHubConnection } from './dext-hubs';

export function useHub() {
  const hub = new DextHubConnection('/hubs/dashboard');
  
  hub.on('LogMessage', (level, message) => {
    // Adiciona ao log viewer
  });
  
  hub.on('TestProgress', (passed, failed, total) => {
    // Atualiza progress bar
  });
  
  hub.on('BuildComplete', (success, output) => {
    // Notifica usuário
  });
  
  return { hub };
}
```

### Fase 2: Actions do CLI via Dashboard (1 semana)

#### 2.1 Novas APIs para Actions

| Endpoint | Método | Descrição | CLI Equivalente |
|----------|--------|-----------|-----------------|
| `/api/test/run` | POST | Executa testes | `dext test` |
| `/api/test/run-coverage` | POST | Testes + Coverage | `dext test --coverage` |
| `/api/build` | POST | Compila projeto | `dext build` |
| `/api/migrate/up` | POST | Aplica migrations | `dext migrate:up` |
| `/api/migrate/down` | POST | Reverte migrations | `dext migrate:down` |
| `/api/migrate/generate` | POST | Gera migration | `dext migrate:generate` |
| `/api/logs/stream` | GET (SSE/WS) | Stream de logs | N/A |

#### 2.2 Arquitetura de Execução
```
Dashboard (Browser)
       │
       │ POST /api/test/run
       ▼
  API Handler
       │
       │ Cria processo externo
       ▼
  CLI Command (mesmo código)
       │
       │ Output via Hubs
       ▼
  Dashboard (atualiza em tempo real)
```

#### 2.3 Hub Messages para CLI

| Event | Payload | Descrição |
|-------|---------|-----------|
| `CommandStarted` | `{command, args}` | Comando iniciou |
| `CommandOutput` | `{line, level}` | Linha de output |
| `CommandCompleted` | `{exitCode, duration}` | Comando terminou |
| `TestResult` | `{name, status, duration}` | Resultado de um teste |
| `TestProgress` | `{passed, failed, running, total}` | Progresso geral |
| `BuildProgress` | `{phase, message}` | Progresso de build |
| `CoverageReady` | `{percentage, reportPath}` | Coverage gerada |

### Fase 3: Features Avançadas (2 semanas)

#### 3.1 Terminal Integrado
- Usar `xterm.js` para terminal real
- Output colorido do CLI
- Histórico de comandos
- Auto-complete para comandos Dext

#### 3.2 Heatmap de Coverage
- Visualização estilo GitHub contributions
- Hover mostra detalhes da unidade
- Click abre arquivo na IDE (via `dext://open`)

#### 3.3 Build & Run
- Botão "Run" no Dashboard
- Output de compilation em tempo real
- Erros clicáveis (abre IDE no erro)

---

## 📁 Estrutura de Pastas Final

```
Sources/
├── Hosting/
│   └── CLI/
│       ├── Commands/           # Comandos existentes
│       ├── API/                # Handlers para Dashboard
│       │   ├── Dext.CLI.API.Test.pas
│       │   ├── Dext.CLI.API.Build.pas
│       │   └── Dext.CLI.API.Migrate.pas
│       └── Hubs/               # Hub para Dashboard
│           └── Dext.CLI.DashboardHub.pas
└── Dashboard/
    ├── vue-app/                # Projeto Vue.js
    │   ├── src/
    │   ├── package.json
    │   └── vite.config.ts
    ├── build/                  # Output do build Vue
    └── Dext.Dashboard.Build.ps1 # Script de build
```

---

## ✅ Critérios de Sucesso

1. **Paridade Visual**: Todas as funcionalidades CLI têm um botão/ação no Dashboard
2. **Tempo Real**: Logs e progresso aparecem instantaneamente (< 100ms)
3. **Workflow Dual-Monitor**: IDE + Dashboard funcionam lado a lado
4. **Preservação**: Dashboard atual continua funcionando durante transição
5. **Performance**: Dashboard responde em < 50ms para interações UI

---

## 🔜 Próximos Passos Imediatos

1. [ ] Criar projeto Vue.js com estrutura base
2. [ ] Implementar Sidebar e layout principal
3. [ ] Integrar `dext-hubs.js` existente
4. [ ] Criar primeiro Action: "Run Tests"
5. [ ] Implementar LogViewer em tempo real

---

*Última atualização: 06 de Janeiro de 2026*
