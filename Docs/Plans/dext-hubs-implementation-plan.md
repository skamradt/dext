# 🔌 Dext.Hubs - Real-Time Communication Implementation Plan

## Visão Geral

Este documento define o plano de implementação para comunicação em tempo real no Dext Framework, com design compatível com SignalR e implementação incremental.

> **Princípio:** "Design Big, Build Small" - Interfaces completas, implementação mínima viável.

---

## 📐 Arquitetura

```
┌─────────────────────────────────────────────────────────────────────┐
│                        API de Alto Nível                             │
│   IHubContext<T>  •  IHubClients  •  IClientProxy  •  THub          │
└────────────────────────────┬────────────────────────────────────────┘
                             │
┌────────────────────────────▼────────────────────────────────────────┐
│                     Camada de Abstração                              │
│   IConnectionManager  •  IHubProtocol  •  THubMessage               │
└────────────────────────────┬────────────────────────────────────────┘
                             │
┌────────────────────────────▼────────────────────────────────────────┐
│                     Transportes (Pluggable)                          │
│  ┌───────────────┐  ┌───────────────┐  ┌───────────────────────────┐│
│  │     SSE       │  │  Long-Polling │  │      WebSocket            ││
│  │   (Fase 1)    │  │   (Fase 1)    │  │      (Fase 3)             ││
│  │      ✅       │  │      ✅       │  │         ❌                ││
│  └───────────────┘  └───────────────┘  └───────────────────────────┘│
└─────────────────────────────────────────────────────────────────────┘
```

---

## 🗂️ Estrutura de Arquivos

```
Sources/
└── Hubs/
    ├── Dext.Web.Hubs.pas                    # Exports principais
    ├── Dext.Web.Hubs.Interfaces.pas         # Contratos (Fase 1)
    ├── Dext.Web.Hubs.Types.pas              # THubMessage, enums
    ├── Dext.Web.Hubs.Hub.pas                # THub base class
    ├── Dext.Web.Hubs.Context.pas            # IHubContext implementation
    ├── Dext.Web.Hubs.Clients.pas            # IHubClients, IClientProxy
    ├── Dext.Web.Hubs.Connections.pas        # IConnectionManager
    ├── Dext.Web.Hubs.Protocol.Json.pas      # JSON Protocol
    ├── Dext.Web.Hubs.Middleware.pas         # Middleware para routing
    └── Transports/
        ├── Dext.Web.Hubs.Transport.Base.pas
        ├── Dext.Web.Hubs.Transport.SSE.pas      # Server-Sent Events (Fase 1)
        ├── Dext.Web.Hubs.Transport.LongPoll.pas # Long Polling (Fase 1)
        └── Dext.Web.Hubs.Transport.WebSocket.pas # WebSocket (Fase 3)
```

---

## 📋 Fases de Implementação

### ✅ Fase 1: Interfaces e Infraestrutura Base (7h)
**Status:** ✅ CONCLUÍDO (06/01/2026)  
**Meta:** Definir todos os contratos e tipos base.

| Tarefa | Arquivo | Status | Tempo |
|--------|---------|--------|-------|
| Definir interfaces principais | `Dext.Web.Hubs.Interfaces.pas` | ✅ | 2h |
| Criar tipos de mensagem | `Dext.Web.Hubs.Types.pas` | ✅ | 1h |
| Implementar ConnectionManager | `Dext.Web.Hubs.Connections.pas` | ✅ | 2h |
| Implementar JSON Protocol | `Dext.Web.Hubs.Protocol.Json.pas` | ✅ | 2h |
| Implementar THub base | `Dext.Web.Hubs.Hub.pas` | ✅ | 1h |
| Implementar IHubClients | `Dext.Web.Hubs.Clients.pas` | ✅ | 2h |
| Main exports unit | `Dext.Web.Hubs.pas` | ✅ | 0.5h |

**Critérios de Conclusão:**
- [x] Todas as interfaces compilam sem erros
- [x] Tipos THubMessage definidos
- [ ] Testes unitários básicos (pendente)

---

### ✅ Fase 2: Transporte SSE + Middleware (8h)
**Status:** ✅ CONCLUÍDO (06/01/2026)  
**Meta:** Comunicação funcional server→client via SSE.

| Tarefa | Arquivo | Status | Tempo |
|--------|---------|--------|-------|
| Implementar TSSETransport | `Dext.Web.Hubs.Transport.SSE.pas` | ✅ | 3h |
| Implementar SSEHubConnection | (mesmo arquivo) | ✅ | 1h |
| Middleware /negotiate e /hub | `Dext.Web.Hubs.Middleware.pas` | ✅ | 2h |
| ~~Implementar IHubClients~~ | ~~`Dext.Web.Hubs.Clients.pas`~~ | ✅ | (Fase 1) |
| ~~Implementar THub base~~ | ~~`Dext.Web.Hubs.Hub.pas`~~ | ✅ | (Fase 1) |
| Implementar IHubContext | `Dext.Web.Hubs.Context.pas` | ✅ | 1h |
| Extension MapHub<T> | `Dext.Web.Hubs.Extensions.pas` | ✅ | 1h |
| Cliente JavaScript | `wwwroot/dext-hubs.js` | ✅ | 2h |
| Documentação | `Docs/hubs.md` | ✅ | 1h |
| Projeto de Testes | `Tests/Hubs/TestDextHubs.dpr` | ✅ | 1h |

**Critérios de Conclusão:**
- [x] Polling transport funciona no browser (usando /poll endpoint)
- [x] Hub pode enviar mensagens para clientes
- [x] Exemplo funcional com test project
- [x] ServerTime broadcast funcional
- [x] Groups funcionando

**Nota:** SSE não funciona corretamente com Indy (sem flush). Usamos polling como alternativa.

---

### ⬜ Fase 3: Integração Dashboard (6h)
**Status:** ⬜ Pendente  
**Meta:** Dashboard recebe eventos em tempo real.

| Tarefa | Arquivo | Status | Tempo |
|--------|---------|--------|-------|
| Cliente JS (DextHubConnection) | `wwwroot/js/dext-hubs.js` | ⬜ | 2h |
| Integrar com Dashboard | UI Components | ⬜ | 2h |
| Testes E2E | Test scripts | ⬜ | 2h |

**Critérios de Conclusão:**
- [ ] Dashboard mostra logs em tempo real
- [ ] Dashboard mostra progresso de build/tests
- [ ] Reconexão automática funciona

---

### ⬜ Fase 4: WebSocket Completo (Futuro - 16h+)
**Status:** ⬜ Planejado para v1.1+  
**Meta:** Comunicação bidirecional completa.

| Tarefa | Descrição | Tempo |
|--------|-----------|-------|
| WebSocket Handshake | HTTP Upgrade, Sec-WebSocket-Key | 4h |
| Frame Protocol | RFC 6455 framing | 6h |
| Ping/Pong | Keep-alive | 2h |
| Cliente invoca Server | Bidirecional | 4h |

**Gatilho:** Necessário quando implementarmos Dext Forum ou features colaborativas.

---

## 🔧 Especificação Técnica

### Protocolo de Mensagens (SignalR-Compatible JSON)

```json
// Invocation (Server → Client ou Client → Server)
{
  "type": 1,
  "invocationId": "abc123",
  "target": "ReceiveMessage",
  "arguments": ["Hello", "World"]
}

// Completion (Resultado de invocação)
{
  "type": 3,
  "invocationId": "abc123",
  "result": { "success": true }
}

// Ping (Keep-alive)
{ "type": 6 }

// Close
{
  "type": 7,
  "error": "Connection closed by server"
}
```

### Endpoints HTTP

| Endpoint | Método | Descrição |
|----------|--------|-----------|
| `/hubs/{hubName}/negotiate` | POST | Retorna connectionId e transportes disponíveis |
| `/hubs/{hubName}` | GET | SSE stream (EventSource) |
| `/hubs/{hubName}` | POST | Invoca método no Hub (até termos WebSocket) |

### Negotiate Response

```json
{
  "connectionId": "guid-here",
  "availableTransports": [
    { "transport": "ServerSentEvents", "transferFormats": ["Text"] },
    { "transport": "LongPolling", "transferFormats": ["Text"] }
  ]
}
```

---

## 📝 Exemplo de Uso (API Final)

### Servidor (Delphi)

```pascal
// 1. Definir o Hub
type
  TDashboardHub = class(THub)
  public
    // Métodos que clientes podem chamar (via HTTP POST por enquanto)
    procedure SubscribeToProject(const ProjectId: string);
    procedure SendCommand(const Command: string);
  end;

procedure TDashboardHub.SubscribeToProject(const ProjectId: string);
begin
  Groups.AddToGroupAsync(Context.ConnectionId, 'project:' + ProjectId);
end;

// 2. Registrar no App
App.MapHub<TDashboardHub>('/hubs/dashboard');

// 3. Enviar mensagens de qualquer lugar
var Hub := App.Services.GetService<IHubContext<TDashboardHub>>;
Hub.Clients.All.SendAsync('LogReceived', [TValue.From(LogEntry)]);
Hub.Clients.Group('project:123').SendAsync('BuildProgress', [75, 'Linking...']);
```

### Cliente (JavaScript)

```javascript
// Cliente compatível com SignalR (subset)
const connection = new DextHubConnection('/hubs/dashboard');

// Handlers para mensagens do servidor
connection.on('LogReceived', (log) => {
  terminal.writeln(log.message);
});

connection.on('BuildProgress', (percent, message) => {
  progressBar.value = percent;
  statusLabel.textContent = message;
});

// Conectar
await connection.start();

// Invocar método no servidor
await connection.invoke('SubscribeToProject', 'my-project-id');
```

---

## 🧪 Casos de Uso do Dashboard

### 1. Live Logs
```
Dashboard ──GET /hubs/dashboard────► Server
           ◄──── SSE: LogReceived ────
           ◄──── SSE: LogReceived ────
           ◄──── SSE: LogReceived ────
```

### 2. Build Progress
```
Dashboard ──POST SubscribeToProject──► Server (adds to group)
           ◄──── SSE: BuildStarted ────
           ◄──── SSE: BuildProgress(25) ────
           ◄──── SSE: BuildProgress(50) ────
           ◄──── SSE: BuildCompleted ────
```

### 3. Test Execution
```
Dashboard ──POST RunTests──► Server (via HTTP normal)
           ◄──── SSE: TestStarted ────
           ◄──── SSE: TestPassed('TestA') ────
           ◄──── SSE: TestFailed('TestB') ────
           ◄──── SSE: TestCompleted(10/12) ────
```

---

## 🔗 Documentos Relacionados

- [CLI & Dashboard Unified Plan](./cli-dashboard-unified-plan.md)
- [Web Roadmap - Real-Time Section](../Roadmap/web-roadmap.md#4-real-time--eventing-signalr-like)
- [V1 Beta Roadmap](../Releases/v1-beta-roadmap.md)

---

## 📈 Métricas de Sucesso

| Métrica | Target |
|---------|--------|
| Latência de mensagem | < 50ms |
| Reconexão automática | < 3s |
| Conexões simultâneas | 100+ (Dashboard) |
| Memory per connection | < 1KB |

---

## ⚠️ Limitações Conhecidas (Fase 1-2)

1. **Sem comunicação Client→Server via stream** - usa HTTP POST
2. **Sem MessagePack** - apenas JSON
3. **Sem streaming de resultados** - apenas fire-and-forget
4. **Sem autenticação de conexão** - usa auth do request HTTP

Estas limitações serão resolvidas na Fase 4 com WebSocket completo.

---

*Última atualização: 06 de Janeiro de 2026*
