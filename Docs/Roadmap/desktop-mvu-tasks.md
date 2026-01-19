# Dext Desktop/Mobile MVU - Task Checklist

> **Objetivo:** Implementar arquitetura MVU para aplicações Desktop/Mobile com Dext

---

## 📋 Fase 0: Proof of Concept (Manual)

**Meta:** Validar o conceito MVU sem dependências do framework

### Counter Manual (1-2 dias)
- [ ] Criar pasta `Examples/Desktop.MVU.CounterManual`
- [ ] Criar projeto VCL simples
- [ ] Implementar `TCounterModel` (record imutável)
- [ ] Implementar enum `TCounterMsg`
- [ ] Implementar função `Update(Model, Msg): Model`
- [ ] Implementar procedure `RenderView(Model, Form)`
- [ ] Conectar eventos de botão -> Update -> Render
- [ ] Documentar learnings

### TodoList Manual (2-3 dias)
- [ ] Criar pasta `Examples/Desktop.MVU.TodoManual`
- [ ] Implementar `TTodoModel` com lista de itens
- [ ] Implementar mensagens: AddTodo, RemoveTodo, ToggleTodo, FilterTodos
- [ ] Implementar Update com pattern matching
- [ ] Renderizar lista em TListBox ou TListView
- [ ] Testar fluxo completo

---

## 📋 Fase 1: Foundation (Core MVU)

**Meta:** Criar estrutura base do framework

### Dext.App.Core (1 semana)
- [ ] Criar pasta `Sources/App`
- [ ] Criar unit `Dext.App.pas` (facade)
- [ ] Criar unit `Dext.App.Interfaces.pas`
  - [ ] `IDextApplication`
  - [ ] `IDextAppBuilder`
  - [ ] `IModule`
  - [ ] `IMessageDispatcher`
- [ ] Criar unit `Dext.App.Application.pas`
  - [ ] `TDextApplication` class
  - [ ] `CreateBuilder` factory method
  - [ ] DI integration (`Services` property)
  - [ ] Module registration
- [ ] Criar unit `Dext.App.Module.pas`
  - [ ] `TModule<TModel, TMessage>` base class
  - [ ] Abstract methods: `Init`, `Update`, `View`
  - [ ] `Dispatch` helper method
- [ ] Criar unit `Dext.App.Message.pas`
  - [ ] `TMessage` base class
  - [ ] Timestamp, correlation ID
- [ ] Criar unit `Dext.App.UpdateResult.pas`
  - [ ] `TUpdateResult<T>` record
  - [ ] `NoEffect`, `WithEffect` factory methods
- [ ] Criar unit `Dext.App.Dispatcher.pas`
  - [ ] Simple in-memory message dispatcher
  - [ ] Module routing

### Testes (paralelo)
- [ ] Criar pasta `Sources/Tests/App`
- [ ] Tests para `TMessage`
- [ ] Tests para `TUpdateResult`
- [ ] Tests para `TModule` lifecycle

---

## 📋 Fase 2: State Management

**Meta:** Gerenciamento reativo de estado

### Dext.App.State (1 semana)
- [ ] Criar unit `Dext.App.State.pas`
  - [ ] `IStateStore` interface
  - [ ] `TStateStore` implementation
  - [ ] `GetState<T>`, `SetState<T>`
  - [ ] Subscriptions (observer pattern)
- [ ] Criar unit `Dext.App.State.Immutable.pas`
  - [ ] Helper functions para copiar records
  - [ ] `With<T>` pattern para updates imutáveis
  - [ ] `TImmutableList<T>` wrapper
- [ ] Integrar StateStore com TDextApplication
- [ ] State persistence opcional (arquivo/registry)

---

## 📋 Fase 3: Effects & Side Effects

**Meta:** Abstrair operações assíncronas e efeitos colaterais

### Dext.App.Effects (1 semana)
- [ ] Criar unit `Dext.App.Effect.pas`
  - [ ] `TEffect` base class
  - [ ] `OnSuccess`, `OnError` message types
- [ ] Criar unit `Dext.App.Effects.Http.pas`
  - [ ] `THttpEffect` (URL, Method, Body)
  - [ ] Integration com `TNetHTTPClient`
- [ ] Criar unit `Dext.App.Effects.Db.pas`
  - [ ] `TDbEffect` (Query, Persist, Delete)
  - [ ] Integration com ORM
- [ ] Criar unit `Dext.App.Effects.Timer.pas`
  - [ ] `TDelayEffect` (delay, then dispatch)
  - [ ] `TIntervalEffect` (periodic)
- [ ] Criar unit `Dext.App.Effects.Navigation.pas`
  - [ ] `TNavigateEffect` (module/view)
- [ ] Criar unit `Dext.App.EffectRunner.pas`
  - [ ] Effect executor (async)
  - [ ] Queue management
  - [ ] Error handling

---

## 📋 Fase 4: Pipeline & Behaviors

**Meta:** Cross-cutting concerns para mensagens

### Dext.App.Pipeline (4-5 dias)
- [ ] Criar unit `Dext.App.Behavior.pas`
  - [ ] `IMessageBehavior` interface
  - [ ] Pipeline builder fluent
- [ ] Criar unit `Dext.App.Behaviors.Logging.pas`
  - [ ] Log all messages with ILogger
- [ ] Criar unit `Dext.App.Behaviors.Validation.pas`
  - [ ] Validate message payloads
  - [ ] Integration com `Dext.Validation`
- [ ] Criar unit `Dext.App.Behaviors.Exception.pas`
  - [ ] Global exception handling
  - [ ] Error message dispatch
- [ ] Criar unit `Dext.App.Behaviors.Transaction.pas`
  - [ ] Wrap DB effects in transactions
- [ ] Registrar behaviors no DI

---

## 📋 Fase 5: View Binding

**Meta:** Renderização e binding de dados

### Dext.App.View (1 semana)
- [ ] Criar unit `Dext.App.View.Interfaces.pas`
  - [ ] `IViewRenderer`
  - [ ] `IViewBinder`
- [ ] Criar unit `Dext.App.View.VCL.pas`
  - [ ] VCL-specific view renderer
  - [ ] Control creation helpers
  - [ ] Event -> Message mapping
- [ ] Criar unit `Dext.App.View.FMX.pas`
  - [ ] FMX-specific view renderer (future)
- [ ] Criar unit `Dext.App.View.Binding.pas`
  - [ ] Model property -> Control binding
  - [ ] Auto-update on state change
- [ ] Criar unit `Dext.App.View.Fluent.pas`
  - [ ] Fluent API para construir views
  - [ ] `Container.AddLabel().AddButton().AddEdit()`

---

## 📋 Fase 6: Legacy Integration

**Meta:** Suporte a sistemas existentes

### Dext.App.Legacy (4-5 dias)
- [ ] Criar unit `Dext.App.Legacy.Adapter.pas`
  - [ ] `TLegacyFormAdapter` class
  - [ ] Inject DI services into TForm
  - [ ] Bind form events to messages
- [ ] Criar unit `Dext.App.Legacy.Services.pas`
  - [ ] Extension methods para TForm
  - [ ] `Form.GetService<T>`
  - [ ] `Form.Dispatch(Message)`
- [ ] Documentar migration patterns
- [ ] Criar exemplo híbrido

---

## 📋 Fase 7: Developer Experience

**Meta:** Ferramentas para desenvolvimento

### Dext.App.DevTools (1 semana)
- [ ] Criar unit `Dext.App.DevTools.pas`
  - [ ] DevTools window/form
  - [ ] Enable/Disable flag
- [ ] Message Inspector
  - [ ] List all dispatched messages
  - [ ] Show message details
  - [ ] Filter by type
- [ ] State Viewer
  - [ ] TreeView of current state
  - [ ] Watch specific paths
- [ ] Time-travel (advanced)
  - [ ] Record state transitions
  - [ ] Go back/forward
- [ ] CLI Commands
  - [ ] `dext new module CustomerModule`
  - [ ] `dext new message LoadCustomer`

---

## 📋 Fase 8: Examples & Documentation

**Meta:** Material de aprendizado

### Examples
- [ ] `Desktop.MVU.Counter` - Hello World (Dext powered)
- [ ] `Desktop.MVU.TodoList` - CRUD simples
- [ ] `Desktop.MVU.CustomerCRUD` - CRUD com ORM
- [ ] `Desktop.Hybrid.ERP` - Integração com legado
- [ ] `Mobile.FMX.Catalog` - Mobile catalog app

### Documentation
- [ ] MVU Concepts Guide (teoria)
- [ ] Getting Started (prático)
- [ ] Migration from Forms (para legado)
- [ ] Best Practices
- [ ] API Reference

---

## 🚀 Início Recomendado

1. **Agora:** Criar `Desktop.MVU.CounterManual` para validar conceito
2. **Próximo:** Fase 1 - Foundation
3. **Paralelo:** Refinar design conforme learnings

---

## 📊 Estimativa de Tempo

| Fase | Duração Estimada |
|------|-----------------|
| Fase 0: POC | 3-5 dias |
| Fase 1: Foundation | 5-7 dias |
| Fase 2: State | 5-7 dias |
| Fase 3: Effects | 5-7 dias |
| Fase 4: Pipeline | 4-5 dias |
| Fase 5: View | 5-7 dias |
| Fase 6: Legacy | 4-5 dias |
| Fase 7: DevTools | 5-7 dias |
| Fase 8: Examples | 5-7 dias |
| **Total** | **~7-9 semanas** |

---

*Criado: Janeiro 2026*
