# 🖥️ Dext Desktop/Mobile - MVU Architecture Roadmap

> **Visão:** Trazer a experiência moderna do Dext para aplicações desktop (VCL/FMX) e mobile, usando a arquitetura **Model-View-Update (MVU)** inspirada no Elm, mantendo compatibilidade com sistemas legados.

---

## 📚 O que é MVU (Model-View-Update)?

O MVU é um padrão arquitetural que promove **fluxo unidirecional de dados** e **imutabilidade**, tornando o código mais previsível e testável.

```
┌─────────────────────────────────────────────────────────────┐
│                      MVU Data Flow                          │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│    ┌─────────┐    Message    ┌──────────┐    New Model      │
│    │  VIEW   │ ───────────▶  │  UPDATE  │ ───────────────┐ │
│    └─────────┘               └──────────┘                 │ │
│         ▲                                                 │ │
│         │                    ┌──────────┐                 │ │
│         └────────────────────│  MODEL   │◀───────────────┘  │
│              Render          └──────────┘                   │
│                                (State)                      │
└─────────────────────────────────────────────────────────────┘
```

**Componentes:**
- **Model**: Estado imutável da aplicação (single source of truth)
- **View**: Função pura que renderiza UI a partir do Model
- **Update**: Função pura que recebe mensagens e produz novo Model
- **Message**: Eventos/intenções que disparam mudanças de estado

---

## 🏗️ Arquitetura Proposta: Dext.App

### Visão Geral

```
┌────────────────────────────────────────────────────────────────────────┐
│                         DEXT APPLICATION HOST                          │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │                    TDextApplication (Orchestrator)                │  │
│  │  ┌────────────┐  ┌────────────┐  ┌────────────┐  ┌────────────┐  │  │
│  │  │     DI     │  │   Logger   │  │  Options   │  │ Validation │  │  │
│  │  │ Container  │  │  (ILogger) │  │ (IOptions) │  │ (Fluent)   │  │  │
│  │  └────────────┘  └────────────┘  └────────────┘  └────────────┘  │  │
│  │  ┌────────────┐  ┌────────────┐  ┌────────────┐  ┌────────────┐  │  │
│  │  │  Pipeline  │  │    ORM     │  │   Async    │  │   State    │  │  │
│  │  │ (Behaviors)│  │ (DbContext)│  │  (Fluent)  │  │  Manager   │  │  │
│  │  └────────────┘  └────────────┘  └────────────┘  └────────────┘  │  │
│  └──────────────────────────────────────────────────────────────────┘  │
│                                  │                                      │
│                          ┌───────▼───────┐                              │
│                          │  Dispatcher   │                              │
│                          │  (Message Bus)│                              │
│                          └───────┬───────┘                              │
│                                  │                                      │
│       ┌──────────────────────────┼──────────────────────────┐          │
│       │                          │                          │          │
│       ▼                          ▼                          ▼          │
│  ┌─────────┐               ┌─────────┐               ┌─────────┐       │
│  │ Feature │               │ Feature │               │ Feature │       │
│  │ Module  │               │ Module  │               │ Module  │       │
│  │  (MVU)  │               │  (MVU)  │               │ (Legacy)│       │
│  └─────────┘               └─────────┘               └─────────┘       │
│       │                          │                          │          │
│       ▼                          ▼                          ▼          │
│  ┌─────────┐               ┌─────────┐               ┌─────────┐       │
│  │  VIEW   │               │  VIEW   │               │  FORM   │       │
│  │ (Panel) │               │ (Frame) │               │ (Legacy)│       │
│  └─────────┘               └─────────┘               └─────────┘       │
└────────────────────────────────────────────────────────────────────────┘
```

---

## 🧩 Componentes Core

### 1. `TDextApplication` - O Orquestrador

O coração do sistema, responsável por:
- Gerenciar o ciclo de vida da aplicação
- Hospedar o container DI
- Roteamento de mensagens entre módulos
- Pipeline de behaviors (cross-cutting concerns)
- Gerenciamento de estado global

```pascal
type
  TDextApplication = class
  private
    FServices: IServiceCollection;
    FProvider: IServiceProvider;
    FDispatcher: IMessageDispatcher;
    FStateStore: IStateStore;
  public
    class function CreateBuilder: IDextAppBuilder;
    
    procedure Run;
    procedure Dispatch(const Msg: TMessage);
    procedure RegisterModule<T: TModule>;
    
    property Services: IServiceProvider read FProvider;
  end;
```

### 2. `IStateStore` - Gerenciador de Estado Global

```pascal
type
  IStateStore = interface
    function GetState<T>: T;
    procedure SetState<T>(const Value: T);
    procedure Subscribe<T>(Handler: TProc<T>);
  end;
```

### 3. `TModule` - Encapsulamento MVU

Cada feature é um módulo MVU independente:

```pascal
type
  TCustomerModule = class(TModule<TCustomerModel, TCustomerMessage>)
  protected
    function Init: TCustomerModel; override;
    function Update(const Model: TCustomerModel; 
                    const Msg: TCustomerMessage): TUpdateResult<TCustomerModel>; override;
    procedure View(const Model: TCustomerModel; const Container: TWinControl); override;
  end;
```

### 4. `TMessage` - Mensagens Tipadas

```pascal
type
  // Base message
  TMessage = class abstract
    Timestamp: TDateTime;
  end;
  
  // Módulo específico
  TCustomerMessage = class(TMessage)
  end;
  
  TLoadCustomer = class(TCustomerMessage)
    CustomerId: Integer;
  end;
  
  TCustomerLoaded = class(TCustomerMessage)
    Customer: TCustomer;
  end;
  
  TUpdateField = class(TCustomerMessage)
    FieldName: string;
    NewValue: TValue;
  end;
```

### 5. `TUpdateResult<T>` - Resultado com Efeitos Colaterais

```pascal
type
  TEffect = class abstract
    // Comandos assíncronos (API calls, DB, etc.)
  end;
  
  THttpEffect = class(TEffect)
    Url: string;
    Method: string;
    OnSuccess: TClass; // Message type to dispatch
    OnError: TClass;
  end;
  
  TUpdateResult<T> = record
    Model: T;
    Effects: TArray<TEffect>;
    
    class function NoEffect(const AModel: T): TUpdateResult<T>; static;
    class function WithEffect(const AModel: T; 
                               const AEffects: TArray<TEffect>): TUpdateResult<T>; static;
  end;
```

---

## 🔌 Integração com Recursos Existentes do Dext

| Recurso Dext | Uso no Desktop MVU |
|---|---|
| **DI Container** | Injeção de serviços em Modules e Views |
| **ILogger** | Logging estruturado de mensagens e transições de estado |
| **IOptions<T>** | Configuração tipada (temas, preferências, conexões) |
| **Validation** | Validação automática de Models antes de persistir |
| **Fluent Async** | Effects assíncronos (HTTP, Timers, DB) |
| **ORM** | Persistência via Effects |
| **Pipelines** | Behaviors cross-cutting (Logging, Validation, Caching) |
| **Testing/Mocks** | Testes unitários de Update functions |
| **Fluent Assertions** | Assertivas expressivas em testes |

---

## 🔄 Pipeline de Behaviors

Similar aos middlewares web, mas para mensagens:

```pascal
type
  IMessageBehavior = interface
    function Handle<T>(const Msg: T; 
                        Next: TFunc<T, TUpdateResult>): TUpdateResult;
  end;
  
  // Exemplos
  TLoggingBehavior = class(TInterfacedObject, IMessageBehavior)
    function Handle<T>(const Msg: T; Next: TFunc<T, TUpdateResult>): TUpdateResult;
  end;
  
  TValidationBehavior = class(TInterfacedObject, IMessageBehavior)
    function Handle<T>(const Msg: T; Next: TFunc<T, TUpdateResult>): TUpdateResult;
  end;
  
  TTransactionBehavior = class(TInterfacedObject, IMessageBehavior)
    function Handle<T>(const Msg: T; Next: TFunc<T, TUpdateResult>): TUpdateResult;
  end;
```

---

## 🏠 Modelo Híbrido (Legado + MVU)

Para sistemas existentes, o Dext permite convivência pacífica:

```
┌────────────────────────────────────────────────────────────┐
│                    HYBRID APPLICATION                       │
├────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌─────────────────────┐    ┌─────────────────────────┐    │
│  │    LEGACY ZONE      │    │       MVU ZONE          │    │
│  │   (TForm-based)     │    │   (Dext Modules)        │    │
│  │                     │    │                         │    │
│  │  ┌───────────────┐  │    │  ┌─────────────────┐   │    │
│  │  │ MainForm      │  │    │  │ CustomerModule  │   │    │
│  │  │ (TMainForm)   │◀─┼────┼──│ (MVU + DI)      │   │    │
│  │  │               │  │    │  └─────────────────┘   │    │
│  │  │  ┌─────────┐  │  │    │                         │    │
│  │  │  │TPanel   │◀─┼──┼────┼── MVU View Container    │    │
│  │  │  │(Host)   │  │  │    │                         │    │
│  │  │  └─────────┘  │  │    │  ┌─────────────────┐   │    │
│  │  └───────────────┘  │    │  │ OrderModule     │   │    │
│  │                     │    │  │ (MVU + ORM)     │   │    │
│  │  ┌───────────────┐  │    │  └─────────────────┘   │    │
│  │  │ LegacyForm    │  │    │                         │    │
│  │  │ (TDataModule) │◀─┼────┼── Shared Services       │    │
│  │  └───────────────┘  │    │                         │    │
│  └─────────────────────┘    └─────────────────────────┘    │
│              │                          │                   │
│              └────────────┬─────────────┘                   │
│                           ▼                                 │
│                ┌─────────────────────┐                      │
│                │  TDextApplication   │                      │
│                │  (Shared Services)  │                      │
│                │  - DI Container     │                      │
│                │  - DbContext        │                      │
│                │  - ILogger          │                      │
│                │  - IConfiguration   │                      │
│                └─────────────────────┘                      │
└────────────────────────────────────────────────────────────┘
```

### Adapter para Forms Legados

```pascal
type
  TLegacyFormAdapter = class
  private
    FForm: TForm;
    FApp: TDextApplication;
  public
    constructor Create(AForm: TForm; AApp: TDextApplication);
    
    // Inject services into form
    procedure InjectServices;
    
    // Subscribe to state changes
    procedure BindState<T>(const PropertyPath: string; Control: TControl);
  end;
```

---

## 📋 Roadmap de Implementação

### Fase 1: Foundation (Core MVU)
- [ ] **Dext.App.Core**
  - [ ] `TDextApplication` - Orquestrador base
  - [ ] `IServiceProvider` integration 
  - [ ] `IMessageDispatcher` - Message bus simples
  - [ ] `TModule` - Base class para módulos MVU
  - [ ] `TMessage` - Base para mensagens tipadas
  - [ ] `TUpdateResult<T>` - Result type com effects

### Fase 2: State Management
- [ ] **Dext.App.State**
  - [ ] `IStateStore` - Gerenciador de estado global
  - [ ] State immutability helpers
  - [ ] State subscriptions (reactive)
  - [ ] State persistence (optional)
  - [ ] Time-travel debugging (dev mode)

### Fase 3: Effects & Side Effects
- [ ] **Dext.App.Effects**
  - [ ] `TEffect` - Base para side effects
  - [ ] `THttpEffect` - HTTP requests
  - [ ] `TDbEffect` - Database operations (ORM)
  - [ ] `TTimerEffect` - Delays/Timers
  - [ ] `TNavigationEffect` - Navegação entre views
  - [ ] Effect executor (async runner)

### Fase 4: Pipeline & Behaviors
- [ ] **Dext.App.Pipeline**
  - [ ] `IMessageBehavior` - Interface para behaviors
  - [ ] `TLoggingBehavior` - Log de mensagens
  - [ ] `TValidationBehavior` - Validação automática
  - [ ] `TExceptionBehavior` - Tratamento de erros
  - [ ] `TTransactionBehavior` - Transações automáticas
  - [ ] Pipeline builder fluent

### Fase 5: View Binding
- [ ] **Dext.App.View**
  - [ ] `IViewRenderer` - Interface de renderização
  - [ ] VCL Renderer (Panels, Frames)
  - [ ] FMX Renderer (Cross-platform)
  - [ ] Data binding helpers
  - [ ] Event to Message mapping

### Fase 6: Legacy Integration
- [ ] **Dext.App.Legacy**
  - [ ] `TLegacyFormAdapter` - Adapter para forms existentes
  - [ ] DI injection em forms legados
  - [ ] State binding para controls legados
  - [ ] Migration guide

### Fase 7: Developer Experience
- [ ] **Dext.App.DevTools**
  - [ ] Message inspector (log visual)
  - [ ] State viewer (árvore de estado)
  - [ ] Time-travel debugger
  - [ ] Performance profiler
  - [ ] Code generator (CLI: `dext new module`)

### Fase 8: Examples & Documentation
- [ ] **Examples**
  - [ ] `Desktop.MVU.Counter` - Hello World MVU
  - [ ] `Desktop.MVU.TodoList` - CRUD básico
  - [ ] `Desktop.MVU.CustomerCRUD` - CRUD com ORM
  - [ ] `Desktop.Hybrid.ERP` - Integração com legado
  - [ ] `Mobile.FMX.Catalog` - Mobile com FMX
- [ ] **Documentation**
  - [ ] MVU Concepts guide
  - [ ] Getting Started
  - [ ] Migration from Forms
  - [ ] Best Practices

---

## 🎯 Exemplo Prático: Counter App

### Versão "Manual" (sem Dext)

```pascal
// Counter sem Dext - mostra o conceito puro MVU
unit Counter.Manual;

type
  // MODEL
  TCounterModel = record
    Count: Integer;
  end;
  
  // MESSAGES
  TCounterMsg = (cmIncrement, cmDecrement, cmReset);
  
  // UPDATE
  function Update(Model: TCounterModel; Msg: TCounterMsg): TCounterModel;
  begin
    Result := Model;
    case Msg of
      cmIncrement: Result.Count := Model.Count + 1;
      cmDecrement: Result.Count := Model.Count - 1;
      cmReset:     Result.Count := 0;
    end;
  end;
  
  // VIEW (manual)
  procedure RenderView(const Model: TCounterModel; Form: TForm);
  begin
    TLabel(Form.FindComponent('lblCount')).Caption := Model.Count.ToString;
  end;
  
  // WIRING (no form)
  procedure TMainForm.FormCreate(Sender: TObject);
  begin
    FModel := Default(TCounterModel);
    RenderView(FModel, Self);
  end;
  
  procedure TMainForm.btnIncrementClick(Sender: TObject);
  begin
    FModel := Update(FModel, cmIncrement);
    RenderView(FModel, Self);
  end;
```

### Versão Dext Powered

```pascal
// Counter com Dext - toda a infraestrutura pronta
unit Counter.Dext;

type
  // MODEL
  TCounterModel = record
    Count: Integer;
    LastAction: string;
  end;
  
  // MESSAGES
  TCounterMessage = class(TMessage)
  end;
  
  TIncrement = class(TCounterMessage)
    Amount: Integer;
    constructor Create(AAmount: Integer = 1);
  end;
  
  TDecrement = class(TCounterMessage)
  end;
  
  TReset = class(TCounterMessage)
  end;
  
  // MODULE
  TCounterModule = class(TModule<TCounterModel, TCounterMessage>)
  private
    FLogger: ILogger;
  protected
    function Init: TCounterModel; override;
    function Update(const Model: TCounterModel; 
                    const Msg: TCounterMessage): TUpdateResult<TCounterModel>; override;
    procedure View(const Model: TCounterModel; const Container: TWinControl); override;
  public
    constructor Create(Logger: ILogger); // DI!
  end;

implementation

constructor TCounterModule.Create(Logger: ILogger);
begin
  inherited Create;
  FLogger := Logger;
end;

function TCounterModule.Init: TCounterModel;
begin
  Result.Count := 0;
  Result.LastAction := 'Initialized';
end;

function TCounterModule.Update(const Model: TCounterModel;
  const Msg: TCounterMessage): TUpdateResult<TCounterModel>;
var
  NewModel: TCounterModel;
begin
  NewModel := Model;
  
  if Msg is TIncrement then
  begin
    NewModel.Count := Model.Count + TIncrement(Msg).Amount;
    NewModel.LastAction := 'Incremented by ' + TIncrement(Msg).Amount.ToString;
    FLogger.LogInformation('Counter incremented to {Count}', [NewModel.Count]);
  end
  else if Msg is TDecrement then
  begin
    NewModel.Count := Model.Count - 1;
    NewModel.LastAction := 'Decremented';
  end
  else if Msg is TReset then
  begin
    NewModel.Count := 0;
    NewModel.LastAction := 'Reset';
  end;
  
  Result := TUpdateResult<TCounterModel>.NoEffect(NewModel);
end;

procedure TCounterModule.View(const Model: TCounterModel; 
  const Container: TWinControl);
begin
  // Fluent view building (ou binding automático)
  Container
    .Clear
    .AddLabel('Count: ' + Model.Count.ToString)
    .AddLabel('Last: ' + Model.LastAction)
    .AddButton('+ Increment', procedure 
      begin 
        Dispatch(TIncrement.Create); 
      end)
    .AddButton('- Decrement', procedure 
      begin 
        Dispatch(TDecrement.Create);
      end)
    .AddButton('Reset', procedure 
      begin 
        Dispatch(TReset.Create);
      end);
end;

// APP STARTUP
program CounterApp;

begin
  TDextApplication
    .CreateBuilder
    .ConfigureServices(procedure(Services: IServiceCollection)
      begin
        Services.AddLogging;
        Services.AddModule<TCounterModule>;
      end)
    .Build
    .Run;
end.
```

---

## 🧪 Testabilidade

O padrão MVU brilha em testes:

```pascal
procedure TCounterTests.TestIncrement;
var
  Module: TCounterModule;
  InitialModel, NewModel: TCounterModel;
  Result: TUpdateResult<TCounterModel>;
begin
  // Arrange
  Module := TCounterModule.Create(TMock<ILogger>.Create.Instance);
  InitialModel := Module.Init;
  
  // Act
  Result := Module.Update(InitialModel, TIncrement.Create(5));
  
  // Assert
  Should(Result.Model.Count).Be(5);
  Should(Result.Model.LastAction).Contain('Incremented');
  Should(Result.Effects).BeEmpty;
end;
```

---

## 📊 Comparativo

| Aspecto | Forms Tradicional | MVVM | **Dext MVU** |
|---|---|---|---|
| Estado | Espalhado nos controls | ViewModel | **Model centralizado** |
| Fluxo de dados | Bidirecional | Bidirecional (binding) | **Unidirecional** |
| Testabilidade | Difícil | Média | **Alta** |
| Complexidade | Baixa inicial, alta depois | Média | **Baixa constante** |
| Debugging | Difícil (estado mutável) | Médio | **Fácil (imutável)** |
| Reuso | Baixo | Alto | **Alto** |
| Learning curve | Baixa | Média | **Média** |
| Integração DI | Manual | Possível | **Nativa** |
| Legado | É o legado | Requer refactor | **Híbrido possível** |

---

## 🎯 Próximos Passos

1. **Proof of Concept**: Criar `Desktop.MVU.Counter` manual
2. **Core Framework**: Implementar Fase 1 (Foundation)
3. **Refinar API**: Testar ergonomia com exemplo CRUD
4. **Documentação**: Criar Getting Started

---

*Última atualização: Janeiro de 2026*
