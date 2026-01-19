# 🎯 Desktop.MVU.Counter

Um exemplo simples e didático demonstrando o padrão **Model-View-Update (MVU)** para aplicações desktop Delphi.

## 📚 O que é MVU?

MVU é um padrão arquitetural que promove:

- **Estado Imutável**: O Model nunca é modificado diretamente
- **Fluxo Unidirecional**: User Event → Message → Update → New Model → Render
- **Funções Puras**: A função Update é determinística e testável
- **Separação Clara**: View só renderiza, Update só processa lógica

```
┌─────────┐    Message    ┌──────────┐    New Model    ┌─────────┐
│  VIEW   │ ───────────▶  │  UPDATE  │ ─────────────▶  │  MODEL  │
└─────────┘               └──────────┘                 └─────────┘
     ▲                                                       │
     └───────────────────── Render ──────────────────────────┘
```

## 🏗️ Estrutura do Projeto

```
Desktop.MVU.Counter/
├── DesktopMVUCounter.dpr   # Projeto principal
├── Counter.Main.pas        # Form que orquestra o loop MVU
├── Counter.Main.dfm        # Layout do form
├── Counter.MVU.pas         # Implementação MVU (Model, Update, View)
└── README.md               # Este arquivo
```

## 📦 Componentes

### `TCounterModel` (Model)
Record imutável que representa o estado:
```pascal
TCounterModel = record
  Count: Integer;
  Step: Integer;
  History: string;
end;
```

### `TCounterMessage` (Messages)
Enum com todas as ações possíveis:
```pascal
TCounterMessage = (
  IncrementMsg,
  DecrementMsg,
  IncrementByStepMsg,
  DecrementByStepMsg,
  ResetMsg,
  SetStep1Msg,
  SetStep5Msg,
  SetStep10Msg
);
```

### `TCounterUpdate` (Update)
Função pura que processa mensagens:
```pascal
class function Update(const Model: TCounterModel; 
                      const Msg: TCounterMessage): TCounterModel;
```

### `TCounterView` (View)
Renderiza UI e dispara mensagens:
```pascal
procedure Render(const Model: TCounterModel);
```

## ▶️ Como Executar

1. Abra `DesktopMVUCounter.dproj` no Delphi
2. Compile (Ctrl+F9)
3. Execute (F9)

## 🧪 Testabilidade

O grande benefício do MVU é a testabilidade. A função Update é pura:

```pascal
procedure TestIncrement;
var
  Initial, Result: TCounterModel;
begin
  Initial := TCounterModel.Init;
  
  Result := TCounterUpdate.Update(Initial, IncrementMsg);
  
  Assert(Result.Count = 1);
  Assert(Initial.Count = 0); // Original não mudou!
end;
```

## 📖 Aprendizados

Este exemplo demonstra:

1. **Records como Model**: Value semantics garantem imutabilidade
2. **With Pattern**: Métodos `WithX()` para criar cópias modificadas
3. **Dispatch Callback**: View comunica intenções via mensagens
4. **Render Loop**: Toda mudança de estado causa re-render
5. **Orquestrador Simples**: O Form só coordena, não contém lógica

## 🚀 Próximos Passos

- Ver `Desktop.MVU.TodoList` para CRUD básico
- Ver `Desktop.MVU.CustomerCRUD` para integração com ORM
- Ver documentação do Dext.App para versão framework-powered

---

*Dext Framework - MVU for Delphi*
