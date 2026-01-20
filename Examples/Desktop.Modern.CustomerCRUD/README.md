# 🏢 Desktop.Modern.CustomerCRUD

Exemplo de aplicação Desktop moderna usando Dext Framework, demonstrando padrões de desenvolvimento profissional para aplicações empresariais.

## 🎯 O Que Este Exemplo Demonstra

| Recurso | Descrição |
|---------|-----------|
| **Injeção de Dependência** | Services injetados no Form via DI Container |
| **ViewModel** | Separação entre UI e lógica de apresentação |
| **Data Binding** | Propriedades notificam mudanças para a UI |
| **ORM** | Entidades mapeadas com atributos |
| **Validação** | Validação automática no ViewModel |
| **Logging** | Log de operações via ILogger |

## 📁 Estrutura do Projeto

```
Desktop.Modern.CustomerCRUD/
├── App/
│   └── App.Startup.pas         # Configuração DI
├── Entities/
│   └── Customer.Entity.pas     # TCustomer com atributos ORM
├── Services/
│   └── Customer.Service.pas    # ICustomerService
├── ViewModels/
│   └── Customer.ViewModel.pas  # TCustomerViewModel
├── Views/
│   ├── Main.Form.pas + .dfm    # Form principal
│   ├── Customer.List.pas + .dfm # Grid de clientes
│   └── Customer.Edit.pas + .dfm # Edição de cliente
├── DesktopModernCustomerCRUD.dpr
└── DesktopModernCustomerCRUD.dproj
```

## 🔧 Arquitetura

```
┌─────────────────────────────────────────────────────────────┐
│                       MainForm                               │
│  ┌─────────────┐  ┌──────────────────────────────────────┐  │
│  │  SidePanel  │  │           ContentPanel               │  │
│  │             │  │  ┌─────────────────────────────────┐ │  │
│  │ [Customers] │  │  │    CustomerListFrame            │ │  │
│  │             │  │  │    (TFrame)                     │ │  │
│  │ [About]     │  │  └─────────────────────────────────┘ │  │
│  │             │  │  ┌─────────────────────────────────┐ │  │
│  │             │  │  │    CustomerEditFrame            │ │  │
│  │             │  │  │    (TFrame + ViewModel)         │ │  │
│  └─────────────┘  │  └─────────────────────────────────┘ │  │
└───────────────────┴──────────────────────────────────────┴──┘
                              │
                              ▼
                    ┌─────────────────┐
                    │ ICustomerService│
                    │   + ILogger     │
                    │   (Injected)    │
                    └─────────────────┘
                              │
                              ▼
                    ┌─────────────────┐
                    │   Repository    │
                    │   (ORM)         │
                    └─────────────────┘
```

## 🚀 Como Executar

1. Abra `DesktopModernCustomerCRUD.dproj` no Delphi
2. Compile (Ctrl+F9)
3. Execute (F9)

O exemplo já vem com 3 clientes de demonstração pré-cadastrados.

## 📝 Principais Componentes

### Entity com ORM
```pascal
[Table('customers')]
TCustomer = class
  [PrimaryKey, AutoIncrement]
  FId: Integer;
  
  [Column('name'), Required, MaxLength(100)]
  FName: string;
end;
```

### Service com DI
```pascal
TCustomerService = class(TInterfacedObject, ICustomerService)
private
  FRepository: IRepository<TCustomer>;
  FLogger: ILogger;
public
  constructor Create(ARepository: IRepository<TCustomer>; ALogger: ILogger);
end;
```

### ViewModel com Binding
```pascal
TCustomerViewModel = class
private
  FOnPropertyChanged: TProc<string>;
public
  property Name: string read GetName write SetName;  // Notifica mudanças
  property IsValid: Boolean read GetIsValid;
  property IsDirty: Boolean read FIsDirty;
end;
```

### DI Container Setup
```pascal
// App.Startup.pas
TAppStartup.Configure;

// Inject into MainForm
MainForm.InjectDependencies(
  TAppStartup.GetCustomerService,
  TAppStartup.GetLogger
);
```

## ✅ Vantagens sobre MVU Puro

| Aspecto | MVU | Desktop Modern |
|---------|-----|----------------|
| Curva de aprendizado | Alta | Familiar |
| Arquivos por feature | 4-5 | 2-3 |
| Integração Dext | Nenhuma | Total |
| Produtividade | Baixa | Alta |
| Designer IDE | Limitado | Completo |

---

*Dext Framework - Modern Desktop Development for Delphi*
