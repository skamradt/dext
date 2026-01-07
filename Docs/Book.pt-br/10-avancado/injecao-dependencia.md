# Injeção de Dependência

Um container de Injeção de Dependência (DI) completo e de alta performance.

## Conceitos Básicos

Injeção de Dependência permite que você escreva código desacoplado, injetando as dependências de uma classe através do seu construtor em vez de instanciá-las internamente.

### 1. Definir uma Interface e Classe

```pascal
type
  IEmailService = interface
    ['{GUID}']
    procedure Enviar(Email, Assunto, Mensagem: string);
  end;

  TEmailService = class(TInterfacedObject, IEmailService)
  public
    procedure Enviar(Email, Assunto, Mensagem: string);
  end;
```

### 2. Registrar no Container

```pascal
TWebHostBuilder.CreateDefault(nil)
  .ConfigureServices(procedure(Services: IServiceCollection)
    begin
      // Registrar como Singleton (uma única instância global)
      Services.AddSingleton<IEmailService, TEmailService>;
      
      // Registrar como Scoped (uma instância por requisição HTTP)
      Services.AddScoped<IUserService, TUserService>;
      
      // Registrar como Transient (nova instância a cada solicitação)
      Services.AddTransient<IValidator, TValidator>;
    end)
```

## Vida Útil dos Serviços (Lifetimes)

| Lifetime | Descrição |
|----------|-----------|
| **Singleton** | Criado uma única vez e compartilhado por toda a aplicação. |
| **Scoped** | Criado uma vez por escopo (geralmente uma requisição HTTP). |
| **Transient** | Criado toda vez que for solicitado. |

## Injeção via Construtor

O Dext resolve dependências automaticamente através de construtores:

```pascal
type
  TUserController = class(TController)
  private
    FUserService: IUserService;
  public
    constructor Create(UserService: IUserService); // Injetado automaticamente
  end;
```

## Atributo [ServiceConstructor]

Se uma classe tiver múltiplos construtores, use o atributo para indicar qual deve ser usado pela DI:

```pascal
type
  TMyService = class
  public
    constructor Create; overload;
    
    [ServiceConstructor]
    constructor Create(Logger: ILogger); overload;
  end;
```

---

[← Tópicos Avançados](README.md) | [Próximo: Background Services →](background-services.md)
