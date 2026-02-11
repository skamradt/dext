# Injeção de Dependência

O Dext fornece um container de DI completo com injeção via construtor, escopos e gerenciamento de tempo de vida.

## Registro de Serviços

Registre serviços no `ConfigureServices` usando o record fluente `TDextServices`:

```pascal
procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  Services
    // Interface para Implementação
    .AddScoped<IUserService, TUserService>
    .AddSingleton<ILogger, TConsoleLogger>
    .AddTransient<IValidator, TValidator>
    // DbContext
    .AddDbContext<TAppDbContext>(ConfigureDatabase)
    // Controllers
    .AddControllers;
end;
```

> [!IMPORTANT]
> `TDextServices` é um **Record** — nunca chame `.Free` nele. Ele é gerenciado pela stack.

## Tempos de Vida (Lifetimes)

| Lifetime | Comportamento | Caso de Uso |
|----------|---------------|-------------|
| **Singleton** | Instância única por toda a vida da aplicação | Loggers, Configuração, Caches |
| **Scoped** | Uma instância por requisição HTTP | DbContext, Sessão de Usuário |
| **Transient** | Nova instância a cada solicitação | Validadores, Factories |

## Injeção via Construtor

Serviços são injetados automaticamente via construtor:

```pascal
type
  TUserService = class(TInterfacedObject, IUserService)
  private
    FRepository: IUserRepository;
    FLogger: ILogger;
  public
    constructor Create(Repository: IUserRepository; Logger: ILogger);
  end;

constructor TUserService.Create(Repository: IUserRepository; Logger: ILogger);
begin
  FRepository := Repository;
  FLogger := Logger;
end;
```

Basta registrar todos os serviços:

```pascal
Services
  .AddScoped<IUserRepository, TUsuarioRepository>
  .AddSingleton<ILogger, TConsoleLogger>
  .AddScoped<IUserService, TUserService>;  // Dependências auto-injetadas!
```

## Registro com Factory

Para serviços que precisam de inicialização customizada:

```pascal
Services.AddSingleton<IJwtTokenHandler, TJwtTokenHandler>(
  function(Provider: IServiceProvider): TObject
  begin
    Result := TJwtTokenHandler.Create(JWT_SECRET, JWT_ISSUER, JWT_AUDIENCE, JWT_EXPIRATION);
  end);
```

> [!WARNING]
> Use a forma explícita com dois parâmetros genéricos para evitar ambiguidade:  
> ✅ `Services.AddSingleton<IAuthService, TAuthService>(FactoryFunc)`  
> ❌ `Services.AddSingleton<IAuthService>(FactoryFunc)` — Pode falhar com E2250/E2003

## Resolução de Serviços

### Em Minimal APIs (Recomendado: Injeção Genérica)

```pascal
// ✅ Serviços são auto-injetados via overloads genéricos
Builder.MapGet<IUserService, IResult>('/api/users',
  function(Svc: IUserService): IResult
  begin
    Result := Results.Ok(Svc.GetAll);
  end);
```

> [!WARNING]
> ⛔ **NUNCA** resolva serviços manualmente em Minimal APIs:  
> ❌ `var Service := Ctx.RequestServices.GetService<IUserService>;`

### Em Controllers (Injeção via Construtor)

```pascal
type
  [ApiController('/api/users')]
  TUsersController = class
  private
    FUserService: IUserService;
  public
    constructor Create(UserService: IUserService);  // Auto-injetado
  end;
```

### Resolução Manual (Seeders, Background Tasks)

```pascal
var Scope := Provider.CreateScope;
try
  var Db := Scope.ServiceProvider.GetService(TAppDbContext) as TAppDbContext;
  // Use o serviço...
finally
  Scope := nil;  // Libera todos os serviços scoped
end;
```

## Construtor Customizado

Use `[ServiceConstructor]` quando uma classe tiver múltiplos construtores:

```pascal
type
  TUserService = class(TInterfacedObject, IUserService)
  public
    constructor Create; overload;
    
    [ServiceConstructor]  // DI usará este
    constructor Create(Repo: IUserRepository; Logger: ILogger); overload;
  end;
```

## Registrando DbContext

Separe a configuração do banco em um método privado no Startup:

```pascal
procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  Services
    .AddDbContext<TAppDbContext>(ConfigureDatabase)
    .AddScoped<IUserService, TUserService>;
end;

procedure TStartup.ConfigureDatabase(Options: TDbContextOptions);
begin
  Options
    .UseSQLite('App.db')
    .WithPooling(True);  // OBRIGATÓRIO para APIs Web
end;
```

> [!WARNING]
> **Connection Pooling**: APIs Web são multithreaded por natureza. **SEMPRE** habilite pooling em produção para evitar exaustão de conexões.

## Escopo de Serviço

Crie escopos filhos para processamento em background ou seed de banco:

```pascal
var Scope := Provider.CreateScope;
try
  var Service := Scope.ServiceProvider.GetService(TAppDbContext) as TAppDbContext;
  // Use serviço...
finally
  Scope := nil;  // Escopo libera todos os serviços scoped
end;
```

## Melhores Práticas

1. **Prefira injeção via construtor** ao invés de Service Locator
2. **Use interfaces** para testabilidade
3. **Scoped para DbContext** — um por requisição
4. **Singleton para serviços sem estado** (loggers, config)
5. **Habilite Connection Pooling** para todas as APIs Web
6. **Use encadeamento fluente** para registro
7. **Evite Dependências Cativas** — não injete `Scoped` dentro de `Singleton`

---

[← Tópicos Avançados](README.md) | [Próximo: Background Services →](background-services.md)
