# Inicialização da Aplicação

Para projetos profissionais, o Dext recomenda separar a configuração do arquivo `.dpr` principal usando uma **Classe Startup**.

## Por que usar uma Classe Startup?

- **Código Limpo**: Mantém o arquivo `.dpr` minimalista e focado apenas em iniciar o processo.
- **Separação de Preocupações**: Serviços e Middlewares são configurados em uma classe dedicada.
- **Testabilidade**: Mais fácil de "mockar" configurações durante testes de integração.
- **Manutenibilidade**: Evita o código "macarronada" em blocos globais.

## O Padrão Startup Class

Crie uma nova unit (ex: `MeuProjeto.Startup.pas`) implementando a interface `IStartup`:

```pascal
unit MeuProjeto.Startup;

interface

uses
  Dext.Entity.Core,     // TDbContextOptions
  // Facades POR ÚLTIMO
  Dext,                 // IConfiguration, TDextServices
  Dext.Web;             // IWebApplication, IStartup

type
  TStartup = class(TInterfacedObject, IStartup)
  public
    procedure ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
    procedure Configure(const App: IWebApplication);
  private
    procedure ConfigureDatabase(Options: TDbContextOptions);
  end;

implementation

uses
  MeuProjeto.Data.Context,
  MeuProjeto.Endpoints;

procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  Services
    .AddDbContext<TAppDbContext>(ConfigureDatabase)
    .AddScoped<IUserService, TUserService>
    .AddControllers;
end;

procedure TStartup.Configure(const App: IWebApplication);
begin
  // Configurações Globais de JSON
  JsonDefaultSettings(JsonSettings.CamelCase.CaseInsensitive.ISODateFormat);

  App.Builder
    // 1. Exception Handler (sempre o primeiro)
    .UseExceptionHandler
    // 2. Logging HTTP
    .UseHttpLogging
    // 3. CORS
    .UseCors(CorsOptions.AllowAnyOrigin.AllowAnyMethod.AllowAnyHeader)
    // 4. Mapear Endpoints (Minimal APIs)
    .MapEndpoints(TMeusEndpoints.MapEndpoints)
    // 5. Mapear Controllers (se usar)
    .MapControllers
    // 6. Swagger (DEPOIS de mapear as rotas)
    .UseSwagger(Swagger.Title('Minha API').Version('v1'));
end;

procedure TStartup.ConfigureDatabase(Options: TDbContextOptions);
begin
  Options
    .UseSQLite('App.db')
    .WithPooling(True); // OBRIGATÓRIO para APIs Web em produção
end;

end.
```

> [!IMPORTANT]
> **Pontos Chave**:
> - `TDextServices` é um **Record** — nunca chame `.Free` nele.
> - Assinatura de `ConfigureServices`: `(const Services: TDextServices; const Configuration: IConfiguration)`.
> - `Configure` recebe `IWebApplication`, não `IApplicationBuilder`.
> - Use `App.Builder` para o pipeline **fluente** de middlewares.
> - Sempre separe a configuração do banco em um método privado (`ConfigureDatabase`).

## Programa Principal (.dpr)

Com a classe Startup, seu arquivo principal fica extremamente limpo:

```pascal
program Web.MeuProjeto;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  Dext,
  Dext.Web,
  MeuProjeto.Startup in 'MeuProjeto.Startup.pas';

var
  App: IWebApplication;
  Provider: IServiceProvider;
begin
  SetConsoleCharSet;
  try
    App := WebApplication;
    App.UseStartup(TStartup.Create);
    Provider := App.BuildServices;
    App.Run(9000);
  except
    on E: Exception do
      WriteLn('Fatal: ' + E.Message);
  end;
  ConsolePause;
end.
```

> [!WARNING]
> **Erros Comuns**:
> - ❌ `var App := WebApplication;` — O compilador pode inferir a classe concreta em vez da interface, causando problemas de ARC no shutdown.
> - ❌ Declarar `Provider: IServiceProvider` sem `Dext` no uses → `E2003 Undeclared identifier`.
> - ❌ Esquecer `SetConsoleCharSet` → saída UTF-8 quebrada.

## Configuração Fluente (Padrão Obrigatório)

Tanto o registro de serviços quanto o pipeline de middleware **DEVEM** usar chamadas fluentes (encadeadas):

```pascal
// ✅ CORRETO: Encadeamento fluente
Services
  .AddDbContext<TMyContext>(ConfigureDatabase)
  .AddScoped<IUserService, TUserService>
  .AddSingleton<ICache, TMemoryCache>;

App.Builder
  .UseExceptionHandler
  .UseHttpLogging
  .UseCors(CorsOptions.AllowAnyOrigin)
  .MapEndpoints(TMyEndpoints.MapEndpoints)
  .UseSwagger(Swagger.Title('Minha API').Version('v1'));

// ❌ ERRADO: Variáveis intermediárias
var Builder := App.Builder;        // Evite isso
Builder.UseExceptionHandler;       // Quebra o padrão fluente
Builder.UseHttpLogging;
```

## Modelos de Execução: Run vs Start

| Método | Comportamento | Caso de Uso |
| :--- | :--- | :--- |
| **`Run`** | Bloqueia a thread atual até ser parado (Ctrl+C). | **Apps Console**, Services, Daemons. |
| **`Start`** | Não bloqueia (Non-blocking). Inicia o servidor e retorna imediatamente. | **Apps GUI (VCL/FMX)** como ferramentas de bandeja (Tray). |

### Exemplo: Aplicação GUI (Sidecar)

Em uma aplicação VCL, você deve usar `Start` para evitar congelar o formulário principal:

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  FApp := WebApplication;
  FApp.UseStartup(TStartup.Create);
  FProvider := FApp.BuildServices;
  FApp.Start;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FApp.Stop; // Encerramento seguro (Graceful shutdown)
end;
```

## Seed de Dados

Faça o seed dos dados no `.dpr` principal **antes** de `App.Run`:

```pascal
var
  App: IWebApplication;
  Provider: IServiceProvider;
begin
  SetConsoleCharSet;
  try
    App := WebApplication;
    App.UseStartup(TStartup.Create);
    Provider := App.BuildServices;

    // Seed ANTES de rodar
    TDbSeeder.Seed(Provider);

    App.Run(9000);
  except
    on E: Exception do
      WriteLn('Fatal: ' + E.Message);
  end;
  ConsolePause;
end.
```

### Padrão Seeder

```pascal
class procedure TDbSeeder.Seed(const Provider: IServiceProvider);
begin
  var Scope := Provider.CreateScope;
  try
    var Db := Scope.ServiceProvider.GetService(TAppDbContext) as TAppDbContext;

    if Db.EnsureCreated then // Retorna True se o schema foi criado
    begin
      // Use .Any para verificar existência sem carregar todos os registros
      if not Db.Users.QueryAll.Any then
      begin
        var Admin := TUser.Create;
        Admin.Name := 'Admin';
        Db.Users.Add(Admin);
        Db.SaveChanges;
        // SaveChanges popula Admin.Id (AutoInc) automaticamente
      end;
    end;
  finally
    Scope := nil; // Libera todos os serviços scoped
  end;
end;
```

> [!WARNING]
> **Aviso SQLite :memory:**: NUNCA chame `BuildServiceProvider` manualmente dentro do Seeder. Isso cria um *novo* container e um *novo* banco em memória vazio.

## Avançado: Separação de Responsabilidades (Endpoints & Auth)

Para projetos maiores, separe seu código em módulos dedicados:

### Criando um Módulo de Endpoints

```pascal
unit MeuProjeto.Endpoints;

interface

uses
  Dext.Web; // TAppBuilder, IResult, Results

type
  TMeusEndpoints = class
  public
    class procedure MapEndpoints(const Builder: TAppBuilder); static;
  end;

implementation

uses
  MeuProjeto.Auth,
  MeuProjeto.Data.Context;

class procedure TMeusEndpoints.MapEndpoints(const Builder: TAppBuilder);
begin
  // Health Check
  Builder.MapGet<IResult>('/health',
    function: IResult
    begin
      Result := Results.Ok('healthy');
    end);

  // Auth - Login com DI + Model Binding
  Builder.MapPost<TLoginRequest, IAuthService, IResult>('/api/auth/login',
    function(Req: TLoginRequest; Auth: IAuthService): IResult
    begin
      Result := Results.Ok(Auth.Login(Req));
    end);
end;

end.
```

> [!IMPORTANT]
> O tipo do parâmetro para `MapEndpoints` é `TAppBuilder` (de `Dext.Web`), **não** `IApplicationBuilder`.

### Criando um Módulo de Autenticação

```pascal
unit MeuProjeto.Auth;

interface

uses
  Dext.Web, Dext.Auth.JWT;

type
  TLoginRequest = record
    username: string;
    password: string;
  end;

  TLoginResponse = record
    token: string;
  end;

  IAuthService = interface
    ['{...}']
    function Login(const Req: TLoginRequest): TLoginResponse;
  end;

  TAuthConfig = class
  public
    const JWT_SECRET = 'sua-chave-secreta-minimo-32-chars';
    const JWT_ISSUER = 'MeuApp';
  end;
```

### Benefícios

- **Responsabilidade Única**: Cada arquivo tem um propósito claro
- **Testabilidade**: Endpoints podem ser testados independentemente
- **Escalabilidade**: Fácil adicionar novos grupos de endpoints
- **Manutenibilidade**: Mudanças de configuração não afetam lógica de negócio

> 📦 **Referências**: 
> - [Web.EventHub](../../../Examples/Web.EventHub/) - Padrões modernos 2026
> - [Web.TicketSales](../../../Examples/Web.TicketSales/) - Padrão Ouro (Controllers + JWT + ORM)
> - [Web.SalesSystem](../../../Examples/Web.SalesSystem/) - Minimal APIs + CQRS

---

[← Estrutura do Projeto](estrutura-projeto.md) | [Próximo: Framework Web →](../02-framework-web/README.md)
