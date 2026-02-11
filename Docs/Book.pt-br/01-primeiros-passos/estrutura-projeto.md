# Estrutura do Projeto

Um projeto Dext típico segue esta estrutura:

## Layout Recomendado

```
MeuProjeto/
├── Server/
│   ├── MeuProjeto.Startup.pas      # Somente configuração (IStartup)
│   ├── MeuProjeto.Endpoints.pas    # Definição de todas as rotas
│   ├── MeuProjeto.Auth.pas         # Serviços de autenticação & DTOs
│   └── Web.MeuProjeto.dpr          # Ponto de entrada (Entry point)
├── Domain/
│   ├── MeuProjeto.Domain.Entities.pas
│   ├── MeuProjeto.Domain.Models.pas
│   └── MeuProjeto.Domain.Enums.pas
├── Data/
│   ├── MeuProjeto.Data.Context.pas
│   └── MeuProjeto.Data.Seeder.pas
└── Tests/
    ├── MeuProjeto.Tests.dpr         # Test runner
    └── MeuProjeto.Tests.pas         # Test fixtures
```

## Template do Programa Principal (.dpr)

> [!IMPORTANT]
> Todo projeto console (Web APIs, Test Runners) **DEVE** seguir estas regras:
> 1. Adicionar `Dext.Utils` ao uses
> 2. Chamar `SetConsoleCharSet;` como a **primeira** instrução
> 3. Chamar `ConsolePause;` como a **última** instrução
> 4. Declarar variáveis de interface (`IWebApplication`, `IServiceProvider`) com **tipos explícitos** no bloco `var`

```pascal
program Web.MeuProjeto;

{$APPTYPE CONSOLE}

uses
  Dext.MM,            // Opcional: Wrapper FastMM5 para detecção de leaks
  Dext.Utils,         // SetConsoleCharSet, ConsolePause
  System.SysUtils,
  // Facades do Dext (SEMPRE POR ÚLTIMO no grupo Dext)
  Dext,               // IServiceProvider, IConfiguration
  Dext.Web,           // IWebApplication, WebApplication
  // Units do projeto
  MeuProjeto.Startup in 'MeuProjeto.Startup.pas';

var
  App: IWebApplication;
  Provider: IServiceProvider;
begin
  SetConsoleCharSet;   // 1. Configura UTF-8 para emojis e acentos
  try
    App := WebApplication;
    App.UseStartup(TStartup.Create);

    // Construir serviços e popular banco ANTES de rodar
    Provider := App.BuildServices;
    TDbSeeder.Seed(Provider);

    App.Run(9000);
  except
    on E: Exception do
      WriteLn('Fatal: ' + E.Message);
  end;
  ConsolePause;        // 2. Pausa (apenas quando rodando via IDE)
end.
```

> [!WARNING]
> **Segurança de Memória**: Sempre declare `App` e `Provider` como variáveis de interface **tipadas explicitamente** no bloco `var` (não `var` inline). Isso garante que o ARC libere as interfaces na ordem correta durante o shutdown, evitando Access Violations.

## Ordem da Cláusula Uses

O Dext utiliza **Facade Units** com Record Helpers. Como o Delphi aplica apenas o último helper declarado, as fachadas **devem** vir por último:

```pascal
uses
  // 1. Units do Sistema Delphi
  System.SysUtils,
  System.Classes,
  // 2. Units de Terceiros
  // ...
  // 3. Units Especializadas do Dext (alfabética)
  Dext.Auth.JWT,
  Dext.Entity.Core,      // Obrigatório para IDbSet<T>
  Dext.Types.Nullable,   // Obrigatório para Nullable<T>
  // 4. Facades do Dext (SEMPRE POR ÚLTIMO no grupo Dext)
  Dext,                  // Facade Core
  Dext.Entity,           // Facade ORM
  Dext.Web,              // Facade Web
  // 5. Units do Projeto (alfabética)
  MeuProjeto.Data.Context,
  MeuProjeto.Domain.Entities;
```

> [!IMPORTANT]
> **Tipos Genéricos** (`IDbSet<T>`, `Nullable<T>`, `Lazy<T>`, `Prop<T>`) **NÃO** são reexportados pelas fachadas.  
> Você **DEVE** adicionar a unit original (`Dext.Entity.Core`, `Dext.Types.Nullable`, etc.) ao uses.

## Arquivo de Configuração

`appsettings.json`:

```json
{
  "Database": {
    "Provider": "SQLite",
    "ConnectionString": "App.db"
  },
  "Jwt": {
    "SecretKey": "sua-chave-secreta-minimo-32-chars",
    "ExpirationMinutes": 60
  }
}
```

Carregue com:

```pascal
// Em ConfigureServices, use o parâmetro IConfiguration
procedure TStartup.ConfigureServices(const Services: TDextServices; const Configuration: IConfiguration);
begin
  var DbConn := Configuration.GetValue('Database:ConnectionString');
  // ...
end;
```

## Search Paths (.dproj)

> [!WARNING]
> **NUNCA** aponte os search paths para o diretório `Sources` do framework. Sempre use os DCUs compilados do diretório `Output`. Misturar fontes causa o erro fatal "Unit was compiled with a different version".

```xml
<DCC_ExeOutput>..\..\Output\</DCC_ExeOutput>
<DCC_DcuOutput>..\..\Output\$(ProductVersion)_$(Platform)_$(Config)</DCC_DcuOutput>
<DCC_UnitSearchPath>..\..\..\Output\$(ProductVersion)_$(Platform)_$(Config);$(DCC_UnitSearchPath)</DCC_UnitSearchPath>
```

---

[← Hello World](hello-world.md) | [Próximo: Inicialização da Aplicação →](inicializacao-aplicacao.md)
