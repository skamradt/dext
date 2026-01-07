# Estrutura do Projeto

Um projeto Dext típico segue esta estrutura:

## Layout Recomendado

```
MeuProjeto/
├── MeuProjeto.dpr              # Programa principal
├── MeuProjeto.dproj            # Arquivo de projeto Delphi
├── src/
│   ├── Controllers/            # Classes de controllers
│   │   └── UsersController.pas
│   ├── Entities/               # Classes de entidades ORM
│   │   └── User.pas
│   ├── Services/               # Lógica de negócios
│   │   └── UserService.pas
│   └── Middleware/             # Middleware customizado
│       └── LoggingMiddleware.pas
├── config/
│   └── appsettings.json        # Configuração
├── migrations/                 # Migrations de banco
│   └── 001_CreateUsers.pas
└── tests/
    └── UserServiceTests.pas
```

## Template do Programa Principal

```pascal
program MeuProjeto;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Dext.Web,
  Dext.DependencyInjection,
  Dext.Entity,
  // Suas units
  UsersController in 'src\Controllers\UsersController.pas',
  UserService in 'src\Services\UserService.pas',
  User in 'src\Entities\User.pas';

begin
  TWebHostBuilder.CreateDefault(nil)
    .UseUrls('http://localhost:5000')
    .ConfigureServices(procedure(Services: IServiceCollection)
      begin
        // Registrar serviços
        Services.AddScoped<IUserService, TUserService>;
        
        // Registrar DbContext
        Services.AddDbContext<TAppDbContext>;
      end)
    .Configure(procedure(App: IApplicationBuilder)
      begin
        // Middleware
        App.UseExceptionHandler;
        App.UseCors;
        
        // Controllers
        App.MapController<TUsersController>;
        
        // Ou Minimal APIs
        App.MapGet('/health', procedure(Ctx: IHttpContext)
          begin
            Ctx.Response.Json('{"status": "saudável"}');
          end);
      end)
    .Build
    .Run;
end.
```

## Arquivo de Configuração

`config/appsettings.json`:

```json
{
  "Database": {
    "Provider": "PostgreSQL",
    "ConnectionString": "Server=localhost;Database=meuapp;User=postgres;Password=segredo"
  },
  "Jwt": {
    "SecretKey": "sua-chave-secreta-aqui",
    "ExpirationMinutes": 60
  }
}
```

Carregue com:

```pascal
var
  Config: IConfiguration;
begin
  Config := TConfigurationBuilder.Create
    .AddJsonFile('config/appsettings.json')
    .Build;
    
  var DbConnection := Config.GetValue('Database:ConnectionString');
end;
```

---

[← Hello World](hello-world.md) | [Próximo: Framework Web →](../02-framework-web/README.md)
