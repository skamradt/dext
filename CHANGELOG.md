# 📢 Novidades do Dext Framework / Dext Framework News

> **[PT-BR]** Este documento contém as últimas novidades, breaking changes e novas features do Dext Framework. As atualizações mais recentes aparecem primeiro.
>
> **[EN]** This document contains the latest news, breaking changes and new features of the Dext Framework. Most recent updates appear first.

---

## 2026-02-05 - Dext.Entity: Many-to-Many & Full Attribute Suite

### ✨ Nova Feature / New Feature

#### Many-to-Many Relationships & WebSalesSystem Support
**[PT-BR]** Implementação motivada por limitações de relacionamento identificadas no novo projeto `WebSalesSystem`. Suporte completo a relacionamentos Muitos-para-Muitos via atributo `[ManyToMany]`. Gerenciamento automático de tabelas de ligação, suporte a Lazy Loading e Eager Loading (`Include`). Novos métodos `LinkManyToMany`, `UnlinkManyToMany` e `SyncManyToMany` adicionados ao `IDbSet<T>`.

**[EN]** Implementation driven by relationship limitations identified in the new `WebSalesSystem` project. Full support for Many-to-Many relationships via `[ManyToMany]` attribute. Automatic join table management, support for Lazy and Eager loading (`Include`). New methods `LinkManyToMany`, `UnlinkManyToMany`, and `SyncManyToMany` added to `IDbSet<T>`.

#### Full Attribute Suite
**[PT-BR]** Expansão do mapeamento para suportar os requisitos de modelagem do `WebSalesSystem`:
- `[SoftDelete]`: Filtro automático e deleção lógica.
- `[Version]`: Controle de concorrência otimista.
- `[CreatedAt]` / `[UpdatedAt]`: Auditoria automática de timestamps.
- `[JsonColumn]`: Armazenamento de objetos e listas como JSON.
- `[DbType]`, `[Precision]`, `[MaxLength]`: Controle refinado de tipos e constraints.

**[EN]** Mapping expansion to support `WebSalesSystem` modeling requirements:
- `[SoftDelete]`: Automatic filtering and logical deletion.
- `[Version]`: Optimistic concurrency control.
- `[CreatedAt]` / `[UpdatedAt]`: Automatic timestamp auditing.
- `[JsonColumn]`: Storage of objects and lists as JSON.
- `[DbType]`, `[Precision]`, `[MaxLength]`: Refined control over types and constraints.

### 🐛 Bug Fixes

- **WebSalesSystem List Deserialization**: Resolvido `EBindingException` na desserialização de `IList<T>` em DTOs complexos. Implementado fallback para `TSmartList<T>` no `TActivator` quando fábricas de coleções não são encontradas.
- **Lazy Loading Memory Leak**: Correção de `Invalid pointer operation` causado por dupla liberação de entidades. Agora o `LazyLoader` respeita o ciclo de vida do `DbContext` (`OwnsObjects := False`).
- **FireDAC Params**: Resolvido erro de "Parameter not found" causado por limpeza incorreta de definições de parâmetros em reuso de comandos.
- **M2M Index**: Corrigido erro de índice (off-by-one) na recuperação de IDs da tabela de ligação.

---

## 2026-02-01 - Zero-Leak Architecture & Attribute Revamp

### ⚠️ Breaking Changes & Modernization

#### TDextServices Refactoring
**[PT-BR]** `TDextServices` e os Builders (`AddHealthChecks`, `AddBackgroundServices`) agora são **Records**. Não é mais necessário (nem possível) chamar `.Free`. Isso elimina os memory leaks causados por capturas de ciclos em closures.

**[EN]** `TDextServices` and Builders (`AddHealthChecks`, `AddBackgroundServices`) are now **Records**. It is no longer necessary (nor possible) to call `.Free`. This eliminates memory leaks caused by cycle captures in closures.

#### New Attribute Names (Parity with .NET)

| Antes / Before | Depois / After |
|----------------|----------------|
| `[Controller]` | `[ApiController]` |
| `[Get]` | `[HttpGet]` |
| `[Post]` | `[HttpPost]` |
| `[Put]` | `[HttpPut]` |
| `[Delete]` | `[HttpDelete]` |
| `[Patch]` | `[HttpPatch]` |

**[PT-BR]** Os atributos antigos continuam funcionando mas estão **deprecated**. Use preferred names para melhor compatibilidade com o ecossistema .NET. O novo atributo `[Route]` agora é suportado na classe para prefixos de rota.

**[EN]** Old attributes still work but are **deprecated**. Use preferred names for better compatibility with the .NET ecosystem. The new `[Route]` attribute is now supported at the class level for route prefixes.

**Novo Exemplo / New Example:**
```pascal
[ApiController]
[Route('/api/orders')]
TOrdersController = class
  [HttpGet]
  procedure GetAll(Ctx: IHttpContext);
  
  [HttpPost('{id}/cancel')]
  procedure Cancel(Ctx: IHttpContext; [FromRoute] Id: string);
end;
```

#### Deprecated Extensions (Memory Leak Fixes)

**[PT-BR]** As seguintes classes foram marcadas como **deprecated** por causarem memory leaks ou serem redundantes com a nova API `TDextServices`:

**[EN]** The following classes have been marked as **deprecated** because they caused memory leaks or are redundant with the new `TDextServices` API:

| Classe Deprecated | Substituição / Replacement |
|-------------------|----------------------------|
| `TServiceCollectionExtensions` | `TDextServices` |
| `TServiceProviderExtensions` | `IServiceProvider.GetService<T>` |
| `TApplicationBuilderModelBindingExtensions` | `TApplicationBuilderExtensions` |
| `TApplicationBuilderWithModelBinding` | `TApplicationBuilderExtensions.MapPost<T>` |

**Antes / Before (memory leak):**
```pascal
TApplicationBuilderModelBindingExtensions
  .WithModelBinding(App)
  .MapPost<TUserRequest>('/api/users',
    procedure(Req: TUserRequest)
    var UserService: IUserIntegrationService;
    begin
      UserService := TServiceProviderExtensions.GetService<IUserIntegrationService>(App.GetServiceProvider);
      UserService.ProcessUser(Req);
    end
  );
```

**Depois / After (sem leak, DI automático):**
```pascal
TApplicationBuilderExtensions.MapPost<TUserRequest, IUserIntegrationService>(App, '/api/users',
  procedure(Req: TUserRequest; UserService: IUserIntegrationService)
  begin
    // Service injetado automaticamente!
    UserService.ProcessUser(Req);
  end
);
```

---

## 2026-01-31 - API Cleanup: JSON, CORS & Swagger

### ⚠️ Breaking Changes (com compatibilidade / with backward compatibility)

Os tipos e métodos antigos foram marcados como `deprecated` e continuarão funcionando. Recomendamos migrar para a nova API.

**The old types and methods have been marked as `deprecated` and will continue to work. We recommend migrating to the new API.**

#### JSON Settings

| Antes / Before | Depois / After |
|----------------|----------------|
| `TDextSettings` | `TJsonSettings` |
| `TDextCaseStyle` | `TCaseStyle` |
| `TDextEnumStyle` | `TEnumStyle` |
| `TDextFormatting` | `TJsonFormatting` |
| `TDextDateFormat` | `TDateFormat` |
| `.WithCamelCase` | `.CamelCase` |
| `.WithCaseInsensitive` | `.CaseInsensitive` |
| `.WithEnumAsString` | `.EnumAsString` |

**Sintaxe antiga / Old syntax:**
```pascal
TDextJson.SetDefaultSettings(TDextSettings.Default.WithCamelCase.WithCaseInsensitive);
```

**Sintaxe nova / New syntax:**
```pascal
DefaultJsonSettings(JsonSettings.CamelCase.CaseInsensitive);
```

#### CORS Configuration

| Antes / Before | Depois / After |
|----------------|----------------|
| `.WithOrigins(...)` | `.Origins(...)` |
| `.WithMethods(...)` | `.Methods(...)` |
| `.WithHeaders(...)` | `.Headers(...)` |
| `TCorsBuilder.Create...` | `Cors...` |

**Sintaxe antiga / Old syntax:**
```pascal
App.Builder.UseCors(
  procedure(Builder: TCorsBuilder)
  begin
    Builder.WithAllowAnyOrigin.WithAllowAnyMethod;
  end);
```

**Sintaxe nova / New syntax:**
```pascal
Builder.UseCors(Cors.AllowAnyOrigin.AllowAnyMethod.AllowAnyHeader);
```

#### Swagger Configuration

| Antes / Before | Depois / After |
|----------------|----------------|
| `.WithTitle(...)` | `.Title(...)` |
| `.WithVersion(...)` | `.Version(...)` |
| `.WithDescription(...)` | `.Description(...)` |
| `TOpenAPIBuilder.Create...` | `Swagger...` |

**Sintaxe antiga / Old syntax:**
```pascal
var SwaggerOpts := TOpenAPIBuilder.Create;
SwaggerOpts.WithTitle('My API');
SwaggerOpts.WithVersion('v1');
App.Builder.UseSwagger(SwaggerOpts);
```

**Sintaxe nova / New syntax:**
```pascal
Builder.UseSwagger(Swagger.Title('My API').Version('v1'));
```

#### Controller Attributes

| Antes / Before | Depois / After |
|----------------|----------------|
| `[DextController('/api')]` | `[Route('/api')]` ou `[Controller('/api')]` |
| `[DextGet('')]` | `[Get('')]` |
| `[DextPost('')]` | `[Post('')]` |
| `[DextPut('/{id}')]` | `[Put('/{id}')]` |
| `[DextDelete('/{id}')]` | `[Delete('/{id}')]` |
| `[DextPatch('/{id}')]` | `[Patch('/{id}')]` |
| `EDextHttpException` | `HttpException` |

#### Web Application & Hosting

| Antes / Before | Depois / After |
|----------------|----------------|
| `TDextApplication` | `TWebApplication` |
| `TDextAppBuilder` | `AppBuilder` |
| `TDextWebHost` | `WebHost` |
| `TWebApplication.Create` | `WebApplication` (Global Function) |

**Sintaxe antiga / Old syntax:**
```pascal
[DextController('/api/orders')]
TOrdersController = class
  [DextGet('')]
  procedure GetAll(Ctx: IHttpContext);
  
  [DextPost('')]
  procedure Create(Ctx: IHttpContext; Request: TCreateOrderRequest);
end;
```

**Sintaxe nova / New syntax:**
```pascal
[Route('/api/orders')]
TOrdersController = class
  [Get('')]
  procedure GetAll(Ctx: IHttpContext);
  
  [Post('')]
  procedure Create(Ctx: IHttpContext; Request: TCreateOrderRequest);
end;
```

### ✨ Novas Features / New Features

1. **Função global `JsonSettings`**: Retorna um `TJsonSettings` padrão para configuração fluente.
   
   **Global function `JsonSettings`**: Returns a default `TJsonSettings` for fluent configuration.

2. **Procedure `DefaultJsonSettings`**: Atalho para `TDextJson.SetDefaultSettings`.
   
   **Procedure `DefaultJsonSettings`**: Shorthand for `TDextJson.SetDefaultSettings`.

3. **Função global `Cors`**: Cria um `TCorsBuilder` para configuração fluente.
   
   **Global function `Cors`**: Creates a `TCorsBuilder` for fluent configuration.

4. **Função global `Swagger`**: Cria um `TOpenAPIBuilder` para configuração fluente.
   
   **Global function `Swagger`**: Creates a `TOpenAPIBuilder` for fluent configuration.

5. **Função global `WebApplication`**: Atalho para `TWebApplication.Create`.

   **Global function `WebApplication`**: Shorthand for `TWebApplication.Create`.

6. **Modulariedade (DEXT_ENABLE_ENTITY)**: Agora é possível desativar a dependência do ORM/Banco de dados globalmente no `Dext.inc` ao comentar a diretiva `{$DEFINE DEXT_ENABLE_ENTITY}`. Isso reduz o tamanho do binário para projetos que não utilizam o ORM.

   **Modularity (DEXT_ENABLE_ENTITY)**: It is now possible to disable ORM/Database dependency globally in `Dext.inc` by commenting the `{$DEFINE DEXT_ENABLE_ENTITY}` directive. This reduces binary size for projects not using the ORM.

7. **Regra de Ordem de Importação ("Last Helper Wins")**: Para garantir que todos os métodos fluentes (Core + Entity + Web) estejam disponíveis no `TDextServices`, as units devem ser importadas na ordem específica: `Dext, Dext.Entity, Dext.Web`.

   **Unit Order Rule ("Last Helper Wins")**: To ensure all fluent methods (Core + Entity + Web) are available in `TDextServices`, units must be imported in a specific order: `Dext, Dext.Entity, Dext.Web`.

8. **Padrão `var Builder`**: Novo padrão recomendado no `TStartup.Configure`:

```pascal
procedure TStartup.Configure(const App: IWebApplication);
begin
  var Builder := App.Builder;
  
  DefaultJsonSettings(JsonSettings.CamelCase.CaseInsensitive);
  
  Builder
    .UseExceptionHandler
    .UseHttpLogging;
    
  Builder.UseCors(Cors.AllowAnyOrigin.AllowAnyMethod.AllowAnyHeader);
  
  Builder.MapControllers;
  
  Builder.UseSwagger(Swagger.Title('My API').Version('v1'));
end;
```

### 📄 Documentação / Documentation

- RFC-001 criado em `Docs/RFC/RFC-001-JSON-API-CLEANUP.md`
- RFC-002 criado em `Docs/RFC/RFC-002-DEXT-WEB-API-CLEANUP.md`
- SKILL.md atualizado com novas convenções
- Exemplo `DextFood.Startup.pas` atualizado

---

## 2026-01-30 - Dext.Entity: FireDAC Transaction Fix

### 🐛 Bug Fix

**Correção crítica no driver FireDAC**: Transações explícitas agora funcionam corretamente.

**Critical fix in FireDAC driver**: Explicit transactions now work correctly.

O construtor `TFireDACTransaction.Create` agora vincula corretamente a transação ao `Connection.Transaction` e `Connection.UpdateTransaction`, garantindo que os comandos SQL respeitem a transação ativa.

---

## 2026-01-28 - Dext.Net.RestClient

### ✨ Nova Feature

**Dext Rest Client**: Novo cliente HTTP moderno para Delphi com:

- API fluente e intuitiva
- Suporte a async/await com Promises
- Serialização JSON integrada
- Interceptadores de request/response
- Retry policies

```pascal
var Response := RestClient
  .BaseUrl('https://api.example.com')
  .Get('/users')
  .Execute;
```

---

## 2026-01-21 - Dext.UI Navigator Framework

### ✨ Nova Feature

**Navigator Framework**: Sistema de navegação para aplicações MVVM Desktop.

- `INavigator` interface para navegação entre views
- `TSimpleNavigator` implementação leve
- Integração com DI Container
- Gerenciamento automático de lifecycle de ViewModels

```pascal
Navigator.NavigateTo<TCustomerEditViewModel>(
  procedure(VM: TCustomerEditViewModel)
  begin
    VM.LoadCustomer(CustomerId);
  end);
```

---

## 2026-01-15 - Dext.Entity Smart Properties

### ✨ Nova Feature

**Smart Properties (Prototype Pattern)**: Consultas LINQ-like com propriedades tipadas.

```pascal
var Customer := Prototype.Entity<TCustomer>;
var List := Db.Customers
  .Where(Customer.Active = True)
  .Where(Customer.City = 'São Paulo')
  .OrderBy(Customer.Name)
  .ToList;
```

---

## Como Contribuir / How to Contribute

Se você encontrar bugs ou tiver sugestões, por favor abra uma issue no GitHub.

**If you find bugs or have suggestions, please open an issue on GitHub.**

