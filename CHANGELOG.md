# 📢 Novidades do Dext Framework / Dext Framework News

> **[PT-BR]** Este documento contém as últimas novidades, breaking changes e novas features do Dext Framework. As atualizações mais recentes aparecem primeiro.
>
> **[EN]** This document contains the latest news, breaking changes and new features of the Dext Framework. Most recent updates appear first.

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

