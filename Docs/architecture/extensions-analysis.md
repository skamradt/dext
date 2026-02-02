# Análise de Extensions - Dext Framework
## Status: ✅ CONCLUÍDO

Data: 2026-02-01 (Atualizado)

## Contexto

Com a evolução do framework Dext, especialmente a criação do `TDextServices` (record fluente) e a refatoração por packages, muitas extensions criadas no início do projeto se tornaram redundantes ou obsoletas.

Este documento analisa cada unit de Extensions e propõe sua categorização.

---

## Resumo das Extensions

| Unit | Status | Motivo |
|------|--------|--------|
| `Dext.Core.Extensions.pas` | ✅ MANTER | Extensões de tipos básicos (strings, arrays) |
| `Dext.Collections.Extensions.pas` | ✅ MANTER | Extensões de coleções |
| `Dext.DI.Extensions.pas` | ⚠️ DEPRECATED | Substituído por `TDextServices` |
| `Dext.DI.Extensions.utf8.pas` | ❌ REMOVER | Duplicata com encoding diferente |
| `Dext.Logging.Extensions.pas` | ✅ MANTER | Contém `AddLogging` - essencial |
| `Dext.Options.Extensions.pas` | ✅ MANTER | Configuração de options |
| `Dext.Web.Hubs.Extensions.pas` | ✅ MANTER | SignalR/Hubs |
| `Dext.OpenAPI.Extensions.pas` | ✅ MANTER | Integração OpenAPI/Swagger |
| `Dext.Web.Extensions.pas` | ✅ MANTER | Helpers Web |
| `Dext.Web.Middleware.Extensions.pas` | ✅ MANTER | Configuração de middlewares |
| `Dext.Web.ApplicationBuilder.Extensions.pas` | ✅ **PADRÃO ATUAL** | `MapGet<T>`, `MapPost<T, Service>` com DI automático |
| `Dext.Web.ModelBinding.Extensions.pas` | ⚠️ DEPRECATED | **CAUSAVA MEMORY LEAKS**, substituído por `TApplicationBuilderExtensions` |

---

## Classes Marcadas como Deprecated

### `Dext.DI.Extensions.pas`
```pascal
[Deprecated('Use TDextServices instead')]
TServiceCollectionExtensions = class

[Deprecated('Use IServiceProvider.GetService<T> instead')]
TServiceProviderExtensions = class
```

### `Dext.Web.ModelBinding.Extensions.pas`
```pascal
[Deprecated('Use TApplicationBuilderExtensions.MapPost<T> instead')]
TApplicationBuilderWithModelBinding = class

[Deprecated('Use TApplicationBuilderExtensions instead')]
TApplicationBuilderModelBindingExtensions = class
```

---

## Guia de Migração

### DI: De `TServiceCollectionExtensions` para `TDextServices`

**Antes (deprecated):**
```pascal
TServiceCollectionExtensions.AddSingleton<IUserService, TUserService>(Services);
```

**Depois (recomendado):**
```pascal
var Svc := TDextServices.Create(Services);
Svc.AddSingleton<IUserService, TUserService>;
```

### Web: De `TApplicationBuilderModelBindingExtensions` para `TApplicationBuilderExtensions`

**Antes (deprecated - causa memory leaks):**
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

**Depois (recomendado - sem memory leaks, DI automático):**
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

## Checklist de Migração

- [x] Marcar `TServiceCollectionExtensions` como deprecated
- [x] Marcar `TServiceProviderExtensions` como deprecated
- [x] Marcar `TApplicationBuilderWithModelBinding` como deprecated
- [x] Marcar `TApplicationBuilderModelBindingExtensions` como deprecated
- [x] Refatorar `Dext.Json.Test.pas` para usar padrões novos
- [ ] Verificar outros exemplos que usam extensions obsoletas
- [ ] Atualizar documentação principal

---

## Benefícios da Migração

1. **Sem Memory Leaks**: O padrão antigo criava ciclos de referência
2. **Menos Código**: DI automático elimina resolução manual de serviços
3. **Tipo Seguro**: Parâmetros genéricos garantem tipos corretos em compile-time
4. **Consistente**: Um único padrão para DI em todo o framework
