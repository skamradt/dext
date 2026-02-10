# RFC-001: JSON API Cleanup & Simplification

**Data:** 2026-01-31  
**Status:** Em Implementação  
**Autor:** Cesar Romero  
**Breaking Change:** Sim (com deprecated fallback)

---

## Resumo

Esta RFC propõe a simplificação da API de JSON do Dext Framework, removendo prefixos verbosos e criando funções globais auxiliares para uma sintaxe mais limpa e moderna.

## Motivação

A sintaxe atual é verbosa e não segue o padrão de design que estamos estabelecendo no framework:

```pascal
// Atual (verboso)
TDextJson.SetDefaultSettings(TDextSettings.Default.WithCamelCase.WithCaseInsensitive);
```

Compare com a nova API de CORS e Swagger:
```pascal
// Novo padrão (limpo)
App.Builder.UseCors(Cors.AllowAnyOrigin.AllowAnyMethod);
App.Builder.UseSwagger(Swagger.Title('API').Version('v1'));
```

## Proposta

### 1. Renomeação de Tipos

| Atual | Proposta | Justificativa |
|-------|----------|---------------|
| `TDextSettings` | `TJsonSettings` | Mais específico para JSON |
| `TDextCaseStyle` | `TCaseStyle` | Remover prefixo Dext desnecessário |
| `TDextEnumStyle` | `TEnumStyle` | Remover prefixo Dext |
| `TDextFormatting` | `TJsonFormatting` | Mais específico |
| `TDextDateFormat` | `TDateFormat` | Remover prefixo Dext |

### 2. Função Global `JsonSettings`

```pascal
// Nova função global (sem .Default)
function JsonSettings: TJsonSettings;

// Uso
TDextJson.SetDefaultSettings(JsonSettings.CamelCase.CaseInsensitive);
```

### 3. Métodos Fluentes (sem prefixo `With`)

| Atual (deprecated) | Novo |
|--------------------|------|
| `WithCamelCase` | `CamelCase` |
| `WithPascalCase` | `PascalCase` |
| `WithSnakeCase` | `SnakeCase` |
| `WithEnumAsString` | `EnumAsString` |
| `WithEnumAsNumber` | `EnumAsNumber` |
| `WithIgnoreNullValues` | `IgnoreNullValues` |
| `WithCaseInsensitive` | `CaseInsensitive` |
| `WithISODateFormat` | `ISODateFormat` |
| `WithUnixTimestamp` | `UnixTimestamp` |
| `WithCustomDateFormat` | `CustomDateFormat` |

### 4. API Final

```pascal
// ANTES
TDextJson.SetDefaultSettings(TDextSettings.Default.WithCamelCase.WithCaseInsensitive);

// DEPOIS
TDextJson.SetDefaultSettings(JsonSettings.CamelCase.CaseInsensitive);
```

## Estratégia de Migração

1. **Manter compatibilidade**: Todos os símbolos antigos serão mantidos como `deprecated`
2. **Aliases de tipo**: `TDextSettings = TJsonSettings deprecated 'Use TJsonSettings'`
3. **Métodos deprecated**: `function WithCamelCase: TJsonSettings; deprecated 'Use CamelCase'`

## Arquivos Afetados

### Core
- [x] `Dext.Json.pas` - Refatoração principal
- [ ] `Dext.pas` - Atualizar aliases no facade

### Web
- [x] `Dext.Web.Cors.pas` - Já atualizado (TCorsBuilder)
- [x] `Dext.OpenAPI.Generator.pas` - Já atualizado (TOpenAPIBuilder)

### Examples
- [x] `DextFood.Startup.pas` - Atualizar uso

### Docs
- [ ] `SKILL.md` - Atualizar documentação
- [ ] DextBook - Revisar capítulos de JSON

### Tests
- [ ] Revisar testes de JSON para usar nova API

## Cronograma

1. ✅ **Fase 1** (2026-01-31): CORS e Swagger builders
2. 🔄 **Fase 2** (2026-01-31): JSON settings refactoring
3. ⏳ **Fase 3**: Documentação e testes

## Changelog

### 2026-01-31
- [x] `TCorsBuilder` convertido para record com API fluente
- [x] Função global `Cors` criada
- [x] Métodos `With*` deprecated no `TCorsBuilder`
- [x] `TOpenAPIBuilder` criado como record
- [x] Função global `Swagger` criada
- [x] Padrão `var Builder := App.Builder` documentado
- [x] `TDextSettings` → `TJsonSettings` refactoring
- [x] Função global `JsonSettings` criada
- [x] Enums renomeados: `TCaseStyle`, `TEnumStyle`, `TJsonFormatting`, `TDateFormat`
- [x] Métodos `With*` deprecated em `TJsonSettings`
- [x] `DextFood.Startup.pas` atualizado com nova API
- [x] `SKILL.md` atualizado com nova documentação

---

## Aprovação

- [x] Cesar Romero - Aprovado
