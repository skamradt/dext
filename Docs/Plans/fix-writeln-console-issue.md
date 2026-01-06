# Plano: Eliminar Chamadas WriteLn no Framework Dext

## 📋 Problema

Quando o framework Dext é utilizado em aplicações **VCL/FMX** (não-console), as chamadas diretas a `WriteLn` causam o erro **I/O error 105** ("File not open for output"), pois não existe console disponível.

## 🔍 Análise do Impacto

Foram encontradas **60+ ocorrências** de `WriteLn`/`Write` distribuídas em:

### Módulos Afetados

| Módulo | Arquivo | # Ocorrências | Tipo |
|--------|---------|---------------|------|
| **Data/Entity** | `Dext.Entity.Migrations.Runner.pas` | 6 | Informativo/Progress |
| **Data/Entity** | `Dext.Entity.Scaffolding.pas` | 13 | Debug |
| **Data/Entity** | `Dext.Entity.Migrations.Json.pas` | 1 | Error |
| **Data/Entity** | `Dext.Entity.Drivers.FireDAC.pas` | 1 | Critical Error |
| **Data/Entity** | `Dext.Entity.DbSet.pas` | 4 | Error |
| **Data/Entity** | `Dext.Entity.Context.pas` | 4 | Warning/Error |
| **CLI** | `Dext.Hosting.CLI.pas` | 9 | CLI Output |
| **CLI** | `Dext.Hosting.CLI.Tools.Sonar.pas` | 9 | CLI Output |
| **Core** | `Dext.Hosting.BackgroundService.pas` | 7 | Info/Error |
| **Core** | `Dext.Logging.Console.pas` | 2 | Logger (interno) |
| **Core** | `Dext.Utils.pas` | 2 | Debug |
| **Web** | `Dext.Auth.Middleware.pas` | 12 | Debug |
| **Testing** | `Dext.Testing.Runner.pas` | 45+ | Test Output |

## 🎯 Estratégia de Solução

### 1. Criar Função Auxiliar `IsConsoleAvailable`

Adicionar em `Dext.Utils.pas`:

```pascal
function IsConsoleAvailable: Boolean;
begin
  {$IFDEF CONSOLE}
  Result := True;
  {$ELSE}
  {$IFDEF MSWINDOWS}
  Result := GetStdHandle(STD_OUTPUT_HANDLE) <> 0;
  {$ELSE}
  Result := IsConsole; // variável global do System
  {$ENDIF}
  {$ENDIF}
end;
```

### 2. Criar Procedimentos Auxiliares de Saída Segura

Adicionar em `Dext.Utils.pas`:

```pascal
/// <summary>
///   Escreve no console de forma segura, sem gerar erro em aplicações GUI.
///   Se não houver console disponível, a mensagem é silenciosamente ignorada.
/// </summary>
procedure SafeWriteLn(const AMessage: string); overload;
procedure SafeWriteLn; overload;
procedure SafeWrite(const AMessage: string);
```

### 3. Categorizar e Tratar Cada Ocorrência

#### Categoria A: **CLI Output** (Deve manter WriteLn)
Arquivos puramente CLI como:
- `Dext.Hosting.CLI.pas`
- `Dext.Hosting.CLI.Tools.Sonar.pas`

**Ação:** Verificar se `IsConsoleAvailable` antes de cada `WriteLn`, ou usar `SafeWriteLn`.

#### Categoria B: **Debug Messages** (Remover ou redirecionar para Logger)
Mensagens de debug que não deveriam estar em produção:
- `Dext.Entity.Scaffolding.pas` - todas as 13 ocorrências
- `Dext.Auth.Middleware.pas` - todas as 12 ocorrências

**Ação:** 
- Converter para `Logger.LogDebug()` 
- OU usar conditional compilation `{$IFDEF DEBUG}` + `SafeWriteLn`
- OU remover completamente

#### Categoria C: **Error/Warning Messages** (Redirecionar para Logger)
Mensagens de erro importantes que precisam ser registradas:
- `Dext.Entity.DbSet.pas` - 4 ocorrências de erro
- `Dext.Entity.Context.pas` - 4 ocorrências
- `Dext.Entity.Drivers.FireDAC.pas` - 1 ocorrência crítica
- `Dext.Entity.Migrations.Json.pas` - 1 ocorrência

**Ação:** Converter para `Logger.LogError()` ou `Logger.LogWarning()`

#### Categoria D: **Progress/Info Messages** (SafeWriteLn + Logger opcional)
Mensagens de progresso em operações:
- `Dext.Entity.Migrations.Runner.pas` - 6 ocorrências
- `Dext.Hosting.BackgroundService.pas` - 7 ocorrências

**Ação:** Usar `SafeWriteLn` para manter compatibilidade CLI, mas também adicionar `Logger.LogInformation()` opcional.

#### Categoria E: **Testing Framework** (Deve ter console)
O módulo de testes assume console:
- `Dext.Testing.Runner.pas` - 45+ ocorrências

**Ação:** 
1. Verificar `IsConsoleAvailable` na inicialização do runner
2. Se não tiver console, apenas salvar relatórios sem output visual
3. OU manter, pois testes geralmente rodam em console

#### Categoria F: **Console Logger** (Interno - precisa de guarda)
- `Dext.Logging.Console.pas` - 2 ocorrências

**Ação:** Já é logger, mas precisa verificar se console existe antes de escrever.

## 📝 Plano de Implementação

### Fase 1: Infraestrutura (Prioridade Alta) ✅ CONCLUÍDA
1. [x] Adicionar `IsConsoleAvailable` em `Dext.Utils.pas`
2. [x] Adicionar `SafeWriteLn` e `SafeWrite` em `Dext.Utils.pas`
3. [x] Atualizar `Dext.Logging.Console.pas` para usar guarda de console

### Fase 2: Erros e Warnings (Prioridade Alta) ✅ CONCLUÍDA
4. [x] `Dext.Entity.DbSet.pas` - Converter para SafeWriteLn
5. [x] `Dext.Entity.Context.pas` - Converter para SafeWriteLn
6. [x] `Dext.Entity.Drivers.FireDAC.pas` - Converter para SafeWriteLn
7. [x] `Dext.Entity.Migrations.Json.pas` - Converter para SafeWriteLn
8. [x] `Dext.Entity.Migrations.Runner.pas` - Converter para SafeWriteLn

### Fase 3: Debug Messages (Prioridade Média) ✅ CONCLUÍDA
9. [x] `Dext.Entity.Scaffolding.pas` - **REMOVIDO** (era lixo de debug)
10. [x] `Dext.Auth.Middleware.pas` - **REMOVIDO** (era lixo de debug)

### Fase 4: Progress/Info (Prioridade Média) ✅ CONCLUÍDA
11. [x] `Dext.Hosting.BackgroundService.pas` - Converter para SafeWriteLn
12. [x] `Dext.Hosting.CLI.pas` - Converter para SafeWriteLn
13. [x] `Dext.Hosting.CLI.Commands.*.pas` - Converter para SafeWriteLn
14. [x] `Dext.Hosting.CLI.Tools.Sonar.pas` - Converter para SafeWriteLn

### Fase 5: Web Debug Messages ✅ CONCLUÍDA
- [x] `Dext.Web.ModelBinding.pas` - Corrigido e limpo
- [x] `Dext.Web.ControllerScanner.pas` - Limpo
- [x] `Dext.Filters.BuiltIn.pas` - Limpo
- [x] `Dext.Web.Cors.pas` - Limpo
- [x] `Dext.Web.Middleware.Logging.pas` - Limpo

### Fase 6: Testing Framework ✅ CONCLUÍDA
- [x] `Dext.Testing.Runner.pas` - Convertido para SafeWriteLn
- [x] `Dext.Testing.Console.pas` - Convertido para SafeWriteLn
- [x] `Dext.Testing.Dashboard.pas` - Convertido para SafeWriteLn

### Fase 7: Validação ✅ CONCLUÍDA
- [x] Framework compila corretamente (Validado pelo usuário)
- [x] Remoção de WriteLn problemáticos e limpeza de unicode (ControllerScanner, ModelBinding)
- [x] Verificação final de integridade

## 🔧 Código de Implementação

### Dext.Utils.pas - Novas funções

```pascal
unit Dext.Utils;

interface

{$IFDEF MSWINDOWS}
uses
  WinApi.Windows;
{$ENDIF}

function ConsolePause: Boolean;
procedure DebugLog(const AMessage: string);
procedure SetConsoleCharSet(CharSet: Cardinal = 65001);

// Novas funções
function IsConsoleAvailable: Boolean;
procedure SafeWriteLn(const AMessage: string); overload;
procedure SafeWriteLn; overload;
procedure SafeWrite(const AMessage: string);

implementation

uses
  System.SysUtils;

var
  GConsoleAvailable: Boolean = False;
  GConsoleChecked: Boolean = False;

function IsConsoleAvailable: Boolean;
{$IFDEF MSWINDOWS}
var
  Handle: THandle;
{$ENDIF}
begin
  if not GConsoleChecked then
  begin
    GConsoleChecked := True;
    {$IFDEF CONSOLE}
    GConsoleAvailable := True;
    {$ELSE}
      {$IFDEF MSWINDOWS}
      Handle := GetStdHandle(STD_OUTPUT_HANDLE);
      GConsoleAvailable := (Handle <> 0) and (Handle <> INVALID_HANDLE_VALUE);
      {$ELSE}
      GConsoleAvailable := System.IsConsole;
      {$ENDIF}
    {$ENDIF}
  end;
  Result := GConsoleAvailable;
end;

procedure SafeWriteLn(const AMessage: string);
begin
  if IsConsoleAvailable then
  try
    System.Writeln(AMessage);
  except
    // Ignore I/O errors silently
  end;
end;

procedure SafeWriteLn;
begin
  if IsConsoleAvailable then
  try
    System.Writeln;
  except
    // Ignore I/O errors silently
  end;
end;

procedure SafeWrite(const AMessage: string);
begin
  if IsConsoleAvailable then
  try
    System.Write(AMessage);
  except
    // Ignore I/O errors silently
  end;
end;

// ... resto do código existente
```

### Dext.Logging.Console.pas - Com guarda

```pascal
procedure TConsoleLogger.Log(ALevel: TLogLevel; const AMessage: string; const AArgs: array of const);
begin
  if not IsEnabled(ALevel) then Exit;
  if not IsConsoleAvailable then Exit; // NOVO: Guarda de console
  
  // ... resto do código
end;
```

## ⚠️ Considerações

1. **Breaking Changes:** Nenhum - apenas melhoria de robustez
2. **Performance:** Mínimo impacto - verificação de console é cacheada
3. **Backward Compatibility:** Total - apps console continuam funcionando
4. **Testing:** Testes existentes devem continuar passando

## 📅 Próximos Passos

1. Aprovar este plano
2. Implementar Fase 1 (infraestrutura)
3. Implementar Fase 2 (crítico - erros/warnings)
4. Testar com app VCL de exemplo
5. Continuar com Fases 3-5
6. Atualizar documentação

---
*Plano criado: 2026-01-05*
*Issue relacionado: WriteLn causes I/O error 105 in VCL applications*
