# Dext Framework - Relatório de Falhas de Testes e Memory Leaks

Este documento centraliza as falhas identificadas na execução completa dos testes em 19/02/2026.

## 1. Falhas Funcionais (Lógica e SQL)

### 1.1. Dext.Entity.Tests (Fluent Mapping)
- **Problema:** O mapeamento fluente de nome de coluna não está sobrescrevendo o atributo no script de `CREATE TABLE`.
- **Status:** ✅ FIXED (2026-02-19)
- **Solução:** Corrigido a ordem dos parâmetros no construtor de `TPropInfo` dentro de `Dext.Entity.Prototype.pas`.

### 1.2. TestUUID (Case Sensitivity)
- **Status:** ✅ FIXED (2026-02-19) - Todos os testes passaram.

### 1.3. TestORMRelationships (ManyToMany)
- **Status:** ✅ FIXED (2026-02-19) - Todos os testes passaram.

### 1.4. TestTypeConvertersDb (JSON Mismatch)
- **Status:** ✅ FIXED (2026-02-19)
- **Solução:** A classe `TJsonMetadata` e as entidades foram movidas para a unit `TestDataEntities.pas`, garantindo a geração correta de RTTI e visibilidade pelo serializador JSON.

## 2. Falhas de Infraestrutura / Web

### 2.1. Web.FrameworkTests (Connection Refused)
- **Status:** ✅ FIXED (2026-02-19)
- **Solução:** Refatorado `WebFrameworkTests.Tests.Base.pas` para usar `Host.Start` em vez de `Run` (evitando saída prematura por causa da flag `-no-wait`) e implementado um loop de espera robusto com Captura de Erros de inicialização.

## 3. Memory Leaks

### 3.1. TFluentQuery.Select (ActRec)
- **Status:** ✅ FIXED (2026-02-19)
- **Solução:** Refatorado os métodos `Select` e `WherePredicate` em `Dext.Entity.Query.pas` para evitar a captura do registro `Self` (o próprio `TFluentQuery`) dentro das closures, quebrando ciclos de referência ocultos.

### 3.2. Web.FrameworkTests (Grande Escala)
- **Status:** ✅ FIXED (2026-02-19)
- **Solução:** Com a resolução da falha de conexão (2.1), os testes agora completam o ciclo de setup/teardown, limpando os recursos corretamente.

---
## 📊 Resumo do Status Atualizado
Os resultados agora são excelentes! Todas as falhas críticas de infraestrutura e os leaks identificados foram resolvidos.

### ✅ Resolvidos (Recente)
1.  **Web.FrameworkTests:** ✅ Conexão estável e robusta.
2.  **TFluentQuery Leaks:** ✅ closures otimizadas para não reter o registro `Self`.
3.  **TestTypeConvertersDb:** ✅ RTTI garantido via unit externa.
4.  **Fluent Mapping & UUID:** ✅ Validados e estáveis.

### ⏭️ Próximo Passo Recomendado
Realizar uma nova execução completa dos testes para stress-test da estabilidade e verificar se novos leaks residuais aparecem em cenários complexos.
