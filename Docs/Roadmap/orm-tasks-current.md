# Pendências do ORM - Fevereiro 2026

Este documento lista as melhorias, testes e refatorações identificadas após a implementação bem-sucedida do relacionamento Many-to-Many e a correção dos vazamentos de memória no Lazy Loading.

## 1. Testes de Integração Faltantes
Embora o motor suporte as funcionalidades, precisamos de suítes de testes automatizadas para garantir a estabilidade a longo prazo:

**Status: ✅ CONCLUÍDO (2026-02-06)** - `Tests\Testing\TestORMFeatures.pas`

- [x] **Optimistic Concurrency (Version)**: ✅ PASSANDO - Validar se `EOptimisticConcurrencyException` é lançada em conflitos de atualização.
- [x] **Soft Delete**: ✅ PASSANDO - Validar filtros automáticos em consultas e o uso de `IgnoreQueryFilters`.
- [x] **Campos de Auditoria (CreatedAt/UpdatedAt)**: ✅ PASSANDO - Validar preenchimento automático em INSERTs e UPDATEs.
- [x] **Relações 1:1 e N:1**: ✅ PASSANDO - Lazy loading funcionando corretamente sem memory leaks.
- [x] **Consultas JSON**: ✅ PASSANDO - Consultas em colunas JSON/JSONB funcionando (PostgreSQL).

### Resultado dos Testes (2026-02-06):
```
Total: 16 testes | Passando: 16 | Falhando: 0 | Taxa: 100%
Memory Leaks: 0
```

### Bugs Corrigidos Durante os Testes:
1. **Lazy Loading para tipos `Lazy<T>`** - O código não extraía corretamente o tipo interno de `Lazy<T>` para encontrar o DbSet correto. Corrigido em `Dext.Entity.LazyLoading.pas`.

2. **Memory Leak em `DetachAll`** - Objetos extraídos do IdentityMap via `ExtractPair` não eram liberados. Implementada lista `FOrphans` em `TDbSet<T>` para rastrear objetos detached e liberá-los no destrutor.

3. **Sobrecarga de `RegisterFixture`** - Adicionada sobrecarga que aceita array de classes para simplificar registro de múltiplos fixtures.

4. **JSON Queries PostgreSQL** - Corrigido cast automático `::jsonb` no INSERT e `::text` em comparações numéricas para colunas JSON no PostgreSQL.

## 2. Refatoração de Mapeamento
- [x] **Dext.Entity.Mapping.pas**: ✅ CONCLUÍDO - Atributos `[SoftDelete]`, `[Version]`, `[JsonColumn]`, etc. agora são processados corretamente no mapa interno da entidade.

## 3. Melhorias de Funcionalidade
- [x] **Suporte a Consultas JSON**: ✅ CONCLUÍDO (2026-02-06) - Implementada tradução de expressões de busca dentro de colunas `[JsonColumn]` usando `.Json('path')`.
- [x] **Propagação de DbType**: ✅ CONCLUÍDO (2026-02-06) - Atributo `[DbType]` propagado até a criação de parâmetros FireDAC, garantindo mapeamento exato de tipos.
- [x] **Estabilidade de Paging**: ✅ CONCLUÍDO (2026-02-06) - Implementada paginação via `ROWNUM` para Oracle e arquitetura de wrapper para paginação em dialetos legados.

## 5. Documentação
- [x] **Dext Book (EN)**: ✅ Adicionado suporte a `DbType` e `Paging` legacy.
- [x] **Dext Book (PT-BR)**: ✅ Adicionado suporte a `DbType` e `Paging` legacy.
- [x] **CHANGELOG**: ✅ Atualizado com DbType e Paging (2026-02-06)

---
*Assinado: Antigravity AI*
*Última atualização: 2026-02-06*
