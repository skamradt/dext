# Pendências do ORM - Fevereiro 2026

Este documento lista as melhorias, testes e refatorações identificadas após a implementação bem-sucedida do relacionamento Many-to-Many e a correção dos vazamentos de memória no Lazy Loading.

## 1. Testes de Integração Faltantes
Embora o motor suporte as funcionalidades, precisamos de suítes de testes automatizadas para garantir a estabilidade a longo prazo:

**Status: ✅ CONCLUÍDO (2026-02-05)** - `Tests\Testing\TestORMFeatures.pas`

- [x] **Optimistic Concurrency (Version)**: ✅ PASSANDO - Validar se `EOptimisticConcurrencyException` é lançada em conflitos de atualização.
- [x] **Soft Delete**: ✅ PASSANDO - Validar filtros automáticos em consultas e o uso de `IgnoreQueryFilters`.
- [x] **Campos de Auditoria (CreatedAt/UpdatedAt)**: ✅ PASSANDO - Validar preenchimento automático em INSERTs e UPDATEs.
- [x] **Relações 1:1 e N:1**: ✅ PASSANDO - Lazy loading funcionando corretamente sem memory leaks.

### Resultado dos Testes (2026-02-05):
```
Total: 13 testes | Passando: 13 | Falhando: 0 | Taxa: 100%
Memory Leaks: 0
```

### Bugs Corrigidos Durante os Testes:
1. **Lazy Loading para tipos `Lazy<T>`** - O código não extraía corretamente o tipo interno de `Lazy<T>` para encontrar o DbSet correto. Corrigido em `Dext.Entity.LazyLoading.pas`.

2. **Memory Leak em `DetachAll`** - Objetos extraídos do IdentityMap via `ExtractPair` não eram liberados. Implementada lista `FOrphans` em `TDbSet<T>` para rastrear objetos detached e liberá-los no destrutor.

3. **Sobrecarga de `RegisterFixture`** - Adicionada sobrecarga que aceita array de classes para simplificar registro de múltiplos fixtures.

## 2. Refatoração de Mapeamento
- [ ] **Dext.Entity.Mapping.pas**: Atualizar o método `DiscoverAttributes` para incluir todos os novos atributos (`[SoftDelete]`, `[Version]`, `[JsonColumn]`, etc.) no mapa interno da entidade. Atualmente, o motor detecta via RTTI em tempo de execução, mas o mapa interno está incompleto.

## 3. Melhorias de Funcionalidade
- [ ] **Suporte a Consultas JSON**: Implementar tradução de expressões de busca dentro de colunas `[JsonColumn]` (ex: `Where(p => p.Metadata.Value('role') == 'admin')`).
- [ ] **Propagação de DbType**: Garantir que o atributo `[DbType]` seja respeitado por todos os dialetos de SQL na geração de parâmetros e scripts DDL.

## 4. Estabilidade de Paging
- [ ] **Dialetos Legados**: Validar comportamento de paginação (offset/limit) em versões mais antigas do SQL Server e Oracle.

---
*Assinado: Antigravity AI*
*Última atualização: 2026-02-05*


