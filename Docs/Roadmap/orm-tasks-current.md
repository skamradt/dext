# Pendências do ORM - Fevereiro 2026

Este documento lista as melhorias, testes e refatorações identificadas após a implementação bem-sucedida do relacionamento Many-to-Many e a correção dos vazamentos de memória no Lazy Loading.

## 1. Testes de Integração Faltantes
Embora o motor suporte as funcionalidades, precisamos de suítes de testes automatizadas para garantir a estabilidade a longo prazo:

- [ ] **Optimistic Concurrency (Version)**: Validar se `EOptimisticConcurrencyException` é lançada em conflitos de atualização.
- [ ] **Soft Delete**: Validar filtros automáticos em consultas e o uso de `IgnoreQueryFilters`.
- [ ] **Campos de Auditoria (CreatedAt/UpdatedAt)**: Validar preenchimento automático em INSERTs e UPDATEs.
- [ ] **Relações 1:1 e N:1**: Validar se a correção de "Invalid pointer operation" (OwnsObjects := False) foi aplicada corretamente a todas as formas de carregamento tardio de listas.

## 2. Refatoração de Mapeamento
- [ ] **Dext.Entity.Mapping.pas**: Atualizar o método `DiscoverAttributes` para incluir todos os novos atributos (`[SoftDelete]`, `[Version]`, `[JsonColumn]`, etc.) no mapa interno da entidade. Atualmente, o motor detecta via RTTI em tempo de execução, mas o mapa interno está incompleto.

## 3. Melhorias de Funcionalidade
- [ ] **Suporte a Consultas JSON**: Implementar tradução de expressões de busca dentro de colunas `[JsonColumn]` (ex: `Where(p => p.Metadata.Value('role') == 'admin')`).
- [ ] **Propagação de DbType**: Garantir que o atributo `[DbType]` seja respeitado por todos os dialetos de SQL na geração de parâmetros e scripts DDL.

## 4. Estabilidade de Paging
- [ ] **Dialetos Legados**: Validar comportamento de paginação (offset/limit) em versões mais antigas do SQL Server e Oracle.

---
*Assinado: Antigravity AI*
