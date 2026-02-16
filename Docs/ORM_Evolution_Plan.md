# Plano Maestro de Evolução: Dext ORM (2026)

Este documento consolida a visão estratégica de roadmap do Dext ORM, integrando os planos de 2025 com os novos objetivos de eliminação total de boilerplate e paridade com EF Core.

## 1. Pilares da Evolução

### 1.1 Zero Boilerplate & Auto-Lazy (Proxies)
*   **Objetivo**: Eliminar a necessidade de implementar `Get/Set` manuais para propriedades de navegação.
*   **Estratégia**: Implementar **Auto-Proxies** com `TVirtualMethodInterceptor`. Propriedades `virtual` serão interceptadas para carregar dados sob demanda (Lazy Loading) e notificar o Change Tracker.
*   **Referência**: `property Orders: TList<TOrder> virtual;` -> Dext gerencia o carregamento.

### 1.2 Unificação via Prop<T> (Unified Smart Types)
*   **Problema**: O modelo antigo de `TEntityType<T>` exigia classes de metadados separadas, gerando boilerplate e risco de dessincronização.
*   **Solução**: Consolidar o uso de `Prop<T>` como o único ponto de verdade. O `Prop<T>` funciona em dois modos:
    1.  **Modo Execução**: Atua como container de dados (Value).
    2.  **Modo Protótipo**: Atua como gerador de expressões (Metadata/Expression Builder).
*   **Impacto**: Paridade total entre POCO e metadados em uma única classe de domínio.

### 1.3 Shadow Properties (Propriedades de Sombra Internas)
*   **Problema**: Certas colunas de banco (ex: `TenantId`) poluem o domínio mas são necessárias.
*   **Solução**: O ORM gerenciará internamente colunas não mapeadas em `Prop<T>`, armazenando-as no Change Tracker. Não requer nenhuma classe extra, apenas configuração fluente ou convenção.

### 1.4 ThenInclude & Paridade Query (Fluent API)
*   **Objetivo**: Segurança de tipos em qualquer profundidade de relacionamento.
*   **Estratégia**: Introduzir `IIncludableQuery<T, TProp>` para permitir `.Include(o.User).ThenInclude(u.Profile)`.

### 1.5 Operações Assíncronas & Desconectadas
*   **Objetivo**: Suportar o fluxo moderno de APIs Web (Stateless).
*   **Estratégia**: `DbSet.Attach/Update` para objetos JSON deserializados e métodos `Async` em toda a stack.

---

## 📅 Roadmap de Implementação Consolidado

### Fase 1: Evolução do Prop<T> e Engine Interna
- [x] **Task 1.1**: Implementar `ThenInclude` tipado estendendo o `TPrototype` baseado em campos `Prop<T>`.
- [x] **Task 1.2**: Adicionar **Explicit Operators** em `Prop<T>` para permitir casts seguros (ex: `Integer(PropValue)`).
- [x] **Task 1.3**: Otimizar o **Internal Metadata Cache** (engine) para acelerar o `Prototype.Entity<T>` sem depender de RTTI pesado em loops de query.
- [x] **Task 1.4**: Implementar suporte a **Shadow Properties** dentro do `TEntityEntry` (acesso via `.Member()`).

### Fase 2: Fluent API & Consultas Avançadas
- [x] **Task 2.1**: Implementar **Fluent Mapping Improvements** (Typed Selectors `Prop(u.X)`, Audit/Version/Lazy markers).
- [x] **Task 2.2**: Refatorar `TFluentQuery` para suportar 100% dos filtros de `ISpecification` (Paridade Total).
- [x] **Task 2.3**: Adicionar overloads de `Assert.Should` para Smart Types em `Dext.Assertions`.

### Fase 3: Proxies & Lazy Loading (O Fim do Boilerplate)
- [x] **Task 3.1**: Criar **TProxyFactory** para propriedades virtuais (Auto-Proxy Lazy Loading).
- [x] **Task 3.2**: Implementar `TTrackingList<T>` (Proxy de Coleção) para gerenciar relacionamentos sem `Link/Unlink`.

### Fase 4: Async & Raw SQL
- [x] **Task 4.1**: Implementar `ToListAsync` e `SaveChangesAsync`.
    *   *Requisito*: Validar uso de Connection Pooling (Exception se inativo).
    *   *Infra*: Baseado em `Dext.Threading.Async` e `CancellationToken`.
    *   *Docs*: Guia didático sobre o custo de threads vs. benefício de paralelismo, com referência ao livro [Delphi Multithreading](https://www.cesarromero.com.br/#livros).
- [ ] **Task 4.2**: Suporte a **Multi-Mapping** (Dapper-Style) e Otimização do Database as API.
    *   *Status*: [Em Progresso] Dynamic Specification Mapping (Filtros via QueryString) concluído. Implementation of `TUtf8JsonWriter` for zero-allocation responses.
    *   *Pendente*: Multi-Mapping engine e integração do writer no generic handler.
- [ ] **Task 4.3**: Implementar **Pessimistic Locking** (SELECT FOR UPDATE / Bloqueios Offline).

### Fase 5: Entidades Não-Tabela (View/Query Mapping)
- [ ] **Task 5.1**: Mapeamento de resultados de Stored Procedures e Views complexas para POCOs sem necessidade de tabela física.

---

## 📄 Documentos Relacionados (Legacy/Archive)
*   `Roadmap/orm-roadmap.md` (Estratégico 2025)
*   `Roadmap/orm-tasks-current.md` (Concluído em Fev/2026)
*   `Roadmap/orm-type-system-enhancement.md` (Concluído em Dez/2025)

---
*Assinado: Antigravity AI*
*Data: 16 de Fevereiro de 2026*
