# Roadmap Dext ORM

Este documento mapeia as funcionalidades do Dext ORM em comparação com o Entity Framework, definindo o status atual e o roadmap de prioridades para o desenvolvimento.

## 📊 Comparativo de Features

| Feature | Entity Framework | Dext ORM (Atual) | Status | Prioridade |
| :--- | :--- | :--- | :--- | :--- |
| **Basic CRUD** | `Add`, `Update`, `Remove`, `Find` | `Add`, `Find`, `List` implementados. `Update` e `Remove` vazios. | ⚠️ Incompleto | 🚨 **Crítica** |
| **Querying** | LINQ (`Where`, `Select`) | Specifications (`Criteria`) | ✅ Implementado | - |
| **Mapping** | Attributes & Fluent API | Attributes apenas | ⚠️ Parcial | 📉 Baixa |
| **Relationships** | Navigation Props (1:N, N:N) | Não suportado | ❌ Ausente | 🔥 **Média** |
| **Change Tracking**| Automático (`SaveChanges`) | Explícito (`Update` method) | ❌ Ausente | 📉 Baixa (Decisão de Design) |
| **Migrations** | `Add-Migration` | Manual SQL | ❌ Ausente | 📉 Baixa |
| **Identity Map** | Cache local de entidades (L1) | Não existe (cada query cria novos objetos) | ❌ Ausente | 🔥 **Média** |
| **Concurrency** | Optimistic Concurrency | Não suportado | ❌ Ausente | 📉 Baixa |

---

## 🗺️ Roadmap de Implementação

Com base na análise, definimos as seguintes fases para estabilizar o ORM.

### 📍 Fase 1: Core CRUD & Estabilidade (Imediato)
*O objetivo é tornar o ORM funcional para operações básicas de persistência.*

1.  **Implementar `TDbSet<T>.Update`**:
    *   ✅ Implementado (Gera SQL dinâmico).
2.  **Implementar `TDbSet<T>.Remove`**:
    *   ✅ Implementado (Gera SQL DELETE).
3.  **Refinar Conversão de Tipos (`Hydrate`)**:
    *   ✅ **Base Implementada**: `Dext.Core.ValueConverters` criado com suporte a Primitivos, Enums, GUIDs e Datas.

### 📍 Fase 2: Relacionamentos Básicos (Curto Prazo)
*Permitir carregar dados relacionados sem complexidade excessiva.*

1.  **Suporte a Foreign Keys**:
    *   ✅ Atributo `[ForeignKey('ColumnId')]` implementado.
2.  **Eager Loading Simples**:
    *   ✅ Capacidade de carregar objetos filhos (ex: `User.Address`) implementada via `Hydrate`.

### 📍 Fase 3: Produtividade & Tooling (Médio Prazo)
1.  **Identity Map**:
    *   ✅ Implementado cache local no `TDbSet<T>` (`FIdentityMap`).
    *   `Find(1)` retorna a mesma instância se já carregada.
    *   Entidades são gerenciadas pelo contexto (User não deve liberar).
2.  **Gerador de Schema (Básico)**:
    *   ✅ Método `EnsureCreated()` implementado.
    *   Gera SQL `CREATE TABLE` baseado nos metadados da entidade e dialeto.
    *   ✅ **Estável**: Bugs de interface e AV resolvidos.

### 📍 Fase 4: Cenários Avançados & Legado (Longo Prazo)
1.  **Chaves Primárias Compostas**:
    *   Suporte a múltiplos campos com `[PK]`.
    *   Adaptação do `Find` para aceitar array de valores (`Find([1, 2])`).
    *   Adaptação do Identity Map para chaves compostas.
2.  **Transações Aninhadas / Savepoints**: Melhor controle transacional.
3.  **Lazy Loading**: Proxies virtuais para carregar listas grandes sob demanda.
