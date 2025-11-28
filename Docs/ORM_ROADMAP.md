# 🗺️ Dext Entity ORM - Roadmap

Este documento rastreia o desenvolvimento do **Dext Entity**, o ORM nativo do framework Dext.

> **Visão:** Um ORM moderno, leve e performático para Delphi, inspirado no Entity Framework Core e Hibernate, mas com a simplicidade do Delphi.

---

## 📊 Status Atual: **Alpha 0.5** 🏗️

O núcleo do ORM está funcional, suportando operações CRUD, mapeamento básico, relacionamentos simples e controle de concorrência.

### ✅ Funcionalidades Implementadas

#### 1. Core & Mapeamento
- [x] **Entity Mapping**: Atributos `[Table]`, `[Column]`, `[PK]`, `[AutoInc]`, `[NotMapped]`.
- [x] **Identity Map**: Cache de primeiro nível para garantir instância única por Contexto.
- [x] **Database Drivers**: Abstração de driver (FireDAC implementado).
- [x] **Dialects**: Suporte multi-banco (SQLite, PostgreSQL).
- [x] **Schema Generation**: Geração automática de scripts `CREATE TABLE`.

#### 2. CRUD & Operações
- [x] **Basic CRUD**: `Add`, `Update`, `Remove`, `Find` (por ID).
- [x] **Composite Keys**: Suporte a chaves primárias compostas.
- [x] **Bulk Operations**: `AddRange`, `UpdateRange`, `RemoveRange` (Iterativo).
- [x] **Cascade Insert**: Inserção automática de entidades filhas novas.
- [x] **Optimistic Concurrency**: Controle de concorrência via atributo `[Version]`.

#### 3. Relacionamentos
- [x] **Foreign Keys**: Mapeamento via `[ForeignKey]`.
- [x] **Cascade Delete**: Suporte via Constraint de banco de dados.

---

## 📅 Próximos Passos

### 🚀 Fase 3: Advanced Querying (Foco Atual)
O objetivo é permitir consultas complexas de forma tipada e fluente.

- [ ] **Fluent Query API**: Builder para consultas (`Where`, `OrderBy`, `Skip`, `Take`).
  - *Exemplo:* `Context.Entities<TUser>.Where(User.Age > 18).OrderBy(User.Name).List;`
- [ ] **Metadados Tipados (TypeOf)**: Geração de metadados para evitar strings mágicas nas queries.
- [ ] **Specifications Pattern**: Integração completa com o padrão Specification.

### 📦 Fase 4: Loading Strategies
Melhorar como os dados relacionados são carregados.

- [ ] **Eager Loading (.Include)**: Carregamento antecipado de relacionamentos.
  - *Exemplo:* `Context.Entities<TUser>.Include('Address').Find(1);`
- [ ] **Lazy Loading**: Carregamento sob demanda (via Proxies ou Virtual getters).
- [ ] **Explicit Loading**: Carregamento manual de navegações (`Context.Entry(User).Collection('Orders').Load()`).

### ⚡ Fase 5: Performance & Tuning
- [ ] **True Bulk SQL**: Otimizar `AddRange` para usar `INSERT INTO ... VALUES (...), (...)`.
- [ ] **Batch Updates**: `UPDATE ... WHERE ...` em massa sem carregar entidades.
- [ ] **Query Caching**: Cache de planos de execução ou resultados.
- [ ] **No-Tracking Queries**: Consultas rápidas sem overhead do Identity Map.

### 🛠️ Fase 6: Tooling & Migrations
- [ ] **Migrations**: Sistema de migração de schema Code-First.
- [ ] **CLI Tools**: Comandos para gerar migrations e atualizar banco.
- [ ] **Scaffolding**: Gerar classes de entidade a partir de banco existente (Db-First).

---

## 🗄️ Roadmap de Suporte a Bancos de Dados

### Status Atual
- ✅ **SQLite**: Suporte completo e testado
- ⚠️ **PostgreSQL**: Dialeto implementado, mas não validado completamente

### Expansão Planejada (Baseada em Pesquisa de Mercado Delphi)

#### Prioridade 1 - Crítica (Mercado BR + Prototipagem)
1. **Firebird 3.0/4.0**
   - **Segmento**: ERPs Modernos, Mercado BR
   - **Driver**: FireDAC (TFDPhysFBDriverLink)
   - **Desafios**: Dialeto SQL, Transações, Generators
   - **Status**: ❌ Não implementado

2. **SQLite** ✅
   - **Segmento**: Mobile, Testes, Prototipagem
   - **Driver**: FireDAC (TFDPhysSQLiteDriverLink)
   - **Desafios**: Concorrência (Locking), Tipos
   - **Status**: ✅ **Implementado e Validado**

#### Prioridade 2 - Alta (Legado + Cloud)
3. **Firebird 2.5**
   - **Segmento**: Legado, Migração
   - **Driver**: FireDAC (TFDPhysFBDriverLink)
   - **Desafios**: Paginação (FirstSkip), Boolean
   - **Status**: ❌ Não implementado (pode reutilizar dialeto FB 3.0/4.0)

4. **PostgreSQL**
   - **Segmento**: Microserviços, Cloud, Docker
   - **Driver**: FireDAC (TFDPhysPGDriverLink)
   - **Desafios**: JSONB, Case Sensitivity, Batch
   - **Status**: ⚠️ **Dialeto criado, precisa validação completa**

#### Prioridade 3 - Média (Corporativo)
5. **SQL Server**
   - **Segmento**: Corporativo, Integração .NET
   - **Driver**: FireDAC (TFDPhysMSSQLDriverLink)
   - **Desafios**: Schemas, Tipos DateTime
   - **Status**: ❌ Não implementado

6. **MySQL/MariaDB**
   - **Segmento**: Web Hosting, Linux Barato
   - **Driver**: FireDAC (TFDPhysMySQLDriverLink)
   - **Desafios**: Transações Aninhadas, Engines
   - **Status**: ❌ Não implementado

#### Prioridade 4 - Baixa (Legado Oracle)
7. **Oracle**
   - **Segmento**: Grandes Corporações
   - **Driver**: FireDAC (TFDPhysOracleDriverLink)
   - **Desafios**: Sequences, Tipos
   - **Status**: ❌ Não implementado

---

## 📝 Notas de Design

- **Performance First**: Evitar Reflection excessivo em loops críticos (cache de RTTI já implementado).
- **Simplicidade**: API limpa e fácil de usar.
- **Extensibilidade**: Arquitetura baseada em Interfaces (`IDbSet`, `IDbContext`, `IDbCommand`).
