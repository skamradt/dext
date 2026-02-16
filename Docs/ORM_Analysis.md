# Análise Profunda do Dext ORM: Relacionamentos, Herança e Inferência

Esta análise detalha as capacidades atuais do Dext ORM, focando em automação, convenções de nomenclatura e a eliminação de strings mágicas.

## 1. Relacionamentos (Relationships)

O Dext ORM suporta relacionamentos complexos com carregamento tardio (Lazy Loading) e carregamento antecipado (Eager Loading).

### 1.1 One-to-Many (1:N)
O cenário mais comum (ex: Pedido possui itens).
- **Atributos**: `[HasMany]` para coleções e `[BelongsTo]` para a referência inversa.
- **Lazy Loading**: Utiliza `IList<T>` para carregar itens sob demanda.
- **Eager Loading**: Suporta `.Include('Items')` ou o novo `.Include(o.BuyerId)` para carregar relacionamentos em uma única consulta (ou consultas otimizadas em lote).

### 1.2 Many-to-One (N:1)
Referência de um filho para um pai (ex: Item aponta para Pedido).
- **Atributos**: `[ForeignKey]`.
- **Inovação**: Se a propriedade seguir o padrão `NomeEntidadeId` (ex: `OrderId`), o ORM **infere automaticamente** que se trata de uma chave estrangeira, tornando o atributo `[ForeignKey]` opcional.

### 1.3 Many-to-Many (N:N)
Relacionamentos vinculados por uma tabela de junção (ex: Produto possui Categoria).
- **Inovação**: Suporta `[ManyToMany]` com tabelas de junção automáticas. O ORM gerencia as inserções/deleções na tabela intermediária de forma transparente.

---

## 2. Herança (Inheritance)

O Dext ORM implementa estratégias sólidas de herança, permitindo modelos de domínio ricos.

### 2.1 Table Per Hierarchy (TPH)
Toda a hierarquia em uma única tabela, diferenciada por uma coluna discriminadora.
- **Vantagem**: Performance superior em consultas de polimorfismo.
- **Configuração**: `[Inheritance(TablePerHierarchy)]` e `[DiscriminatorValue('Admin')]`.

### 2.2 Table Per Type (TPT)
Cada classe possui sua própria tabela, compartilhando a chave primária com a tabela base.
- **Vantagem**: Normalização total do banco de dados.
- **Configuração**: `[Inheritance(TablePerType)]`.

---

## 3. Inferência e Convenções de Nomenclatura

O "poder oculto" do Dext está na sua capacidade de adivinhar intenções, reduzindo o código repetitivo (Boilerplate).

### 3.1 Tabelas e Colunas
- Se `[Table]` não for especificado, o ORM utiliza o nome da classe (removendo o 'T' inicial) em `snake_case` ou `PascalCase` conforme a estratégia configurada.
- O mesmo vale para `[Column]`. Propriedades como `FirstName` tornam-se colunas `first_name` automaticamente em dialetos como PostgreSQL.

### 3.2 Chaves Estrangeiras (FKs)
O ORM utiliza uma lógica de fallback inteligente:
1. Procura por `[ForeignKey]`.
2. Procura por `[InverseProperty]`.
3. Tenta encontrar propriedades no alvo que apontem para o nome da classe atual + 'Id'.

---

## 4. Impactos e Benefícios

1. **Redução de Código**: Menos atributos significam classes de entidade mais limpas e legíveis.
2. **Segurança de Tipos (Strong Typing)**: Com o uso de `Prop<T>` e `TPrototype`, eliminamos strings em filtros `Where` e agora expandimos isso para o `Include`.
3. **Manutenibilidade**: Mudanças no nome de propriedades através de Refactor no Delphi não quebram o mapeamento se a convenção for seguida.
4. **Performance**: O carregamento automático de relacionamentos via `Include` evita o problema N+1, disparando consultas otimizadas.

---

## Próximos Passos Sugeridos

- Implementar `ThenInclude` para carregamento de múltiplos níveis de profundidade.
- Expandir a inferência de chaves compostas.
- Adicionar validação em tempo de compilação (via scripts de build) para garantir que as propriedades mapeadas existem no banco.
