# Entidades & Mapeamento

Configure como as classes equivalem a tabelas no banco.

## Estilos de Mapeamento

O Dext suporta dois modelos de mapeamento:

1. **Mapeamento Explícito** (strings em atributos) — Use quando o banco já existe ou nomes não seguem um padrão.
2. **Estratégias de Nomeação** (Naming Strategies) — Recomendado para novos projetos (mapeamento automático).

## Mapeamento por Atributos

### Entidade Básica

```pascal
uses
  Dext.Entity; // Facade: Table, Column, PK, AutoInc, Required, MaxLength

type
  [Table('users')]
  TUser = class
  private
    FId: Integer;
    FName: string;
    FEmail: string;
    FCreatedAt: TDateTime;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;

    [Required, MaxLength(100)]
    property Name: string read FName write FName;

    [Required, MaxLength(200)]
    property Email: string read FEmail write FEmail;

    [CreatedAt]
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  end;
```

> [!IMPORTANT]
> **Estilo de Declaração**: Coloque atributos na mesma linha, separados por vírgula.  
> - ✅ `[Required, MaxLength(50), JSONName('code')]`  
> - ❌ `[Required]` em uma linha, `[MaxLength(50)]` na próxima.

## Atributos Disponíveis

### Mapeamento de Tabela

| Atributo | Descrição |
|----------|-----------|
| `[Table('nome')]` | Mapeia classe para tabela |
| `[Table]` | Mapeia usando Naming Strategy |
| `[Schema('schema')]` | Especifica o schema |

### Mapeamento de Coluna

| Atributo | Descrição |
|----------|-----------|
| `[Column('nome')]` | Mapeia para coluna específica |
| `[Column]` | Mapeia usando Naming Strategy |
| `[PK]` | Chave Primária |
| `[AutoInc]` | Auto-incremento |
| `[NotMapped]` | Exclui do mapeamento E do JSON |
| `[Version]` | Controle de concorrência otimista |
| `[SoftDelete('deleted', 1, 0)]` | Exclusão lógica |
| `[CreatedAt]` | Timestamp automático na inserção |
| `[UpdatedAt]` | Timestamp automático na atualização |

### Atributos de Validação

| Atributo | Descrição |
|----------|-----------|
| `[Required]` | Constraint NOT NULL (validado no SaveChanges) |
| `[MaxLength(N)]` | Tamanho máximo de string |
| `[MinLength(N)]` | Tamanho mínimo de string |

> [!WARNING]
> **`[StringLength]` NÃO existe no Dext!** Use `[MaxLength(N)]`.

Esses atributos requerem `Dext.Entity` no uses A validação roda automaticamente no `SaveChanges`.

### Chaves Estrangeiras

| Atributo | Descrição |
|----------|-----------|
| `[ForeignKey('col')]` | Coluna Chave Estrangeira |
| `[InverseProperty('prop')]` | Link de navegação reverso |

### Coleções (IList) & Gerenciamento de Memória

Ao definir `IList<T>` gerenciadas pelo `DbContext`:

1. Use `FItems: IList<TChild>` como field privado.
2. Inicialize no construtor com `TCollections.CreateList<TChild>(False)`.
3. **Crucial**: Passe `False` para `OwnsObjects`.
   - **Razão**: O DbContext já gerencia o ciclo de vida. Se a lista também for dona (`True`), ocorrerá **Invalid Pointer Operation** (Double Free) no shutdown.
4. **Testes Unitários**: Como não há DbContext, você **DEVE liberar manualmente** os itens filhos no `finally` do teste.

### Dicas de Tipo

| Atributo | Descrição |
|----------|-----------|
| `[Precision(18, 2)]` | Precisão e Escala para numéricos |
| `[Default('val')]` | Valor padrão no banco |
| `[JsonColumn]` | Trata coluna como JSON |
| `[DbType(ftGuid)]` | Força um TFieldType específico |

### Conversão de Tipos

| Atributo | Descrição |
|----------|-----------|
| `[TypeConverter(TMyConverter)]` | Converter customizado para esta propriedade |

```pascal
type
  TUnixTimestampConverter = class(TTypeConverterBase)
  public
    function CanConvert(ATypeInfo: PTypeInfo): Boolean; override;
    function ToDatabase(const AValue: TValue; ADialect: TDatabaseDialect): TValue; override;
    function FromDatabase(const AValue: TValue; ATypeInfo: PTypeInfo): TValue; override;
  end;
```

## Colunas Anuláveis (Nullable)

Use `Nullable<T>` para colunas que aceitam NULL:

```pascal
uses
  Dext.Types.Nullable;  // Obrigatório para Nullable<T>

type
  [Table('tickets')]
  TTicket = class
  private
    FId: Integer;
    FAssigneeId: Nullable<Integer>;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;

    [ForeignKey('Assignee')]
    property AssigneeId: Nullable<Integer> read FAssigneeId write FAssigneeId;
  end;
```

**Conversão implícita** funciona automaticamente:
```pascal
// Integer → Nullable<Integer>
Ticket.AssigneeId := AgentId;   // Funciona sem cast

// Verificar valor
if Ticket.AssigneeId.HasValue then
  WriteLn('Atribuído para: ', Ticket.AssigneeId.Value);

// Obter com default
var AssignId := Ticket.AssigneeId.GetValueOrDefault(0);

// Setar null
Ticket.AssigneeId := Nullable<Integer>.Null;
```

> [!WARNING]
> **`NavType<T>` NÃO existe no Dext!** Use sempre `Nullable<T>`.

## Rastreamento de Mudanças (Change Tracking)

O `ChangeTracker` pode não detectar mudanças se a entidade estiver detached. **Sempre** chame `Update` explicitamente antes de salvar:

```pascal
// ❌ INCORRETO: Pode falhar silenciosamente
Event.Status := esPublicado;
FDb.SaveChanges;

// ✅ CORRETO: Força State = Modified
Event.Status := esPublicado;
FDb.Events.Update(Event);  // Garante o update
FDb.SaveChanges;
```

## IDs Gerados Automaticamente

`SaveChanges` popula automaticamente os IDs de entidades inseridas (`[AutoInc]`).

```pascal
var User := TUser.Create;
User.Name := 'Alice';
FDb.Users.Add(User);
FDb.SaveChanges;

// ✅ User.Id já está populado — não consulte o banco novamente!
WriteLn('Novo ID: ', User.Id);
```

> [!WARNING]
> ⛔ **NUNCA** consulte o banco novamente para recuperar o ID após salvar. O objeto já está atualizado.

## Detach (Gerenciamento de Memória)

`FDb.Detach(Entity)` apenas remove a entidade do IdentityMap. Ele **NÃO** libera a memória.

```pascal
// ❌ INCORRETO: Memory Leak (entidade vira órfã)
FDb.Detach(Entity);
Entity := FDb.Find(ID);

// ✅ CORRETO: Libere a memória explicitamente
FDb.Detach(Entity);
Entity.Free;
Entity := FDb.Find(ID);
```

## Convenções de Nomenclatura

Por padrão, o Dext usa o nome da propriedade como nome da coluna. Para novos projetos, configure uma Naming Strategy:

```pascal
// No DbContext
procedure TAppDbContext.OnModelCreating(Builder: TModelBuilder);
begin
  Builder.UseNamingStrategy(TSnakeCaseNamingStrategy);
end;
```

Com `TSnakeCaseNamingStrategy`:
- Tabela `TUser` → `user`
- Coluna `CreatedAt` → `created_at`

Sobrescreva com `[Table('nome')]` e `[Column('nome')]` quando necessário.

> 💡 **Referência**: Veja o exemplo [Orm.EntityStyles](../../../Examples/Orm.EntityStyles/) para uma comparação lado a lado.

---

[← Primeiros Passos](primeiros-passos.md) | [Próximo: Consultas →](consultas.md)
