# Entidades & Mapeamento

Configure como classes mapeiam para tabelas de banco de dados.

## Estilos de Mapeamento

O Dext suporta dois estilos de mapeamento:

1. **Baseado em Atributos** (recomendado para a maioria dos casos)
2. **Mapeamento Fluente** (para classes POCO)

## Mapeamento por Atributos

### Entidade Básica

```pascal
type
  [Table('users')]
  TUser = class
  private
    FId: Integer;
    FName: string;
    FEmail: string;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    [Column('full_name')]
    property Name: string read FName write FName;
    
    [Column('email')]
    property Email: string read FEmail write FEmail;
  end;
```

## Atributos Disponíveis

### Mapeamento de Tabela

| Atributo | Descrição |
|----------|-----------|
| `[Table('nome')]` | Mapear classe para tabela |
| `[Schema('schema')]` | Especificar schema |

### Mapeamento de Coluna

| Atributo | Descrição |
|----------|-----------|
| `[Column('nome')]` | Mapear para coluna específica |
| `[PK]` | Chave primária |
| `[AutoInc]` | Auto-incremento |
| `[NotMapped]` | Excluir do mapeamento |
| `[Version]` | Controle de concorrência otimista |
| `[SoftDelete('col_deletado', 1, 0)]` | Exclusão lógica com valores para Deletado e Não Deletado |
| `[CreatedAt]` | Timestamp automático na inserção |
| `[UpdatedAt]` | Timestamp automático na atualização |

### Dicas de Escopo de Tipo

| Atributo | Descrição |
|----------|-----------|
| `[StringLength(100)]` | Comprimento máximo da string |
| `[MaxLength(100)]` | Alias para StringLength |
| `[Precision(18, 2)]` | Precisão e Escala para tipos numéricos/decimais |
| `[Required]` | Restrição NOT NULL |
| `[Default('valor')]` | Valor padrão no banco de dados |
| `[JsonColumn]` | Trata a coluna como JSON (converte para objeto/lista) |
| `[DbType(ftGuid)]` | Força um TFieldType específico para parâmetros |

### Relacionamentos

| Atributo | Descrição |
|----------|-----------|
| `[ForeignKey('col')]` | Coluna de chave estrangeira |
| `[InverseProperty('prop')]` | Link de navegação |

### Coleções & Ownership de Entidades

Ao definir propriedades `IList<T>` que também são gerenciadas pelo `DbContext`:

1. Use `FItems: IList<TFilho>` como field privado.
2. Inicialize no construtor com `TCollections.CreateList<TFilho>(False)`.
3. **Crucial**: Passe `False` para `OwnsObjects`.
   - **Razão**: O `DbContext` já gerencia o ciclo de vida das entidades rastreadas. Se a lista também tentar liberá-las (`True`), ocorrerá **Invalid Pointer Operation** (Double Free) no shutdown.
4. **Testes Unitários**: Como não há DbContext, você **deve liberar manualmente** os itens filhos no bloco `finally` do teste.

### Conversão de Tipos

| Atributo | Descrição |
|----------|-----------|
| `[TypeConverter(TMeuConverter)]` | Converter customizado para esta propriedade |

Use `[TypeConverter]` para sobrescrever como uma propriedade específica é convertida para/do banco:

```pascal
type
  // Converter customizado: armazena TDateTime como Unix timestamp
  TUnixTimestampConverter = class(TTypeConverterBase)
  public
    function CanConvert(ATypeInfo: PTypeInfo): Boolean; override;
    function ToDatabase(const AValue: TValue; ADialect: TDatabaseDialect): TValue; override;
    function FromDatabase(const AValue: TValue; ATypeInfo: PTypeInfo): TValue; override;
  end;

  [Table('events')]
  TEvent = class
  private
    FId: Integer;
    FName: string;
    FCreatedAt: TDateTime;
    FScheduledAt: TDateTime;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    
    // Usa converter customizado - armazenado como Unix timestamp (Integer)
    [TypeConverter(TUnixTimestampConverter)]
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    
    // Usa converter TDateTime padrão (formato ISO)
    property ScheduledAt: TDateTime read FScheduledAt write FScheduledAt;
  end;
```

> 💡 O converter de propriedade tem prioridade sobre converters globais de tipo.

## Colunas Anuláveis

Use `Nullable<T>` para colunas que podem ser NULL:

```pascal
uses
  Dext.Types.Nullable;

type
  [Table('products')]
  TProduct = class
  private
    FId: Integer;
    FDescription: Nullable<string>;  // Pode ser NULL
    FDiscount: Nullable<Double>;      // Pode ser NULL
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    property Description: Nullable<string> read FDescription write FDescription;
    property Discount: Nullable<Double> read FDiscount write FDiscount;
  end;
```

Usando valores anuláveis:

```pascal
// Verificar se tem valor
if Product.Discount.HasValue then
  WriteLn('Desconto: ', Product.Discount.Value);

// Obter valor com padrão
var Desc := Product.Discount.GetValueOrDefault(0);

// Definir como null
Product.Discount := Nullable<Double>.Null;
```

> 💡 **Referência**: Veja o exemplo [Orm.EntityStyles](../../../Examples/Orm.EntityStyles/) para uma comparação lado a lado entre entidades Clássicas e Smart.

---

[← Primeiros Passos](primeiros-passos.md) | [Próximo: Consultas →](consultas.md)
