# Controle de Tipos (DbType)

O Dext.Entity permite que você controle explicitamente o tipo de dado do banco de dados usado para parâmetros e mapeamento de colunas. Isso é particularmente útil quando você precisa distinguir entre diferentes tipos que mapeiam para o mesmo tipo Delphi (por exemplo, `TDateTime` mapeando para `Date` vs `DateTime` no banco de dados).

## O Atributo [DbType]

Ao usar o atributo `[DbType]`, você informa ao ORM exatamente qual `TFieldType` deve ser usado ao criar parâmetros para comandos SQL.

### Exemplo: Date vs DateTime

Por padrão, uma propriedade `TDateTime` pode ser mapeada para `DateTime` ou `TimeStamp` no banco de dados. Se você quiser garantir que ela seja tratada como uma `Date` pura:

```pascal
type
  [Table('Events')]
  TEvent = class
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    [DbType(ftDate)]
    property RegistrationDate: TDateTime read FDate write FDate;
  end;
```

### Tipos Suportados

A maioria dos valores padrão de `TFieldType` é suportada, incluindo:

- `ftDate`, `ftTime`, `ftDateTime`, `ftTimeStamp`
- `ftString`, `ftWideString`, `ftMemo`, `ftWideMemo`
- `ftInteger`, `ftSmallint`, `ftLargeint`, `ftBCD`, `ftFMTBcd`
- `ftBoolean`
- `ftBlob`, `ftGuid`

## Decimais de Alta Precisão

Para aplicações financeiras, o uso de `ftFMTBcd` é recomendado para evitar erros de arredondamento:

```pascal
type
  [Table('Products')]
  TProduct = class
  public
    [DbType(ftFMTBcd), Precision(18, 4)]
    property Price: Currency read FPrice write FPrice;
  end;
```

## Mapeamento Fluente

Se você preferir usar o Mapeamento Fluente em vez de atributos, pode usar o método `HasDbType`:

```pascal
modelBuilder.Entity<TProduct>()
  .Prop('Price')
    .HasDbType(ftFMTBcd)
    .HasPrecision(18, 4);
```

## Paginação Legada

O Dext.Entity lida automaticamente com a paginação para diferentes motores de banco de dados. Para bancos de dados mais novos, utiliza a sintaxe padrão `OFFSET ... FETCH NEXT`.

### Sintaxe Moderna (PostgreSQL, SQL Server 2012+, Oracle 12c+, Firebird 3.0+)

```sql
SELECT * FROM Users 
ORDER BY Id 
OFFSET 10 ROWS FETCH NEXT 10 ROWS ONLY
```

### Suporte Legado (Oracle 11g, SQL Server 2008)

Para versões mais antigas do Oracle, o Dext envolve automaticamente sua consulta usando uma estratégia de `ROWNUM`:

```sql
SELECT * FROM (
  SELECT a.*, ROWNUM rnum 
  FROM (SELECT * FROM Users ORDER BY Id) a 
  WHERE ROWNUM <= 20
) WHERE rnum > 10
```

Isso é tratado de forma transparente pelo `TOracleDialect`. Você não precisa alterar o código da sua aplicação; basta usar `.Skip(x).Take(y)` e o Dext gerará o SQL correto para o seu dialeto.
