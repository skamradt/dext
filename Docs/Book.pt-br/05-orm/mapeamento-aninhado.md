# Multi-Mapping (Objetos Aninhados)

O Dext suporta **Multi-Mapping** (semelhante ao multi-mapping do Dapper), permitindo que você hidrate grafos de objetos complexos a partir de uma única consulta SQL com múltiplos joins. Isso é alcançado usando o atributo `[Nested]`.

## O Atributo [Nested]

O atributo `[Nested]` informa ao ORM que uma propriedade representa um objeto aninhado que deve ser hidratado a partir das colunas do conjunto de resultados atual, em vez de ser carregado via uma consulta separada (Lazy Loading) ou um join `Include`.

### Exemplo

```pascal
type
  TAddress = class
  public
    property Street: string read FStreet write FStreet;
    property City: string read FCity write FCity;
  end;

  [Table('Users')]
  TUser = class
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;

    [Nested]
    property Address: TAddress read FAddress write FAddress;
  end;
```

## Lógica de Hidratação

Quando você executa uma consulta para `TUser`, o Dext espera que o conjunto de resultados contenha colunas tanto para o Usuário quanto para o Endereço.

```pascal
// SQL gerado ou manual
// SELECT Id, Name, Street, City FROM Users

var Users := Db.Users.ToList;
// Cada objeto User terá sua propriedade Address automaticamente instanciada e populada.
```

## Multi-Mapping Avançado

Você pode usar o `[Nested]` com um prefixo se suas colunas seguirem convenções de nomenclatura específicas:

```pascal
type
  TUser = class
  public
    [Nested('addr_')]
    property Address: TAddress read FAddress write FAddress;
  end;

// Espera colunas: addr_Street, addr_City
```

## Quando usar Multi-Mapping vs Include

*   **Use `Include`**: Para relacionamentos padrão (1:1, 1:N) onde a entidade relacionada tem sua própria tabela e ID. Este é o modo "ORM Padrão".
*   **Use `[Nested]`**:
    *   Para **Value Objects** (padrão DDD) que não possuem identidade própria e são armazenados na mesma tabela que o proprietário.
    *   Para otimizar manualmente joins complexos ao usar `FromSql`.
    *   Quando você deseja evitar o overhead de rastrear múltiplas entidades separadas e quer apenas uma hidratação plana e única.

## Aninhamento Recursivo

O Dext suporta aninhamento recursivo. Você pode ter uma propriedade `[Nested]` dentro de outra classe `[Nested]`, e o motor de hidratação percorrerá a árvore enquanto colunas correspondentes forem encontradas no conjunto de resultados.
