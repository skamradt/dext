# Consultas (Querying)

API fluente para recuperação de dados.

## Tipos de Retorno: IList\<T\>

> [!IMPORTANT]
> O Dext usa `IList<T>` de `Dext.Collections`, não `TObjectList<T>` de `System.Generics.Collections`.

```pascal
uses
  Dext.Collections;

var Users: IList<TUser>;
begin
  Users := Context.Users.ToList;  // Retorna IList<TUser>
  
  for var User in Users do
    WriteLn(User.Name);
end;
```

### Criando Novas Listas

Use a factory `TCollections.CreateList<T>`:

```pascal
// Criar nova lista
var MinhaLista := TCollections.CreateList<TUser>;
MinhaLista.Add(User1);
MinhaLista.Add(User2);

// Criar com ownership (libera objetos automaticamente)
var ListaComOwner := TCollections.CreateList<TUser>(True);
```

> [!TIP]
> `IList<T>` é uma interface, então não precisa de `try/finally Free`!

## Consultas Básicas

### Obter Todos

```pascal
var Users := Context.Users.ToList;
```

### Buscar por ID

```pascal
var User := Context.Users.Find(1);
if User <> nil then
  WriteLn('Encontrado: ', User.Name);
```

### First / FirstOrDefault

```pascal
// Lança exceção se não encontrar
var User := Context.Users.First;

// Retorna nil se não encontrar
var User := Context.Users.FirstOrDefault;
```

## Filtragem (Where)

### Smart Properties (Recomendado)

Use `var` inline para Props (simulando variáveis lambda do C#):

```pascal
// ✅ CORRETO: var inline camelCase, próximo ao uso
var u := TUser.Props;
var UsuariosAtivos := Context.Users
  .Where(u.IsActive = True)
  .ToList;

// Múltiplas condições
var Resultados := Context.Users
  .Where((u.Age >= 18) and (u.Status = 'active'))
  .ToList;
```

> [!IMPORTANT]
> **Por que camelCase?** Para diferenciar de variáveis de método PascalCase.  
> Em queries multi-tabela com Include/Join, cada tabela tem sua própria var inline (`var u`, `var o`, `var p`).

### Expressão Lambda

```pascal
var UsuariosAtivos := Context.Users
  .Where(function(U: TUser): Boolean
    begin
      Result := U.IsActive;
    end)
  .ToList;
```

## Ordenação (OrderBy)

> [!WARNING]
> `OrderBy` exige `.Asc` ou `.Desc` — passar uma Prop diretamente causa o erro `E2010 Incompatible types`.

```pascal
var u := TUser.Props;

// Ascendente
var Users := Context.Users
  .QueryAll
  .OrderBy(u.Name.Asc)
  .ToList;

// Descendente
var Users := Context.Users
  .QueryAll
  .OrderBy(u.CreatedAt.Desc)
  .ToList;

// Múltiplas colunas
var Users := Context.Users
  .QueryAll
  .OrderBy(u.LastName.Asc)
  .OrderBy(u.FirstName.Asc)
  .ToList;
```

## Paginação

```pascal
var u := TUser.Props;
var Pagina := Context.Users
  .QueryAll
  .OrderBy(u.Id.Asc)
  .Skip(20)   // Pula os primeiros 20
  .Take(10)   // Pega os próximos 10
  .ToList;
```

## Projeção (Select)

```pascal
type
  TUserDto = record
    Nome: string;
    Email: string;
  end;

var Dtos := Context.Users
  .Select<TUserDto>(function(U: TUser): TUserDto
    begin
      Result.Nome := U.Name;
      Result.Email := U.Email;
    end)
  .ToList;
```

## Agregações

```pascal
var u := TUser.Props;

// Count
var Total := Context.Users.Count;
var AtivosCount := Context.Users
  .Where(u.IsActive = True)
  .Count;

// Any / Exists (preferido para verificação em Seeders)
var TemAdmin := Context.Users
  .Where(u.Role = 'admin')
  .Any;
```

## Tipos Personalizados (Enums)

Se um tipo específico (como um Enum) não tiver um alias predefinido, crie um usando `Prop<T>`:

```pascal
type
  StatusType = Prop<TOrderStatus>;  // Tipo smart customizado
```

Use na query:
```pascal
var o := TOrder.Props;
var Pagos := Context.Orders
  .Where(o.Status = osPaid)
  .OrderBy(o.Total.Desc)
  .ToList;
```

## Entidades Fantasma (Prototype)

Use `Prototype.Entity<T>` de `Dext.Entity.Prototype` para metadados typesafe em queries complexas:

```pascal
var p := Prototype.Entity<TProduto>;
var Caros := Context.Products
  .Where(p.Preco > 100)
  .ToList;
```

## Execução da Query

Queries são lazy (preguiçosas) — executadas apenas quando você chama um método terminal:

| Método | Efeito |
|--------|--------|
| `.ToList` | Executa e retorna lista |
| `.First` / `.FirstOrDefault` | Executa e retorna um |
| `.Count` | Executa e retorna contagem |
| `.Any` | Executa e retorna boolean |
| `.Find(id)` | Executa e retorna por PK |


## Performance & Cache

### Cache de Geração SQL
O Dext inclui um singleton `TSQLCache` que armazena o SQL gerado para consultas com base em sua estrutura (Assinatura AST). Isso melhora significativamente a performance de queries repetitivas.

O cache é **ativado por padrão** e é thread-safe.

#### Desativando o Cache
Se precisar desativar (ex: para depuração):

```pascal
uses
  Dext.Entity.Cache;

// Desativar globalmente
TSQLCache.Instance.Enabled := False;
```

---

[← Entidades](entidades.md) | [Próximo: Smart Properties →](smart-properties.md)
