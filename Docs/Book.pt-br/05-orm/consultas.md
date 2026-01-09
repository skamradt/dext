# Consultas

API fluente de queries para recuperar dados.

## Tipos de Retorno: IList<T>

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

// Criar com ownership (auto-free de objetos)
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

### Expressão Lambda

```pascal
var UsuariosAtivos := Context.Users
  .Where(function(U: TUser): Boolean
    begin
      Result := U.IsActive;
    end)
  .ToList;
```

### Smart Properties (Recomendado)

```pascal
var UsuariosAtivos := Context.Users
  .Where(TUser.Props.IsActive = True)
  .ToList;

// Múltiplas condições
var Results := Context.Users
  .Where(
    (TUser.Props.Age >= 18) and 
    (TUser.Props.Status = 'active')
  )
  .ToList;
```

## Ordenação

```pascal
// Ascendente
var Users := Context.Users
  .OrderBy(TUser.Props.Name)
  .ToList;

// Descendente
var Users := Context.Users
  .OrderByDescending(TUser.Props.CreatedAt)
  .ToList;

// Múltiplas colunas
var Users := Context.Users
  .OrderBy(TUser.Props.LastName)
  .ThenBy(TUser.Props.FirstName)
  .ToList;
```

## Paginação

```pascal
var Page := Context.Users
  .OrderBy(TUser.Props.Id)
  .Skip(20)   // Pular primeiros 20
  .Take(10)   // Pegar próximos 10
  .ToList;
```

## Agregações

```pascal
// Count
var Total := Context.Users.Count;
var ActiveCount := Context.Users
  .Where(TUser.Props.IsActive = True)
  .Count;

// Any / Exists
var TemAdmin := Context.Users
  .Where(TUser.Props.Role = 'admin')
  .Any;
```

## Execução de Query

Queries são lazy - executadas apenas quando você:

| Método | Efeito |
|--------|--------|
| `.ToList` | Executar e retornar lista |
| `.First` / `.FirstOrDefault` | Executar e retornar um |
| `.Count` | Executar e retornar contagem |
| `.Any` | Executar e retornar boolean |
| `.Find(id)` | Executar e retornar por PK |

```pascal
// Query NÃO executada ainda
var Query := Context.Users.Where(TUser.Props.Age > 18);

// Query executada AQUI
var Users := Query.ToList;
```


## Performance & Caching

### Cache de Geração SQL
O Dext inclui um singleton `TSQLCache` que armazena o SQL gerado para consultas com base em sua estrutura (Assinatura AST). Isso melhora significativamente a performance de queries repetitivas, pulando a fase de geração de SQL.

O cache é **ativado por padrão** e é thread-safe.

#### Desativando o Cache
Se precisar desativar o cache (ex: para debugging ou cenários dinâmicos específicos), você pode alterá-lo globalmente:

```pascal
uses
  Dext.Entity.Cache;

// Desativar globalmente
TSQLCache.Instance.Enabled := False;
```

---

[← Entidades](entidades.md) | [Próximo: Smart Properties →](smart-properties.md)
