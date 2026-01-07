# Consultas

API fluente de queries para recuperar dados.

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

---

[← Entidades](entidades.md) | [Próximo: Smart Properties →](smart-properties.md)
