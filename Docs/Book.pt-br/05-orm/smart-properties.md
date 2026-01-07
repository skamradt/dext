# Smart Properties

Expressões de query type-safe usando `Prop<T>`.

> 📦 **Exemplo**: [Web.SmartPropsDemo](../../../Examples/Web.SmartPropsDemo/)

## O Problema

Queries tradicionais baseadas em strings são propensas a erros:

```pascal
// Typo não será capturado em tempo de compilação!
Query.Where('usre_name = ?', ['João']);  // Bug: "usre_name"
```

## A Solução: Smart Properties

```pascal
type
  [Table('users')]
  TUser = class
  public
    class var Props: TUserProps;  // Smart properties
    
    property Id: Integer;
    property Name: string;
    property Age: Integer;
  end;
  
  TUserProps = record
    Id: Prop<Integer>;
    Name: Prop<string>;
    Age: Prop<Integer>;
  end;

// Query type-safe!
var Adultos := Context.Users
  .Where(TUser.Props.Age >= 18)  // Verificado em tempo de compilação!
  .OrderBy(TUser.Props.Name)
  .ToList;
```

## Operações de Query

### Comparação

```pascal
TUser.Props.Age = 25       // Igual
TUser.Props.Age <> 25      // Diferente
TUser.Props.Age > 18       // Maior que
TUser.Props.Age >= 18      // Maior ou igual
TUser.Props.Age < 65       // Menor que
TUser.Props.Age <= 65      // Menor ou igual
```

### Operações de String

```pascal
TUser.Props.Name.Contains('João')
TUser.Props.Name.StartsWith('J')
TUser.Props.Name.EndsWith('Silva')
TUser.Props.Email.IsNull
TUser.Props.Email.IsNotNull
```

### Operadores Lógicos

```pascal
// AND
(TUser.Props.Age >= 18) and (TUser.Props.Age <= 65)

// OR
(TUser.Props.Status = 'ativo') or (TUser.Props.IsAdmin = True)

// NOT
not TUser.Props.IsDeleted
```

## Exemplo Completo

```pascal
var
  AdultosAtivos: IList<TUser>;
begin
  AdultosAtivos := Context.Users
    .Where(
      (TUser.Props.Age >= 18) and 
      (TUser.Props.Status = 'ativo') and
      (TUser.Props.Email.IsNotNull)
    )
    .OrderBy(TUser.Props.Name)
    .Take(10)
    .ToList;
end;
```

---

[← Consultas](consultas.md) | [Próximo: Specifications →](specifications.md)
