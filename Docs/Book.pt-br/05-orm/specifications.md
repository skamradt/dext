# Specifications

Padrões de query reutilizáveis e composáveis.

> 📦 **Exemplo**: [Orm.Specification](../../../Examples/Orm.Specification/)

## O Problema

Queries são frequentemente duplicadas pelo codebase:

```pascal
// No UserService
Users.Where(U => U.IsActive and U.Age >= 18).ToList;

// No ReportService - mesma lógica duplicada
Users.Where(U => U.IsActive and U.Age >= 18).ToList;
```

## A Solução: Specifications

Encapsule lógica de query em classes reutilizáveis:

```pascal
type
  TAdultoAtivoSpec = class(TSpecification<TUser>)
  public
    function IsSatisfiedBy(Entity: TUser): Boolean; override;
    function ToExpression: TSpecExpression; override;
  end;

function TAdultoAtivoSpec.IsSatisfiedBy(Entity: TUser): Boolean;
begin
  Result := Entity.IsActive and (Entity.Age >= 18);
end;

function TAdultoAtivoSpec.ToExpression: TSpecExpression;
begin
  Result := (TUser.Props.IsActive = True) and (TUser.Props.Age >= 18);
end;
```

## Usando Specifications

```pascal
var
  Spec: ISpecification<TUser>;
  Users: IList<TUser>;
begin
  Spec := TAdultoAtivoSpec.Create;
  Users := Context.Users.Where(Spec).ToList;
end;
```

## Combinando Specifications

### AND

```pascal
CombinedSpec := ActiveSpec.And(AdultSpec);
```

### OR

```pascal
CombinedSpec := AdminSpec.Or(ModeratorSpec);
```

### NOT

```pascal
InactiveSpec := ActiveSpec.Not;
```

## Benefícios

1. **Reutilização** - Defina uma vez, use em todo lugar
2. **Testabilidade** - Teste lógica de query isoladamente
3. **Composição** - Construa queries complexas a partir de partes simples
4. **Manutenibilidade** - Altere a lógica em um único lugar

---

[← Smart Properties](smart-properties.md) | [Próximo: Relacionamentos →](relacionamentos.md)
