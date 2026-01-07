# Specifications

Reusable, composable query patterns.

> 📦 **Example**: [Orm.Specification](../../../Examples/Orm.Specification/)

## The Problem

Queries are often duplicated across the codebase:

```pascal
// In UserService
Users.Where(U => U.IsActive and U.Age >= 18).ToList;

// In ReportService - same logic duplicated
Users.Where(U => U.IsActive and U.Age >= 18).ToList;
```

## The Solution: Specifications

Encapsulate query logic in reusable classes:

```pascal
type
  TActiveAdultSpec = class(TSpecification<TUser>)
  public
    function IsSatisfiedBy(Entity: TUser): Boolean; override;
    function ToExpression: TSpecExpression; override;
  end;

function TActiveAdultSpec.IsSatisfiedBy(Entity: TUser): Boolean;
begin
  Result := Entity.IsActive and (Entity.Age >= 18);
end;

function TActiveAdultSpec.ToExpression: TSpecExpression;
begin
  Result := (TUser.Props.IsActive = True) and (TUser.Props.Age >= 18);
end;
```

## Using Specifications

```pascal
var
  Spec: ISpecification<TUser>;
  Users: IList<TUser>;
begin
  Spec := TActiveAdultSpec.Create;
  Users := Context.Users.Where(Spec).ToList;
end;
```

## Combining Specifications

### AND

```pascal
var
  ActiveSpec: ISpecification<TUser>;
  AdultSpec: ISpecification<TUser>;
  CombinedSpec: ISpecification<TUser>;
begin
  ActiveSpec := TActiveUserSpec.Create;
  AdultSpec := TAdultUserSpec.Create;
  
  CombinedSpec := ActiveSpec.And(AdultSpec);
  
  Users := Context.Users.Where(CombinedSpec).ToList;
end;
```

### OR

```pascal
CombinedSpec := AdminSpec.Or(ModeratorSpec);
```

### NOT

```pascal
InactiveSpec := ActiveSpec.Not;
```

## Parameterized Specifications

```pascal
type
  TAgeRangeSpec = class(TSpecification<TUser>)
  private
    FMinAge, FMaxAge: Integer;
  public
    constructor Create(MinAge, MaxAge: Integer);
    function ToExpression: TSpecExpression; override;
  end;

constructor TAgeRangeSpec.Create(MinAge, MaxAge: Integer);
begin
  inherited Create;
  FMinAge := MinAge;
  FMaxAge := MaxAge;
end;

function TAgeRangeSpec.ToExpression: TSpecExpression;
begin
  Result := (TUser.Props.Age >= FMinAge) and (TUser.Props.Age <= FMaxAge);
end;

// Usage
var
  MiddleAged := TAgeRangeSpec.Create(40, 60);
  Users := Context.Users.Where(MiddleAged).ToList;
```

## Common Specifications

```pascal
// By ID
TByIdSpec<T>.Create(Id);

// Active entities
TActiveSpec<T>.Create;

// Created in date range
TCreatedBetweenSpec<T>.Create(StartDate, EndDate);

// Search by text
TSearchSpec<T>.Create('search term');
```

## Benefits

1. **Reusability** - Define once, use everywhere
2. **Testability** - Test query logic in isolation
3. **Composability** - Build complex queries from simple parts
4. **Maintainability** - Change logic in one place

---

[← Smart Properties](smart-properties.md) | [Next: Relationships →](relationships.md)
