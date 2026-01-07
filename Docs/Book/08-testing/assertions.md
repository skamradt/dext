# Assertions

Fluent assertion syntax with `Should()`.

## Basic Assertions

```pascal
uses
  Dext.Testing.Assertions;

// Equality
Value.Should.Be(42);
Value.Should.NotBe(0);

// Nil checks
Obj.Should.NotBeNil;
Obj.Should.BeNil;

// Boolean
Flag.Should.BeTrue;
Flag.Should.BeFalse;

// Comparisons
Value.Should.BeGreaterThan(10);
Value.Should.BeLessThan(100);
Value.Should.BeInRange(1, 100);
```

## String Assertions

```pascal
Text.Should.Be('Hello');
Text.Should.Contain('ell');
Text.Should.StartWith('He');
Text.Should.EndWith('lo');
Text.Should.Match('^[A-Z]');  // Regex
Text.Should.BeEmpty;
Text.Should.NotBeEmpty;
Text.Should.HaveLength(5);
```

## Collection Assertions

```pascal
List.Should.HaveCount(5);
List.Should.BeEmpty;
List.Should.NotBeEmpty;
List.Should.Contain(Item);
List.Should.NotContain(Item);
List.Should.ContainOnly(Item1, Item2);
List.Should.BeOrdered;

// All items match predicate
List.Should.AllMatch(function(X: Integer): Boolean
  begin
    Result := X > 0;
  end);
```

## Object Assertions

```pascal
// Type checking
Obj.Should.BeOfType<TUser>;
Obj.Should.BeAssignableTo<IPerson>;

// Deep equality
Obj1.Should.BeEquivalentTo(Obj2);

// Property checking
User.Should.HaveProperty('Name').WithValue('John');
```

## Exception Assertions

```pascal
Should.Raise<EArgumentException>(procedure
  begin
    Service.DoSomethingBad;
  end);

Should.RaiseAny(procedure
  begin
    Service.DoSomethingBad;
  end);

Should.NotRaise(procedure
  begin
    Service.DoSomethingSafe;
  end);
```

## Custom Messages

```pascal
Value.Should.Be(42, 'Expected value to be 42');
```

## Soft Assertions

Collect multiple failures before reporting:

```pascal
Assert.Multiple(procedure
  begin
    User.Name.Should.Be('John');
    User.Age.Should.BeGreaterThan(18);
    User.Email.Should.Contain('@');
  end);
// All assertions run, all failures reported together
```

---

[← Mocking](mocking.md) | [Next: Snapshots →](snapshots.md)
