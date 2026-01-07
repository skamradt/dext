# Assertions

Sintaxe fluente e expressiva para validação de testes com `Should`.

## Sintaxe Should

Dext Testing fornece uma API fluente que torna os testes fáceis de ler:

```pascal
Value.Should.Be(10);
Name.Should.Contain('Dext');
List.Should.HaveCount(5);
```

## Tipos de Asserções

### Valores Numéricos

```pascal
Id.Should.Be(123);
Count.Should.BeGreaterThan(0);
Price.Should.BeInRange(10, 50);
```

### Strings

```pascal
Email.Should.NotBeEmpty;
Name.Should.StartWith('C');
Description.Should.MatchRegex('^[a-z]+$');
```

### Objetos e Nil

```pascal
User.Should.NotBeNil;
Order.Should.BeOfType<TOrder>;
```

### Listas e Coleções

```pascal
Users.Should.NotBeEmpty;
Users.Should.Contain(AdminUser);
Users.Should.All.Satisfy(function(U: TUser): Boolean
  begin
    Result := U.IsActive;
  end);
```

### Exceções

Verifique se um código lança a exceção correta:

```pascal
Assert.Throws<EArgumentException>(procedure
  begin
    Service.Process(nil);
  end);
```

## Soft Asserts (Multiple)

Verifique múltiplas condições sem interromper no primeiro erro:

```pascal
Assert.Multiple(procedure
  begin
    User.Name.Should.Be('João');
    User.Email.Should.Contain('@');
    User.Age.Should.Be(30);
  end);
```

---

[← Mocking](mocking.md) | [Próximo: Snapshots →](snapshots.md)
