# Mocking

Crie objetos simulados para testes unitários com `Mock<T>`.

> 📦 **Exemplo**: [Testes do Core](../../../Sources/Testing/)

## Por que usar Mocks?

Mocks permitem isolar a classe que você está testando (SUT - System Under Test) de suas dependências (banco de dados, APIs externas, serviços complexos).

## Criando um Mock

```pascal
var
  MockRepo: Mock<IUserRepository>;
begin
  MockRepo := Mock<IUserRepository>.Create;
  
  // Instance retorna o objeto que implementa a interface
  Service := TUserService.Create(MockRepo.Instance);
end;
```

## Configurando Comportamento (Setup)

Configure o que o mock deve retornar quando um método for chamado:

```pascal
// Retornar um valor fixo
MockRepo.Setup
  .WhenCalling('GetById')
  .WithArgs([1])
  .Returns(UsuarioEsperado);

// Lançar uma exceção
MockRepo.Setup
  .WhenCalling('Delete')
  .Throws(EInvalidOperation.Create('Não permitido'));
```

## Verificação (Verify)

Verifique se um método foi chamado com os argumentos corretos:

```pascal
// Verficar se foi chamado uma vez
MockRepo.Received(Times.Once).Save(IdValido);

// Verificar se NUNCA foi chamado
MockRepo.DidNotReceive.Delete(Arg.Any<Integer>);

// Verificar número exato de vezes
MockRepo.Received(3).Update(Arg.Is<TUser>(function(U: TUser): Boolean
  begin
    Result := U.Status = 'Ativo';
  end));
```

## Argument Matchers

Use matchers se você não souber o valor exato:

```pascal
MockRepo.Received.Find(Arg.Any<Integer>);      // Qualquer inteiro
MockRepo.Received.Search(Arg.Contains('txt')); // String que contém
```

---

[← Testes](README.md) | [Próximo: Assertions →](assertions.md)
