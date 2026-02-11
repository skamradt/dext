# Mocking

Crie dublês de teste com `Mock<T>` para interfaces.

> [!IMPORTANT]
> `Mock<T>` é um **Record Genérico** que vive em `Dext.Mocks` — ele **NÃO** faz parte da facade `Dext.Testing`. Ele **NÃO** precisa de `.Free`.

## Habilitando Mock

Interfaces DEVEM ter RTTI habilitado (`{$M+}`) para serem mockáveis:

```pascal
uses
  Dext.Mocks; // Mock<T>, Arg, Times

type
  {$M+} // OBRIGATÓRIO para interfaces mockáveis
  IServico = interface
    ['{...}']
    function Calcular(A: Integer): Integer;
  end;
  {$M-}

procedure TestMock;
begin
  // 1. Criar Mock
  var MeuMock := Mock<IServico>.Create;

  // 2. Setup (definição fluente)
  MeuMock.Setup.Returns(42).When.Calcular(Arg.Any<Integer>);

  // 3. Act
  var Resultado := MeuMock.Instance.Calcular(10); // Retorna 42

  // 4. Verify
  MeuMock.Received(Times.Once).Calcular(10);
end;
```

## Métodos de Setup

```pascal
// Retorna um valor específico
MeuMock.Setup.Returns(42).When.Calcular(Arg.Any<Integer>);

// Retorna valores diferentes em sequência
MeuMock.Setup.Returns(User1).When.GetNext;
// Na segunda chamada, retorna User2, etc.

// Lança exceção
MeuMock.Setup.Throws(ENotFound).When.GetById(Arg.Any<Integer>);
```

## Matching de Argumentos

```pascal
// Qualquer valor de um tipo
MeuMock.Setup.Returns(User).When.FindById(Arg.Any<Integer>);

// Valor exato
MeuMock.Received(Times.Once).FindById(42);

// Argumento condicional
MeuMock.Setup.Returns(True).When.IsValid(Arg.Is<string>(
  function(S: string): Boolean
  begin
    Result := S.Length > 5;
  end));
```

## Verificação

```pascal
// Verificar chamado exatamente uma vez
MeuMock.Received(Times.Once).FindById(1);

// Verificar chamado N vezes
MeuMock.Received(Times.Exactly(3)).Salvar(Arg.Any<TUser>);

// Verificar nunca chamado
MeuMock.DidNotReceive.Deletar(Arg.Any<Integer>);

// Verificar pelo menos/no máximo
MeuMock.Received(Times.AtLeast(1)).ListarTodos;
MeuMock.Received(Times.AtMost(5)).ListarTodos;

// Verificar que nenhuma outra chamada foi feita
MeuMock.VerifyNoOtherCalls;
```

## Exemplo de Test Fixture

```pascal
uses
  Dext.Testing,  // Facade: Should, [TestFixture], [Test]
  Dext.Mocks;    // Mock<T>

type
  [TestFixture]
  TUsuarioServiceTests = class
  private
    FMockRepo: Mock<IUsuarioRepository>;
    FService: TUsuarioService;
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure Deve_Encontrar_Usuario;
  end;

procedure TUsuarioServiceTests.Setup;
begin
  FMockRepo := Mock<IUsuarioRepository>.Create;
  FService := TUsuarioService.Create(FMockRepo.Instance);
end;

procedure TUsuarioServiceTests.Deve_Encontrar_Usuario;
begin
  // Arrange
  var User := TUsuario.Create;
  User.Name := 'Alice';
  FMockRepo.Setup.Returns(User).When.FindById(1);

  // Act
  var Encontrado := FService.GetById(1);

  // Assert
  Should(Encontrado).NotBeNil;
  Should(Encontrado.Name).Be('Alice');

  // Verify
  FMockRepo.Received(Times.Once).FindById(1);
end;
```

---

[← Testes](README.md) | [Próximo: Asserções →](assertions.md)
