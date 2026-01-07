# 8. Testes

O Dext inclui um framework de testes poderoso com mocking, assertions e test runners.

## Capítulos

1. [Mocking](mocking.md) - `Mock<T>` para interfaces e classes
2. [Assertions](assertions.md) - Sintaxe fluente `Should()`
3. [Snapshots](snapshots.md) - Testes de snapshot JSON

## Início Rápido

```pascal
uses
  Dext.Testing;

type
  [TestFixture]
  TUserServiceTests = class
  private
    FService: TUserService;
    FMockRepo: Mock<IUserRepository>;
  public
    [Setup]
    procedure Setup;
    
    [Test]
    procedure GetUser_RetornaUsuario_QuandoExiste;
  end;

procedure TUserServiceTests.Setup;
begin
  FMockRepo := Mock<IUserRepository>.Create;
  FService := TUserService.Create(FMockRepo.Instance);
end;

procedure TUserServiceTests.GetUser_RetornaUsuario_QuandoExiste;
var
  User: TUser;
begin
  // Arrange
  User := TUser.Create;
  User.Name := 'João';
  
  FMockRepo.Setup.WhenCalling('FindById').Returns(User);
  
  // Act
  var Result := FService.GetById(1);
  
  // Assert
  Result.Should.NotBeNil;
  Result.Name.Should.Be('João');
  
  // Verify
  FMockRepo.Received(Times.Once).FindById(1);
end;
```

## Executar Testes

```bash
dext test
dext test --coverage
dext test --html --output RelatorioTestes.html
```

---

[← Tempo Real](../07-tempo-real/README.md) | [Próximo: Mocking →](mocking.md)
