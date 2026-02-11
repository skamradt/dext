# 8. Testes

O Dext inclui um framework de testes poderoso inspirado no NUnit/xUnit, com mocking, assertions fluentes e test runner integrado.

## Capítulos

1. [Mocking](mocking.md) - `Mock<T>` para interfaces
2. [Assertions](assertions.md) - Sintaxe fluente `Should()`
3. [Snapshots](snapshots.md) - Testes de snapshot JSON

## Estrutura do Projeto de Testes (.dpr)

Crie um projeto Console Application separado para os testes:

```pascal
program MeuProjeto.Testes;

{$APPTYPE CONSOLE}

uses
  Dext.MM,             // Opcional: Wrapper FastMM5
  Dext.Utils,          // SetConsoleCharSet, ConsolePause
  System.SysUtils,
  Dext.Testing,        // Facade principal (Assert, TTest, Should)
  MeusTestes in 'MeusTestes.pas';

begin
  SetConsoleCharSet;   // OBRIGATÓRIO para todos os projetos console
  try
    // Configuração Fluente do Runner
    // TTest.SetExitCode define automaticamente ExitCode=0 (sucesso) ou 1 (falha)
    TTest.SetExitCode(
      TTest.Configure
        .Verbose            // Inclusão obrigatória para saída detalhada
        .RegisterFixtures([TCalculadoraTests, TUsuarioServiceTests])
        .Run
    );
  except
    on E: Exception do
      Writeln('Erro: ', E.Message);
  end;
  ConsolePause;        // OBRIGATÓRIO: mantém console aberto na IDE
end.
```

## Escrevendo Testes (Atributos)

```pascal
uses
  Dext.Testing; // Facade única

type
  [TestFixture]
  TCalculadoraTests = class
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure Deve_Somar_Numeros;

    [Test]
    [TestCase(1, 2, 3)]
    [TestCase(10, 5, 15)]
    procedure Deve_Somar_Com_Parametros(A, B, Esperado: Integer);
  end;
```

## Início Rápido

```pascal
uses
  Dext.Testing,   // Facade: Assert, Should, TTest
  Dext.Mocks;     // Mock<T> - record genérico, NÃO está na facade

type
  [TestFixture]
  TUsuarioServiceTests = class
  private
    FService: TUsuarioService;
    FMockRepo: Mock<IUsuarioRepository>;
  public
    [Setup]
    procedure Setup;

    [Test]
    procedure GetUsuario_RetornaUsuario_SeExiste;
  end;

procedure TUsuarioServiceTests.Setup;
begin
  FMockRepo := Mock<IUsuarioRepository>.Create;
  FService := TUsuarioService.Create(FMockRepo.Instance);
end;

procedure TUsuarioServiceTests.GetUsuario_RetornaUsuario_SeExiste;
var
  User: TUsuario;
begin
  // Arrange
  User := TUsuario.Create;
  User.Name := 'João';
  FMockRepo.Setup.Returns(User).When.FindById(Arg.Any<Integer>);

  // Act
  var Result := FService.GetById(1);

  // Assert
  Should(Result).NotBeNil;
  Should(Result.Name).Be('João');

  // Verify
  FMockRepo.Received(Times.Once).FindById(1);
end;
```

> [!IMPORTANT]
> `Mock<T>` é um **Record Genérico** — ele vive em `Dext.Mocks` (NÃO na facade `Dext.Testing`) e **NÃO** precisa de `.Free`.

## Assertions Fluentes

```pascal
// Valores Simples
Should(Total).Be(100);
Should(Nome).StartWith('Jo').AndAlso.EndWith('ão');

// Coleções
Should(Lista).Contain(Item);
Should(Lista).HaveCount(5);

// Exceções
Should(procedure begin Calc.DivByZero end).Throw<EInvalidOp>;

// Asserções Inteligentes (Fortemente Tipadas)
var u := Prototype.Entity<TUsuario>; // Entidade fantasma para metadados
Should(User).HaveValue(u.Name, 'Maria');
```

## Teste de Entidades (Aviso sobre Memória)

Ao testar entidades com coleções `OwnsObjects=False` (necessário para compatibilidade com ORM), **você deve liberar itens filhos manualmente** no `finally` do teste:

```pascal
var Pedido := TPedido.Create;
var Item := TItemPedido.Create;  // Criado manualmente
try
  Pedido.Itens.Add(Item);
  Pedido.CalcularTotal;
  // Assert...
finally
  Pedido.Free; // Libera Pedido e a Lista (mas NÃO o Item)
  Item.Free;   // OBRIGATÓRIO: Libera o item filho manualmente
end;
```

**Por quê?** Entidades usam `OwnsObjects = False` para evitar Double Free quando gerenciadas por um DbContext. Em testes unitários (sem DbContext), isso significa que objetos filhos devem ser liberados pelo testador.

## Testes de Integração (Scripts PowerShell)

Toda Web API deve ter um script de teste de integração PowerShell (ex: `Test.MeuProjeto.ps1`) na raiz do projeto.

### Estrutura Recomendada

1. Configuração (BaseURL, codificação UTF-8)
2. Health Check (valida se o servidor responde)
3. Auth / Geração de Token
4. Testes de casos de uso (CRUD, Fluxos de Negócio)
5. Validação de resultados (Códigos HTTP, conteúdo JSON)

### Dicas

- **Erros IPv6/404**: Use sempre `$baseUrl = "http://127.0.0.1:9000"` em vez de `localhost`
- **Headers**: Defina `Accept: application/json` e `Content-Type: application/json; charset=utf-8` explicitamente
- **Enums**: Por padrão, o Dext serializa enums como strings (`"tsAberto"` e não `1`)
- **JWT**: Se a API usar JWT, inclua uma função `New-JwtToken` no script

## Executar Testes

```bash
dext test
dext test --coverage
dext test --html --output RelatorioTestes.html
```

---

[← Tempo Real](../07-tempo-real/README.md) | [Próximo: Mocking →](mocking.md)
