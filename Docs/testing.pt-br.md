# 🧪 Dext Testing Framework

O **Dext Testing Framework** é uma biblioteca de testes nativa para Delphi, projetada para padrões modernos como TDD (Test Driven Development) e BDD (Behavior Driven Development). Ela fornece uma API fluente e expressiva para **Mocks** e **Asserções**, eliminando a necessidade de dependências externas como extensões do DUnitX ou bibliotecas de mock de terceiros.

## 📦 Funcionalidades

### 1. Fluent Assertions (Asserções Fluentes)
Inspirado no *FluentAssertions* do .NET, escreva testes legíveis e expressivos:

```pascal
// Strings
Should('Hello World').StartWith('Hello').And.EndWith('World');

// Números
Should(Order.Total).BeGreaterThan(0);

// Coleções
ShouldList<string>.Create(Items).HaveCount(3).Contain('Dext');

// Exceções
Should(procedure begin raise EInvalidOp.Create('Erro'); end)
  .Throw<EInvalidOp>;
```

### 2. Mocking Expressivo
Um poderoso motor de mocks utilizando `TVirtualInterface` para criar proxies dinâmicos para interfaces.

```pascal
var
  EmailEngine: Mock<IEmailEngine>;
  Calculator: Mock<ICalculator>; // Assuming ICalculator is defined elsewhere for these examples
begin
  EmailEngine := Mock<IEmailEngine>.Create;
  
  // Configurar comportamento
  // Retorno Básico
  EmailEngine.Setup.Returns(True).When.Send('john@doe.com', Arg.Any<string>);

  // Exemplos adicionais de configuração de comportamento:
  // Retorno Básico
  Calculator := Mock<ICalculator>.Create;
  Calculator.Setup.Returns(42).When.Add(Arg.Any<Integer>, Arg.Any<Integer>);

  // Sequência de Retornos (1ª chamada -> 10, 2ª chamada -> 20)
  Calculator.Setup.ReturnsInSequence([10, 20]).When.Add(1, 1);

  // Callback (Efeitos colaterais ou Captura de Argumentos)
  Calculator.Setup.Callback(procedure(Args: TArray<TValue>)
    begin
      Log('Chamado com ' + Args[0].ToString);
    end).When.DoSomething(Arg.IsAny);
  
  // Usar o proxy
  MyService.Process(EmailEngine.Instance);
  
  // Verificar chamadas
  EmailEngine.Received(Times.Once).Send('john@doe.com', Arg.Any<string>);
end;
```

## 🚀 Começando

### Instalação
O framework de testes é parte da distribuição principal do Dext.
1. Certifique-se de que o pacote `Dext.Testing.dpk` está compilado.
2. Adicione `Dext.Mocks`, `Dext.Assertions` e `Dext.Interception` ao uses da sua unit.

> ⚠️ **Importante:** As interfaces a serem mockadas DEVEM ter a diretiva `{$M+}` (geração de RTTI) ativada.

### Escrevendo seu primeiro teste

```pascal
program MyTests;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Dext.Assertions,
  Dext.Mocks;

type
  {$M+} // Habilita RTTI para mocking
  ICalculator = interface
    ['{GUID}']
    function Add(A, B: Integer): Integer;
  end;
  {$M-}

procedure TestCalculator;
var
  MockCalc: Mock<ICalculator>;
begin
  // Arrange (Preparar)
  MockCalc := Mock<ICalculator>.Create;
  MockCalc.Setup.Returns(10).When.Add(5, 5);

  // Act (Agir)
  var Result := MockCalc.Instance.Add(5, 5);

  // Assert (Verificar)
  Should(Result).Be(10);
  MockCalc.Received.Add(5, 5);
end;

begin
  try
    TestCalculator;
    WriteLn('Todos os testes passaram!');
  except
    on E: Exception do WriteLn('Teste Falhou: ', E.Message);
  end;
end.
```

## 🔍 Assertions API

A unit `Dext.Assertions` fornece uma função global `Should()` para a maioria dos tipos.

### Strings
```pascal
Should(Name).Be('John');
Should(Name).NotBe('Doe');
Should(Name).StartWith('Jo');
Should(Name).EndWith('hn');
Should(Name).Contain('oh');
Should(Name).BeEmpty;
Should(Name).NotBeEmpty;
Should(Name).BeEquivalentTo('JOHN'); // Case insensitive (ignora maiúsculas/minúsculas)
```

### Números (Integer, Double, Int64)
```pascal
Should(Age).Be(18);
Should(Age).BeGreaterThan(10);
Should(Age).BeLessThan(100);
Should(Age).BeInRange(18, 99);
Should(Age).BePositive;
Should(Age).BeNegative;
Should(Age).BeZero;
```

### Booleans
```pascal
Should(IsActive).BeTrue;
Should(IsActive).BeFalse;
```

### Datas e Horas
Use `ShouldDate()` para clareza e evitar ambiguidade com números.
```pascal
ShouldDate(Now).BeCloseTo(Now, 1000); // 1000ms de tolerância
ShouldDate(DueDate).BeAfter(SomeDate);
ShouldDate(DueDate).BeBefore(SomeDate);
ShouldDate(EventDate).BeSameDateAs(Now); // Ignora hora
```

### Objetos
```pascal
Should(User).BeNil;
Should(User).NotBeNil;
Should(User).BeOfType<TAdmin>;

// Deep Comparison (usando serialização JSON)
Should(Dto1).BeEquivalentTo(Dto2);
```

### Actions (Exceções)
```pascal
Should(procedure begin ... end).Throw<EInvalidOp>;
Should(procedure begin ... end).NotThrow;
```

### Listas e Coleções
Para coleções, use `ShouldList<T>.Create(...)`.

```pascal
var List: TList<Integer>;
...
ShouldList<Integer>.Create(List).HaveCount(5)
  .Contain(10)
  .NotContain(99);
```

## 🎭 Mocking API

A unit `Dext.Mocks` permite definir comportamentos e verificar interações.

### Configurar Retornos (Returns)
```pascal
// Retornar valor específico
Repo.Setup.Returns(User).When.GetById(1);

// Retornar baseado em argumentos (stubbing)
Repo.Setup.Returns(nil).When.GetById(Arg.Is<Integer>(function(Id: Integer): Boolean
  begin
    Result := Id < 0;
  end));
```

### Returns Simplificado (Overloads)
Sintaxe simplificada para tipos comuns:
```pascal
Mock.Setup.Returns(10).When.GetInt;      // Integer
Mock.Setup.Returns('Data').When.GetString; // String
Mock.Setup.Returns(True).When.GetBool;   // Boolean
```

### Argument Matchers
- `Arg.Any<T>`: Corresponde a qualquer valor do tipo T.
- `Arg.Is<T>(Predicate)`: Corresponde se o predicado retornar true.
- `Arg.Matches<T>(Value)`: Corresponde se for igual ao Valor.

### Verificação
```pascal
// Garantir que o método foi chamado exatamente uma vez
Mock.Received(Times.Once).Save(Arg.Any<TUser>);

// Garantir que o método nunca foi chamado
Mock.Received(Times.Never).Delete(Arg.Any<Integer>);

// Garantir que o método foi chamado pelo menos N vezes
Mock.Received(Times.AtLeast(2)).Log(Arg.Any<string>);
```

### Strict Mocks
Por padrão, mocks são **Loose** (métodos retornam valores padrão se não configurados). Você pode criar mocks **Strict** que levantam exceções para chamadas não configuradas.

```pascal
var M := Mock<IFaa>.Create(TMockBehavior.Strict);
```

### Mocking de Classes
Você pode criar mocks de métodos `virtual` de classes normais, similar a interfaces.
```pascal
type
  TCustomerRepo = class
  public
    function Count: Integer; virtual; // Deve ser virtual
  end;
  
var 
  Repo: Mock<TCustomerRepo>;
begin
  Repo := Mock<TCustomerRepo>.Create;
  Repo.Setup.Returns(10).When.Count;
end;
```

## 🛠️ Integração via CLI e Code Coverage

O Dext fornece uma ferramenta CLI poderosa para rodar seus testes e analisar a cobertura de código automaticamente.

### Rodando Testes

Execute todos os testes do seu projeto via linha de comando:

```bash
dext test
```

### Gerando Cobertura de Código

Para gerar um relatório de cobertura, simplesmente adicione a flag `--coverage`:

```bash
dext test --coverage
```

Este comando irá:
1. Compilar seu projeto com informações de debug (arquivo `-map`).
2. Executar seus testes usando `CodeCoverage.exe`.
3. Gerar um relatório HTML legível em `TestOutput/report`.
4. Gerar um relatório XML compatível com **SonarQube** (`dext_coverage.xml`).

### Quality Gates (Limite de Cobertura)

Você pode obrigar uma porcentagem mínima de cobertura para falhar o build (Quality Gate) se o padrão não for atendido. Configure isso no `dext.json`:

```json
{
  "test": {
    "project": "Tests/MyTests.dproj",
    "coverageThreshold": 80.0,
    "coverageExclude": [
      "*Dext.*",
      "*Test*"
    ]
  }
}
```

Se a cobertura cair abaixo de 80%, o comando `dext test` sairá com um código de erro, perfeito para pipelines de CI/CD.

## 🌟 Recursos Avançados

### Snapshot Testing
Simplifique o teste de objetos complexos ou strings grandes comparando-os com um arquivo de "snapshot" armazenado.

```pascal
// Primeira execução: Cria 'Snapshots/User_V1.json'
// Próximas execuções: Compara o resultado com o arquivo
Should(UserDTO).MatchSnapshot('User_V1');
```

Para atualizar os snapshots, defina a variável de ambiente `SNAPSHOT_UPDATE=1`.

### Auto-Mocking Container
Reduza o código repetitivo (boilerplate) em seus testes criando mocks automaticamente e injetando-os no construtor do Sistema Sob Teste (SUT).

```pascal
uses Dext.Mocks.Auto;

var
  Mocker: TAutoMocker;
  Service: TMyService;
begin
  Mocker := TAutoMocker.Create;
  try
    // Cria mocks automaticamente (Interfaces e Classes Virtuais) e injeta no construtor
    Service := Mocker.CreateInstance<TMyService>;
    
    // Acesse o mock injetado para configurar comportamento
    Mocker.GetMock<IRepo>.Setup.Returns(User).When.GetById(1);
    
    Service.DoWork;
  finally
    Mocker.Free;
  end;
end;
```
