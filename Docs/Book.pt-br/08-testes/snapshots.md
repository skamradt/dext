# Snapshots

Simplifique a verificação de objetos complexos comparando contra baselines JSON.

## Por que usar Snapshots?

Em vez de escrever dezenas de asserções `Should.Be` para um objeto grande, você salva o objeto inteiro em um arquivo JSON na primeira execução e o Dext comparará as execuções futuras contra esse "visto".

## Exemplo Básico

```pascal
[Test]
procedure TestarRespostaComplexa;
begin
  var Resultado := Servico.GerarRelatorio(123);
  
  // Na primeira vez, cria o arquivo .json
  // Nas próximas, compara se o conteúdo é idêntico
  Resultado.MatchSnapshot;
end;
```

## Onde os Snapshots são salvos?

Os arquivos são criados automaticamente em uma pasta chamada `__snapshots__` no mesmo diretório do seu arquivo de teste.

```
tests/
├── GeradorRelatorioTests.pas
└── __snapshots__/
    └── GeradorRelatorioTests.TestarRespostaComplexa.json
```

## Atualizando Snapshots

Se você alterou a lógica e o novo JSON está correto, você pode atualizar os snapshots via linha de comando:

```bash
dext test --update-snapshots
```

## Ignorando Campos Variáveis

Se o seu JSON contém campos que mudam a cada execução (como Timestamps ou IDs aleatórios), você pode ignorá-los:

```pascal
Resultado.MatchSnapshot(procedure(Options: TSnapshotOptions)
  begin
    Options.IgnorePaths(['$.DataGeracao', '$.IdentificadorUnico']);
  end);
```

---

[← Assertions](assertions.md) | [Próximo: CLI →](../09-cli/README.md)
