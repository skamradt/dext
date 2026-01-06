# Configuração da Aplicação

O Dext Framework oferece um sistema de configuração robusto e hierárquico, permitindo que você controle o comportamento da sua aplicação através de diferentes fontes, como arquivos JSON, YAML, variáveis de ambiente e argumentos de linha de comando.

## Fontes de Configuração Padrão

Ao inicializar uma aplicação web padrão (`Dext.Web.WebApplication`), o framework configura automaticamente o `IConfiguration` carregando valores das seguintes fontes, nesta ordem de prioridade (fontes carregadas por último sobrescrevem as anteriores):

1.  **Arquivos Base**:
    *   `appsettings.json` (Opcional)
    *   `appsettings.yaml` (Opcional)
    *   `appsettings.yml` (Opcional)

2.  **Arquivos de Ambiente** (Baseados na variável `DEXT_ENVIRONMENT`):
    *   `appsettings.{Environment}.json` (Opcional)
    *   `appsettings.{Environment}.yaml` (Opcional)
    *   `appsettings.{Environment}.yml` (Opcional)

3.  **Variáveis de Ambiente**:
    *   Carrega todas as variáveis de ambiente do sistema.

> **Nota do Ambiente**: O sufixo `{Environment}` é determinado pela variável de ambiente `DEXT_ENVIRONMENT`. Se esta variável não estiver definida, o valor padrão assumido é `Production`.

### Exemplo de Sobrescrita

Se você definir `Config:Porta` como `8080` no `appsettings.json` e como `9000` no `appsettings.Production.yaml`, o valor final será `9000` quando rodar em produção.

## Formatos Suportados

### JSON

O formato padrão e mais comum.

```json
{
  "Database": {
    "Host": "localhost",
    "Port": 5432
  },
  "Logging": {
    "Level": "Debug"
  }
}
```

### YAML

O Dext suporta nativamente arquivos YAML (`.yaml` ou `.yml`), que oferecem uma sintaxe mais limpa e legível.

```yaml
Database:
  Host: "localhost"
  Port: 5432

Logging:
  Level: "Debug"
```

### Variáveis de Ambiente

Chaves hierárquicas podem ser definidas usando dois pontos (`:`) ou underline duplo (`__`) como separadores.

*   Linux/Bash: `export Database__Host=192.168.1.1`
*   Windows (PowerShell): `$env:Database__Host="192.168.1.1"`

## Acessando Configurações

### Via IConfiguration (Acesso direto)

Você pode injetar `IConfiguration` em qualquer serviço.

```delphi
uses Dext.Configuration.Interfaces;

// ...

constructor TMyService.Create(Config: IConfiguration);
begin
  var Host := Config['Database:Host']; // Retorna string
end;
```

### Via Options Pattern (Recomendado)

Para uma abordagem mais limpa e fortemente tipada, utilize o **Options Pattern**. O Dext mapeia automaticamente a hierarquia de configuração para classes Delphi.

Veja o guia completo em [Options Pattern](options-pattern.md).

```delphi
type
  TDatabaseConfig = class
  public
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
  end;

// ...

// O mapeamento funciona igual, venha do JSON ou do YAML
TOptionsServiceCollectionExtensions.Configure<TDatabaseConfig>(
  Services, Configuration.GetSection('Database')
);
```

## Adicionando Fontes Personalizadas

Você pode adicionar suas próprias fontes de configuração modificando o `IConfigurationBuilder` antes da construção da aplicação, embora o padrão cubra a maioria dos casos.

```delphi
var
  Builder: IConfigurationBuilder;
begin
  Builder := TConfigurationBuilder.Create;
  Builder.Add(TYamlConfigurationSource.Create('config/custom.yaml'));
  // ...
end;
```
