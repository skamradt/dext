# 🔧 Core.TestConfig - Demo do Sistema de Configuração

Uma aplicação console simples demonstrando o **Sistema de Configuração do Dext** - um gerenciamento de configuração flexível e em camadas, inspirado no ASP.NET Core.

---

## ✨ Funcionalidades

- **Múltiplas Fontes de Configuração**: Combine arquivos JSON, variáveis de ambiente e mais
- **Chaves Hierárquicas**: Acesse valores aninhados usando `:` como delimitador (ex: `Logging:LogLevel:Default`)
- **Override via Variáveis de Ambiente**: Sobrescreva qualquer valor de config via variáveis de ambiente
- **Cross-Platform**: Funciona no Windows e Linux/macOS
- **Filtro por Prefixo**: Filtre variáveis de ambiente por prefixo (ex: `DEXT_`)

---

## 🚀 Começando

### Pré-requisitos
- Delphi 11+ (Alexandria ou posterior)
- Dext Framework no Library Path

### Executando o Exemplo

1. Abra `Core.TestConfig.dproj` no Delphi
2. Compile o projeto (Console Application)
3. Execute o binário

> **Nota**: O exemplo irá **criar automaticamente** um `appsettings.json` padrão na pasta de output se não existir. Isso é apenas para propósitos de demonstração - em aplicações de produção, este arquivo deve ser deployado com sua aplicação (veja comentários no código fonte).

### Saída Esperada

```
Starting Configuration Test...
Adding JSON source...
Adding Environment Variables source...
Building configuration...
Reading values...
AppSettings:Message = Hello from appsettings.json!
Logging:LogLevel:Default = Information
PATH length = 1234
Test Finished.
```

---

## 📖 Como Funciona

### 1. Criar Configuration Builder

```delphi
var
  Builder: IConfigurationBuilder;
  Config: IConfigurationRoot;
begin
  Builder := TConfigurationBuilder.Create;
```

### 2. Adicionar Fontes de Configuração

```delphi
// Adicionar fonte de arquivo JSON (optional = true significa que o arquivo é opcional)
Builder.Add(TJsonConfigurationSource.Create('appsettings.json', True));

// Adicionar fonte de variáveis de ambiente
Builder.Add(TEnvironmentVariablesConfigurationSource.Create);

// Adicionar variáveis de ambiente com filtro de prefixo
Builder.Add(TEnvironmentVariablesConfigurationSource.Create('DEXT_'));
```

### 3. Construir e Ler Configuração

```delphi
Config := Builder.Build;

// Ler valores usando chaves hierárquicas
var Message := Config['AppSettings:Message'];
var LogLevel := Config['Logging:LogLevel:Default'];

// Variáveis de ambiente também são acessíveis
var Path := Config['PATH'];
```

---

## 🔑 Conceitos Principais

### Chaves Hierárquicas

Valores de configuração são acessados usando `:` como delimitador:

| Chave | Caminho JSON |
|-------|--------------|
| `AppSettings:Message` | `{ "AppSettings": { "Message": "..." } }` |
| `Logging:LogLevel:Default` | `{ "Logging": { "LogLevel": { "Default": "..." } } }` |

### Mapeamento de Variáveis de Ambiente

Variáveis de ambiente usam `__` (duplo underscore) para representar `:`:

| Variável de Ambiente | Chave de Configuração |
|---------------------|----------------------|
| `DEXT__Database__Host` | `Database:Host` |
| `AppSettings__Message` | `AppSettings:Message` |

### Prioridade das Fontes

Fontes posteriores sobrescrevem as anteriores:

```delphi
Builder.Add(TJsonConfigurationSource.Create('appsettings.json', True));  // Base
Builder.Add(TJsonConfigurationSource.Create('appsettings.local.json', True));  // Override
Builder.Add(TEnvironmentVariablesConfigurationSource.Create);  // Override final
```

---

## 🛠️ Fontes de Configuração

| Fonte | Unit | Descrição |
|-------|------|-----------|
| Arquivo JSON | `Dext.Configuration.Json` | Carrega de arquivos `.json` |
| Variáveis de Ambiente | `Dext.Configuration.EnvironmentVariables` | Carrega do ambiente do SO |
| Memória | `Dext.Configuration.Core` | Dicionário em memória |

---

## 🐧 Suporte Cross-Platform

A fonte de variáveis de ambiente funciona em:
- **Windows**: Usa API `GetEnvironmentStrings`
- **Linux/macOS**: Usa variável global POSIX `environ`

Nenhuma mudança de código necessária - usa a implementação correta automaticamente.

---

## 📚 Documentação Relacionada

- [Guia de Configuração do Dext](../../Docs/configuration-guide.md)
- [Documentação do Dext Framework](../../README.md)
- [English Version](README.md)

---

## 📄 Licença

Este exemplo faz parte do Dext Framework e está licenciado sob a Apache License 2.0.
