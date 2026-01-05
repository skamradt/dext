# 🛠️ Documentação da Dext CLI Tool

A `Dext.Hosting.CLI` (também referida como `dext.exe` ou `DextTool.exe`) é a interface de linha de comando para o Dext Framework. Ela fornece utilitários essenciais para gerenciamento de projetos, testes e migrações de banco de dados.

> 📝 **Nota**: A ferramenta CLI geralmente é embutida em sua aplicação se você usa o `Dext.Hosting`, mas também pode ser compilada como uma ferramenta independente.

## 🚀 Sintaxe de Uso

```bash
dext <comando> [argumentos] [opções]
```

Para ver os comandos disponíveis:

```bash
dext help
```

---

## 🖥️ Dashboard UI
 
### `ui`
Inicia o painel de controle web para gerenciamento visual do Dext, configurações e ambientes.
 
**Sintaxe:**
```bash
dext ui [--port <numero>]
```
 
**Funcionalidades:**
- **Projects**: Visualiza projetos recentes e seus status.
- **Tests**: Visualiza resultados das últimas execuções de teste, métricas de cobertura de código e acesso ao relatório HTML completo.
- **Settings**: Configura paths globais (Dext CLI, Code Coverage) e gerencia ambientes Delphi.
- **Tools**: Instalação automática de ferramentas como CodeCoverage (via Settings).
 
---
 
## 🧪 Comandos de Teste

### `test`
Executa a suíte de testes do projeto. Detecta automaticamente seu arquivo `.dproj` (deve conter "Test" no nome), compila-o e executa o binário resultante.

**Sintaxe:**
```bash
dext test [opções]
```

**Opções:**
- `--project=<caminho>`: Especifica o arquivo de projeto Delphi (`.dproj`) para compilar e testar. Se omitido, procura por um `*Test*.dproj` no diretório atual.
- `--coverage`: Habilita análise de cobertura de código.
  - Compila o projeto com informações de debug (arquivo `-map`).
  - Executa testes usando `CodeCoverage.exe`.
  - Gera relatórios HTML e XML em `TestOutput/report`.
  - **Quality Gate**: Verifica `coverage.threshold` do `dext.json` e falha se não atingido.

**Configuração (`dext.json`):**
Valores no `dext.json` servem como padrões se as flags da CLI não forem fornecidas.

```json
{
  "test": {
    "project": "Tests/MyProjectTests.dproj",
    "reportDir": "build/reports",
    "coverageThreshold": 80.0,
    "coverageExclude": [
      "*Dext.*",
      "*ThirdParty*"
    ]
  }
}
```

---

## 🗄️ Comandos de Migração

A CLI integra-se com `Dext.Entity` para gerenciar migrações de esquema de banco de dados abrangentes.

### `migrate:up`
Aplica todas as migrações pendentes ao banco de dados.

**Sintaxe:**
```bash
dext migrate:up [--source <caminho>]
```

**Opções:**
- `--source <caminho>` (alias `-s`): Diretório contendo arquivos JSON de migração. Se omitido, usa o registro interno.

### `migrate:down`
Reverte migrações. Por padrão, reverte a última migração aplicada.

**Sintaxe:**
```bash
dext migrate:down [--target <id>]
```

**Opções:**
- `--target <id>` (alias `-t`): Reverte migrações sequencialmente até que o ID de migração especificado seja alcançado (inclusive). Se omitido, reverte apenas a última.

### `migrate:list`
Lista o status de todas as migrações conhecidas (Aplicadas vs. Pendentes).

**Sintaxe:**
```bash
dext migrate:list
```

**Exemplo de Saída:**
```text
Migration Status:
-----------------
[Applied]   202501010000_SchemaInicial
[Pending]   202501021230_AdicionarUsuarios
```

### `migrate:generate`
Cria um novo arquivo de migração JSON vazio com um ID timestamped.

**Sintaxe:**
```bash
dext migrate:generate <nome> [--path <dir>]
```

**Argumentos:**
- `<nome>`: Um nome descritivo para a migração (ex: `AdicionarTabelaCliente`).

**Opções:**
- `--path <dir>` (alias `-p`): Diretório para salvar o arquivo. Padrão é o diretório atual.

**Saída:**
Gera um arquivo como `20260104223000_AdicionarTabelaCliente.json`.

---

---
 
## 🌍 Comandos de Ambiente
 
Gerencia as instalações do Delphi detectadas e configura qual versão utilizar para compilação.
 
### `env scan`
Escaneia o registro do Windows em busca de instalações do Delphi disponíveis e atualiza o `config.yaml` global.
 
**Sintaxe:**
```bash
dext env scan
```
 
### `env list`
Lista todas as instalações do Delphi configuradas e indica qual é a padrão (Default).
 
**Sintaxe:**
```bash
dext env list
```
 
---
 
## ⚙️ Opções Globais

- `--help` / `-h` / `help`: Exibe a tela de ajuda com a lista de comandos disponíveis.

---

## 📦 Instalação

Se compilando a partir do código-fonte:

1. Abra `Sources/DextFramework.groupproj`.
2. Compile o projeto `DextTool` (encontrado em `Apps/CLI`).
3. Adicione o diretório de saída ao `PATH` do sistema.
