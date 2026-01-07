# Comandos CLI

Visão geral de todos os comandos disponíveis no CLI `dext`.

## Comandos Gerais

### Help
Mostra ajuda para o CLI ou um comando específico.
```bash
dext help
dext help migrate:up
```

### Version
Mostra a versão atual do framework e do CLI.
```bash
dext --version
```

## Migrations (`migrate:`)

| Comando | Descrição |
|---------|-----------|
| `migrate:up` | Aplica todas as migrations pendentes. |
| `migrate:down` | Reverte a última migration aplicada. |
| `migrate:list` | Lista todas as migrations e seu status (aplicada/pendente). |
| `migrate:generate` | Cria um arquivo unit de migration vazio com timestamp. |

## Testes (`test`)

Executa a suíte de testes do projeto.

| Opção | Descrição |
|-------|-----------|
| `--coverage` | Gera relatório de cobertura de código. |
| `--html` | Gera relatório visual em HTML. |
| `--xml` | Saída em formato JUnit XML (para CI/CD). |
| `--filter` | Filtra testes por nome ou categoria. |

```bash
dext test --coverage --html
```

## Scaffolding (`scaffold`)

Gera classes de entidade a partir de um banco de dados existente.

```bash
dext scaffold -c "Server=localhost;Database=Vendas" -d mssql
```

## Dashboard (`ui`)

Inicia o Dashboard administrativo web do Dext.

```bash
dext ui --port 3000
```

---

[← CLI](README.md) | [Próximo: Migrations →](migrations.md)
