# CLI: Scaffolding

O comando `scaffold` automatiza a criação de entidades mapeando tabelas de um banco de dados real.

## Uso Básico

```bash
dext scaffold -c "caminho_ou_string_conexao" -d driver
```

## Exemplo para SQLite

```bash
dext scaffold -c "C:\dados\vendas.db" -d sqlite -o src\Models\DbModels.pas
```

## Opções Detalhadas

| Opção | Atalho | Descrição |
|--------|-------|-------------|
| `--connection` | `-c` | String de conexão (FireDAC) ou path do banco. |
| `--driver` | `-d` | Driver: `sqlite`, `pg` (PostgreSQL), `mssql`, `firebird`. |
| `--output` | `-o` | Diretório ou arquivo onde a Unit será salva. |
| `--tables` | `-t` | Filtro de tabelas. Ex: `pedidos,itens_pedido`. |
| `--fluent` | | Gera mapeamento fluente (RegisterMappings) em vez de `[Atributos]`. |

## Por que usar Scaffolding?

- **Velocidade**: Evita a escrita manual de dezenas de propriedades e atributos.
- **Precisão**: Garante que os tipos Delphi (`Integer`, `string`, `TDateTime`) correspondam exatamente aos tipos do banco.
- **Consistência**: Mantém o padrão de nomenclatura e mapeamento em todo o projeto.

---

[← Migrations](migrations.md) | [Próximo: Testes →](testes.md)
