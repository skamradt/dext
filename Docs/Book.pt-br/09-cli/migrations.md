# CLI: Migrations

Gerencie o ciclo de vida do seu banco de dados via linha de comando.

## Comandos Disponíveis

### migrate:up
Aplica todas as migrations que ainda não foram executadas no banco de dados.
```bash
dext migrate:up
```

### migrate:down
Reverte a última migration que foi aplicada. Útil para desfazer mudanças rápidas em desenvolvimento.
```bash
dext migrate:down
```

### migrate:list
Mostra uma tabela com todas as migrations registradas na aplicação e se elas já foram aplicadas ou estão pendentes.
```bash
dext migrate:list
```

### migrate:generate
Cria um novo arquivo unit para sua migration com o padrão de nomenclatura correto e timestamp.
```bash
dext migrate:generate --name AdicionarTabelaProdutos
```

## Multi-Tenancy em Migrations

Se sua aplicação for Multi-Tenant, os comandos de migration podem percorrer todos os tenants automaticamente:

```bash
# Aplica migrations em todos os schemas de todos os clientes
dext migrate:up --all-tenants
```

## Dicas

- **Commit sempre**: Sempre comite suas migrations junto com o código que depende delas.
- **Não altere migrations antigas**: Se precisar mudar algo que já foi para produção, crie uma nova migration.

---

[← Comandos](comandos.md) | [Próximo: Scaffolding →](scaffolding.md)
