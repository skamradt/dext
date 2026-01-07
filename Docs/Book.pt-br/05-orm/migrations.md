# Migrations

Controle de versão para o schema do seu banco de dados.

## Visão Geral

Migrations permitem:
- Rastrear mudanças no schema ao longo do tempo
- Aplicar/reverter mudanças de forma reproduzível
- Compartilhar o schema entre membros da equipe

## Criando uma Migration

```pascal
unit Migration_001_CreateUsers;

interface

uses
  Dext.Entity.Migrations;

type
  [Migration(1, 'CreateUsers')]
  TMigration_001_CreateUsers = class(TMigration)
  public
    procedure Up; override;
    procedure Down; override;
  end;

implementation

procedure TMigration_001_CreateUsers.Up;
begin
  CreateTable('users', procedure(T: TTableBuilder)
    begin
      T.AddColumn('id').AsInteger.PrimaryKey.AutoIncrement;
      T.AddColumn('name').AsString(100).NotNull;
      T.AddColumn('email').AsString(255).NotNull.Unique;
      T.AddColumn('created_at').AsDateTime.Default('CURRENT_TIMESTAMP');
    end);
end;

procedure TMigration_001_CreateUsers.Down;
begin
  DropTable('users');
end;

end.
```

## Comandos CLI

```bash
# Aplicar migrations pendentes
dext migrate:up

# Reverter última migration
dext migrate:down

# Verificar status
dext migrate:list

# Gerar esqueleto
dext migrate:generate --name CriarTabelaPedidos
```

## API do Table Builder

### Colunas

```pascal
T.AddColumn('id').AsInteger.PrimaryKey.AutoIncrement;
T.AddColumn('name').AsString(100).NotNull;
T.AddColumn('email').AsString(255).Nullable;
T.AddColumn('preco').AsDecimal(10, 2).Default('0.00');
T.AddColumn('is_active').AsBoolean.Default('true');
T.AddColumn('created_at').AsDateTime;
```

---

[← Relacionamentos](relacionamentos.md) | [Próximo: Scaffolding →](scaffolding.md)
