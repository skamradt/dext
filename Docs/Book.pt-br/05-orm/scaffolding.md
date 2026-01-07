# Scaffolding CLI

Gere classes de entidade a partir de um esquema de banco de dados existente.

## Início Rápido

```bash
dext scaffold -c "meubanco.db" -d sqlite -o Entidades.pas
```

## Opções

| Opção | Atalho | Descrição |
|--------|-------|-------------|
| `--connection` | `-c` | String de conexão ou caminho do arquivo |
| `--driver` | `-d` | Driver: `sqlite`, `pg`, `mssql`, `firebird` |
| `--output` | `-o` | Arquivo de saída (padrão: `Entities.pas`) |
| `--unit` | `-u` | Nome da unit (padrão: baseado no arquivo) |
| `--fluent` | | Gerar mapeamento fluente em vez de atributos |
| `--tables` | `-t` | Tabelas específicas (separadas por vírgula) |

## Exemplos

### SQLite

```bash
dext scaffold -c "meuapp.db" -d sqlite -o Models/Entidades.pas
```

### PostgreSQL

```bash
dext scaffold \
  -c "Server=localhost;Port=5432;Database=meuapp;User_Name=postgres;Password=segredo" \
  -d pg \
  -o Entidades.pas
```

## Código Gerado

### Mapeamento por Atributos (Padrão)

```pascal
unit Entidades;

interface

uses
  Dext.Entity.Attributes;

type
  [Table('users')]
  TUser = class
  private
    FId: Integer;
    FName: string;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    [Column('name')]
    property Name: string read FName write FName;
  end;

implementation

end.
```

---

[← Migrations](migrations.md) | [Próximo: Multi-Tenancy →](multi-tenancy.md)
