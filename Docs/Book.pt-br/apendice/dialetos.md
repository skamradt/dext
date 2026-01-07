# Dialetos de Banco de Dados

O Dext abstrai as diferenças entre motores de banco de dados usando **Dialetos**.

## Dialetos Suportados

### PostgreSQL
Otimizado para performance com suporte a JSONB, Arrays e UUIDs nativos.
```pascal
Dialect := TPostgreSQLDialect.Create;
```

### SQL Server
Suporte completo ao T-SQL, incluindo TOP, offsets e tipos de data modernos.
```pascal
Dialect := TMSSQLDialect.Create;
```

### SQLite
Ideal para desenvolvimento local, mobile e testes unitários.
```pascal
Dialect := TSQLiteDialect.Create;
```

### Firebird
Suporte a versões 2.5 até 5.0, tratando corretamente diferenças de paginação (ROWS vs OFFSET).
```pascal
Dialect := TFirebirdDialect.Create;
```

## Recursos por Dialeto

| Recurso | PG | SQL Server | SQLite | Firebird |
|---------|----|------------|--------|----------|
| Paginação | ✅ | ✅ | ✅ | ✅ |
| UUID Nativo | ✅ | ✅ | ❌ | ❌ |
| JSON Support | ✅ | ✅ | ❌ | ❌ |
| Bulk Insert | ✅ | ✅ | ✅ | 🟡 |
| Multi-Tenancy (Schema) | ✅ | ✅ | ❌ | ❌ |

## Criando seu próprio Dialeto

Se precisar de suporte a um banco de dados não listado, você pode implementar a classe `TSQLDialect`:

```pascal
type
  TMyCustomDialect = class(TSQLDialect)
  public
    function GetLimitTemplate: string; override;
    function MapType(Field: TField): string; override;
  end;
```

---

[← Sistema de Tipos](sistema-tipos.md) | [Próximo: Solução de Problemas →](solucao-problemas.md)
