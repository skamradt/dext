# Referência do Sistema de Tipos

O Dext utiliza um sistema de tipos moderno para garantir compatibilidade entre banco de dados e APIs JSON.

## Mapeamento de Tipos

Quando você define uma propriedade na sua Entidade ou DTO, o Dext faz o mapeamento automático:

| Tipo Pascal | JSON | Banco de Dados |
|-------------|------|----------------|
| `Integer` / `Int64` | `Number` | `INTEGER` / `BIGINT` |
| `string` | `String` | `VARCHAR` / `TEXT` |
| `Double` / `Currency` | `Number` | `FLOAT` / `DECIMAL` |
| `Boolean` | `Boolean` | `BIT` / `BOOLEAN` |
| `TDateTime` | `String (ISO)` | `DATETIME` / `TIMESTAMP` |
| `TDate` / `TTime` | `String (ISO)` | `DATE` / `TIME` |
| `TUUID` (GUID) | `String (UUID)` | `UUID` / `CHAR(36)` |
| `TBytes` | `String (Base64)` | `BLOB` / `BYTEA` |

## Tipos Nativos Dext

### TUUID (Dext.Core.Types)
Suporte nativo para UUIDs modernos (RFC 9562), incluindo **UUID v7** (totalmente ordenável por tempo).

### Nullable<T> (Dext.Core.Types)
Permite que tipos primitivos como `Integer` ou `Boolean` aceitem valores nulos do banco de dados ou requisições JSON.

```pascal
property Age: Nullable<Integer>;
```

### Smart Properties (Prop<T>)
Usado para definir propriedades de Entidade que permitem construção de queries fluentes e verificação em tempo de compilação.

```pascal
class var Props: record
  Name: Prop<string>;
end;
```

---

[← Tópicos Avançados](../10-avancado/README.md) | [Próximo: Dialetos →](dialetos.md)
