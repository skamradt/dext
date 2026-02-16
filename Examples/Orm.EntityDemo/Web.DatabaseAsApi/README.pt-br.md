# Exemplo Database as API

Este exemplo demonstra a feature **Database as API** - expondo automaticamente entidades do banco de dados como endpoints REST sem código boilerplate.

## 🚀 Funcionalidades Demonstradas

| Funcionalidade | Descrição |
|----------------|-----------|
| **REST Zero-Code** | `TDataApiHandler<T>.Map` cria endpoints CRUD automaticamente |
| **JSON Snake Case** | `UseSnakeCase` transforma `CreatedAt` → `created_at` |
| **OpenAPI/Swagger** | `UseSwagger` gera documentação da API automaticamente |
| **Exclusão de Campos** | `[NotMapped]` oculta campos da API e do banco |
| **PK/AutoInc** | `[PK, AutoInc]` para chave primária automática |

## 🛠️ Como Iniciar

1. **Compile** `Web.DatabaseAsApi.dproj`
2. **Execute** `Web.DatabaseAsApi.exe`
   - Banco SQLite em memória é criado automaticamente
   - Servidor inicia em **http://localhost:5000**
3. **Teste**:
   ```powershell
   .\Test.Web.DatabaseAsApi.ps1
   ```

## 📍 Endpoints Gerados Automaticamente

| Método | Path | Descrição |
|--------|------|-----------|
| `GET` | `/api/customers` | Listar todos os clientes |
| `GET` | `/api/customers/{id}` | Buscar cliente por ID |
| `POST` | `/api/customers` | Criar novo cliente |
| `PUT` | `/api/customers/{id}` | Atualizar cliente |
| `DELETE` | `/api/customers/{id}` | Deletar cliente |
| `GET` | `/swagger` | Swagger UI |
| `GET` | `/swagger.json` | OpenAPI spec |

## 💡 Destaques do Código

### Definição Mínima de Entidade
```delphi
[Table('Customers')]
TCustomer = class
private
  FId: Integer;
  FName: string;
  FEmail: string;
  FActive: Boolean;
  FInternalCode: string;
  FCreatedAt: TDateTime;
public
  [PK, AutoInc]
  property Id: Integer read FId write FId;
  property Name: string read FName write FName;
  property Email: string read FEmail write FEmail;
  property Active: Boolean read FActive write FActive;
  
  [NotMapped]  // Excluído da API e do banco
  property InternalCode: string read FInternalCode write FInternalCode;
  
  property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
end;
```

### Setup da API em Uma Linha
```delphi
TDataApiHandler<TCustomer>.Map(ABuilder, '/api/customers', FDbContext,
  TDataApiOptions<TCustomer>.Create
    .UseSnakeCase       // JSON: created_at ao invés de CreatedAt
    .UseSwagger         // Habilita documentação OpenAPI
    .Tag('Customers')   // Tag personalizada no Swagger
);
```

### Saída JSON (Snake Case)
```json
{
  "id": 1,
  "name": "John Doe",
  "email": "john@example.com",
  "active": true,
  "created_at": "2025-01-02T10:30:00"
}
```

Nota: `InternalCode` não aparece na saída devido ao `[NotMapped]`.

## 🔧 Opções de Configuração

| Opção | Descrição |
|-------|-----------|
| `UseSnakeCase` | Converte nomes de propriedades para snake_case no JSON |
| `UseSwagger` | Habilita geração de documentação OpenAPI |
| `Tag(name)` | Define nome de tag personalizado no Swagger UI |

## 🔗 Veja Também

- [Guia Database as API](../../docs/database-as-api.md)
- [Atributos de Mapeamento ORM](../../docs/orm-mapping-attributes.md)
- [Integração Swagger](../../docs/swagger.md)
