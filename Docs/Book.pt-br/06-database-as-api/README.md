# 6. Database as API

Gere APIs REST automaticamente a partir das suas entidades - sem código necessário.

> 📦 **Exemplo**: [Web.DatabaseAsApi](../../../Examples/Web.DatabaseAsApi/)

## Início Rápido

```pascal
type
  [Table('products')]
  TProduct = class
  private
    FId: Integer;
    FName: string;
    FPrice: Double;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Price: Double read FPrice write FPrice;
  end;

// Uma linha para expor CRUD completo!
App.Configure(procedure(App: IApplicationBuilder)
  begin
    TDataApiHandler<TProduct>.Map(App, '/api/products');
  end);
```

## Endpoints Gerados

| Método | URL | Descrição |
|--------|-----|-----------|
| GET | `/api/products` | Listar todos (com paginação) |
| GET | `/api/products/:id` | Buscar por ID |
| POST | `/api/products` | Criar novo |
| PUT | `/api/products/:id` | Atualizar |
| DELETE | `/api/products/:id` | Excluir |

## Recursos

- **Paginação Automática**: `?page=1&pageSize=20`
- **Filtragem**: `?name=Widget&price_gt=100`
- **Ordenação**: `?orderBy=price&desc=true`
- **Políticas de Segurança**: Restringir acesso por operação

---

[← ORM](../05-orm/README.md) | [Próximo: Tempo Real →](../07-tempo-real/README.md)
