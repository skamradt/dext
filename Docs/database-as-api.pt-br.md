# Database as API

> **Zero-Code REST API** - Expõe entidades do banco de dados diretamente como endpoints REST com configuração mínima.

## Visão Geral

O módulo **Database as API** permite criar endpoints CRUD completos para suas entidades com uma única linha de código. Ele suporta:

- ✅ Todos os métodos HTTP (GET, POST, PUT, DELETE)
- ✅ Parse automático de JSON para entidades
- ✅ Filtros dinâmicos via query string
- ✅ Paginação integrada
- ✅ Segurança baseada em roles/claims
- ✅ **Suporte a Mapeamento/Ignore** (Atributos & Fluent)
- ✅ **Estratégias de Nomenclatura** (CamelCase, SnakeCase)

---

## Início Rápido

### 1. Configuração Mínima

```pascal
uses
  Dext.Web.DataApi;

// Expõe TCustomer com todos os endpoints CRUD
TDataApiHandler<TCustomer>.Map(App, '/api/customers', DbContext);
```

Este código gera automaticamente:

| Método | Endpoint | Descrição |
|--------|----------|-----------|
| `GET` | `/api/customers` | Lista todos (com filtros) |
| `GET` | `/api/customers/{id}` | Busca por ID |
| `POST` | `/api/customers` | Cria novo registro |
| `PUT` | `/api/customers/{id}` | Atualiza registro |
| `DELETE` | `/api/customers/{id}` | Remove registro |

---

## Guia Passo a Passo: Criando uma API do Zero

### Passo 1: Defina sua Entidade

```pascal
unit Entities.Customer;

interface

uses
  Dext.Entity;

type
  [Table('customers')]
  TCustomer = class(TEntity)
  private
    FId: Integer;
    FName: string;
    FEmail: string;
    FActive: Boolean;
    FCreatedAt: TDateTime;
  published
    [PrimaryKey, AutoIncrement]
    property Id: Integer read FId write FId;
    
    [Column('name'), Required, MaxLength(100)]
    property Name: string read FName write FName;
    
    [Column('email'), MaxLength(255)]
    property Email: string read FEmail write FEmail;
    
    [Column('active')]
    property Active: Boolean read FActive write FActive;
    
    [Column('created_at')]
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  end;

implementation

end.
```

### Passo 2: Configuração e Nomenclatura (Naming Strategies)

Por padrão, a API utiliza **camelCase** (ex: `createdAt`). Você pode configurar para **snake_case** (ex: `created_at`) se preferir.

```pascal
procedure TStartup.ConfigureRoutes;
begin
  // Configuração Padrão (CamelCase)
  // JSON: { "id": 1, "name": "...", "createdAt": "..." }
  TDataApiHandler<TCustomer>.Map(FApp, '/api/customers', FDbContext);

  // Configuração SnakeCase
  // JSON: { "id": 1, "name": "...", "created_at": "..." }
  TDataApiHandler<TCustomer>.Map(FApp, '/api/customers_snake', FDbContext,
    TDataApiOptions<TCustomer>.Create
      .UseSnakeCase
  );
end;
```

---

## Configurações de Endpoint

### Somente Leitura (GET)

```pascal
TDataApiHandler<TCustomer>.Map(App, '/api/customers', DbContext,
  TDataApiOptions<TCustomer>.Create
    .Allow([amGet, amGetList])  // Apenas GET e GET List
);
```

### Somente Escrita (POST)

```pascal
TDataApiHandler<TLog>.Map(App, '/api/logs', DbContext,
  TDataApiOptions<TLog>.Create
    .Allow([amPost])  // Apenas POST
);
```

### Métodos Disponíveis

| Constante | Método HTTP | Descrição |
|-----------|-------------|-----------|
| `amGet` | GET /{id} | Busca por ID |
| `amGetList` | GET | Lista todos |
| `amPost` | POST | Cria novo |
| `amPut` | PUT /{id} | Atualiza |
| `amDelete` | DELETE /{id} | Remove |
| `AllApiMethods` | Todos | Valor padrão |

---

## Filtros via Query String

### Sintaxe Básica

```
GET /api/customers?{propriedade}={valor}
```

### Exemplos de Filtros

```bash
# Filtrar por booleano
GET /api/customers?active=true

# Filtrar por string
GET /api/customers?name=John

# Filtrar por número
GET /api/customers?id=5

# Múltiplos filtros (AND)
GET /api/customers?active=true&name=John
```

### Tipos Suportados

| Tipo Pascal | Exemplo Query | Conversão |
|-------------|---------------|-----------|
| `Integer` | `?id=123` | `StrToInt` |
| `Int64` | `?code=999999999` | `StrToInt64` |
| `String` | `?name=John` | Direto |
| `Boolean` | `?active=true` | `true/false` ou `1/0` |

> **Nota**: Propriedades não encontradas na entidade são ignoradas silenciosamente.

---

## Suporte a Mapeamento (Mapping)

O DataApi respeita as configurações de mapeamento do Dext ORM via `TEntityMap`. Propriedades ignoradas via Attributes (`[NotMapped]`) ou Fluent API (`Ignore`) serão automaticamente excluídas de todas as operações API (Read & Write).

### Exemplo (Attribute)
```pascal
type
  TCustomer = class
  published
    property Name: string ...;
    
    [NotMapped] // Ignorado na API
    property InternalCode: string ...;
  end;
```

### Exemplo (Fluent API)
```pascal
ModelBuilder.Entity<TCustomer>
  .Ignore('InternalCode');
```

Além disso, properties somente leitura (sem setter) são automaticamente ignoradas nas operações de escrita (POST/PUT).

---

## Estratégias de Nomenclatura JSON

O parser evita expor os nomes das colunas do banco de dados, utilizando os nomes das Propriedades da classe, com a conversão configurada:

| Propriedade Pascal | CamelCase (Padrão) | SnakeCase (`UseSnakeCase`) |
|-------------------|--------------------|----------------------------|
| `Name` | `name` | `name` |
| `EmailAddress` | `emailAddress` | `email_address` |
| `IsActive` | `isActive` | `is_active` |
| `CreatedAt` | `createdAt` | `created_at` |

---

## Referência da API

### TDataApiOptions\<T\>

```pascal
TDataApiOptions<T> = class
  // Métodos permitidos
  function Allow(AMethods: TApiMethods): TDataApiOptions<T>;
  
  // Multi-tenancy
  function RequireTenant: TDataApiOptions<T>;
  
  // Autenticação
  function RequireAuth: TDataApiOptions<T>;
  
  // Roles
  function RequireRole(const ARoles: string): TDataApiOptions<T>;       // Todas ops
  function RequireReadRole(const ARoles: string): TDataApiOptions<T>;   // GET
  function RequireWriteRole(const ARoles: string): TDataApiOptions<T>;  // POST/PUT/DELETE
  
  // Naming
  function UseCamelCase: TDataApiOptions<T>;  // Padrão
  function UseSnakeCase: TDataApiOptions<T>;
end;
```
