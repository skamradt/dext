# Database as API

> **Zero-Code REST API** - Expõe entidades do banco de dados diretamente como endpoints REST com configuração mínima.

## Visão Geral

O módulo **Database as API** permite criar endpoints CRUD completos para suas entidades com uma única linha de código. Ele suporta:

- ✅ Todos os métodos HTTP (GET, POST, PUT, DELETE)
- ✅ Parse automático de JSON para entidades
- ✅ Filtros dinâmicos via query string
- ✅ Paginação integrada
- ✅ Segurança baseada em roles/claims

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
  published
    [PrimaryKey, AutoIncrement]
    property Id: Integer read FId write FId;
    
    [Column('name'), Required, MaxLength(100)]
    property Name: string read FName write FName;
    
    [Column('email'), MaxLength(255)]
    property Email: string read FEmail write FEmail;
    
    [Column('active')]
    property Active: Boolean read FActive write FActive;
  end;

implementation

end.
```

### Passo 2: Crie a Classe Startup

```pascal
unit App.Startup;

interface

uses
  Dext.Web.Core,
  Dext.Web.DataApi,
  Dext.Entity.Context,
  Entities.Customer;

type
  TAppDbContext = class(TDbContext)
  public
    function Customers: IDbSet<TCustomer>;
  end;

  TStartup = class
  private
    FDbContext: TAppDbContext;
    FApp: IApplicationBuilder;
  public
    procedure Initialize;
    procedure ConfigureRoutes;
    procedure Run;
    procedure Finalize;
  end;

implementation

{ TAppDbContext }

function TAppDbContext.Customers: IDbSet<TCustomer>;
begin
  Result := Entities<TCustomer>;
end;

{ TStartup }

procedure TStartup.Initialize;
begin
  FDbContext := TAppDbContext.Create;
  FDbContext.EnsureCreated;
  
  FApp := TDextApp.CreateBuilder.Build;
end;

procedure TStartup.ConfigureRoutes;
begin
  // Mapeia a entidade para o endpoint
  TDataApiHandler<TCustomer>.Map(FApp, '/api/customers', FDbContext);
end;

procedure TStartup.Run;
begin
  FApp.Run('http://localhost:5000');
end;

procedure TStartup.Finalize;
begin
  FDbContext.Free;
end;

end.
```

### Passo 3: Crie o Programa Principal

```pascal
program MyApi;

uses
  App.Startup;

var
  Startup: TStartup;
begin
  Startup := TStartup.Create;
  try
    Startup.Initialize;
    Startup.ConfigureRoutes;
    Startup.Run;
  finally
    Startup.Finalize;
    Startup.Free;
  end;
end.
```

### Passo 4: Teste sua API

```bash
# Listar todos
curl http://localhost:5000/api/customers

# Buscar por ID
curl http://localhost:5000/api/customers/1

# Criar novo
curl -X POST http://localhost:5000/api/customers \
  -H "Content-Type: application/json" \
  -d '{"name":"John Doe","email":"john@example.com","active":true}'

# Atualizar
curl -X PUT http://localhost:5000/api/customers/1 \
  -H "Content-Type: application/json" \
  -d '{"name":"John Updated","email":"john.new@example.com","active":false}'

# Deletar
curl -X DELETE http://localhost:5000/api/customers/1
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

### CRUD Completo sem DELETE

```pascal
TDataApiHandler<TCustomer>.Map(App, '/api/customers', DbContext,
  TDataApiOptions<TCustomer>.Create
    .Allow([amGet, amGetList, amPost, amPut])  // Sem DELETE
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

## Paginação

### Parâmetros Reservados

| Parâmetro | Descrição | Exemplo |
|-----------|-----------|---------|
| `_limit` | Quantidade de registros | `?_limit=10` |
| `_offset` | Pular registros | `?_offset=20` |

### Exemplos de Paginação

```bash
# Primeira página (10 itens)
GET /api/customers?_limit=10

# Segunda página
GET /api/customers?_limit=10&_offset=10

# Terceira página
GET /api/customers?_limit=10&_offset=20

# Combinar com filtros
GET /api/customers?active=true&_limit=5&_offset=0
```

---

## Segurança

### Exigir Autenticação

```pascal
TDataApiHandler<TCustomer>.Map(App, '/api/customers', DbContext,
  TDataApiOptions<TCustomer>.Create
    .RequireAuth  // Qualquer usuário autenticado
);
```

### Exigir Role Específica (Todas Operações)

```pascal
TDataApiHandler<TCustomer>.Map(App, '/api/customers', DbContext,
  TDataApiOptions<TCustomer>.Create
    .RequireRole('Admin')  // Apenas Admins
);
```

### Roles Diferentes para Leitura e Escrita

```pascal
TDataApiHandler<TCustomer>.Map(App, '/api/customers', DbContext,
  TDataApiOptions<TCustomer>.Create
    .RequireReadRole('Viewer,User,Admin')    // GET pode ser Viewer, User ou Admin
    .RequireWriteRole('Editor,Admin')        // POST/PUT/DELETE só Editor ou Admin
);
```

### API Pública com Escrita Protegida

```pascal
TDataApiHandler<TProduct>.Map(App, '/api/products', DbContext,
  TDataApiOptions<TProduct>.Create
    .RequireWriteRole('Admin')  // Leitura pública, escrita protegida
);
```

### Códigos de Resposta HTTP

| Código | Significado | Quando |
|--------|-------------|--------|
| `401` | Unauthorized | Autenticação necessária |
| `403` | Forbidden | Role não autorizada |

---

## JSON Body Parsing

### Criação (POST)

**Request:**
```http
POST /api/customers
Content-Type: application/json

{
  "name": "John Doe",
  "email": "john@example.com",
  "active": true
}
```

**Response (201 Created):**
```json
{
  "id": 3,
  "name": "John Doe",
  "email": "john@example.com",
  "active": true
}
```

### Atualização (PUT)

**Request:**
```http
PUT /api/customers/3
Content-Type: application/json

{
  "name": "John Updated",
  "email": "john.new@example.com",
  "active": false
}
```

**Response (200 OK):**
```json
{
  "id": 3,
  "name": "John Updated",
  "email": "john.new@example.com",
  "active": false
}
```

### Convenções de Naming

O parser suporta **camelCase to PascalCase** automaticamente:

| JSON (camelCase) | Property (PascalCase) |
|------------------|----------------------|
| `name` | `Name` |
| `email` | `Email` |
| `isActive` | `IsActive` |
| `createdAt` | `CreatedAt` |

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
end;
```

### TDataApiHandler\<T\>

```pascal
TDataApiHandler<T: class> = class
  // Com DbContext explícito
  class procedure Map(
    const ABuilder: IApplicationBuilder; 
    const APath: string; 
    ADbContext: TDbContext); overload;
  
  // Com opções de configuração
  class procedure Map(
    const ABuilder: IApplicationBuilder; 
    const APath: string; 
    ADbContext: TDbContext;
    AOptions: TDataApiOptions<T>); overload;
  
  // Via DI (DbContext registrado no container)  
  class procedure Map(
    const ABuilder: IApplicationBuilder; 
    const APath: string); overload;
end;
```

---

## Exemplos Completos

### API de Produtos (E-commerce)

```pascal
// Produtos: leitura pública, escrita admin
TDataApiHandler<TProduct>.Map(App, '/api/products', DbContext,
  TDataApiOptions<TProduct>.Create
    .RequireWriteRole('Admin,ProductManager')
);

// Categorias: somente leitura para todos
TDataApiHandler<TCategory>.Map(App, '/api/categories', DbContext,
  TDataApiOptions<TCategory>.Create
    .Allow([amGet, amGetList])
);

// Pedidos: usuário autenticado pode ler seus pedidos
TDataApiHandler<TOrder>.Map(App, '/api/orders', DbContext,
  TDataApiOptions<TOrder>.Create
    .RequireAuth
);
```

### API de Logs (Append-Only)

```pascal
// Logs: somente inserção, sem leitura/update/delete
TDataApiHandler<TAuditLog>.Map(App, '/api/audit', DbContext,
  TDataApiOptions<TAuditLog>.Create
    .Allow([amPost])
    .RequireAuth
);
```

### API Administrativa

```pascal
// Usuários: apenas admins podem gerenciar
TDataApiHandler<TUser>.Map(App, '/api/admin/users', DbContext,
  TDataApiOptions<TUser>.Create
    .RequireRole('SuperAdmin')
);
```

---

## Tratamento de Erros

### Respostas de Erro Padrão

```json
{"error": "Request body is empty"}
{"error": "Invalid JSON in request body"}  
{"error": "Missing id parameter"}
{"error": "Invalid id format"}
{"error": "Entity with id 123 not found"}
{"error": "Authentication required"}
{"error": "Forbidden - requires one of roles: Admin,Editor"}
```

### Códigos HTTP

| Código | Situação |
|--------|----------|
| `200` | Sucesso (GET, PUT) |
| `201` | Criado (POST) |
| `400` | Bad Request (JSON inválido, ID faltando) |
| `401` | Não autenticado |
| `403` | Não autorizado (role) |
| `404` | Não encontrado |
| `500` | Erro interno |

---

## Arquivos Relacionados

- [Dext.Web.DataApi.pas](../Sources/Web/Dext.Web.DataApi.pas) - Implementação principal
- [Dext.Auth.Identity.pas](../Sources/Web/Dext.Auth.Identity.pas) - Sistema de Claims/Roles
- [DatabaseAsApi](../Examples/DatabaseAsApi/) - Projeto de exemplo
