# Model Binding

Converta automaticamente requisições HTTP em objetos Delphi a partir de múltiplas fontes: corpo JSON, headers, parâmetros de query e parâmetros de rota.

> 📦 **Exemplos**: 
> - [Web.MinimalAPI](../../../Examples/Web.MinimalAPI/)
> - [Multi-Tenancy](../../../Examples/Dext.Examples.MultiTenancy/)

## Visão Geral

O Model Binding no Dext suporta binding de dados de:

| Fonte | Atributo | Exemplo |
|-------|----------|---------|
| Corpo JSON | `[FromBody]` (padrão para POST/PUT) | Payload da requisição |
| Headers | `[FromHeader('X-Header')]` | API keys, tenant IDs |
| Query String | `[FromQuery('param')]` | `?search=...&page=1` |
| Parâmetros de Rota | `[FromRoute('id')]` | `/users/{id}` |
| Serviços (DI) | `[FromServices]` | Serviços injetados |

## Binding Básico de Corpo JSON

### Records (Recomendado)

```pascal
type
  TCreateUserRequest = record
    Name: string;
    Email: string;
    Age: Integer;
  end;

// Minimal API - binding automático
App.MapPost<TCreateUserRequest, IResult>('/users',
  function(Request: TCreateUserRequest): IResult
  begin
    // Request é automaticamente populado do corpo JSON
    Result := Results.Created('/users/1', Request);
  end);
```

### Estilo Controller

```pascal
App.MapPost('/users', procedure(Ctx: IHttpContext)
  var
    Request: TCreateUserRequest;
  begin
    Request := Ctx.Request.BindBody<TCreateUserRequest>;
    // Use Request.Name, Request.Email, Request.Age
  end);
```

## Binding de Headers

Use `[FromHeader]` para API keys, tenant IDs ou qualquer header customizado:

```pascal
type
  TTenantRequest = record
    [FromHeader('X-Tenant-Id')]
    TenantId: string;
    [FromHeader('Authorization')]
    Token: string;
  end;

App.MapGet<TTenantRequest, IResult>('/api/data',
  function(Request: TTenantRequest): IResult
  begin
    if Request.TenantId = '' then
      Exit(Results.BadRequest('Header X-Tenant-Id é obrigatório'));
      
    Result := Results.Ok(Format('Tenant: %s', [Request.TenantId]));
  end);
```

**Requisição:**
```bash
curl -H "X-Tenant-Id: acme-corp" \
     -H "Authorization: Bearer token123" \
     http://localhost:8080/api/data
```

## Binding de Parâmetros de Query

Use `[FromQuery]` para parâmetros de query string na URL:

```pascal
type
  TSearchRequest = record
    [FromQuery('q')]
    Query: string;
    [FromQuery('page')]
    Page: Integer;
    [FromQuery('limit')]
    Limit: Integer;
  end;

App.MapGet<TSearchRequest, IResult>('/search',
  function(Request: TSearchRequest): IResult
  begin
    Result := Results.Ok(Format('Busca: %s, Página: %d', 
      [Request.Query, Request.Page]));
  end);
```

**Requisição:**
```bash
curl "http://localhost:8080/search?q=delphi&page=1&limit=20"
```

## Binding de Parâmetros de Rota

Use `[FromRoute]` para parâmetros no caminho da URL:

```pascal
type
  TRouteRequest = record
    [FromRoute('id')]
    Id: Integer;
    [FromRoute('category')]
    Category: string;
  end;

App.MapGet<TRouteRequest, IResult>('/products/{id}/category/{category}',
  function(Request: TRouteRequest): IResult
  begin
    Result := Results.Ok(Format('Produto %d na categoria %s', 
      [Request.Id, Request.Category]));
  end);
```

**Requisição:**
```bash
curl http://localhost:8080/products/42/category/eletronicos
```

## Binding Misto (Múltiplas Fontes)

O recurso mais poderoso: combine dados de diferentes fontes em um único record.

### Header + Body (Padrão Multi-Tenancy)

```pascal
type
  TProductCreateRequest = record
    // Do header HTTP
    [FromHeader('X-Tenant-Id')]
    TenantId: string;
    
    // Do corpo JSON (sem atributo = padrão body)
    Name: string;
    Description: string;
    Price: Currency;
    Stock: Integer;
  end;

App.MapPost<IProductService, TProductCreateRequest, IResult>('/api/products',
  function(Service: IProductService; Request: TProductCreateRequest): IResult
  begin
    // TenantId vem do header
    // Name, Description, Price, Stock vêm do corpo JSON
    
    if Request.TenantId = '' then
      Exit(Results.BadRequest('Header X-Tenant-Id é obrigatório'));
      
    var Product := Service.Create(Request);
    Result := Results.Created('/api/products/' + IntToStr(Product.Id), Product);
  end);
```

**Requisição:**
```bash
curl -X POST http://localhost:8080/api/products \
     -H "X-Tenant-Id: acme-corp" \
     -H "Content-Type: application/json" \
     -d '{"name": "Widget Pro", "description": "Um ótimo widget", "price": 99.99, "stock": 100}'
```

### Route + Body (Padrão de Atualização)

```pascal
type
  TProductUpdateRequest = record
    [FromRoute('id')]
    Id: Integer;
    
    // Do corpo JSON
    Name: string;
    Price: Currency;
  end;

App.MapPut<TProductUpdateRequest, IResult>('/api/products/{id}',
  function(Request: TProductUpdateRequest): IResult
  begin
    // Id vem do caminho da URL
    // Name e Price vêm do corpo JSON
    Result := Results.Ok(Format('Produto %d atualizado', [Request.Id]));
  end);
```

### Route + Query (Padrão de Filtro)

```pascal
type
  TProductFilterRequest = record
    [FromRoute('category')]
    Category: string;
    [FromQuery('sort')]
    Sort: string;
    [FromQuery('page')]
    Page: Integer;
  end;

App.MapGet<TProductFilterRequest, IResult>('/api/products/{category}',
  function(Request: TProductFilterRequest): IResult
  begin
    Result := Results.Ok(Format('Categoria: %s, Ordenação: %s, Página: %d',
      [Request.Category, Request.Sort, Request.Page]));
  end);
```

### Todas as Fontes Combinadas

```pascal
type
  TFullRequest = record
    [FromHeader('X-Api-Key')]
    ApiKey: string;
    [FromRoute('id')]
    ResourceId: Integer;
    [FromQuery('include')]
    Include: string;
    // Campos do body (padrão)
    Data: string;
    Count: Integer;
  end;

App.MapPut<TFullRequest, IResult>('/api/resources/{id}',
  function(Request: TFullRequest): IResult
  begin
    // ApiKey do header
    // ResourceId da rota /api/resources/123
    // Include da query ?include=details
    // Data e Count do corpo JSON
    Result := Results.Ok('Todas as fontes de binding utilizadas!');
  end);
```

## Prioridade de Binding (Sem Atributos Explícitos)

Quando um campo não tem atributo de binding explícito, o Dext usa fallback inteligente:

1. **Corpo JSON** - Primeiro tenta encontrar o campo no corpo da requisição
2. **Parâmetros de Rota** - Se não estiver no body, verifica parâmetros de rota (para IDs)
3. **Parâmetros de Query** - Finalmente verifica a query string

Isso permite que campos como `Id` sejam automaticamente vinculados da URL sem `[FromRoute]` explícito.

## Case Sensitivity

O matching de campos JSON é case-insensitive. Todos estes formatos JSON funcionam:

```json
{"name": "John", "email": "john@example.com"}
{"Name": "John", "Email": "john@example.com"}
{"NAME": "John", "EMAIL": "john@example.com"}
```

## Conversão de Tipos

Conversão automática para tipos comuns:

| Tipo | Valores de Exemplo |
|------|-------------------|
| `string` | Qualquer texto |
| `Integer` | `123`, `-456` |
| `Int64` | Números grandes |
| `Double` | `99.99`, `-0.5` |
| `Currency` | `1234.56` |
| `Boolean` | `true`, `false`, `1`, `0` |
| `TDateTime` | Formato ISO 8601 |
| `TGUID` | `{FORMATO-UUID}` |
| `TUUID` | String UUID |

## Binding de Parâmetros em Controllers

Em controllers, use atributos diretamente nos parâmetros do método:

```pascal
[HttpGet('/users/{id}')]
function GetById(
  [FromRoute] Id: Integer;
  [FromHeader('Authorization')] Token: string
): IActionResult;

[HttpPost('/users')]
function Create([FromBody] Request: TCreateUserRequest): IActionResult;

[HttpGet('/search')]
function Search(
  [FromQuery] Q: string;
  [FromQuery] Page: Integer;
  [FromQuery] Limit: Integer
): IActionResult;
```

## Objetos Aninhados

```pascal
type
  TAddress = record
    Street: string;
    City: string;
    ZipCode: string;
  end;
  
  TCreateUserRequest = record
    Name: string;
    Address: TAddress;  // Aninhado!
  end;
```

JSON:
```json
{
  "name": "John",
  "address": {
    "street": "Rua Principal 123",
    "city": "São Paulo",
    "zipCode": "01310-100"
  }
}
```

## Arrays

```pascal
type
  TBulkCreateRequest = record
    Users: TArray<TCreateUserRequest>;
  end;
```

JSON:
```json
{
  "users": [
    {"name": "John", "email": "john@example.com"},
    {"name": "Jane", "email": "jane@example.com"}
  ]
}
```

## Boas Práticas

1. **Use Records** para DTOs de requisição (tipos por valor, sem gerenciamento de memória)
2. **Seja Explícito** com atributos ao misturar fontes
3. **Valide Headers Obrigatórios** no código do seu handler
4. **Use `[FromHeader]`** para tenant IDs, API keys, correlation IDs
5. **Use `[FromQuery]`** para filtros, paginação, ordenação
6. **Use `[FromRoute]`** para identificadores de recursos

## Gerenciamento Automático de Memória para Classes

Ao usar **DTOs baseados em classes** em vez de records, o framework automaticamente libera o objeto após a execução do handler:

```pascal
type
  TCreateOrderDto = class
  public
    Items: IList<TOrderItemDto>;
  end;

// O framework automaticamente libera o Dto após o handler retornar
Builder.MapPost<TCreateOrderDto, IResult>('/api/orders',
  function(Dto: TCreateOrderDto): IResult
  begin
    // Use o Dto normalmente
    // NÃO chame Dto.Free - o framework cuida disso!
    Result := Results.Created('/api/orders/1', Dto);
  end);
```

> [!NOTE]
> Isso segue o princípio "quem cria, destrói": o Model Binding cria o objeto, então é responsável por limpá-lo.

---

[← Controllers](controllers.md) | [Próximo: Routing →](routing.md)
