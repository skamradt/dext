# Minimal APIs

Minimal APIs fornecem uma abordagem leve, baseada em lambdas, para construir endpoints HTTP.

> 📦 **Exemplo**: [Web.MinimalAPI](../../../Examples/Web.MinimalAPI/)

## Endpoints Básicos

```pascal
App.MapGet('/hello', procedure(Ctx: IHttpContext)
  begin
    Ctx.Response.Write('Olá, Mundo!');
  end);

App.MapPost('/data', procedure(Ctx: IHttpContext)
  begin
    var Body := Ctx.Request.BodyAsString;
    Ctx.Response.Json(Body);
  end);
```

## Parâmetros de Rota

> [!IMPORTANT]
> O Dext usa a sintaxe **`{param}`** para parâmetros de rota (como ASP.NET Core), não `:param` (estilo Express).

### Usando Record para Model Binding (Recomendado)

```pascal
type
  TUserIdRequest = record
    [FromRoute('id')]
    Id: Integer;
  end;

App.MapGet<IUserService, TUserIdRequest, IResult>('/users/{id}',
  function(Service: IUserService; Request: TUserIdRequest): IResult
  begin
    var User := Service.FindById(Request.Id);
    if User = nil then
      Result := Results.NotFound('Usuário não encontrado')
    else
      Result := Results.Ok(User);
  end);
```

### Usando Context Diretamente

```pascal
App.MapGet('/users/{id}', procedure(Ctx: IHttpContext)
  begin
    var Id := Ctx.Request.RouteParam('id');
    Ctx.Response.Write('ID do Usuário: ' + Id);
  end);
```

## Parâmetros de Query

### Usando Record com Model Binding

```pascal
type
  TSearchFilter = record
    [FromQuery('q')]
    Query: string;
    [FromQuery('limit')]
    Limit: Integer;
  end;

App.MapGet<TSearchFilter, IResult>('/search',
  function(Filter: TSearchFilter): IResult
  begin
    Result := Results.Ok(Format('Busca: %s, Limite: %d', [Filter.Query, Filter.Limit]));
  end);
```

## Binding de Headers

Para cenários multi-tenant ou chaves de API:

```pascal
type
  TTenantRequest = record
    [FromHeader('X-Tenant-Id')]
    TenantId: string;
  end;

App.MapGet<ITenantService, TTenantRequest, IResult>('/api/data',
  function(Service: ITenantService; Request: TTenantRequest): IResult
  begin
    if Request.TenantId = '' then
      Exit(Results.BadRequest('Header X-Tenant-Id obrigatório'));
      
    Result := Results.Ok(Service.GetDataForTenant(Request.TenantId));
  end);
```

## Endpoints Tipados com Injeção de Dependência

Os overloads genéricos injetam serviços e fazem bind dos dados automaticamente:

```pascal
// Injeção de serviço + model binding do body
App.MapPost<IUserService, TCreateUserDto, IResult>('/users',
  function(Service: IUserService; Dto: TCreateUserDto): IResult
  var
    User: TUser;
  begin
    User := Service.Create(Dto);
    Result := Results.Created('/users/' + IntToStr(User.Id), User);
  end);
```

## Padrão Results

Use o helper `Results` para respostas consistentes:

```pascal
Results.Ok(Data)             // 200 com corpo JSON
Results.Ok<T>(Data)          // 200 com serialização tipada
Results.Created('/path', E)  // 201 com header Location
Results.NoContent            // 204
Results.BadRequest('msg')    // 400
Results.NotFound('msg')      // 404
```

Execute com context:
```pascal
Results.Ok(User).Execute(Ctx);
```

Ou retorne diretamente de handlers tipados:
```pascal
function(...): IResult
begin
  Result := Results.Ok(User);
end;
```

## Resolução de Serviços

### Via Context
```pascal
var Service := Ctx.Services.GetService<IUserService>;
```

### Via Injeção Genérica (Recomendado)
```pascal
App.MapGet<IUserService, IResult>('/users',
  function(Service: IUserService): IResult
  begin
    Result := Results.Ok(Service.GetAll);
  end);
```

## Binding do Body

### Via Record DTO (Recomendado)

```pascal
type
  TCreateUserRequest = record
    [Required]
    Name: string;
    [StringLength(5, 100)]
    Email: string;
  end;

// DTO é automaticamente populado do body
App.MapPost<IUserService, TCreateUserRequest, IResult>('/users',
  function(Service: IUserService; Request: TCreateUserRequest): IResult
  ...
```

---

[← Framework Web](README.md) | [Próximo: Controllers →](controllers.md)
