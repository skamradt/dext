# OpenAPI / Swagger

Gere documentação interativa da API automaticamente.

> 📦 **Exemplo**: [Web.SwaggerExample](../../../Examples/Web.SwaggerExample/)

## Configuração Rápida

```pascal
App.Configure(procedure(App: IApplicationBuilder)
  begin
    App.UseSwagger;
    App.UseSwaggerUI;
    
    // Seus endpoints...
  end);
```

Visite `http://localhost:5000/swagger` para ver a interface!

## Documentando Endpoints

### Minimal APIs

```pascal
App.MapGet('/users', procedure(Ctx: IHttpContext)
  begin
    Ctx.Response.Json(UserService.GetAll);
  end)
  .SwaggerEndpoint
    .Summary('Listar todos os usuários')
    .Description('Retorna uma lista de todos os usuários registrados')
    .Tag('Usuários')
    .Response(200, 'Lista de usuários')
    .Response(401, 'Não autorizado');
```

### Controllers

```pascal
type
  [Route('/api/users')]
  [SwaggerTag('Usuários', 'Endpoints de gerenciamento de usuários')]
  TUsersController = class(TController)
  public
    [HttpGet]
    [SwaggerSummary('Listar todos os usuários')]
    [SwaggerResponse(200, 'Sucesso', TArray<TUser>)]
    function GetAll: IActionResult;
    
    [HttpGet('/:id')]
    [SwaggerSummary('Buscar usuário por ID')]
    [SwaggerParam('id', 'ID do usuário', True)]
    [SwaggerResponse(200, 'Usuário encontrado', TUser)]
    [SwaggerResponse(404, 'Usuário não encontrado')]
    function GetById(Id: Integer): IActionResult;
  end;
```

## Atributos Swagger

| Atributo | Descrição |
|----------|-----------|
| `[SwaggerSummary('')]` | Descrição curta |
| `[SwaggerDescription('')]` | Descrição detalhada |
| `[SwaggerTag('Nome')]` | Agrupar endpoints |
| `[SwaggerParam('nome', 'desc')]` | Documentar parâmetro |
| `[SwaggerBody(TType)]` | Tipo do request body |
| `[SwaggerResponse(code, 'desc')]` | Documentação de resposta |

---

[← Recursos da API](README.md) | [Próximo: Rate Limiting →](rate-limiting.md)
