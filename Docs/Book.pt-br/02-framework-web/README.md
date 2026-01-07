# 2. Framework Web

O Dext oferece dois estilos para construir APIs web: **Minimal APIs** e **Controllers**.

## Capítulos

1. [Minimal APIs](minimal-apis.md) - Handlers baseados em lambda
2. [Controllers](controllers.md) - Estilo MVC com classes
3. [Model Binding](model-binding.md) - Mapeamento de requisição para objeto
4. [Rotas](rotas.md) - Padrões de URL & parâmetros
5. [Middleware](middleware.md) - Pipeline de requisições

## Comparação Rápida

### Estilo Minimal API

```pascal
App.MapGet('/users/:id', procedure(Ctx: IHttpContext)
  var
    Id: Integer;
  begin
    Id := StrToInt(Ctx.Request.RouteParam('id'));
    Ctx.Response.Json(UserService.GetById(Id));
  end);
```

### Estilo Controller

```pascal
type
  [Route('/users')]
  TUsersController = class(TController)
  public
    [HttpGet('/:id')]
    function GetById(Id: Integer): IActionResult;
  end;

function TUsersController.GetById(Id: Integer): IActionResult;
begin
  Result := Ok(UserService.GetById(Id));
end;
```

## Quando Usar Cada Um

| Minimal APIs | Controllers |
|-------------|-------------|
| Protótipos rápidos | Aplicações grandes |
| Microsserviços | Roteamento complexo |
| CRUD simples | Middleware compartilhado |
| Estilo lambda | Estilo OOP |

---

[← Primeiros Passos](../01-primeiros-passos/README.md) | [Próximo: Minimal APIs →](minimal-apis.md)
