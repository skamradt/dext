# RFC-002: Dext.Web API Cleanup

> **Status**: Proposto  
> **Data**: 2026-01-31  
> **Autor**: Dext Team  

## Objetivo

Simplificar a API pública do `Dext.Web.Core` removendo prefixos `Dext` desnecessários dos atributos e tipos, criando aliases deprecated para compatibilidade retroativa.

## Contexto

O controller de exemplo usa atributos como:
```pascal
[DextController('/api/categories')]
[DextGet('')]
[DextPost('')]
[DextPut('/{id}')]
[DextDelete('/{id}')]
[DextPatch('/{id}/status')]
```

Esses prefixos são redundantes quando já se usa `Dext.Web`. A proposta é simplificar para:
```pascal
[Route('/api/categories')]  // ou [Controller('/api')]
[Get('')]
[Post('')] 
[Put('/{id}')]
[Delete('/{id}')]
[Patch('/{id}/status')]
```

## Mudanças Propostas

### 1. Atributos de Routing (Dext.Web.Routing.Attributes)

| Atual | Novo | Alias Deprecated |
|-------|------|------------------|
| `DextControllerAttribute` | `RouteAttribute` ou `ControllerAttribute` | `DextController = RouteAttribute deprecated` |
| `DextGetAttribute` | `GetAttribute` | `DextGet = GetAttribute deprecated` |
| `DextPostAttribute` | `PostAttribute` | `DextPost = PostAttribute deprecated` |
| `DextPutAttribute` | `PutAttribute` | `DextPut = PutAttribute deprecated` |
| `DextDeleteAttribute` | `DeleteAttribute` | `DextDelete = DeleteAttribute deprecated` |
| `DextPatchAttribute` | `PatchAttribute` | `DextPatch = PatchAttribute deprecated` |
| `DextHeadAttribute` | `HeadAttribute` | `DextHead = HeadAttribute deprecated` |
| `DextOptionsAttribute` | `OptionsAttribute` | `DextOptions = OptionsAttribute deprecated` |
| `DextRouteAttribute` | `RouteAttribute` | Usar o novo `RouteAttribute` |

### 2. Atributos de Model Binding (Dext.Web.ModelBinding)

Estes já estão corretos:
- `FromBodyAttribute` ✅
- `FromQueryAttribute` ✅
- `FromRouteAttribute` ✅
- `FromHeaderAttribute` ✅
- `FromServicesAttribute` ✅

### 3. Atributos de Auth (Dext.Auth.Attributes)

Já estão corretos:
- `AuthorizeAttribute` ✅
- `AllowAnonymousAttribute` ✅

### 4. Atributos de Swagger (Dext.OpenAPI.Attributes)

Já estão corretos:
- `SwaggerTagAttribute` ✅
- `SwaggerOperationAttribute` ✅
- `SwaggerResponseAttribute` ✅
- `SwaggerParamAttribute` ✅
- `SwaggerIgnoreAttribute` ✅

### 5. Funções Globais e Builders

#### OpenAPI/Swagger Builder
```pascal
// Atual
var Opts := TOpenAPIOptions.Create;
Opts.Title := 'My API';
Builder.UseSwagger(Opts);

// Novo (função global + fluent)
Builder.UseSwagger(Swagger.Title('My API').Version('v1'));
```

A função global `Swagger` já foi criada em `Dext.OpenAPI.Generator.pas`.

#### CORS Builder
```pascal
// Atual  
Builder.UseCors(Cors.AllowAnyOrigin.AllowAnyMethod);
```
Já está implementado ✅

## Resumo de Ações

### A. Renomear Atributos em `Dext.Web.Routing.Attributes.pas`

```pascal
// Novos nomes (preferidos)
RouteAttribute = class(TCustomAttribute)  // Para class-level routing
GetAttribute = class(TCustomAttribute)
PostAttribute = class(TCustomAttribute)
PutAttribute = class(TCustomAttribute)
DeleteAttribute = class(TCustomAttribute)
PatchAttribute = class(TCustomAttribute)
HeadAttribute = class(TCustomAttribute)
OptionsAttribute = class(TCustomAttribute)

// Aliases deprecated para compatibilidade
DextControllerAttribute = RouteAttribute deprecated 'Use RouteAttribute instead';
DextGetAttribute = GetAttribute deprecated 'Use GetAttribute instead';
DextPostAttribute = PostAttribute deprecated 'Use PostAttribute instead';
DextPutAttribute = PutAttribute deprecated 'Use PutAttribute instead';
DextDeleteAttribute = DeleteAttribute deprecated 'Use DeleteAttribute instead';
DextPatchAttribute = PatchAttribute deprecated 'Use PatchAttribute instead';
DextHeadAttribute = HeadAttribute deprecated 'Use HeadAttribute instead';
DextOptionsAttribute = OptionsAttribute deprecated 'Use OptionsAttribute instead';
```

### B. Atualizar `Dext.Web.pas` (Facade)

Adicionar exports para os novos tipos:
```pascal
// Novos aliases limpos
RouteAttribute = Dext.Web.Routing.Attributes.RouteAttribute;
GetAttribute = Dext.Web.Routing.Attributes.GetAttribute;
PostAttribute = Dext.Web.Routing.Attributes.PostAttribute;
// ... etc

// Aliases curtos para conveniência em uses
Route = RouteAttribute;
Get = GetAttribute;
Post = PostAttribute;
// ... etc (se não conflitar)
```

### C. Atualizar Documentação e Exemplos

- Atualizar `OrderAPI.Controllers.pas` para usar novos atributos
- Atualizar outros exemplos
- Atualizar SKILL.md
- Adicionar entrada no CHANGELOG.md

## Exemplo Final

### Antes
```pascal
[DextController('/api/orders')]
[Authorize]
[SwaggerTag('Orders')]
TOrdersController = class
  [DextGet('')]
  procedure GetAll(Ctx: IHttpContext);
  
  [DextPost('')]
  procedure Create(Ctx: IHttpContext; const Request: TCreateOrderRequest);
  
  [DextPut('/{id}')]
  procedure Update(Ctx: IHttpContext; [FromRoute] Id: Int64);
end;
```

### Depois
```pascal
[Route('/api/orders')]
[Authorize]
[SwaggerTag('Orders')]
TOrdersController = class
  [Get('')]
  procedure GetAll(Ctx: IHttpContext);
  
  [Post('')]
  procedure Create(Ctx: IHttpContext; const Request: TCreateOrderRequest);
  
  [Put('/{id}')]
  procedure Update(Ctx: IHttpContext; [FromRoute] Id: Int64);
end;
```

## Impacto

- **Baixo risco**: Aliases deprecated mantêm compatibilidade total
- **Alto benefício**: API mais limpa e intuitiva
- **Esforço**: Médio (renomear classes, criar aliases, atualizar facade)

## Checklist

- [ ] Renomear atributos em `Dext.Web.Routing.Attributes.pas`
- [ ] Criar aliases deprecated
- [ ] Atualizar `Dext.Web.pas` facade
- [ ] Atualizar `Dext.Web.ControllerScanner.pas` para reconhecer novos atributos
- [ ] Atualizar exemplos
- [ ] Atualizar SKILL.md
- [ ] Adicionar ao CHANGELOG.md
- [ ] Compilar e testar
