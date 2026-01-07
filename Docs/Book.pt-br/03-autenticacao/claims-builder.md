# Claims Builder

Construa payloads JWT de forma rápida e segura.

## Atributos de Tokens (Claims)

Claims são declarações sobre uma entidade (geralmente o usuário). No Dext, você usa o `TClaimsBuilder` para criar esses conjuntos de dados.

## Exemplo de Uso

```pascal
uses
  Dext.Web.Security;

var
  Claims: TArray<TClaim>;
begin
  Claims := TClaimsBuilder.Create
    .AddSub('user_12345')             // Subject (ID do usuário)
    .AddName('João Silva')            // Nome real
    .AddEmail('joao@provedor.com')    // Email
    .AddRole('Admin')                 // Papel/Regra
    .AddRole('Financeiro')            // Múltiplos papéis
    .AddClaim('tenant_id', '789')     // Claim customizada
    .AddClaim('premium', 'true')
    .Build;
end;
```

## Claims Padrão (RFC 7519)

| Método | Claim | Descrição |
|--------|-------|-----------|
| `.AddSub()` | `sub` | Identificador único do usuário |
| `.AddIss()` | `iss` | Emissor do token |
| `.AddAud()` | `aud` | Público que pode usar o token |
| `.AddExp()` | `exp` | Data de expiração |
| `.AddIat()` | `iat` | Data de emissão |

## Verificação no Handler

Quando o usuário envia um token válido, você acessa as claims diretamente no contexto:

```pascal
App.MapGet('/me', procedure(Ctx: IHttpContext)
  begin
    var Nome := Ctx.User.FindFirst('name');
    var EhAdmin := Ctx.User.IsInRole('Admin');
    
    Ctx.Response.Json(Format('{"usuario": "%s", "admin": %s}', [Nome, BoolToStr(EhAdmin, 'sim', 'não')]));
  end)
  .RequireAuthorization;
```

---

[← Autenticação JWT](jwt-auth.md) | [Próximo: Recursos da API →](../04-recursos-api/README.md)
