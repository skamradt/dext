# 4. Recursos da API

Recursos essenciais para construir APIs prontas para produção.

## Capítulos

1. [OpenAPI / Swagger](openapi-swagger.md) - Documentação auto-gerada
2. [Rate Limiting](rate-limiting.md) - Limitação de requisições
3. [CORS](cors.md) - Compartilhamento de recursos cross-origin
4. [Cache de Resposta](cache.md) - Headers de cache & estratégias
5. [Health Checks](health-checks.md) - Endpoints de monitoramento

## Exemplos Rápidos

### Swagger

```pascal
App.UseSwagger;
App.UseSwaggerUI;
// Visite: /swagger
```

### Rate Limiting

```pascal
App.UseRateLimiting(
  TRateLimitOptions.Create
    .Limit(100)
    .PerMinute
);
```

### CORS

```pascal
App.UseCors(
  TCorsOptions.Create
    .AllowOrigin('https://meuapp.com')
    .AllowMethods(['GET', 'POST'])
);
```

### Health Check

```pascal
App.MapGet('/health', procedure(Ctx: IHttpContext)
  begin
    Ctx.Response.Json('{"status": "saudável"}');
  end);
```

---

[← Autenticação](../03-autenticacao/README.md) | [Próximo: OpenAPI/Swagger →](openapi-swagger.md)
