# Rate Limiting

Proteja sua API contra abusos com limitação de requisições.

> 📦 **Exemplo**: [Web.RateLimitDemo](../../../Examples/Web.RateLimitDemo/)

## Configuração Rápida

```pascal
App.Configure(procedure(App: IApplicationBuilder)
  begin
    App.UseRateLimiting(
      TRateLimitOptions.Create
        .Limit(100)
        .PerMinute
    );
    
    // Endpoints...
  end);
```

## Opções de Configuração

### Janela Fixa

```pascal
TRateLimitOptions.Create
  .Limit(100)        // 100 requisições
  .PerMinute         // por minuto
  
TRateLimitOptions.Create
  .Limit(1000)
  .PerHour
  
TRateLimitOptions.Create
  .Limit(10000)
  .PerDay
```

### Por Chave

```pascal
// Por IP (padrão)
TRateLimitOptions.Create
  .ByIP
  .Limit(100).PerMinute

// Por ID de Usuário
TRateLimitOptions.Create
  .ByUser
  .Limit(1000).PerHour

// Por API Key
TRateLimitOptions.Create
  .ByHeader('X-API-Key')
  .Limit(5000).PerDay
```

## Limites por Endpoint

```pascal
// Limite global
App.UseRateLimiting(GlobalOptions);

// Sobrescrever para endpoint específico
App.MapPost('/api/operacao-cara', Handler)
  .RateLimit(
    TRateLimitOptions.Create
      .Limit(10)
      .PerMinute
  );
```

## Resposta de Limite Excedido

Quando limite é excedido, retorna `429 Too Many Requests`:

```json
{
  "error": "Limite de requisições excedido",
  "retryAfter": 30
}
```

---

[← OpenAPI/Swagger](openapi-swagger.md) | [Próximo: CORS →](cors.md)
