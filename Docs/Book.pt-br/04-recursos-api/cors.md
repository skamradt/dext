# CORS

Configure Cross-Origin Resource Sharing para clientes de browser.

## Configuração Rápida

```pascal
App.Configure(procedure(App: IApplicationBuilder)
  begin
    App.UseCors(
      TCorsOptions.Create
        .AllowAnyOrigin
        .AllowAnyMethod
        .AllowAnyHeader
    );
    
    // Endpoints...
  end);
```

> ⚠️ `AllowAnyOrigin` é apenas para desenvolvimento. Em produção, especifique origens permitidas.

## Configuração de Produção

```pascal
App.UseCors(
  TCorsOptions.Create
    .AllowOrigins(['https://meuapp.com', 'https://admin.meuapp.com'])
    .AllowMethods(['GET', 'POST', 'PUT', 'DELETE'])
    .AllowHeaders(['Content-Type', 'Authorization'])
    .AllowCredentials
    .MaxAge(3600)  // Cache de preflight: 1 hora
);
```

## Referência de Opções

| Método | Descrição |
|--------|-----------|
| `.AllowAnyOrigin` | Permitir todas as origens (apenas dev) |
| `.AllowOrigins([...])` | Permitir origens específicas |
| `.AllowAnyMethod` | Permitir todos os métodos HTTP |
| `.AllowMethods([...])` | Permitir métodos específicos |
| `.AllowAnyHeader` | Permitir todos os headers |
| `.AllowHeaders([...])` | Permitir headers específicos |
| `.AllowCredentials` | Permitir cookies/headers de auth |
| `.MaxAge(segundos)` | Cache da resposta preflight |

## Problemas Comuns

### "No 'Access-Control-Allow-Origin' header"

O middleware CORS deve estar **antes** dos seus endpoints:

```pascal
// ✅ Ordem correta
App.UseCors(CorsOptions);
App.MapGet('/api', Handler);

// ❌ Ordem errada
App.MapGet('/api', Handler);
App.UseCors(CorsOptions);  // Tarde demais!
```

---

[← Rate Limiting](rate-limiting.md) | [Próximo: Health Checks →](health-checks.md)
