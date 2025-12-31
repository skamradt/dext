# Exemplo Rate Limiting

Demonstra como proteger sua API contra abusos usando o middleware `TApplicationBuilderRateLimitExtensions.UseRateLimiting`.

## 🚀 Funcionalidades

*   **Política de Janela Fixa**: Limita requisições a um número específico por janela de tempo (ex: 10 requisições por minuto).
*   **Tratamento de Rejeição**: Retorna `429 Too Many Requests` com um JSON personalizado quando o limite é excedido.
*   **Headers Informativos**: Adiciona automaticamente headers padrão da indústria:
    *   `X-RateLimit-Limit`: Máximo de requisições permitidas.
    *   `X-RateLimit-Remaining`: Requisições restantes na janela atual.
    *   `Retry-After`: Segundos para esperar antes que o limite reinicie.

## 🛠️ Como Iniciar

1.  **Compile** `Web.RateLimitDemo.dproj`.
2.  **Execute** `Web.RateLimitDemo.exe`.
    *   O servidor inicia em **http://localhost:8080**.
3.  **Teste**:
    ```powershell
    .\Test.Web.RateLimitDemo.ps1
    ```
    *   O script envia 15 requisições em rápida sucessão para acionar o limite.

## ⚙️ Configuração

Veja `Web.RateLimitDemo.dpr`:

```delphi
var Policy := TRateLimitPolicy.FixedWindow(10, 60) // 10 requisições por 60 segundos
  .WithRejectionMessage('{"error":"Too many requests!"}')
  .WithRejectionStatusCode(429);
  
TApplicationBuilderRateLimitExtensions.UseRateLimiting(Builder, Policy);
```
