# Rate Limiting Example

Demonstrates how to protect your API from abuse using the `TApplicationBuilderRateLimitExtensions.UseRateLimiting` middleware.

## 🚀 Features

*   **Fixed Window Policy**: Limits requests to a specific number per time window (e.g., 10 requests per minute).
*   **Rejection Handling**: Returns `429 Too Many Requests` with a custom JSON payload when the limit is exceeded.
*   **Informative Headers**: Automatically adds industry-standard headers:
    *   `X-RateLimit-Limit`: Maximum requests allowed.
    *   `X-RateLimit-Remaining`: Requests remaining in the current window.
    *   `Retry-After`: Seconds to wait before the limit resets.

## 🛠️ Getting Started

1.  **Compile** `Web.RateLimitDemo.dproj`.
2.  **Run** `Web.RateLimitDemo.exe`.
    *   Server starts on **http://localhost:8080**.
3.  **Test**:
    ```powershell
    .\Test.Web.RateLimitDemo.ps1
    ```
    *   The script sends 15 requests in rapid succession to trigger the limit.

## ⚙️ Configuration

Check `Web.RateLimitDemo.dpr`:

```delphi
var Policy := TRateLimitPolicy.FixedWindow(10, 60) // 10 requests per 60 seconds
  .WithRejectionMessage('{"error":"Too many requests!"}')
  .WithRejectionStatusCode(429);
  
TApplicationBuilderRateLimitExtensions.UseRateLimiting(Builder, Policy);
```
