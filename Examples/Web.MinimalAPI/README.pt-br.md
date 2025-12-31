# Exemplo Minimal API

A forma mais simples de criar endpoints HTTP com Dext — sem Controllers!

## 🚀 Funcionalidades

*   **Mapeamento Simples**: Defina rotas diretamente com `App.MapGet()`, `App.MapPost()`, etc.
*   **Handlers Tipados**: Use `MapGetR<T>()` para retornos fortemente tipados
*   **Injeção de Dependência**: Acesse serviços do contexto da requisição
*   **Parâmetros de Query**: Leia query string com `Context.Request.Query`
*   **Respostas JSON**: Envie JSON com `Context.Response.Json()` ou `Results.Ok()`

## 🛠️ Como Iniciar

1.  **Compile** `Web.MinimalAPIExample.dproj`
2.  **Execute** `Web.MinimalAPIExample.exe`
    *   O servidor inicia em **http://localhost:5000**
3.  **Teste**:
    ```powershell
    .\Test.Web.MinimalAPI.ps1
    ```

## 📍 Endpoints

| Endpoint | Descrição |
|----------|-----------|
| `GET /hello?name=World` | Saudação com serviço DI |
| `GET /echo?text=foo` | Ecoa "Echo: foo" |
| `GET /time` | Hora atual do servidor |
| `GET /json` | Objeto JSON |
| `GET /health` | Health check |

## 📖 Exemplos de Código

### Mapeamento Básico de Rota

```pascal
// Endpoint GET simples
App.MapGet('/echo',
  procedure(Context: IHttpContext)
  begin
    var Text := Context.Request.Query.Values['text'];
    Context.Response.Write('Echo: ' + Text);
  end);
```

### Handlers Tipados com IResult

```pascal
// Recomendado para APIs - retorna IResult
TApplicationBuilderExtensions.MapGetR<IResult>(App, '/time',
  function: IResult
  begin
    Result := Results.Ok(Format('Hora do servidor: %s', [DateTimeToStr(Now)]));
  end);
```

### Injeção de Dependência

```pascal
// Registrar serviço
Builder.ConfigureServices(
  procedure(Services: IServiceCollection)
  begin
    TServiceCollectionExtensions.AddSingleton<IGreetingService, TGreetingService>(Services);
  end);

// Resolver no handler
App.MapGet('/hello',
  procedure(Context: IHttpContext)
  var
    Svc: IGreetingService;
  begin
    if Supports(Context.Services.Resolve<IGreetingService>, IGreetingService, Svc) then
      Context.Response.Write(Svc.GetGreeting('World'));
  end);
```

### Resposta JSON

```pascal
App.MapGet('/json',
  procedure(Context: IHttpContext)
  begin
    Context.Response.Json('{"message": "Hello JSON!"}');
  end);
```

## 📚 Veja Também

- [Web.SwaggerExample](../Web.SwaggerExample) - Minimal API com documentação OpenAPI
- [Web.ControllerExample](../Web.ControllerExample) - Usando Controllers para APIs complexas
- [Web.JwtAuthDemo](../Web.JwtAuthDemo) - Adicionando autenticação JWT
