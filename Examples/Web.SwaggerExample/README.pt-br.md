# Exemplo Swagger/OpenAPI

Este exemplo demonstra como integrar o Swagger UI e a geração de especificação OpenAPI em uma aplicação web Dext.

## 🚀 Funcionalidades

*   **Geração Automática de OpenAPI**: Use atributos como `[SwaggerSchema]`, `[SwaggerProperty]` para definir a documentação da API diretamente no código.
*   **Middleware Swagger UI**: Serve a interface interativa do Swagger em `/swagger`.
*   **Integração Minimal API**: Endpoints `MapGet`, `MapPost` são automaticamente descobertos e documentados.
*   **DSL Fluente**: Configure endpoints com a elegante API fluente `SwaggerEndpoint`.

## 🛠️ Como Iniciar

1.  **Compile** `Web.SwaggerExample.dproj`.
2.  **Execute** `Web.SwaggerExample.exe`.
    *   O servidor inicia em **http://localhost:5000**.
3.  **Explore**:
    *   **Interface Gráfica**: Abra `http://localhost:5000/swagger` no seu navegador.
    *   **Especificação JSON**: `http://localhost:5000/swagger.json`.
4.  **Teste**:
    ```powershell
    .\Test.Web.SwaggerExample.ps1
    ```

## 📝 API Fluente (Recomendada)

A nova DSL fluente oferece configuração limpa e encadeável:

```pascal
uses
  Dext.OpenAPI.Fluent;

SwaggerEndpoint.From(App.MapGet('/api/users/{id}', Handler))
  .Summary('Obter usuário por ID')
  .Description('Retorna detalhes do usuário pelo identificador único')
  .Tag('Users')
  .Response(200, TypeInfo(TUser), 'Usuário encontrado')
  .Response(404, TypeInfo(TErrorResponse), 'Usuário não encontrado');
```

### Métodos Disponíveis

| Método | Descrição |
|--------|-----------|
| `.Summary(...)` | Descrição curta exibida no cabeçalho do Swagger UI |
| `.Description(...)` | Documentação detalhada do endpoint |
| `.Tag(...)` | Agrupa endpoints no Swagger UI |
| `.Tags([...])` | Múltiplas tags de uma vez |
| `.Response(code, type, desc)` | Documenta schemas de resposta |
| `.RequestType(type)` | Documenta schema do corpo da requisição |
| `.RequireAuthorization(scheme)` | Marca como endpoint protegido |

## 📝 API Tradicional (Alternativa)

Para controle explícito, use `TEndpointMetadataExtensions`:

```pascal
uses
  Dext.OpenAPI.Extensions;

TEndpointMetadataExtensions.WithMetadata(
  App.MapGet('/api/users', Handler),
  'Listar usuários',
  'Retorna todos os usuários cadastrados',
  ['Users']);
TEndpointMetadataExtensions.WithResponse(App, 200, 'OK', TypeInfo(TUser));
```

## 📚 Veja Também

- [Documentação Swagger](../../Docs/swagger.md)
- [Atributos Swagger](../../Docs/swagger-attributes.md)
- [Segurança OpenAPI](../../Docs/openapi-security.md)
