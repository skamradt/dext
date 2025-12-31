# Swagger + Controllers Exemplo

Este exemplo demonstra como integrar documentação **Swagger/OpenAPI** com **Controllers MVC** no Dext Framework.

## 🚀 Funcionalidades

*   **Swagger com Controllers**: Use `[SwaggerOperation]`, `[SwaggerResponse]`, `[SwaggerTag]` em actions de controllers
*   **Documentação de Schemas**: DTOs anotados com `[SwaggerSchema]`, `[SwaggerProperty]`, `[SwaggerExample]`
*   **Integração com Segurança**: Atributos `[Authorize]` aparecem como ícones de cadeado no Swagger UI
*   **Descoberta Automática**: Controllers são auto-descobertos e documentados

## 🛠️ Como Usar

1.  **Compile** `Web.SwaggerControllerExample.dproj`
2.  **Execute** `Web.SwaggerControllerExample.exe`
    *   Servidor inicia em **http://localhost:8080**
3.  **Explore**:
    *   **Swagger UI**: http://localhost:8080/swagger
    *   **OpenAPI JSON**: http://localhost:8080/swagger.json
4.  **Teste**:
    ```powershell
    .\Test.Web.SwaggerControllerExample.ps1
    ```

## 📝 Referência de Atributos

### Atributos de Controller

| Atributo | Descrição |
|----------|-----------|
| `[DextController('/path')]` | Define o prefixo de rota do controller |
| `[SwaggerTag('Nome')]` | Agrupa endpoints sob uma tag no Swagger UI |
| `[Authorize('Scheme')]` | Marca todas as actions como requerendo autenticação |

### Atributos de Action

| Atributo | Descrição |
|----------|-----------|
| `[DextGet('/path')]` | Endpoint HTTP GET |
| `[DextPost('/path')]` | Endpoint HTTP POST |
| `[DextPut('/path')]` | Endpoint HTTP PUT |
| `[DextPatch('/path')]` | Endpoint HTTP PATCH |
| `[DextDelete('/path')]` | Endpoint HTTP DELETE |
| `[SwaggerOperation('resumo', 'descrição')]` | Documentação do endpoint |
| `[SwaggerResponse(código, 'descrição')]` | Documentação de resposta |
| `[AllowAnonymous]` | Permite acesso não autenticado |

### Atributos de Schema

| Atributo | Descrição |
|----------|-----------|
| `[SwaggerSchema('nome', 'descrição')]` | Documentação do tipo |
| `[SwaggerProperty('descrição')]` | Documentação da propriedade |
| `[SwaggerExample('valor')]` | Valor de exemplo |
| `[SwaggerRequired]` | Marca campo como obrigatório |
| `[SwaggerFormat('formato')]` | Dica de formato (email, date, etc.) |

## 📖 Exemplo de Controller

```pascal
[DextController('/api/books')]
[SwaggerTag('Books')]
TBooksController = class
public
  [DextGet('')]
  [AllowAnonymous]
  [SwaggerOperation('Listar todos os livros', 'Retorna todos os livros do catálogo')]
  procedure GetAll(Ctx: IHttpContext); virtual;

  [DextPost('')]
  [SwaggerOperation('Criar um novo livro', 'Cria uma nova entrada de livro')]
  [SwaggerResponse(201, 'Livro criado')]
  [SwaggerResponse(400, 'Requisição inválida')]
  procedure Create(Ctx: IHttpContext; const Request: TCreateBookRequest); virtual;
end;
```

## 📚 Veja Também

- [Web.SwaggerExample](../Web.SwaggerExample) - Minimal API com Swagger
- [Web.ControllerExample](../Web.ControllerExample) - Controllers sem Swagger
- [Documentação Swagger](../../Docs/swagger.md)
