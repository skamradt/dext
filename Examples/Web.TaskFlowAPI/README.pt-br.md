# Exemplo TaskFlow API

Um exemplo rico demonstrando recursos avançados do Dext Framework, combinando Minimal API, Controllers, Smart Binding e Injeção de dependência em handlers.

## 🚀 Funcionalidades

*   **Roteamento Híbrido**: Misturando `MapGet` (Minimal API) com `MapControllers` (MVC) na mesma aplicação.
*   **Smart Parameter Binding**: Mapeamento automático de segmentos de URL (`{id}`) para argumentos tipados (ex: `Id: Integer`).
*   **Handler Injection**: Injeção de serviços (ex: `IUserService`) diretamente em handlers delegate junto com parâmetros do corpo da requisição.
*   **Typed Results**: Uso de helpers `IResult` (`Results.Json`, `Results.Created`) para respostas estruturadas e consistentes.
*   **Middleware Funcional**: Definição de middleware de log inline usando procedures anônimas.

## 🛠️ Como Iniciar

1.  **Compile** `Web.TaskFlowAPI.dproj`.
2.  **Execute** `Web.TaskFlowAPI.exe`.
    *   O servidor inicia em **http://localhost:8080**.
3.  **Teste**:
    ```powershell
    .\Test.Web.TaskFlowAPI.ps1
    ```

## 💡 Conceitos Chave

### Handler Injection
Este exemplo mostra como o Dext pode injetar tanto o Corpo da Requisição quanto Serviços em um handler:

```delphi
// TUser vem do Body
// IUserService vem do DI
TApplicationBuilderExtensions.MapPostR<TUser, IUserService, IResult>(App, '/api/users',
  function(User: TUser; UserService: IUserService): IResult
  begin
    // Usa o serviço diretamente
    var Created := UserService.CreateUser(User);
    Result := Results.Created('/api/users/1', Created);
  end);
```

### Smart Binding
Binding de segmentos de URL para tipos primitivos:

```delphi
// {id} vira Id: Integer
TApplicationBuilderExtensions.MapGetR<Integer, IResult>(App, '/api/tasks/{id}',
  function(Id: Integer): IResult
  begin
    // ...
  end);
```
