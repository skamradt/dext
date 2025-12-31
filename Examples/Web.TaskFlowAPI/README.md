# TaskFlow API Example

A rich example demonstrating advanced features of the Dext Framework, combining Minimal API, Controllers, Smart Binding, and Service Injection.

## 🚀 Features

*   **Hybrid Routing**: Mixing `MapGet` (Minimal API) with `MapControllers` (MVC) in the same application.
*   **Smart Parameter Binding**: Automatically mapping URL segments (`{id}`) to heavily typed arguments (e.g., `Id: Integer`).
*   **Handler Injection**: Injecting services (e.g., `IUserService`) directly into delegate handlers alongside request body parameters.
*   **Typed Results**: Using `IResult` helpers (`Results.Json`, `Results.Created`) for structured, consistent responses.
*   **Functional Middleware**: Defining inline logging middleware using anonymous procedures.

## 🛠️ Getting Started

1.  **Compile** `Web.TaskFlowAPI.dproj`.
2.  **Run** `Web.TaskFlowAPI.exe`.
    *   Server starts on **http://localhost:8080**.
3.  **Test**:
    ```powershell
    .\Test.Web.TaskFlowAPI.ps1
    ```

## 💡 Key Concepts

### Handler Injection
This example showcases how Dext can inject both the Request Body and Services into a handler:

```delphi
// TUser comes from Body
// IUserService comes from DI
App.Builder.MapPost<TUser, IUserService, IResult>('/api/users',
  function(User: TUser; UserService: IUserService): IResult
  begin
    // Use the service directly
    var Created := UserService.CreateUser(User);
    Result := Results.Created('/api/users/1', Created);
  end);
```

### Smart Binding
Binding URL segments to primitive types:

```delphi
// {id} becomes Id: Integer
App.Builder.MapGet<Integer, IResult>('/api/tasks/{id}',
  function(Id: Integer): IResult
  begin
    // ...
  end);
```

## 📚 See Also

*   [Web Framework Documentation](../../Docs/web-framework.md)
*   [Dependency Injection](../../Docs/dependency-injection.md)
*   [Routing System](../../Docs/routing.md)
