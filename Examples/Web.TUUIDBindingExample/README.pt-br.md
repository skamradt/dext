# Exemplo Binding TUUID

Exemplos de como trabalhar com records `TUUID` em Aplicações Web Dext, focando especificamente em binding JSON e parsing de parâmetros de URL.

## 🚀 Funcionalidades

*   **Serialização JSON**: Campos `TUUID` em records DTO (ex: `TProductRequest`) são automaticamente serializados/deserializados para strings JSON pela engine Dext JSON.
*   **Parsing Flexível**: `TUUID.FromString` lida robustamente com:
    *   UUIDs Padrão (Com hífens)
    *   Hex Raw (32 caracteres)
    *   GUIDs com chaves (`{...}`)
*   **Validação**: Lógica de exemplo para garantir que IDs de Parâmetros de Rota (URL) correspondam aos IDs do Corpo em requisições PUT.
*   **Interop com Banco de Dados**: Conversão de `TUUID` para `TGUID` do Delphi para compatibilidade com bancos de dados.

## 🛠️ Como Iniciar

1.  **Compile** `WebTUUIDBindingExample.dproj`.
2.  **Execute** `WebTUUIDBindingExample.exe`.
    *   O servidor inicia em **http://localhost:8080**.
3.  **Teste**:
    ```powershell
    .\Test.Web.TUUIDBindingExample.ps1
    ```

## 📍 Endpoints

*   `POST /api/products`: Cria produto (Corpo JSON -> DTO TUUID).
*   `GET /api/products/{id}`: Obtém produto (String URL -> TUUID).
*   `PUT /api/products/{id}`: Atualiza produto (Validação URL vs Corpo).
*   `POST /api/products/generate-v7`: Gera novo UUID.
*   `GET /api/uuid/formats/{id}`: Playground de formatos flexíveis.
