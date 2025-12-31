# Exemplo UUID (Binding Manual)

Este exemplo demonstra como analisar e lidar com UUIDs (Universally Unique Identifiers) manualmente nos handlers do Dext.

> **Nota**: Para binding automático de parâmetros, verifique `Web.TUUIDBindingExample`.

## 🚀 Funcionalidades

*   **Geração UUID v7**: Gerando UUIDs modernos e ordenáveis por tempo usando `TUUID.NewV7`.
*   **Parsing Manual**: Convertendo Parâmetros de Rota (`string`) para `TUUID` ou `TGUID` usando `TUUID.FromString`.
*   **Serialização JSON**: Manipulação automática de UUIDs em corpos JSON via `Dext.Json`.
*   **Interoperabilidade de Formatos**: Conversão entre Dext `TUUID`, Delphi `TGUID` e formatos de string padrão (raw, hifens, chaves).

## 🛠️ Como Iniciar

1.  **Compile** `WebUUIDExample.dproj`.
2.  **Execute** `WebUUIDExample.exe`.
    *   O servidor inicia em **http://localhost:8080**.
3.  **Teste**:
    ```powershell
    .\Test.Web.UUIDExample.ps1
    ```

## 📍 Endpoints

*   `POST /api/products/generate`: Gera um novo TUUID v7 e o retorna.
*   `GET /api/products/{id}`: Busca um recurso pelo seu UUID string.
*   `POST /api/products`: Cria um recurso passando um UUID no corpo JSON.
*   `GET /api/uuid/test`: Diagnósticos mostrando diferentes formatos de UUID e extração de timestamp.
