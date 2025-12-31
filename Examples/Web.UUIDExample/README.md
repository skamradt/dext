# UUID Example (Manual Binding)

This example demonstrates how to parse and handle UUIDs (Universally Unique Identifiers) manually in Dext handlers.

> **Note**: For automatic parameter binding, check `Web.TUUIDBindingExample`.

## 🚀 Features

*   **UUID v7 Generation**: Generating modern, time-sortable UUIDs using `TUUID.NewV7`.
*   **Manual Parsing**: Converting Route Params (`string`) to `TUUID` or `TGUID` using `TUUID.FromString`.
*   **JSON Serialization**: Handling UUIDs in JSON bodies automatically via `Dext.Json`.
*   **Format Interop**: Converting between Dext `TUUID`, Delphi `TGUID`, and standard string formats (raw, hyphens, braces).

## 🛠️ Getting Started

1.  **Compile** `WebUUIDExample.dproj`.
2.  **Run** `WebUUIDExample.exe`.
    *   Server starts on **http://localhost:8080**.
3.  **Test**:
    ```powershell
    .\Test.Web.UUIDExample.ps1
    ```

## 📍 Endpoints

*   `POST /api/products/generate`: Generates a new TUUID v7 and returns it.
*   `GET /api/products/{id}`: Look up a resource by its UUID string.
*   `POST /api/products`: Create a resource passing a UUID in the JSON body.
*   `GET /api/uuid/test`: Diagnostics showing different UUID formats and timestamp extraction.
