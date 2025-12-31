# TUUID Binding Example

Examples of how to work with `TUUID` records in Dext Web Applications, specifically focusing on JSON binding and URL parameter parsing.

## 🚀 Features

*   **JSON Serialization**: `TUUID` fields in DTO records (e.g., `TProductRequest`) are automatically serialized/deserialized to JSON strings by the Dext JSON engine.
*   **Flexible Parsing**: `TUUID.FromString` robustly handles:
    *   Standard UUIDs (Hyphenated)
    *   Raw Hex (32 chars)
    *   Braced GUIDs (`{...}`)
*   **Validation**: Example logic for ensuring URL Route Parameter IDs match Body IDs in PUT requests.
*   **Database Interop**: Converting `TUUID` to Delphi's `TGUID` for database compatibility.

## 🛠️ Getting Started

1.  **Compile** `WebTUUIDBindingExample.dproj`.
2.  **Run** `WebTUUIDBindingExample.exe`.
    *   Server starts on **http://localhost:8080**.
3.  **Test**:
    ```powershell
    .\Test.Web.TUUIDBindingExample.ps1
    ```

## 📍 Endpoints

*   `POST /api/products`: Create product (JSON Body -> TUUID DTO).
*   `GET /api/products/{id}`: Get product (URL String -> TUUID).
*   `PUT /api/products/{id}`: Update product (URL vs Body Validation).
*   `POST /api/products/generate-v7`: Generate new UUID.
*   `GET /api/uuid/formats/{id}`: Flexible format playground.
