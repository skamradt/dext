# UUID Example

This example demonstrates how to parse and handle UUIDs (Universally Unique Identifiers) in Dext handlers using both `TUUID` and `TGUID` types.

## 🚀 Features

- **UUID v7 Generation**: Modern, time-sortable UUIDs via `TUUID.NewV7`
- **Automatic TUUID Binding**: Route parameters bind directly to `TUUID` type
- **JSON Serialization**: `TGUID` fields in DTOs serialize/deserialize automatically
- **Format Interop**: Convert between `TUUID`, `TGUID`, and string formats
- **Case-Insensitive JSON**: Body binding handles camelCase JSON keys

## 🛠️ Getting Started

1. **Compile** `WebUUIDExample.dproj`
2. **Run** `WebUUIDExample.exe`
   - Server starts on **http://localhost:8080**
3. **Test**:
   ```powershell
   .\Test.Web.UUIDExample.ps1
   ```

## 📍 Endpoints

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/api/products/{id}` | Get product by UUID (manual parsing) |
| `POST` | `/api/products` | Create product (TGUID in body) |
| `PUT` | `/api/products/{id}` | Update product (validate URL/body ID match) |
| `POST` | `/api/products/generate` | Generate new UUID v7 |
| `GET` | `/api/uuid/test` | Show UUID format conversions |
| `GET` | `/api/uuid/lookup/{id}` | Get by UUID (auto-bound TUUID) |

## 💡 Code Highlights

### UUID v7 Generation
```delphi
App.Builder.MapPost<IResult>('/api/products/generate',
  function: IResult
  var
    U: TUUID;
    Product: TProductDto;
  begin
    U := TUUID.NewV7;
    Product.Id := U.ToGUID;
    Product.Name := 'Auto-generated Product';
    Product.Price := 0.0;
    Result := Results.Ok<TProductDto>(Product);
  end);
```

### Manual TUUID Parsing (Didactic)
```delphi
App.Builder.MapGet('/api/products/{id}',
  procedure(Context: IHttpContext)
  var
    IdStr: string;
    U: TUUID;
  begin
    IdStr := Context.Request.RouteParams['id'];
    U := TUUID.FromString(IdStr);  // Handles various formats
    // ...
  end);
```

### Automatic TUUID Binding from Route
```delphi
App.Builder.MapGet<TUUID, IResult>('/api/uuid/lookup/{id}',
  function(Id: TUUID): IResult
  begin
    // Id is automatically parsed from route parameter
    WriteLn('Bound ID: ', Id.ToString);
    Result := Results.Ok<TProductDto>(Product);
  end);
```

### PUT with URL/Body ID Validation
```delphi
App.Builder.MapPut<TUUID, TProductDto, IResult>('/api/products/{id}',
  function(Id: TUUID; Dto: TProductDto): IResult
  var
    BodyId: TUUID;
  begin
    BodyId := TUUID.FromGUID(Dto.Id);
    if Id <> BodyId then
      Result := Results.BadRequest('URL ID does not match body ID')
    else
      Result := Results.Ok<TProductDto>(Dto);
  end);
```

## 🔧 Framework Features Demonstrated

- **TUUID/TGUID Route Binding**: The framework automatically binds route parameters to `TUUID` or `TGUID` types
- **TGUID JSON Normalization**: JSON GUIDs without braces are automatically normalized before parsing
- **Case-Insensitive Body Binding**: Body fields match record fields regardless of case

## 🔗 See Also

- [Web.SmartPropsDemo](../Web.SmartPropsDemo) - Smart properties and model binding
- [Dext.Types.UUID](../../Sources/Core/Dext.Types.UUID.pas) - UUID implementation
