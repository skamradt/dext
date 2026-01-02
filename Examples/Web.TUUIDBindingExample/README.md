# TUUID Binding Example

This example demonstrates comprehensive TUUID binding scenarios in Dext Web applications, including automatic model binding, JSON deserialization, and format parsing.

## 🚀 Features Demonstrated

| Feature | Endpoint | Description |
|---------|----------|-------------|
| **Manual Parsing** | `GET /api/products/{id}` | `TUUID.FromString` for flexible format handling |
| **Auto Binding** | `GET /api/products/lookup/{id}` | Framework automatically binds TUUID from route |
| **Body Binding** | `POST /api/products` | TUUID field in DTO deserialized from JSON |
| **Mixed Sources** | `PUT /api/products/{id}` | TUUID from URL + Body with validation |
| **UUID v7 Generation** | `POST /api/products/generate-v7` | `TUUID.NewV7` for time-ordered UUIDs |
| **Format Parsing** | `GET /api/uuid/formats/{id}` | Accepts hyphenated, raw hex, and braced formats |

## 🛠️ Getting Started

1. **Compile** `Web.TUUIDBindingExample.dproj`
2. **Run** `Web.TUUIDBindingExample.exe`
   - Server starts on **http://localhost:8080**
3. **Test**:
   ```powershell
   .\Test.Web.TUUIDBindingExample.ps1
   ```

## 💡 Code Highlights

### Automatic TUUID Binding from Route
```delphi
App.Builder.MapGet<TUUID, IResult>('/api/products/lookup/{id}',
  function(Id: TUUID): IResult
  begin
    // Id is automatically parsed from route parameter
    WriteLn('Auto-bound: ', Id.ToString);
    Result := Results.Ok<TProductRequest>(Product);
  end);
```

### TUUID in JSON Body
```delphi
TProductRequest = record
  Id: TUUID;  // Automatically deserialized from JSON
  Name: string;
  Price: Double;
end;

App.Builder.MapPost<TProductRequest, IResult>('/api/products',
  function(Product: TProductRequest): IResult
  begin
    // Product.Id is a TUUID parsed from JSON {"id":"xxx-xxx-..."}
    Result := Results.Created<TProductRequest>(Product);
  end);
```

### URL vs Body ID Validation
```delphi
App.Builder.MapPut<TUUID, TProductRequest, IResult>('/api/products/{id}',
  function(UrlId: TUUID; Body: TProductRequest): IResult
  begin
    // TUUID supports equality operator
    if UrlId <> Body.Id then
      Exit(Results.BadRequest('URL ID does not match body ID'));
      
    Result := Results.Ok<TProductRequest>(Body);
  end);
```

### UUID v7 with Timestamp Extraction
```delphi
NewId := TUUID.NewV7;
WriteLn('Generated: ', NewId.ToString);
WriteLn('Timestamp: ', NewId.ToTimestamp, ' ms');  // Unix timestamp
```

### Flexible Format Parsing
```delphi
// All these inputs produce the same TUUID:
U := TUUID.FromString('a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11');  // Standard
U := TUUID.FromString('a0eebc999c0b4ef8bb6d6bb9bd380a11');      // No hyphens  
U := TUUID.FromString('{a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11}'); // Braced
```

## 🔗 See Also

- [UUID Guide](../../docs/uuid-guide.md) - TUUID technical details
- [Model Binding Guide](../../docs/model-binding.md) - Binding configuration
- [Web.UUIDExample](../Web.UUIDExample) - General UUID usage with TGUID interop
