# Exemplo TUUID Binding

Este exemplo demonstra cenários completos de binding TUUID em aplicações Dext Web, incluindo model binding automático, deserialização JSON e parsing de formatos.

## 🚀 Funcionalidades Demonstradas

| Funcionalidade | Endpoint | Descrição |
|----------------|----------|-----------|
| **Parsing Manual** | `GET /api/products/{id}` | `TUUID.FromString` para formatos flexíveis |
| **Binding Automático** | `GET /api/products/lookup/{id}` | Framework vincula TUUID automaticamente |
| **Binding de Body** | `POST /api/products` | Campo TUUID no DTO deserializado do JSON |
| **Fontes Mistas** | `PUT /api/products/{id}` | TUUID da URL + Body com validação |
| **Geração UUID v7** | `POST /api/products/generate-v7` | `TUUID.NewV7` para UUIDs ordenados por tempo |
| **Parsing de Formatos** | `GET /api/uuid/formats/{id}` | Aceita formatos com hífen, hex puro e com chaves |

## 🛠️ Como Iniciar

1. **Compile** `Web.TUUIDBindingExample.dproj`
2. **Execute** `Web.TUUIDBindingExample.exe`
   - O servidor inicia em **http://localhost:8080**
3. **Teste**:
   ```powershell
   .\Test.Web.TUUIDBindingExample.ps1
   ```

## 💡 Destaques do Código

### Binding Automático de TUUID da Rota
```delphi
App.Builder.MapGet<TUUID, IResult>('/api/products/lookup/{id}',
  function(Id: TUUID): IResult
  begin
    // Id é automaticamente parseado do parâmetro da rota
    WriteLn('Auto-bound: ', Id.ToString);
    Result := Results.Ok<TProductRequest>(Product);
  end);
```

### TUUID no Body JSON
```delphi
TProductRequest = record
  Id: TUUID;  // Deserializado automaticamente do JSON
  Name: string;
  Price: Double;
end;

App.Builder.MapPost<TProductRequest, IResult>('/api/products',
  function(Product: TProductRequest): IResult
  begin
    // Product.Id é TUUID parseado de {"id":"xxx-xxx-..."}
    Result := Results.Created<TProductRequest>(Product);
  end);
```

### Validação ID URL vs Body
```delphi
App.Builder.MapPut<TUUID, TProductRequest, IResult>('/api/products/{id}',
  function(UrlId: TUUID; Body: TProductRequest): IResult
  begin
    // TUUID suporta operador de igualdade
    if UrlId <> Body.Id then
      Exit(Results.BadRequest('ID da URL não corresponde ao ID do body'));
      
    Result := Results.Ok<TProductRequest>(Body);
  end);
```

### UUID v7 com Extração de Timestamp
```delphi
NewId := TUUID.NewV7;
WriteLn('Gerado: ', NewId.ToString);
WriteLn('Timestamp: ', NewId.ToTimestamp, ' ms');  // Unix timestamp
```

### Parsing de Formatos Flexíveis
```delphi
// Todas estas entradas produzem o mesmo TUUID:
U := TUUID.FromString('a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11');  // Padrão
U := TUUID.FromString('a0eebc999c0b4ef8bb6d6bb9bd380a11');      // Sem hífens  
U := TUUID.FromString('{a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11}'); // Com chaves
```

## 🔗 Veja Também

- [Guia UUID](../../docs/uuid-guide.md) - Detalhes técnicos do TUUID
- [Guia Model Binding](../../docs/model-binding.md) - Configuração de binding
- [Web.UUIDExample](../Web.UUIDExample) - Uso geral de UUID com interop TGUID
