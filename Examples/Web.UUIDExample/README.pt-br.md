# Exemplo UUID

Este exemplo demonstra como analisar e manipular UUIDs (Universally Unique Identifiers) nos handlers do Dext usando os tipos `TUUID` e `TGUID`.

## 🚀 Funcionalidades

- **Geração UUID v7**: UUIDs modernos e ordenáveis por tempo via `TUUID.NewV7`
- **Binding Automático de TUUID**: Parâmetros de rota vinculam diretamente ao tipo `TUUID`
- **Serialização JSON**: Campos `TGUID` em DTOs serializam/deserializam automaticamente
- **Interoperabilidade**: Conversão entre `TUUID`, `TGUID` e formatos string
- **JSON Case-Insensitive**: Body binding aceita chaves JSON em camelCase

## 🛠️ Como Iniciar

1. **Compile** `WebUUIDExample.dproj`
2. **Execute** `WebUUIDExample.exe`
   - O servidor inicia em **http://localhost:8080**
3. **Teste**:
   ```powershell
   .\Test.Web.UUIDExample.ps1
   ```

## 📍 Endpoints

| Método | Rota | Descrição |
|--------|------|-----------|
| `GET` | `/api/products/{id}` | Busca produto por UUID (parsing manual) |
| `POST` | `/api/products` | Cria produto (TGUID no body) |
| `PUT` | `/api/products/{id}` | Atualiza produto (valida ID URL/body) |
| `POST` | `/api/products/generate` | Gera novo UUID v7 |
| `GET` | `/api/uuid/test` | Mostra conversões de formato UUID |
| `GET` | `/api/uuid/lookup/{id}` | Busca por UUID (auto-bound TUUID) |

## 💡 Destaques do Código

### Geração UUID v7
```delphi
App.Builder.MapPost<IResult>('/api/products/generate',
  function: IResult
  var
    U: TUUID;
    Product: TProductDto;
  begin
    U := TUUID.NewV7;
    Product.Id := U.ToGUID;
    Product.Name := 'Produto Auto-gerado';
    Product.Price := 0.0;
    Result := Results.Ok<TProductDto>(Product);
  end);
```

### Parsing Manual de TUUID (Didático)
```delphi
App.Builder.MapGet('/api/products/{id}',
  procedure(Context: IHttpContext)
  var
    IdStr: string;
    U: TUUID;
  begin
    IdStr := Context.Request.RouteParams['id'];
    U := TUUID.FromString(IdStr);  // Aceita vários formatos
    // ...
  end);
```

### Binding Automático de TUUID da Rota
```delphi
App.Builder.MapGet<TUUID, IResult>('/api/uuid/lookup/{id}',
  function(Id: TUUID): IResult
  begin
    // Id é automaticamente parseado do parâmetro da rota
    WriteLn('ID Bound: ', Id.ToString);
    Result := Results.Ok<TProductDto>(Product);
  end);
```

### PUT com Validação ID URL/Body
```delphi
App.Builder.MapPut<TUUID, TProductDto, IResult>('/api/products/{id}',
  function(Id: TUUID; Dto: TProductDto): IResult
  var
    BodyId: TUUID;
  begin
    BodyId := TUUID.FromGUID(Dto.Id);
    if Id <> BodyId then
      Result := Results.BadRequest('ID da URL não corresponde ao ID do body')
    else
      Result := Results.Ok<TProductDto>(Dto);
  end);
```

## 🔧 Funcionalidades do Framework Demonstradas

- **Route Binding TUUID/TGUID**: O framework automaticamente vincula parâmetros de rota aos tipos `TUUID` ou `TGUID`
- **Normalização JSON de TGUID**: GUIDs JSON sem chaves são automaticamente normalizados antes do parsing
- **Body Binding Case-Insensitive**: Campos do body correspondem aos campos do record independente de case

## 🔗 Veja Também

- [Web.SmartPropsDemo](../Web.SmartPropsDemo) - Smart properties e model binding
- [Dext.Types.UUID](../../Sources/Core/Dext.Types.UUID.pas) - Implementação do UUID
