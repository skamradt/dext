# Cache de Resposta

Otimize a performance de sua API com cache no nível de HTTP.

## Atributo [ResponseCache]

No estilo Controller, você pode usar o atributo para definir headers de cache automaticamente:

```pascal
type
  TProductsController = class(TController)
  public
    [HttpGet]
    [ResponseCache(Duration := 60)] // Cache por 60 segundos
    function GetAll: IActionResult;
  end;
```

## Usando no Minimal APIs

```pascal
App.MapGet('/api/noticias', Handler)
  .Cache(60); // Cache por 60 segundos
```

## Cabeçalhos Gerados

O Dext adicionará os seguintes cabeçalhos à resposta:
- `Cache-Control: public, max-age=60`
- `Expires: <Data Atual + 60s>`

## Perfis de Cache (Profiles)

Você pode definir perfis globais na configuração no `Startup`:

```pascal
Services.AddResponseCaching(procedure(Options: TResponseCachingOptions)
  begin
    Options.AddProfile('Padrao', procedure(P: TCacheProfile)
      begin
        P.Duration := 300;
        P.Location := clAny;
      end);
  end);

// Uso
[ResponseCache(Profile := 'Padrao')]
```

## Vantagens do Cache

- **Redução de Carga**: Menos requisições chegam ao banco de dados ou lógica de negócio.
- **Velocidade**: Browsers e Proxies retornam o dado instantaneamente sem consultar o servidor.
- **Economia**: Menor consumo de banda e CPU.

---

[← CORS](cors.md) | [Próximo: Health Checks →](health-checks.md)
