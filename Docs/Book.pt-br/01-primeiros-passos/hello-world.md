# Hello World

Vamos construir sua primeira API Dext!

## Minimal API (Recomendado)

A forma mais rápida de criar uma API:

```pascal
program HelloWorld;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  Dext.Web;

begin
  var App := WebApplication;
  var Builder := App.Builder;

  // Resposta de texto simples
  Builder.MapGet('/hello', procedure(Ctx: IHttpContext)
    begin
      Ctx.Response.Write('Olá, Mundo!');
    end);

  // Resposta JSON
  Builder.MapGet<string, IResult>('/api/saudar/{nome}', 
    function(Nome: string): IResult
    begin
      Result := Results.Json('{"mensagem": "Olá, ' + Nome + '!"}');
    end);

  // Endpoint POST com Model Binding
  Builder.MapPost<TValue, IResult>('/api/eco', 
    function(Body: TValue): IResult
    begin
      Result := Results.Json(Body);
    end);

  App.Run(5000);
end.
```

## Executar & Testar

1. **Execute** a aplicação
2. **Visite** `http://localhost:5000/hello`
3. **Experimente** `http://localhost:5000/api/saudar/SeuNome`

### Testar com curl

```bash
# Requisição GET
curl http://localhost:5000/hello

# GET com parâmetro
curl http://localhost:5000/api/saudar/Dext

# Requisição POST
curl -X POST http://localhost:5000/api/eco -d '{"teste": true}'
```

## O Que Está Acontecendo?

1. `WebApplication` - Cria o servidor e o builder (padrão ARC safe)
2. `App.Builder` - Fornece acesso ao pipeline de rotas e middlewares
3. `MapGet/MapPost` - Registra handlers de rotas com injeção automática
4. `App.Run(5000)` - Inicia o servidor na porta especificada

## Próximos Passos

- Adicionar mais endpoints
- Usar serialização JSON
- Conectar a um banco de dados
- Adicionar autenticação

---

[← Instalação](instalacao.md) | [Próximo: Estrutura do Projeto →](estrutura-projeto.md)
