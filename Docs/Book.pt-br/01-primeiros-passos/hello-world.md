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
  TWebHostBuilder.CreateDefault(nil)
    .UseUrls('http://localhost:5000')
    .Configure(procedure(App: IApplicationBuilder)
      begin
        // Resposta de texto simples
        App.MapGet('/hello', procedure(Ctx: IHttpContext)
          begin
            Ctx.Response.Write('Olá, Mundo!');
          end);

        // Resposta JSON
        App.MapGet('/api/saudar/{nome}', procedure(Ctx: IHttpContext)
          var
            Nome: string;
          begin
            Nome := Ctx.Request.RouteParams['nome'];
            Ctx.Response.Json('{"mensagem": "Olá, ' + Nome + '!"}');
          end);

        // Endpoint POST
        App.MapPost('/api/eco', procedure(Ctx: IHttpContext)
          var
            SR: TStreamReader;
            Body: string;
          begin
            SR := TStreamReader.Create(Ctx.Request.Body);
            try
              Body := SR.ReadToEnd;
              Ctx.Response.Json(Body);
            finally
              SR.Free;
            end;
          end);
      end)
    .Build
    .Run;
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

1. `TWebHostBuilder.CreateDefault` - Cria o servidor web
2. `UseUrls` - Define o endereço de escuta
3. `Configure` - Configura o pipeline de requisições
4. `MapGet/MapPost` - Registra handlers de rotas
5. `Build.Run` - Inicia o servidor

## Próximos Passos

- Adicionar mais endpoints
- Usar serialização JSON
- Conectar a um banco de dados
- Adicionar autenticação

---

[← Instalação](instalacao.md) | [Próximo: Estrutura do Projeto →](estrutura-projeto.md)
