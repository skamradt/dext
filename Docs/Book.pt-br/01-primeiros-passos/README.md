# 1. Primeiros Passos

Bem-vindo ao Dext! Esta seção vai te colocar para rodar em minutos.

## Capítulos

1. [Instalação](instalacao.md) - Configurando Dext no seu projeto Delphi
2. [Hello World](hello-world.md) - Sua primeira API Dext
3. [Estrutura do Projeto](estrutura-projeto.md) - Entendendo o layout

## Início Rápido

```pascal
program HelloDext;

{$APPTYPE CONSOLE}

uses
  Dext.Web;

begin
  TWebHostBuilder.CreateDefault(nil)
    .UseUrls('http://localhost:5000')
    .Configure(procedure(App: IApplicationBuilder)
      begin
        App.MapGet('/hello', procedure(Ctx: IHttpContext)
          begin
            Ctx.Response.Write('Olá, Dext!');
          end);
      end)
    .Build
    .Run;
end.
```

Execute e visite `http://localhost:5000/hello` 🎉

---

[Próximo: Instalação →](instalacao.md)
