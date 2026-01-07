# 1. Getting Started

Welcome to Dext! This section will get you up and running in minutes.

## Chapters

1. [Installation](installation.md) - Setting up Dext in your Delphi project
2. [Hello World](hello-world.md) - Your first Dext API
3. [Project Structure](project-structure.md) - Understanding the layout

## Quick Start

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
            Ctx.Response.Write('Hello, Dext!');
          end);
      end)
    .Build
    .Run;
end.
```

Run and visit `http://localhost:5000/hello` 🎉

---

[Next: Installation →](installation.md)
