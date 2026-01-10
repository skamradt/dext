# Hello World

Let's build your first Dext API!

## Minimal API (Recommended)

The fastest way to create an API:

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
        // Simple text response
        App.MapGet('/hello', procedure(Ctx: IHttpContext)
          begin
            Ctx.Response.Write('Hello, World!');
          end);

        // JSON response
        App.MapGet('/api/greet/{name}', procedure(Ctx: IHttpContext)
          var
            Name: string;
          begin
            Name := Ctx.Request.RouteParams['name'];
            Ctx.Response.Json('{"message": "Hello, ' + Name + '!"}');
          end);

        // POST endpoint
        App.MapPost('/api/echo', procedure(Ctx: IHttpContext)
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

## Run & Test

1. **Run** the application
2. **Visit** `http://localhost:5000/hello`
3. **Try** `http://localhost:5000/api/greet/YourName`

### Test with curl

```bash
# GET request
curl http://localhost:5000/hello

# GET with parameter
curl http://localhost:5000/api/greet/Dext

# POST request
curl -X POST http://localhost:5000/api/echo -d '{"test": true}'
```

## What's Happening?

1. `TWebHostBuilder.CreateDefault` - Creates the web server
2. `UseUrls` - Sets the listening address
3. `Configure` - Configures the request pipeline
4. `MapGet/MapPost` - Registers route handlers
5. `Build.Run` - Starts the server

## Next Steps

- Add more endpoints
- Use JSON serialization
- Connect to a database
- Add authentication

---

[← Installation](installation.md) | [Next: Project Structure →](project-structure.md)
