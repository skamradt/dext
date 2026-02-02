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
  var App := WebApplication;
  var Builder := App.Builder;

  // Simple text response
  Builder.MapGet('/hello', procedure(Ctx: IHttpContext)
    begin
      Ctx.Response.Write('Hello, World!');
    end);

  // JSON response
  Builder.MapGet<string, IResult>('/api/greet/{name}', 
    function(Name: string): IResult
    begin
      Result := Results.Json('{"message": "Hello, ' + Name + '!"}');
    end);

  // POST endpoint with Model Binding
  Builder.MapPost<TValue, IResult>('/api/echo', 
    function(Body: TValue): IResult
    begin
      Result := Results.Json(Body);
    end);

  App.Run(5000);
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

1. `WebApplication` - Creates the server and builder (ARC safe pattern)
2. `App.Builder` - Provides access to the routing and middleware pipeline
3. `MapGet/MapPost` - Registers route handlers with automatic injection
4. `App.Run(5000)` - Starts the server on the specified port

## Next Steps

- Add more endpoints
- Use JSON serialization
- Connect to a database
- Add authentication

---

[← Installation](installation.md) | [Next: Project Structure →](project-structure.md)
