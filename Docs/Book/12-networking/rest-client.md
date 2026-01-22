# REST Client

The `Dext.Net` module provides a modern, fluent, and high-performance HTTP client for Delphi. It is built on top of the native `THttpClient` but adds robust connection pooling, automatic serialization, and a developer-friendly API inspired by modern frameworks like Refit and RestSharp.

## Installation

The REST Client is part of the `Dext.Net` package. Ensure your project references the following units:

```pascal
uses
  Dext.Net.RestClient,
  Dext.Net.RestRequest,
  Dext.Threading.Async;
```

## Basic Usage

The `TRestClient` record provides a fluent interface for making HTTP requests. Use `Create` to initialize it with a base URL, and then chain methods to build and execute the request.

```pascal
var
  LClient: TRestClient;
begin
  LClient := TRestClient.Create('https://api.example.com');
  
  LClient.Get('/users/1')
    .OnComplete(
      procedure(Res: IRestResponse)
      begin
        Writeln('Status: ', Res.StatusCode); // 200
        Writeln('Body: ', Res.ContentString);
      end)
    .Start;
end;
```

> [!NOTE]
> `TRestClient` is a lightweight record. It uses a shared, thread-safe connection pool internally, so you can create instances cheaply without worrying about socket exhaustion.

## Making Requests

### Request Builders

The fluent API supports all standard HTTP verbs:

```pascal
Client.Get('/resource');
Client.Post('/resource');
Client.Put('/resource');
Client.Delete('/resource');
Client.Patch('/resource');
```

### Adding Headers and Query Parameters

You can easily add headers and query parameters using the builder pattern:

```pascal
Client.Get('/search')
  .Header('Authorization', 'Bearer my-token')
  .Header('X-Custom-Header', 'Value')
  .QueryParam('q', 'delphi')
  .QueryParam('page', '1')
  .Start;
```

### Request Body

For `POST` and `PUT` requests, you can provide a body:

**JSON Body (Automatic Serialization)**
```pascal
var
  LUser: TUser;
begin
  LUser := TUser.Create('John Doe');
  try
    Client.Post('/users')
      .Body(LUser) // Automatically serialized to JSON
      .Start;
  finally
    LUser.Free;
  end;
end;
```

**Raw String Body**
```pascal
Client.Post('/data')
  .JsonBody('{"name": "test"}')
  .Start;
```

**Stream Body**
```pascal
Client.Post('/upload')
  .Body(LFileStream)
  .Start;
```

## Handling Responses

You can handle responses as raw strings, streams, or typed objects.

### Typed Responses (Deserialization)

Use `Get<T>`, `Post<T>`, etc., or `.Execute<T>` to automatically deserialize the JSON response into a Delphi object or record.

```pascal
  .Get<TUser>('/users/1') // Returns TAsyncBuilder<TUser>
  .OnComplete(
    procedure(User: TUser)
    begin
      Writeln('User: ', User.Name);
    end)
  .Start;
```

## Asynchronous & Cancellation

The client is fully integrated with `Dext.Threading.Async`.

### Synchronous Execution

If you need to block the current thread and wait for the result (e.g., in a console application or background worker), use `.Await`.

```pascal
var Res := Client.Get<TUser>('/users/1')
  .Await; // Blocks thread until complete
```

### Chaining Operations

```pascal
Client.Get<TToken>('/auth/token')
  .ThenBy<TUser>(
    function(Token: TToken): TUser
    begin
      // Use token to get user profile
      Result := Client.Get('/profile')
        .Header('Authorization', Token.AccessToken)
        .Execute<TUser> // Or use Get<TUser> directly if no headers needed
        .Await; // Synchronous wait inside async task
    end)
  .OnComplete(procedure(User: TUser) ... )
  .Start;
```

### Cancellation

Pass a `ICancellationToken` to cancel long-running requests.

```pascal
var
  LTokenSource: ICancellationTokenSource;
begin
  LTokenSource := TCancellationTokenSource.Create;
  
  Client.Get('/long-process')
    .Cancellation(LTokenSource.Token)
    .Start;
    
  // ... later ...
  LTokenSource.Cancel;
end;
```

## Connection Pooling

The `Dext.Net` client uses a high-performance, thread-safe connection pool (`TConnectionPool`). 

- **Efficient Reuse**: Native `THttpClient` instances are reused, avoiding the overhead of creating new TCP connections and SSL handshakes for every request.
- **Thread Safety**: The pool is fully thread-safe. You can share `TRestClient` instances or create new ones across multiple threads without manual locking.
- **Automatic Cleanup**: Stale connections are automatically managed.

## Authentication

The client supports pluggable authentication providers via `IAuthenticationProvider`.

```pascal
// Bearer Token
Client.Authenticator(TBearerAuthProvider.Create('my-jwt-token'));

// Basic Auth
Client.Authenticator(TBasicAuthProvider.Create('user', 'password'));

// API Key
Client.Authenticator(TApiKeyAuthProvider.Create('X-API-Key', '12345'));
```

## Thread Safety

The `TRestClient` design ensures thread safety:

1.  **Immutable Configuration**: When you call `Execute`, the client creates a snapshot of the current configuration (headers, auth, timeout).
2.  **Isolated Execution**: The actual HTTP request runs in a background task with its own `THttpClient` instance acquired from the pool.
3.  **No Shared State**: Modifying the client *after* calling `Start` does not affect the already running request.
