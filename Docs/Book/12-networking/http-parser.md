# HTTP File Parser

The `Dext.Http` module provides a parser for `.http` files, a popular format used by VS Code REST Client and IntelliJ HTTP Client extensions. This allows you to define and test API requests in a simple, readable format.

## Installation

Add the following units to your project:

```pascal
uses
  Dext.Http.Request,
  Dext.Http.Parser,
  Dext.Http.Executor;
```

## The .http File Format

The `.http` format is a simple text-based way to define HTTP requests:

```http
### Variables
@baseUrl = https://api.example.com
@token = my-secret-token

### Get all users
GET {{baseUrl}}/users
Authorization: Bearer {{token}}
Accept: application/json

### Create user
POST {{baseUrl}}/users
Content-Type: application/json

{
  "name": "John Doe",
  "email": "john@example.com"
}
```

### Syntax Elements

| Element | Description | Example |
|---------|-------------|---------|
| `@name = value` | Variable definition | `@baseUrl = https://api.com` |
| `{{varName}}` | Variable interpolation | `{{baseUrl}}/users` |
| `{{env:NAME}}` | Environment variable | `{{env:API_TOKEN}}` |
| `### Comment` | Request separator with name | `### Get all users` |
| `METHOD URL` | Request line | `GET https://api.com/users` |
| `Header: Value` | Request header | `Authorization: Bearer token` |
| Blank line | Separates headers from body | |
| JSON/Text | Request body | `{"name": "John"}` |

## Parsing .http Files

Use `THttpRequestParser` to parse `.http` file content:

```pascal
var
  Collection: THttpRequestCollection;
begin
  // Parse from file
  Collection := THttpRequestParser.ParseFile('api-tests.http');
  try
    WriteLn('Variables: ', Collection.Variables.Count);
    WriteLn('Requests: ', Collection.Requests.Count);
    
    // List all requests
    for var Request in Collection.Requests do
      WriteLn('  - [', Request.Method, '] ', Request.Name);
  finally
    Collection.Free;
  end;
end;
```

### Parsing from String

You can also parse directly from a string:

```pascal
const
  HttpContent = 
    '@baseUrl = https://api.example.com' + sLineBreak +
    '### Get users' + sLineBreak +
    'GET {{baseUrl}}/users';

var
  Collection: THttpRequestCollection;
begin
  Collection := THttpRequestParser.Parse(HttpContent);
  // ...
end;
```

## Executing Requests

Use `THttpExecutor` to execute parsed requests using `TRestClient`:

```pascal
var
  Collection: THttpRequestCollection;
  Request: THttpRequestInfo;
  Result: THttpExecutionResult;
begin
  Collection := THttpRequestParser.ParseFile('api-tests.http');
  try
    // Find request by name
    Request := Collection.FindByName('Get all users');
    
    if Assigned(Request) then
    begin
      // Execute with variable interpolation
      Result := THttpExecutor.ExecuteSync(Request, Collection.Variables);
      
      WriteLn('Status: ', Result.StatusCode, ' ', Result.StatusText);
      WriteLn('Duration: ', Result.DurationMs, 'ms');
      WriteLn('Body: ', Result.ResponseBody);
    end;
  finally
    Collection.Free;
  end;
end;
```

### Async Execution

For non-blocking execution:

```pascal
THttpExecutor.ExecuteAsync(Request)
  .OnComplete(
    procedure(Response: IRestResponse)
    begin
      WriteLn('Status: ', Response.StatusCode);
    end)
  .Start;
```

## Variable Resolution

Variables are automatically resolved when executing requests:

1. **Static Variables**: Defined with `@name = value`
2. **Environment Variables**: Defined with `@name = {{env:VAR_NAME}}`
3. **Inline Environment**: Used directly as `{{env:VAR_NAME}}`

```pascal
// Manual variable resolution
THttpRequestParser.ResolveRequest(Request, Collection.Variables);

// Now Request.Url, Request.Headers, and Request.Body have interpolated values
WriteLn('Resolved URL: ', Request.Url);
```

## Request Model

The `THttpRequestInfo` class contains all parsed request data:

| Property | Type | Description |
|----------|------|-------------|
| `Name` | `string` | Request name (from `### Comment`) |
| `Method` | `string` | HTTP method (GET, POST, etc.) |
| `Url` | `string` | Request URL (may contain variables) |
| `Headers` | `TDictionary<string, string>` | Request headers |
| `Body` | `string` | Request body (JSON, XML, etc.) |
| `LineNumber` | `Integer` | Line number in source file |

## Use Cases

### API Testing
Create `.http` files to document and test your APIs without external tools.

### Integration Tests
Parse and execute requests in automated tests to validate API behavior.

### API Documentation
Use `.http` files as executable documentation for your APIs.

### Development Tools
Build custom tooling that reads and executes HTTP requests.

## Example File

See `Examples/example-api.http` for a complete example using JSONPlaceholder API.
