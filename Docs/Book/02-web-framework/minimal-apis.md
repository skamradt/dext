# Minimal APIs

Minimal APIs provide a lightweight, lambda-based approach to building HTTP endpoints.

> 📦 **Example**: [Web.MinimalAPI](../../../Examples/Web.MinimalAPI/)

## Basic Endpoints

```pascal
App.MapGet('/hello', procedure(Ctx: IHttpContext)
  begin
    Ctx.Response.Write('Hello, World!');
  end);

App.MapPost('/data', procedure(Ctx: IHttpContext)
  begin
    var Body := Ctx.Request.BodyAsString;
    Ctx.Response.Json(Body);
  end);
```

## Route Parameters

> [!IMPORTANT]
> Dext uses **`{param}`** syntax for route parameters (like ASP.NET Core), not `:param` (Express style).

### Using Record-Based Model Binding (Recommended)

```pascal
type
  TUserIdRequest = record
    [FromRoute('id')]
    Id: Integer;
  end;

App.MapGet<IUserService, TUserIdRequest, IResult>('/users/{id}',
  function(Service: IUserService; Request: TUserIdRequest): IResult
  begin
    var User := Service.FindById(Request.Id);
    if User = nil then
      Result := Results.NotFound('User not found')
    else
      Result := Results.Ok(User);
  end);
```

### Using Context Directly

```pascal
App.MapGet('/users/{id}', procedure(Ctx: IHttpContext)
  begin
    var Id := Ctx.Request.RouteParam('id');
    Ctx.Response.Write('User ID: ' + Id);
  end);
```

## Query Parameters

### Using Record-Based Model Binding

```pascal
type
  TSearchFilter = record
    [FromQuery('q')]
    Query: string;
    [FromQuery('limit')]
    Limit: Integer;
  end;

App.MapGet<TSearchFilter, IResult>('/search',
  function(Filter: TSearchFilter): IResult
  begin
    // Filter.Query and Filter.Limit are auto-bound
    Result := Results.Ok(Format('Search: %s, Limit: %d', [Filter.Query, Filter.Limit]));
  end);
```

## Header Binding

For multi-tenant or API key scenarios:

```pascal
type
  TTenantRequest = record
    [FromHeader('X-Tenant-Id')]
    TenantId: string;
  end;

App.MapGet<ITenantService, TTenantRequest, IResult>('/api/data',
  function(Service: ITenantService; Request: TTenantRequest): IResult
  begin
    if Request.TenantId = '' then
      Exit(Results.BadRequest('X-Tenant-Id header required'));
      
    Result := Results.Ok(Service.GetDataForTenant(Request.TenantId));
  end);
```

## Typed Endpoints with Dependency Injection

The generic overloads automatically inject services and bind request data:

```pascal
// Service injection + body model binding
App.MapPost<IUserService, TCreateUserDto, IResult>('/users',
  function(Service: IUserService; Dto: TCreateUserDto): IResult
  var
    User: TUser;
  begin
    User := Service.Create(Dto);
    Result := Results.Created('/users/' + IntToStr(User.Id), User);
  end);
```

## Results Pattern

Use the `Results` helper for consistent responses:

```pascal
Results.Ok(Data)             // 200 with JSON body
Results.Ok<T>(Data)          // 200 with typed serialization
Results.Created('/path', E)  // 201 with Location header
Results.NoContent            // 204
Results.BadRequest('msg')    // 400
Results.NotFound('msg')      // 404
Results.StatusCode(418, '..') // Custom status
```

Execute with context:
```pascal
Results.Ok(User).Execute(Ctx);
```

Or return directly from typed handlers:
```pascal
function(...): IResult
begin
  Result := Results.Ok(User);
end;
```

## Service Resolution

### Via Context
```pascal
var Service := Ctx.Services.GetService<IUserService>;
```

### Via Generic Injection (Recommended)
```pascal
App.MapGet<IUserService, IResult>('/users',
  function(Service: IUserService): IResult
  begin
    Result := Results.Ok(Service.GetAll);
  end);
```

## Body Binding

### Via Record DTO (Recommended)

```pascal
type
  TCreateUserRequest = record
    [Required]
    Name: string;
    [StringLength(5, 100)]
    Email: string;
  end;

// DTO is auto-bound from body
App.MapPost<IUserService, TCreateUserRequest, IResult>('/users',
  function(Service: IUserService; Request: TCreateUserRequest): IResult
  ...
```

### Manual (Legacy)

```pascal
var Dto := TDextJson.Deserialize<TCreateUserRequest>(Ctx.Request.Body);
```

---

[← Web Framework](README.md) | [Next: Controllers →](controllers.md)
