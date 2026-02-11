# Dext Minimal API (Quick Reference)

Lightweight, lambda-based HTTP endpoints with automatic Dependency Injection and Model Binding.

> 📚 **Full Documentation**: See [Book: Minimal APIs](Book/02-web-framework/minimal-apis.md)

## Introduction

Modern endpoints with type-safe parameters:

```pascal
Builder.MapGet<IResult>('/health', lambda...);
```

## Basic Examples

### Simple Return (IResult)

```pascal
Builder.MapGet<IResult>('/hello',
  function: IResult
  begin
    Result := Results.Ok('Hello World');
  end);
```

### Route Parameters (Automatic Binding)

```pascal
// GET /users/123
Builder.MapGet<Integer, IResult>('/users/{id}',
  function(Id: Integer): IResult
  begin
    Result := Results.Ok(Id);
  end);
```

### Dependency Injection (Automatic Resolution)

```pascal
// GET /users (Injects IUserService)
Builder.MapGet<IUserService, IResult>('/users',
  function(Svc: IUserService): IResult
  begin
    Result := Results.Ok(Svc.GetAll);
  end);
```

### POST Body (DTO binding)

```pascal
// POST /auth
Builder.MapPost<TLoginDto, IAuthService, IResult>('/auth',
  function(Dto: TLoginDto; Auth: IAuthService): IResult
  begin
    var Token := Auth.Login(Dto.Username, Dto.Password);
    Result := Results.Ok(Token);
  end);
```

### Using Route + Body + Services

```pascal
// PUT /users/{id}
Builder.MapPut<Integer, TUpdateUserDto, IUserService, IResult>('/users/{id}',
  function(Id: Integer; Dto: TUpdateUserDto; Svc: IUserService): IResult
  begin
    Svc.Update(Id, Dto);
    Result := Results.NoContent;
  end);
```

## Binding Attributes

Use attributes heavily to clarify sources:

```pascal
type
  TSearchRequest = record
    [FromQuery('q')]
    Term: string;

    [FromHeader('X-Wait')]
    Wait: Integer;
  end;

Builder.MapGet<TSearchRequest, IResult>('/search', ...);
```

## Swagger Metadata

Chain methods for documentation:

```pascal
Builder.MapGet<IResult>('/ping', ...)
  .WithTags('Monitoring')
  .WithSummary('Check liveness')
  .WithDescription('Returns 200 OK if service is alive');
```
