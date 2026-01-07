# Autenticação JWT

Proteja sua API com JSON Web Tokens.

> 📦 **Exemplo**: [Web.JwtAuthDemo](../../../Examples/Web.JwtAuthDemo/)

## Visão Geral

Fluxo de autenticação JWT:
1. Usuário envia credenciais para `/login`
2. Servidor valida e retorna um token JWT
3. Cliente inclui token no header `Authorization: Bearer <token>`
4. Servidor valida token em endpoints protegidos

## Configuração

### 1. Configurar Autenticação

```pascal
TWebHostBuilder.CreateDefault(nil)
  .ConfigureServices(procedure(Services: IServiceCollection)
    begin
      Services.AddAuthentication(procedure(Options: TAuthenticationOptions)
        begin
          Options.SecretKey := 'sua-chave-secreta-deve-ter-pelo-menos-32-caracteres';
          Options.Issuer := 'seu-app';
          Options.Audience := 'sua-api';
          Options.ExpirationMinutes := 60;
        end);
    end)
  .Configure(procedure(App: IApplicationBuilder)
    begin
      App.UseAuthentication;
      // ... endpoints
    end)
  .Build
  .Run;
```

### 2. Criar Endpoint de Login

```pascal
type
  TLoginRequest = record
    Username: string;
    Password: string;
  end;

App.MapPost('/login', procedure(Ctx: IHttpContext)
  var
    Request: TLoginRequest;
    Token: string;
    Claims: TArray<TClaim>;
  begin
    Request := Ctx.Request.BindBody<TLoginRequest>;
    
    // Validar credenciais (substitua por validação real)
    if (Request.Username <> 'admin') or (Request.Password <> 'segredo') then
    begin
      Results.Unauthorized.Execute(Ctx);
      Exit;
    end;
    
    // Construir claims
    Claims := TClaimsBuilder.Create
      .AddSub('user-123')
      .AddName('Usuário Admin')
      .AddEmail('admin@exemplo.com')
      .AddRole('admin')
      .AddClaim('departamento', 'TI')
      .Build;
    
    // Gerar token
    Token := TJwtHelper.GenerateToken(
      'sua-chave-secreta-deve-ter-pelo-menos-32-caracteres',
      Claims,
      60  // expiração em minutos
    );
    
    Ctx.Response.Json('{"token": "' + Token + '"}');
  end);
```

### 3. Proteger Endpoints

```pascal
// Requer autenticação
App.MapGet('/protegido', procedure(Ctx: IHttpContext)
  begin
    var UserId := Ctx.User.FindFirst('sub');
    var UserName := Ctx.User.FindFirst('name');
    
    Ctx.Response.Json(Format(
      '{"mensagem": "Olá %s!", "userId": "%s"}',
      [UserName, UserId]
    ));
  end)
  .RequireAuthorization;

// Requer role específica
App.MapGet('/admin', procedure(Ctx: IHttpContext)
  begin
    Ctx.Response.Json('{"mensagem": "Área administrativa"}');
  end)
  .RequireRole('admin');
```

## Acessando Claims

Em endpoints protegidos, acesse claims via `Ctx.User`:

```pascal
// Obter claim única
var UserId := Ctx.User.FindFirst('sub');
var Email := Ctx.User.FindFirst('email');

// Verificar role
if Ctx.User.IsInRole('admin') then
  // Lógica de admin
```

---

[← Autenticação Basic](basic-auth.md) | [Próximo: Claims Builder →](claims-builder.md)
