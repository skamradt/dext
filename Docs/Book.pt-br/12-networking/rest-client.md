# Cliente REST

O módulo `Dext.Net` fornece um cliente HTTP moderno, fluente e de alto desempenho para Delphi. Ele é construído sobre o `THttpClient` nativo, mas adiciona pool de conexões robusto, serialização automática e uma API amigável inspirada em frameworks modernos como Refit e RestSharp.

## Instalação

O Cliente REST faz parte do pacote `Dext.Net`. Certifique-se de que seu projeto referencie as seguintes units:

```pascal
uses
  Dext.Net.RestClient,
  Dext.Net.RestRequest,
  Dext.Threading.Async;
```

## Uso Básico

O record `TRestClient` fornece uma interface fluente para fazer requisições HTTP. Use `Create` para inicializá-lo com uma URL base e, em seguida, encadeie métodos para construir e executar a requisição.

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
> `TRestClient` é um record leve. Ele usa um pool de conexões compartilhado e thread-safe internamente, então você pode criar instâncias de forma econômica sem se preocupar com exaustão de sockets.

## Fazendo Requisições

### Build de Requisições

A API fluente suporta todos os verbos HTTP padrão:

```pascal
Client.Get('/resource');
Client.Post('/resource');
Client.Put('/resource');
Client.Delete('/resource');
Client.Patch('/resource');
```

### Adicionando Headers e Query Parameters

Você pode adicionar headers e parâmetros de consulta facilmente usando o padrão builder:

```pascal
Client.Get('/search')
  .Header('Authorization', 'Bearer my-token')
  .Header('X-Custom-Header', 'Value')
  .QueryParam('q', 'delphi')
  .QueryParam('page', '1')
  .Start;
```

### Corpo da Requisição (Body)

Para requisições `POST` e `PUT`, você pode fornecer um corpo:

**Corpo JSON (Serialização Automática)**
```pascal
var
  LUser: TUser;
begin
  LUser := TUser.Create('John Doe');
  try
    Client.Post('/users')
      .Body(LUser) // Automaticamente serializado para JSON
      .Start;
  finally
    LUser.Free;
  end;
end;
```

**Corpo String (Raw)**
```pascal
Client.Post('/data')
  .JsonBody('{"name": "test"}')
  .Start;
```

**Corpo Stream**
```pascal
Client.Post('/upload')
  .Body(LFileStream)
  .Start;
```

## Manipulando Respostas

Você pode manipular respostas como strings brutas, streams ou objetos tipados.

### Respostas Tipadas (Deserialização)

Use `Get<T>`, `Post<T>`, etc., ou `.Execute<T>` para deserializar automaticamente a resposta JSON em um objeto ou record Delphi.

```pascal
  .Get<TUser>('/users/1') // Retorna TAsyncBuilder<TUser>
  .OnComplete(
    procedure(User: TUser)
    begin
      Writeln('User: ', User.Name);
    end)
  .Start;
```

## Assincronismo e Cancelamento

O cliente é totalmente integrado com `Dext.Threading.Async`.

### Execução Síncrona

Se você precisar bloquear a thread atual e esperar pelo resultado (ex: em uma aplicação console ou worker em background), use `.Await`.

```pascal
var Res := Client.Get<TUser>('/users/1')
  .Await; // Bloqueia a thread até completar
```

### Encadeamento de Operações

```pascal
Client.Get<TToken>('/auth/token')
  .ThenBy<TUser>(
    function(Token: TToken): TUser
    begin
      // Usa o token para obter o perfil do usuário
      Result := Client.Get('/profile')
        .Header('Authorization', Token.AccessToken)
        .Execute<TUser> // Ou use Get<TUser> diretamente se não precisar de headers
        .Await; // Espera síncrona dentro da task async
    end)
  .OnComplete(procedure(User: TUser) ... )
  .Start;
```

### Cancelamento

Passe um `ICancellationToken` para cancelar requisições de longa duração.

```pascal
var
  LTokenSource: ICancellationTokenSource;
begin
  LTokenSource := TCancellationTokenSource.Create;
  
  Client.Get('/long-process')
    .Cancellation(LTokenSource.Token)
    .Start;
    
  // ... depois ...
  LTokenSource.Cancel;
end;
```

## Connection Pooling

O cliente `Dext.Net` usa um pool de conexões de alto desempenho e thread-safe (`TConnectionPool`). 

- **Reutilização Eficiente**: Instâncias nativas de `THttpClient` são reutilizadas, evitando o overhead de criar novas conexões TCP e handshakes SSL para cada requisição.
- **Thread Safety**: O pool é totalmente thread-safe. Você pode compartilhar instâncias de `TRestClient` ou criar novas em múltiplas threads sem bloqueios manuais.
- **Limpeza Automática**: Conexões obsoletas são gerenciadas automaticamente.

## Autenticação

O cliente suporta provedores de autenticação plugáveis via `IAuthenticationProvider`.

```pascal
// Bearer Token
Client.Authenticator(TBearerAuthProvider.Create('my-jwt-token'));

// Basic Auth
Client.Authenticator(TBasicAuthProvider.Create('user', 'password'));

// API Key
Client.Authenticator(TApiKeyAuthProvider.Create('X-API-Key', '12345'));
```

## Thread Safety

O design do `TRestClient` garante segurança em ambientes multithread:

1.  **Configuração Imutável**: Quando você chama `Execute`, o cliente cria um snapshot da configuração atual (headers, auth, timeout).
2.  **Execução Isolada**: A requisição HTTP real roda em uma task em segundo plano com sua própria instância de `THttpClient` adquirida do pool.
3.  **Sem Estado Compartilhado**: Modificar o cliente *após* chamar `Start` não afeta a requisição que já está rodando.
