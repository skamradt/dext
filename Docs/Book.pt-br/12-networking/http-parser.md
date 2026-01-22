# Parser de Arquivos HTTP

O módulo `Dext.Http` fornece um parser para arquivos `.http`, um formato popular usado pelas extensões REST Client do VS Code e HTTP Client do IntelliJ. Isso permite definir e testar requisições de API em um formato simples e legível.

## Instalação

Adicione as seguintes units ao seu projeto:

```pascal
uses
  Dext.Http.Request,
  Dext.Http.Parser,
  Dext.Http.Executor;
```

## O Formato .http

O formato `.http` é uma maneira simples baseada em texto para definir requisições HTTP:

```http
### Variáveis
@baseUrl = https://api.example.com
@token = meu-token-secreto

### Buscar todos os usuários
GET {{baseUrl}}/users
Authorization: Bearer {{token}}
Accept: application/json

### Criar usuário
POST {{baseUrl}}/users
Content-Type: application/json

{
  "name": "João Silva",
  "email": "joao@exemplo.com"
}
```

### Elementos de Sintaxe

| Elemento | Descrição | Exemplo |
|----------|-----------|---------|
| `@nome = valor` | Definição de variável | `@baseUrl = https://api.com` |
| `{{nomeVar}}` | Interpolação de variável | `{{baseUrl}}/users` |
| `{{env:NOME}}` | Variável de ambiente | `{{env:API_TOKEN}}` |
| `### Comentário` | Separador de request com nome | `### Buscar todos os usuários` |
| `MÉTODO URL` | Linha de requisição | `GET https://api.com/users` |
| `Header: Valor` | Cabeçalho da requisição | `Authorization: Bearer token` |
| Linha em branco | Separa headers do body | |
| JSON/Texto | Corpo da requisição | `{"name": "João"}` |

## Parseando Arquivos .http

Use `THttpRequestParser` para parsear o conteúdo de arquivos `.http`:

```pascal
var
  Collection: THttpRequestCollection;
begin
  // Parsear de arquivo
  Collection := THttpRequestParser.ParseFile('api-tests.http');
  try
    WriteLn('Variáveis: ', Collection.Variables.Count);
    WriteLn('Requests: ', Collection.Requests.Count);
    
    // Listar todos os requests
    for var Request in Collection.Requests do
      WriteLn('  - [', Request.Method, '] ', Request.Name);
  finally
    Collection.Free;
  end;
end;
```

### Parseando de String

Você também pode parsear diretamente de uma string:

```pascal
const
  HttpContent = 
    '@baseUrl = https://api.example.com' + sLineBreak +
    '### Buscar usuários' + sLineBreak +
    'GET {{baseUrl}}/users';

var
  Collection: THttpRequestCollection;
begin
  Collection := THttpRequestParser.Parse(HttpContent);
  // ...
end;
```

## Executando Requests

Use `THttpExecutor` para executar requests parseados usando `TRestClient`:

```pascal
var
  Collection: THttpRequestCollection;
  Request: THttpRequestInfo;
  Result: THttpExecutionResult;
begin
  Collection := THttpRequestParser.ParseFile('api-tests.http');
  try
    // Encontrar request por nome
    Request := Collection.FindByName('Buscar todos os usuários');
    
    if Assigned(Request) then
    begin
      // Executar com interpolação de variáveis
      Result := THttpExecutor.ExecuteSync(Request, Collection.Variables);
      
      WriteLn('Status: ', Result.StatusCode, ' ', Result.StatusText);
      WriteLn('Duração: ', Result.DurationMs, 'ms');
      WriteLn('Body: ', Result.ResponseBody);
    end;
  finally
    Collection.Free;
  end;
end;
```

### Execução Assíncrona

Para execução não-bloqueante:

```pascal
THttpExecutor.ExecuteAsync(Request)
  .OnComplete(
    procedure(Response: IRestResponse)
    begin
      WriteLn('Status: ', Response.StatusCode);
    end)
  .Start;
```

## Resolução de Variáveis

Variáveis são automaticamente resolvidas ao executar requests:

1. **Variáveis Estáticas**: Definidas com `@nome = valor`
2. **Variáveis de Ambiente**: Definidas com `@nome = {{env:NOME_VAR}}`
3. **Ambiente Inline**: Usadas diretamente como `{{env:NOME_VAR}}`

```pascal
// Resolução manual de variáveis
THttpRequestParser.ResolveRequest(Request, Collection.Variables);

// Agora Request.Url, Request.Headers e Request.Body têm valores interpolados
WriteLn('URL Resolvida: ', Request.Url);
```

## Modelo de Request

A classe `THttpRequestInfo` contém todos os dados do request parseado:

| Propriedade | Tipo | Descrição |
|-------------|------|-----------|
| `Name` | `string` | Nome do request (do `### Comentário`) |
| `Method` | `string` | Método HTTP (GET, POST, etc.) |
| `Url` | `string` | URL do request (pode conter variáveis) |
| `Headers` | `TDictionary<string, string>` | Cabeçalhos do request |
| `Body` | `string` | Corpo do request (JSON, XML, etc.) |
| `LineNumber` | `Integer` | Número da linha no arquivo fonte |

## Casos de Uso

### Testes de API
Crie arquivos `.http` para documentar e testar suas APIs sem ferramentas externas.

### Testes de Integração
Parseie e execute requests em testes automatizados para validar o comportamento da API.

### Documentação de API
Use arquivos `.http` como documentação executável para suas APIs.

### Ferramentas de Desenvolvimento
Construa ferramentas customizadas que leem e executam requisições HTTP.

## Arquivo de Exemplo

Veja `Examples/example-api.http` para um exemplo completo usando a API JSONPlaceholder.
