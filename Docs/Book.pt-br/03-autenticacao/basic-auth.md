# Autenticação Basic

Um esquema de autenticação simples embutido no protocolo HTTP.

## Visão Geral

A Autenticação Basic é a maneira mais simples de proteger sua API. O cliente envia um cabeçalho `Authorization: Basic <credenciais>`, onde as credenciais são uma string em Base64 de `usuario:senha`.

O Dext fornece um middleware integrado para lidar com Basic Auth com uma função de validação simples.

## Configuração

No seu `Startup` ou configuração da aplicação:

```pascal
App.Builder.UseBasicAuthentication(
  'Meu Realm da API',
  function(const Username, Password: string): Boolean
  begin
    // Validação simples fixa
    Result := (Username = 'admin') and (Password = 'secret');
    
    // Ou valide contra um serviço/banco de dados
    // Result := UserServiceProvider.Validate(Username, Password);
  end);
```

## Como Funciona

1. Quando um cliente solicita um recurso protegido sem credenciais, o Dext retorna uma resposta `401 Unauthorized` com um cabeçalho `WWW-Authenticate`.
2. O navegador (ou cliente) solicita as credenciais.
3. As requisições subsequentes incluem o cabeçalho `Authorization`.
4. Se o validador retornar `True`, a requisição prossegue e o `Ctx.User` é populado com uma identidade "Basic".

## Protegendo Endpoints

Assim como no JWT, você pode usar `.RequireAuthorization` para proteger endpoints individuais ou controllers.

```pascal
App.MapGet('/api/privado', procedure(Ctx: IHttpContext)
  begin
    Ctx.Response.Write('Você acessou uma área privada!');
  end)
  .RequireAuthorization;
```

## Quando usar Basic Auth?

- **Ferramentas Internas/Desenvolvimento**: Rápido de configurar para dashboards internos.
- **Comunicação entre Serviços**: Simples de implementar em scripts ou entre microserviços.
- **Sistemas Legados**: Amplamente suportado por quase todos os clientes HTTP.

> ⚠️ **Nota de Segurança**: Use sempre Autenticação Basic sobre **HTTPS**. Como as credenciais são codificadas em Base64 (não criptografadas), elas podem ser facilmente interceptadas em HTTP comum.

---

[← Autenticação](README.md) | [Próximo: Autenticação JWT →](jwt-auth.md)
