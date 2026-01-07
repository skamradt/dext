# Solução de Problemas

Encontre soluções para os erros mais comuns ao trabalhar com o Dext.

## Erros de Instalação

### Unit 'Dext.Web' não encontrada
- **Solução**: Verifique se o diretório `Sources` (e seus subdiretórios) estão no **Library Path** da IDE. Recomendamos usar a variável de ambiente `$(DEXT)`.

### Erro ao carregar pacotes (BPL)
- **Solução**: Certifique-se de que a pasta `Output` onde as BPLs são geradas está no seu **System PATH**.

## Erros de ORM

### "Entity TUser not found in context"
- **Solução**: Verifique se você chamou `Entities<TUser>` no seu `DbContext` ou registrou o mapeamento se estiver usando classes POCO.

### Campos retornando Nulo inesperadamente
- **Solução**: Verifique se os nomes das propriedades ou os atributos `[Column('nome')]` correspondem exatamente ao que está no banco (cuidado com Case-Sensitivity em bancos como PostgreSQL).

## Erros HTTP / Web

### Erro 404 ao chamar endpoint
- **Solução**: Verifique a ordem de registro das rotas. Rotas com parâmetros (ex: `/:id`) podem "engolir" rotas estáticas se registradas inadequadamente.

### Erro de CORS no Browser
- **Solução**: Adicione `App.UseCors` no seu pipeline **antes** de qualquer endpoint. Certifique-se de configurar as origens permitidas em produção.

## Debugging

O Dext possui logs internos que podem ajudar:

```pascal
// Habilitar logs detalhados do HTTP
App.UseHttpLogging;

// Ver as queries SQL geradas pelo ORM
Ctx.Logger := TConsoleLogger.Create;
```

---

[← Dialetos](dialetos.md) | [Índice do Livro →](../README.md)
