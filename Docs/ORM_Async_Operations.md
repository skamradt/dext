# Operações Assíncronas no Dext ORM

O Dext ORM suporta operações assíncronas para evitar o bloqueio da thread principal (UI) durante operações de banco de dados demoradas. Esta funcionalidade é baseada na `Dext.Threading.Async` API.

## 🚀 Métodos Disponíveis

Atualmente, o Dext suporta as seguintes operações assíncronas:

*   **`ToListAsync`**: Disponível em `IDbSet<T>` e no `TFluentQuery<T>`. Materializa o resultado da consulta em uma lista de forma assíncrona.
*   **`SaveChangesAsync`**: Disponível em `IDbContext`. Persiste todas as alterações rastreadas no banco de dados em background.

## ⚠️ Requisito de Thread Safety: Connection Pooling

Para garantir a segurança de threads (Thread Safety), o Dext ORM exige que o **Connection Pooling** esteja habilitado para todas as operações assíncronas.

Se você tentar chamar um método `Async` em um Contexto que usa uma conexão não-poolizada, o framework lançará uma exceção:
`"SaveChangesAsync requires a pooled connection to ensure thread safety."`

### Como habilitar o Pooling:

Ao configurar seu `DbContext`, certifique-se de habilitar o pooling:

```pascal
procedure TMyContext.OnConfiguring(Options: TDbContextOptions);
begin
  Options.UseFireDAC('MyConnectionDef')
         .WithPooling(True); // OBRIGATÓRIO para Async e para projetos Web
end;
```

## 📖 Exemplos de Uso

### 1. Consultas Assíncronas (ToListAsync)

O método `ToListAsync` retorna um `TAsyncBuilder<IList<T>>`, permitindo configurar callbacks fluentes.

```pascal
Context.Entities<TUser>
  .Where(u => u.Active)
  .OrderBy(u => u.Name)
  .ToListAsync
  .OnComplete(
    procedure(Users: IList<TUser>)
    begin
      // Este código roda na UI THREAD
      UserGrid.DataSource := Users;
    end)
  .OnException(
    procedure(E: Exception)
    begin
      ShowMessage('Erro ao carregar usuários: ' + E.Message);
    end)
  .Start; // Não esqueça de chamar .Start()
```

### 2. Persistência Assíncrona (SaveChangesAsync)

Útil para salvar grandes volumes de dados ou evitar pequenos travamentos na interface durante o `Commit`.

```pascal
var
  NewOrder: TOrder;
begin
  NewOrder := TOrder.Create;
  NewOrder.Date := Now;
  Context.Entities<TOrder>.Add(NewOrder);

  Context.SaveChangesAsync
    .OnComplete(
      procedure(AffectedRows: Integer)
      begin
        Log('Pedido salvo. Linhas afetadas: ' + AffectedRows.ToString);
      end)
    .Start;
end;
```

## 💡 Considerações de Performance

Embora o asincronismo melhore a responsividade da UI, ele introduz um pequeno overhead de gerenciamento de threads. 

**Quando usar:**
*   Operações que levam mais de 50-100ms.
*   Consultas complexas com múltiplos `Join` ou `Include`.
*   Aplicações Web (onde o throughput é mais importante que a latência de uma única thread).

**Referência Recomendada:**
Para entender profundamente os conceitos de multithreading no Delphi e como o Dext gerencia essas tarefas, recomendamos a leitura do livro [Delphi Multithreading](https://www.cesarromero.com.br/#livros).

---
*Assinado: Antigravity AI*
*Data: 16 de Fevereiro de 2026*
