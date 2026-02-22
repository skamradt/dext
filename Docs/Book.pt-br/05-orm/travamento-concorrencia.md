# Concorrência e Travamento (Locking)

O Dext suporta estratégias de travamento Otimista e Pessimista para garantir a integridade dos dados em ambientes multi-usuário.

## Concorrência Otimista

A concorrência otimista assume que conflitos são raros. Ela usa uma coluna de versão para detectar se um registro foi modificado por outro processo desde que foi carregado.

### Uso
Adicione o atributo `[Version]` a uma propriedade inteira na sua entidade:

```pascal
type
  [Table('Products')]
  TProduct = class
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    [Version]
    property Version: Integer read FVersion write FVersion;
  end;
```

Quando você chama `SaveChanges`, o Dext verifica automaticamente se a `Version` no banco de dados corresponde àquela na memória. Se não corresponder, uma exceção `DbUpdateConcurrencyException` é lançada.

## Travamento Pessimista

O travamento pessimista é usado quando conflitos são esperados. Ele trava o registro no nível do banco de dados quando ele é lido, impedindo que outros o modifiquem até que sua transação seja concluída.

### Uso
Use o método `Lock` na sua consulta fluente:

```pascal
var Product := Db.Products
  .Where(Prop('Id') = 1)
  .Lock(TLockMode.Update) // FOR UPDATE (PostgreSQL/Oracle) ou UPDLOCK (SQL Server)
  .FirstOrDefault;

try
  Product.Price := Product.Price * 1.1;
  Db.SaveChanges;
finally
  // O lock é liberado automaticamente quando a transação termina
end;
```

### Modos de Travamento Suportados

| Modo | Equivalente SQL | Descrição |
|------|----------------|-----------|
| `TLockMode.None` | (Nenhum) | Comportamento padrão. |
| `TLockMode.Update` | `FOR UPDATE` / `UPDLOCK` | Solicita um lock exclusivo para atualização. |
| `TLockMode.Shared` | `FOR SHARE` / `HOLDLOCK` | Solicita um lock compartilhado (permite que outros leiam, mas não atualizem). |

## Travamento Offline

Para tarefas de longa duração onde uma transação de banco de dados não pode ser mantida aberta (ex: um usuário editando um formulário por 10 minutos), o Dext fornece um mecanismo de **Travamento Offline**.

### Uso
Você pode solicitar um token para uma entidade e tenant específicos:

```pascal
var Token := Db.LockManager.AcquireLock('TProduct', ProductId, TenantId);
if Token <> '' then
begin
  // O registro está travado para este usuário
end;
```

Para liberar o lock:
```pascal
Db.LockManager.ReleaseLock(Token);
```

> **Nota**: Os locks offline são tipicamente armazenados em uma tabela dedicada `DextLocks` e possuem um tempo de expiração.
