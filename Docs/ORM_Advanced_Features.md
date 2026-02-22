# Dext ORM: Funcionalidades Avançadas (2026)

Este documento descreve o uso das novas funcionalidades de Stored Procedures e Pessimistic Locking (DB & Offline).

## 1. Stored Procedures

O Dext ORM permite mapear Stored Procedures para DTOs (Data Transfer Objects) de forma declarativa, suportando parâmetros de entrada, saída e retorno.

### Mapeamento Declarativo

```pascal
[Procedure('SP_CALCULATE_BONUS')]
TBonusDto = class
private
  FEmpId: Integer;
  FBaseAmount: Currency;
  FBonusValue: Currency; // Parâmetro de Saída
  FResult: Integer;      // Resultado da Procedure
public
  [DbParam(ptInput)]
  property EmpId: Integer read FEmpId write FEmpId;
  
  [DbParam(ptInput)]
  property BaseAmount: Currency read FBaseAmount write FBaseAmount;
  
  [DbParam(ptOutput)]
  property BonusValue: Currency read FBonusValue write FBonusValue;
  
  [DbParam(ptResult)]
  property Result: Integer read FResult write FResult;
end;
```

### Execução

```pascal
var
  Bonus: TBonusDto;
begin
  Bonus := TBonusDto.Create;
  try
    Bonus.EmpId := 10;
    Bonus.BaseAmount := 5000;
    
    // Executa, gera o SQL correto (EXEC, CALL ou BEGIN/END) e mapeia os resultados de volta
    db.ExecuteProcedure<TBonusDto>(Bonus);
    
    WriteLn('Bonus calculado: ', Bonus.BonusValue);
  finally
    Bonus.Free;
  end;
end;
```

---

## 2. Pessimistic Locking (DB-Level)

Utilizado para bloquear registros durante uma transação ativa no banco de dados.

### Uso na Fluent API

```pascal
// Bloqueio Exclusivo (SELECT FOR UPDATE / WITH (UPDLOCK))
var
  Order: TOrder;
begin
  db.BeginTransaction;
  try
    Order := db.Entities<TOrder>
      .Where(o.Id = 100)
      .WithLock(lmExclusive)
      .FirstOrDefault;
      
    Order.Status := 'Processing';
    db.SaveChanges;
    db.Commit;
  except
    db.Rollback;
    raise;
  end;
end;
```

**Modos suportados:**
- `lmNone`: Sem bloqueio.
- `lmShared`: Bloqueio de leitura (outros podem ler, mas não alterar).
- `lmExclusive`: Bloqueio de escrita (ninguém mais lê com lock ou altera).
- `lmExclusiveNoWait`: Retorna erro imediato se o registro já estiver travado.

---

## 3. Offline Locking (Application-Level)

Ideal para aplicações Web/Stateless onde o bloqueio deve persistir entre diferentes requisições HTTP enquanto o usuário edita um registro.

### Configuração da Entidade

```pascal
TProduct = class
public
  [PrimaryKey]
  property Id: Integer ...
  
  [LockToken]
  property LockedBy: string ...
  
  [LockExpiration]
  property LockedUntil: TDateTime ...
end;
```

### Operações de Lock

O `TryLock` realiza um `UPDATE` atômico no banco. Ele só terá sucesso se o registro não estiver travado ou se o lock anterior já tiver expirado.

```pascal
if db.Entities<TProduct>.TryLock(Product, 'CesarUser', 30) then
begin
  // Sucesso! O usuário 'CesarUser' tem 30 minutos de exclusividade.
  // O objeto 'Product' é atualizado localmente com o token e expiração.
end
else
begin
  WriteLn('Este registro está sendo editado por outra pessoa.');
end;

// Para liberar o lock manualmente:
db.Entities<TProduct>.Unlock(Product);
```

### Validação Automática

O `TryLock` garante que concorrentes não sobrescrevam o token indevidamente através de uma cláusula `WHERE` protegida:
`WHERE ID = :id AND (LockedBy IS NULL OR LockedUntil < :agora)`
