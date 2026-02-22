# Stored Procedures e Funções

O Dext fornece uma maneira declarativa de mapear e executar Stored Procedures e Funções de banco de dados usando atributos.

## Mapeando um Procedimento

Crie uma classe (ou use seu DbContext) com um método decorado com o atributo `[StoredProcedure]`. Use `[DbParam]` para definir as direções e tipos dos parâmetros.

```pascal
type
  TMyProcs = class
  public
    [StoredProcedure('sp_GetCustomerBalance')]
    procedure GetBalance(
      [DbParam(pdInput)] CustomerId: Integer;
      [DbParam(pdOutput)] out Balance: Currency
    );
  end;
```

## Execução

Você pode resolver a classe de procedimentos a partir do container de injeção de dependência ou instanciá-la manualmente se fornecer uma conexão.

```pascal
var Procs := ServiceProvider.GetService<TMyProcs>;
var MyBalance: Currency;
Procs.GetBalance(123, MyBalance);
```

## Conjuntos de Resultados (Funções)

Para funções armazenadas ou procedimentos que retornam um conjunto de resultados, você pode mapear o valor de retorno para uma lista de objetos ou um tipo primitivo.

```pascal
type
  TMyProcs = class
  public
    [StoredProcedure('fn_GetActiveCustomers')]
    function GetActiveCustomers(
      [DbParam(pdInput)] RegionId: Integer
    ): IList<TCustomer>;
  end;
```

## Retornando Múltiplos Conjuntos de Resultados

O Dext pode lidar com procedimentos que retornam múltiplos conjuntos de resultados usando a interface `IMultipleResults`.

```pascal
var Results := Db.ExecuteProcedure('sp_GetInvoicesAndItems', [CustomerId]);
var Invoices := Results.Read<TInvoice>();
var Items := Results.Read<TInvoiceItem>();
```

## Lidando com parâmetros OUT com [DbParam]

O atributo `[DbParam]` permite especificar detalhes sobre o parâmetro do banco de dados:

*   **Direction**: `pdInput`, `pdOutput`, `pdInputOutput`, `pdReturnValue`.
*   **Name**: Se o nome do parâmetro no banco de dados for diferente do nome do parâmetro no Delphi.
*   **DataType**: `ftDate`, `ftTimeStamp`, etc., de forma explícita.

```pascal
procedure Process(
  [DbParam(pdInput, 'p_input_val')] Val: string;
  [DbParam(pdOutput, 'p_result_code')] out Code: Integer
);
```

## Especificidades dos Dialetos

O Dext lida com as diferenças na invocação de procedimentos entre os bancos de dados automaticamente:
*   **SQL Server**: `EXEC sp_name @p1, @p2`
*   **PostgreSQL**: `SELECT * FROM func_name(:p1, :p2)` ou `CALL proc_name(...)`
*   **Firebird**: `EXECUTE PROCEDURE name ...`
