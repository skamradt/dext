# Stored Procedures & Functions

Dext provides a declarative way to map and execute database Stored Procedures and Functions using attributes.

## Mapping a Procedure

Create a class (or use your DbContext) with a method decorated with the `[StoredProcedure]` attribute. Use `[DbParam]` to define parameter directions and types.

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

## Execution

You can resolve the procedure class from the dependency injection container or instantiate it manually if you provide a connection.

```pascal
var Procs := ServiceProvider.GetService<TMyProcs>;
var MyBalance: Currency;
Procs.GetBalance(123, MyBalance);
```

## Result Sets (Functions)

For stored functions or procedures that return a result set, you can map the return value to a list of objects or a primitive.

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

## Returning Multiple Result Sets

Dext can handle procedures that return multiple result sets using the `IMultipleResults` interface.

```pascal
var Results := Db.ExecuteProcedure('sp_GetInvoicesAndItems', [CustomerId]);
var Invoices := Results.Read<TInvoice>();
var Items := Results.Read<TInvoiceItem>();
```

## Handling OUT Parameters with [DbParam]

The `[DbParam]` attribute allows you to specify details about the database parameter:

*   **Direction**: `pdInput`, `pdOutput`, `pdInputOutput`, `pdReturnValue`.
*   **Name**: If the database parameter name differs from the Delphi parameter name.
*   **DataType**: Explicit `ftDate`, `ftTimeStamp`, etc.

```pascal
procedure Process(
  [DbParam(pdInput, 'p_input_val')] Val: string;
  [DbParam(pdOutput, 'p_result_code')] out Code: Integer
);
```

## Dialect Specifics

Dext handles the differences in procedure invocation across databases automatically:
*   **SQL Server**: `EXEC sp_name @p1, @p2`
*   **PostgreSQL**: `SELECT * FROM func_name(:p1, :p2)` or `CALL proc_name(...)`
*   **Firebird**: `EXECUTE PROCEDURE name ...`
