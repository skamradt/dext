unit DextFood.Domain;

interface

uses
  System.SysUtils,
  Dext,        // For StringType, CurrencyType, DateTimeType
  Dext.Entity; // Facade para ORM (Table, PK, AutoInc)

type
  /// <summary>
  /// Representa os possíveis estados de um pedido no DextFood.
  /// </summary>
  TOrderStatus = (Pending, Preparing, Delivering, Completed);

  /// <summary>
  /// Entidade de domínio representando um pedido.
  /// Mapeamento automático via Naming Strategy (SnakeCase).
  /// </summary>
  [Table]
  TOrder = class
  private
    FId: Integer;
    FStatus: TOrderStatus;
    FTotal: CurrencyType;
    FCreatedAt: DateTimeType;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    // Propriedades comuns são mapeadas automaticamente se o framework encontrar [Table] na classe.
    // Usar Smart Types (Prop<T>) permite queries type-safe.
    property Status: TOrderStatus read FStatus write FStatus;
    property Total: CurrencyType read FTotal write FTotal;
    property CreatedAt: DateTimeType read FCreatedAt write FCreatedAt;
  end;

implementation

end.
