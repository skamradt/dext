unit Customer.Context;

interface

uses
  FireDAC.Comp.UI,
  FireDAC.Stan.Intf,
  FireDAC.UI.Intf,
  FireDAC.VCLUI.Wait,
  Customer.Entity,
  Dext.Entity,
  Dext.Entity.Core;

type
  TCustomerContext = class(TDbContext)
  private
    function GetCustomers: IDbSet<TCustomer>;
  public
    property Customers: IDbSet<TCustomer> read GetCustomers;
  end;

implementation

{ TCustomerContext }

function TCustomerContext.GetCustomers: IDbSet<TCustomer>;
begin
  Result := Entities<TCustomer>;
end;

end.
