unit ComplexQuerying.DbContext;

interface

uses
  System.SysUtils,
  Dext.Entity,
  Dext.Entity.Core,
  ComplexQuerying.Entities;

type
  /// <summary>
  ///   Database context for complex querying demonstrations
  /// </summary>
  TQueryDbContext = class(TDbContext)
  private
    function GetCustomers: IDbSet<TCustomer>;
    function GetOrders: IDbSet<TOrder>;
    function GetProducts: IDbSet<TProduct>;
  public
    property Customers: IDbSet<TCustomer> read GetCustomers;
    property Orders: IDbSet<TOrder> read GetOrders;
    property Products: IDbSet<TProduct> read GetProducts;
  end;

implementation

{ TQueryDbContext }

function TQueryDbContext.GetCustomers: IDbSet<TCustomer>;
begin
  Result := Entities<TCustomer>;
end;

function TQueryDbContext.GetOrders: IDbSet<TOrder>;
begin
  Result := Entities<TOrder>;
end;

function TQueryDbContext.GetProducts: IDbSet<TProduct>;
begin
  Result := Entities<TProduct>;
end;

end.
