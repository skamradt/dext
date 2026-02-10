unit Sales.Data.Context;

interface

uses
  Dext.Entity,
  Dext.Entity.Core,
  Sales.Domain.Entities;

type
  { Application DbContext }
  TSalesDbContext = class(TDbContext)
  private
    function GetCustomers: IDbSet<TCustomer>;
    function GetProducts: IDbSet<TProduct>;
    function GetOrders: IDbSet<TOrder>;
    function GetOrderItems: IDbSet<TOrderItem>;
  public
    // DbSets for Entities as Properties (Modern Pattern)
    property Customers: IDbSet<TCustomer> read GetCustomers;
    property Products: IDbSet<TProduct> read GetProducts;
    property Orders: IDbSet<TOrder> read GetOrders;
    property OrderItems: IDbSet<TOrderItem> read GetOrderItems;
    
    procedure OnConfiguring(Options: TDbContextOptions); override;
  end;

implementation

{ TSalesDbContext }

procedure TSalesDbContext.OnConfiguring(Options: TDbContextOptions);
begin
  inherited;
  Options.UseSnakeCaseNamingConvention; 
end;

function TSalesDbContext.GetCustomers: IDbSet<TCustomer>;
begin
  Result := Entities<TCustomer>;
end;

function TSalesDbContext.GetProducts: IDbSet<TProduct>;
begin
  Result := Entities<TProduct>;
end;

function TSalesDbContext.GetOrders: IDbSet<TOrder>;
begin
  Result := Entities<TOrder>;
end;

function TSalesDbContext.GetOrderItems: IDbSet<TOrderItem>;
begin
  Result := Entities<TOrderItem>;
end;

end.
