unit MultiTenancy.DbContext;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Dext.Entity,
  Dext.Entity.Core,
  MultiTenancy.Entities;

type
  /// <summary>
  ///   Master database context for tenant management.
  /// </summary>
  TTenantDbContext = class(TDbContext)
  private
    function GetTenants: IDbSet<TTenant>;
    function GetProducts: IDbSet<TProduct>;
  public
    property Tenants: IDbSet<TTenant> read GetTenants;
    property Products: IDbSet<TProduct> read GetProducts;
  end;

  /// <summary>
  ///   Tenant context accessor - holds current tenant for the request
  /// </summary>
  ITenantContext = interface
    ['{F1E2D3C4-B5A6-4789-8901-234567890ABC}']
    function GetTenantId: string;
    procedure SetTenantId(const Value: string);
    function GetTenant: TTenant;
    procedure SetTenant(const Value: TTenant);
    property TenantId: string read GetTenantId write SetTenantId;
    property Tenant: TTenant read GetTenant write SetTenant;
  end;

  TTenantContext = class(TInterfacedObject, ITenantContext)
  private
    FTenantId: string;
    FTenant: TTenant;
  public
    function GetTenantId: string;
    procedure SetTenantId(const Value: string);
    function GetTenant: TTenant;
    procedure SetTenant(const Value: TTenant);
  end;

implementation

{ TTenantDbContext }

function TTenantDbContext.GetTenants: IDbSet<TTenant>;
begin
  Result := Entities<TTenant>;
end;

function TTenantDbContext.GetProducts: IDbSet<TProduct>;
begin
  Result := Entities<TProduct>;
end;

{ TTenantContext }

function TTenantContext.GetTenantId: string;
begin
  Result := FTenantId;
end;

procedure TTenantContext.SetTenantId(const Value: string);
begin
  FTenantId := Value;
end;

function TTenantContext.GetTenant: TTenant;
begin
  Result := FTenant;
end;

procedure TTenantContext.SetTenant(const Value: TTenant);
begin
  FTenant := Value;
end;

end.
