unit OrderAPI.Database;

{***************************************************************************}
{  Order API - Database Context                                             }
{  Seguindo o padrão do Web.Dext.Starter.Admin                              }
{***************************************************************************}

interface

uses
  System.SysUtils,
  Dext.Entity,
  OrderAPI.Entities;

type
  /// <summary>
  ///   Database context para o Order API.
  ///   O pooling é configurado no Startup via AddDbContext.
  /// </summary>
  {$M+}
  TOrderDbContext = class(TDbContext)
  protected
    procedure OnModelCreating(Builder: TModelBuilder); override;
  end;

implementation

{ TOrderDbContext }

procedure TOrderDbContext.OnModelCreating(Builder: TModelBuilder);
begin
  inherited;
  
  // Registrar todas as entidades
  Builder.Entity<TCategory>;
  Builder.Entity<TProduct>;
  Builder.Entity<TRestaurantTable>;
  Builder.Entity<TOrder>;
  Builder.Entity<TOrderItem>;
end;

end.
