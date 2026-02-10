unit DextFood.DbSeeder;

interface

uses
  System.SysUtils,
  Dext,
  Dext.DI.Interfaces,
  Dext.Entity,
  DextFood.Domain;

type
  /// <summary>
  /// Responsável por popular o banco de dados com dados iniciais se estiver vazio.
  /// </summary>
  TDbSeeder = class
  public
    class procedure Seed(const Provider: IServiceProvider);
  end;

implementation

uses
  DextFood.Startup; // For TAppDbContext

{ TDbSeeder }

class procedure TDbSeeder.Seed(const Provider: IServiceProvider);
var
  Scope: IServiceScope;
  Db: TAppDbContext;
  Order: TOrder;
begin
  Writeln('[*] Initializing Database Seeding...');
  
  Scope := Provider.CreateScope;
  try
    Db := Scope.ServiceProvider.GetService(TAppDbContext) as TAppDbContext;
    if Assigned(Db) then
    begin
      // OBRIGATÓRIO: Registrar entidades no cache antes de EnsureCreated
      Db.Entities<TOrder>;
      
      // Cria o schema se não existir (SQLite :memory: sempre precisa disso)
      Db.EnsureCreated;
      
      // Verifica se já existem dados
      if Db.Orders.ToList.Count = 0 then
      begin
        Writeln('[*] Database is empty. Seeding sample data...');
        
        // Exemplo 1: Pedido Pendente
        Order := TOrder.Create;
        Order.Status := TOrderStatus.Pending;
        Order.Total := 85.50;
        Order.CreatedAt := Now;
        Db.Orders.Add(Order);
        
        // Exemplo 2: Pedido em Preparo
        Order := TOrder.Create;
        Order.Status := TOrderStatus.Preparing;
        Order.Total := 120.00;
        Order.CreatedAt := Now - (1/24); // 1 hora atrás
        Db.Orders.Add(Order);
        
        // Exemplo 3: Pedido Concluído
        Order := TOrder.Create;
        Order.Status := TOrderStatus.Completed;
        Order.Total := 45.90;
        Order.CreatedAt := Now - 1; // Ontem
        Db.Orders.Add(Order);
        
        Db.SaveChanges;
        Writeln('[OK] Seeding completed successfully.');
      end
      else
        Writeln('[INFO] Database already has data. Skipping seeding.');
    end
    else
      Writeln('[ERROR] Could not resolve TAppDbContext.');
  finally
    Scope := nil;
  end;
end;

end.


