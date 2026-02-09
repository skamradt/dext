unit Sales.Data.Seeder;

interface

uses
  // 1. Delphi Units
  System.SysUtils,
  // 3. Dext Specialized Units
  Dext.DI.Interfaces,
  Dext.Core.SmartTypes,
  // 4. Dext Facades (Last)
  Dext,
  Dext.Entity,
  // 5. Current Project Units
  Sales.Data.Context,
  Sales.Domain.Entities,
  Sales.Domain.Enums;

type
  TDbSeeder = class
  public
    class procedure Seed(const Provider: IServiceProvider);
  end;

implementation

class procedure TDbSeeder.Seed(const Provider: IServiceProvider);
var
  Scope: IServiceScope;
  Db: TSalesDbContext;
begin
  Writeln('[*] Initializing Database Seeding...');
  
  Scope := Provider.CreateScope;
  try
    // Resolve Context via Service Provider
    Db := Scope.ServiceProvider.GetService(TSalesDbContext) as TSalesDbContext;
    
    if Assigned(Db) then
    begin
      // 1. Ensure Schema Created
      // EnsureCreated is now a function returning True if tables were created.
      if Db.EnsureCreated then
        Writeln('[+] Database schema created.')
      else
        Writeln('[i] Database schema already exists.');
        
      // 2. Check if seeding is needed
      // Using QueryAll.Any() for robustness and performance (avoids full list materialization)
      if not Db.Products.QueryAll.Any then
      begin
        Writeln('[*] Seeding initial data...');
        
        // Products
        var P1 := TProduct.Create; P1.Name := 'Laptop Gamer'; P1.Price := 5000; P1.StockQuantity := 10;
        var P2 := TProduct.Create; P2.Name := 'Mouse Wireless'; P2.Price := 150; P2.StockQuantity := 50;
        var P3 := TProduct.Create; P3.Name := 'Mechanical Keyboard'; P3.Price := 400; P3.StockQuantity := 20;
        
        Db.Products.Add(P1);
        Db.Products.Add(P2);
        Db.Products.Add(P3);
        
        // Customers
        var C1 := TCustomer.Create; C1.Name := 'John Doe'; C1.Email := 'john@example.com'; C1.Status := TCustomerStatus.Active; C1.CreditLimit := 10000;
        var C2 := TCustomer.Create; C2.Name := 'Alice Smith'; C2.Email := 'alice@example.com'; C2.Status := TCustomerStatus.Active; C2.CreditLimit := 5000;
        
        Db.Customers.Add(C1);
        Db.Customers.Add(C2);
        
        Db.SaveChanges;
        Writeln('[OK] Data seeded successfully.');
      end
      else
        Writeln('[i] Products already present. Skipping seeding.');
    end
    else
      Writeln('[ERROR] Could not resolve TSalesDbContext.');
  finally
    Scope := nil;
  end;
end;

end.
