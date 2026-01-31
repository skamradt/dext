program Web.SmartPropsDemo;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  System.SysUtils,
  System.Classes,
  Dext,
  Dext.Web,
  Dext.Web.Interfaces,
  Dext.Web.Results,
  Dext.Entity,
  Dext.Json,
  Dext.Entity.Prototype,
  Dext.Core.SmartTypes,
  App.Entities in 'App.Entities.pas',
  App.Context in 'App.Context.pas';

var
  App: IWebApplication;
  
procedure SeedDatabase;
var
  ServiceProvider: IServiceProvider;
  Scope: IServiceScope;
  Db: TAppDbContext;
  P1, P2, P3: TProduct;
begin
  WriteLn('Seeding database...');
  
  // Get ServiceProvider from Application AFTER BuildServices is called  
  ServiceProvider := App.GetApplicationBuilder.GetServiceProvider;
  if ServiceProvider = nil then
  begin
    WriteLn('[ERROR] ServiceProvider is nil');
    Exit;
  end;
  
  Scope := ServiceProvider.CreateScope;
  try
    Db := Scope.ServiceProvider.GetService(TServiceType.FromClass(TAppDbContext)) as TAppDbContext;
    if Db = nil then
    begin
      WriteLn('[ERROR] TAppDbContext not registered');
      Exit;
    end;
    
    // Access property to register entity before EnsureCreated
    var EnsureSet := Db.Products;
    Db.EnsureCreated;
    
    P1 := TProduct.Create;
    P1.Name := 'Gaming Laptop';
    P1.Price := 1999.99;
    P1.IsActive := True;
    Db.Products.Add(P1);

    P2 := TProduct.Create;
    P2.Name := 'Wireless Mouse';
    P2.Price := 29.99;
    P2.IsActive := True;
    Db.Products.Add(P2);

    P3 := TProduct.Create;
    P3.Name := 'Discontinued Phone';
    P3.Price := 499.00;
    P3.IsActive := False;
    Db.Products.Add(P3);

    Db.SaveChanges;
    WriteLn('Seeding complete.');
  finally
    Scope := nil;
  end;
end;

begin
  try
    if FileExists('smart_props.db') then
      DeleteFile('smart_props.db');

    App := TDextApplication.Create;

    // Configure Services
    App.Services.AddDbContext<TAppDbContext>(
      procedure(Options: TDbContextOptions)
      begin
        Options.UseSQLite('smart_props.db');
        Options.WithPooling(True);
      end);

    // Build services BEFORE seeding
    App.BuildServices;
    
    // Seed database AFTER services are built
    SeedDatabase;

    // Endpoint: GET /products (Smart Property Query)
    App.Builder.MapGet('/products',
      procedure(Context: IHttpContext)
      var
        Db: TAppDbContext;
        u: TProduct;
      begin
        Db := Context.Services.GetService(TServiceType.FromClass(TAppDbContext)) as TAppDbContext;

        // Smart Property Query: Price > 100
        u := Prototype.Entity<TProduct>;
        var List := Db.Products.Where(u.Price > 100).ToList;

        // Automatic JSON Serialization
        Context.Response.Json(TDextJson.Serialize(List));
      end);

    // Endpoint: POST /products (Model Binding with DI)
    App.Builder.MapPost<TAppDbContext, TProduct, IResult>('/products',
      function(Db: TAppDbContext; Product: TProduct): IResult
      begin
        if Product = nil then
          Exit(Results.BadRequest('Invalid product data'));
          
        try
          Db.Products.Add(Product);
          Db.SaveChanges;
          Result := Results.Ok(Product);
        except
          on E: Exception do
            Result := Results.InternalError(E);
        end;
      end);

    App.Builder.MapGet('/', 
      procedure(C: IHttpContext) 
      begin 
        C.Response.Write('Smart Properties Demo Running. Go to http://localhost:5000/products');
      end);

    WriteLn('');
    WriteLn('Server listening on http://localhost:5000');
    WriteLn('GET:  http://localhost:5000/products');
    WriteLn('POST: curl -X POST http://localhost:5000/products \');
    WriteLn('      -H "Content-Type: application/json" \');
    WriteLn('      -d "{\"Name\": \"New Gadget\", \"Price\": 99.99, \"IsActive\": true}"');
    WriteLn('');
    
    App.Run(5000);
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
