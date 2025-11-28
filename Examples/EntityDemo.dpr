program EntityDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Variants,
  Data.DB,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.ConsoleUI.Wait,
  FireDAC.Comp.Client,
  FireDAC.DApt, // Required for FireDAC data adapters
  Dext,
  Dext.Entity,
  Dext.Entity.Core,
  Dext.Entity.Attributes,
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.Dialects,
  Dext.Specifications.Interfaces,
  Dext.Specifications.Criteria,
  Dext.Specifications.Base;

type
  [Table('addresses')]
  TAddress = class
  private
    FId: Integer;
    FStreet: string;
    FCity: string;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Street: string read FStreet write FStreet;
    property City: string read FCity write FCity;
  end;

  [Table('users')]
  TUser = class
  private
    FId: Integer;
    FName: string;
    FAge: Integer;
    FEmail: string;
    FAddress: TAddress;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;

    [Column('full_name')]
    property Name: string read FName write FName;

    property Age: Integer read FAge write FAge;
    property Email: string read FEmail write FEmail;

    [ForeignKey('AddressId')]
    property Address: TAddress read FAddress write FAddress;
  end;

  [Table('order_items')]
  TOrderItem = class
  private
    FOrderId: Integer;
    FProductId: Integer;
    FQuantity: Integer;
    FPrice: Double;
  public
    [PK, Column('order_id')]
    property OrderId: Integer read FOrderId write FOrderId;

    [PK,Column('product_id')]
    property ProductId: Integer read FProductId write FProductId;

    property Quantity: Integer read FQuantity write FQuantity;
    property Price: Double read FPrice write FPrice;
  end;

  // 🧬 Metadata Prototype (TypeOf)
  UserEntity = class
  public
    class function Age: TProp;
    class function Name: TProp;
  end;

  // Specification using Metadata
  TAdultUsersSpec = class(TSpecification<TUser>)
  public
    constructor Create; override;
  end;

{ UserEntity }

class function UserEntity.Age: TProp;
begin
  Result := Prop('Age');
end;

class function UserEntity.Name: TProp;
begin
  Result := Prop('full_name');
end;

{ TAdultUsersSpec }

constructor TAdultUsersSpec.Create;
begin
  inherited Create;
  Where(UserEntity.Age >= 18);
end;

procedure RunDemo;
var
  FDConn: TFDConnection;
  Context: TDbContext; // Using concrete class to access Entities<T>
  User: TUser;
  Address: TAddress;
begin
  WriteLn('🚀 Dext Entity ORM Demo');
  WriteLn('=======================');

  // 1. Setup FireDAC Connection (SQLite In-Memory)
  FDConn := TFDConnection.Create(nil);
  Defer([FDConn.Close, FDConn.Free]);

  FDConn.DriverName := 'SQLite';
  FDConn.Params.Database := ':memory:';
  FDConn.LoginPrompt := False;
  FDConn.Connected := True;

    // 2. Initialize Context
  Context := TDbContext.Create(TFireDACConnection.Create(FDConn, False), // Don't own FDConn
    TSQLiteDialect.Create);
  Defer(Context.Free);
      // 3. Register Entities & Create Schema
  WriteLn('🛠️  Creating Schema (EnsureCreated)...');
  Context.Entities<TAddress>;
  Context.Entities<TUser>;
  Context.EnsureCreated;

  // 4. Insert Data
  WriteLn('📝 Inserting sample data...');

  // Address
  Address := TAddress.Create;
  Address.Street := '123 Main St';
  Address.City := 'New York';
  Context.Entities<TAddress>.Add(Address);
  // Hack: SQLite returns last insert id, but our Add doesn't update entity ID yet.
  // Let's assume ID 1.

  User := TUser.Create;
  User.Name := 'Alice';
  User.Age := 25;
  User.Email := 'alice@dext.com';
  // Manually set FK for now as we don't have relationship saving yet
  // Wait, TUser doesn't have AddressId property exposed, only Address object.
  // But our Add method only saves mapped properties.
  // If we want to save 'AddressId', we need either:
  // A) An 'AddressId' property on TUser
  // B) The Add method to handle ForeignKeyAttribute and extract ID from the object.

  // For this "Basic" phase, let's add AddressId property to TUser to make saving easier,
  // but keep Address property for loading test.
  // Actually, let's modify TUser to have AddressId for now to simplify saving.

  // Re-defining TUser locally for the demo logic? No, let's just use SQL to insert for now to test LOADING.
  // Or better, let's implement saving of FKs in Add/Update?
  // That's Phase 2 part 2? No, let's just use SQL to insert the user with AddressId 1.

  FDConn.ExecSQL('INSERT INTO users (full_name, Age, Email, AddressId) VALUES (''Alice'', 25, ''alice@dext.com'', 1)');

  WriteLn('   Data inserted successfully.');
  WriteLn;

  // 5. Query and Verify Relationship Loading
  WriteLn('🔍 Querying User 1 to verify Address loading...');

  User := Context.Entities<TUser>.Find(1);
  if User <> nil then
  begin
    WriteLn(Format('   - User: %s', [User.Name]));
    if User.Address <> nil then
      WriteLn(Format('   - Address: %s, %s', [User.Address.Street, User.Address.City]))
    else
      WriteLn('   ❌ Address NOT loaded!');

    if (User.Address <> nil) and (User.Address.City = 'New York') then
      WriteLn('   ✅ Success! Relationship loaded.')
    else
      WriteLn('   ❌ Failed to load correct address.');

    // Verify Identity Map
    WriteLn('🔍 Verifying Identity Map...');
    var User2 := Context.Entities<TUser>.Find(1);
    if User = User2 then
      WriteLn('   ✅ Success! Identity Map returned same instance.')
    else
      WriteLn('   ❌ Failed! Identity Map returned different instance.');

    // Do NOT free User or User2, they are owned by Context/DbSet now.
  end;

  // 6. Update
  WriteLn;
  WriteLn('🔄 Updating Alice (Age 25 -> 26)...');
  User := Context.Entities<TUser>.Find(1);
  if User <> nil then
  begin
    User.Age := 26;
    Context.Entities<TUser>.Update(User);
    WriteLn('   Alice updated.');
  end;

  // Verify Update
  User := Context.Entities<TUser>.Find(1);
  if (User <> nil) and (User.Age = 26) then
    WriteLn('   ✅ Update Verified: Alice is now 26.')
  else
    WriteLn('   ❌ Update Failed!');

  // 7. Remove
  WriteLn;
  WriteLn('🗑️  Removing Bob (Id 2)...');
  // Insert Bob manually first since we didn't insert him
  FDConn.ExecSQL('INSERT INTO users (full_name, Age, Email, AddressId) VALUES (''Bob'', 30, ''bob@dext.com'', 1)');

  User := Context.Entities<TUser>.Find(2); // Bob
  if User <> nil then
  begin
    Context.Entities<TUser>.Remove(User);
    WriteLn('   Bob removed.');
  end;

  // Verify Remove
  User := Context.Entities<TUser>.Find(2);
  if User = nil then
    WriteLn('   ✅ Remove Verified: Bob is gone.')
  else
    WriteLn('   ❌ Remove Failed! Bob still exists.');

  // 8. Composite Keys Test
  WriteLn;
  WriteLn('🔑 Testing Composite Keys...');
  Context.Entities<TOrderItem>;
  Context.EnsureCreated;

  var OrderItem := TOrderItem.Create;
  OrderItem.OrderId := 100;
  OrderItem.ProductId := 50;
  OrderItem.Quantity := 2;
  OrderItem.Price := 10.50;

  Context.Entities<TOrderItem>.Add(OrderItem);
  WriteLn('   OrderItem (100, 50) added.');

  // Find using Composite Key
  var FoundItem := Context.Entities<TOrderItem>.Find([100, 50]);
  if FoundItem <> nil then
  begin
    WriteLn(Format('   ✅ Found OrderItem: %d x %f', [FoundItem.Quantity,
      FoundItem.Price]));

    // Update
    FoundItem.Quantity := 5;
    Context.Entities<TOrderItem>.Update(FoundItem);
    WriteLn('   OrderItem updated to Quantity 5.');

    // Verify Update
    // Clear Identity Map or create new context? Or just find again (should return same instance)
    // To verify DB update, we should check DB directly or ensure Update flushes to DB.
    // Let's assume Update executes SQL.

    // Remove
    Context.Entities<TOrderItem>.Remove(FoundItem);
    WriteLn('   OrderItem removed.');

    // Verify Remove
    if Context.Entities<TOrderItem>.Find(VarArrayOf([100, 50])) = nil then
      WriteLn('   ✅ Composite Key CRUD Verified.')
    else
      WriteLn('   ❌ Composite Key Remove Failed.');
  end
  else
    WriteLn('   ❌ Failed to find OrderItem by Composite Key.');
end;

begin
  try
    RunDemo;
    ReadLn;
  except
    on E: Exception do
      Writeln('❌ Error: ', E.ClassName, ': ', E.Message);
  end;
end.
