unit EntityDemo.Tests.LazyLoading;

interface

uses
  Dext.Entity,
  Dext.Entity.LazyLoading,
  EntityDemo.Entities,
  EntityDemo.Tests.Base,
  System.SysUtils,
  System.Classes;

type
  TLazyLoadingTest = class(TBaseTest)
  public
    procedure Run; override;
    procedure TestLazyLoadReference;
    procedure TestLazyLoadCollection;
  end;

implementation

{ TLazyLoadingTest }

procedure TLazyLoadingTest.Run;
begin
  WriteLn('  [LazyLoading] Running tests...');
  TestLazyLoadReference;
  TestLazyLoadCollection;
  WriteLn('  [LazyLoading] Done.');
end;

procedure TLazyLoadingTest.TestLazyLoadReference;
var
  User: TUser;
  Addr: TAddress;
  LoadedUser: TUser;
begin
  Write('    - TestLazyLoadReference: ');
  
  // 1. Setup Data
  Addr := TAddress.Create;
  Addr.Street := 'Lazy St';
  Addr.City := 'Sleepy Hollow';
  FContext.Entities<TAddress>.Add(Addr);
  FContext.SaveChanges;
  
  User := TUser.Create;
  User.Name := 'Rip Van Winkle';
  User.AddressId := Addr.Id;
  FContext.Entities<TUser>.Add(User);
  FContext.SaveChanges;
  
  // 2. Clear Context to detach entities
  FContext.Clear;
  
  // 3. Load User (without Include)
  LoadedUser := FContext.Entities<TUser>.Find(User.Id);
  
  // 4. Verify Lazy Loading
  try
    if LoadedUser.Address = nil then
      raise Exception.Create('Address should not be nil (Lazy Loading failed)');
      
    if LoadedUser.Address.City <> 'Sleepy Hollow' then
      raise Exception.Create('Address City mismatch');
      
    WriteLn('OK');
  except
    on E: Exception do
      WriteLn('FAILED: ' + E.ClassName + ': ' + E.Message);
  end;
end;

procedure TLazyLoadingTest.TestLazyLoadCollection;
var
  User1, User2: TUser;
  Addr: TAddress;
  LoadedAddr: TAddress;
begin
  Write('    - TestLazyLoadCollection: ');
  
  // 1. Setup Data
  Addr := TAddress.Create;
  Addr.Street := 'Collection St';
  Addr.City := 'Crowded City';
  FContext.Entities<TAddress>.Add(Addr);
  FContext.SaveChanges;
  
  User1 := TUser.Create;
  User1.Name := 'User One';
  User1.AddressId := Addr.Id;
  FContext.Entities<TUser>.Add(User1);
  
  User2 := TUser.Create;
  User2.Name := 'User Two';
  User2.AddressId := Addr.Id;
  FContext.Entities<TUser>.Add(User2);
  FContext.SaveChanges;

  FContext.Clear;
  LoadedAddr := FContext.Entities<TAddress>.Find(Addr.Id);
  try
    // 1. Verify Automatic Lazy Loading
    if LoadedAddr.Users = nil then
      raise Exception.Create('Users collection should not be nil');

    if LoadedAddr.Users.Count <> 2 then
      raise Exception.Create('Automatic Lazy Loading failed. Expected 2, got ' + LoadedAddr.Users.Count.ToString);

    // 2. Verify Explicit Loading (should not duplicate)
    FContext.Entry(LoadedAddr).Collection('Users').Load;
    
    if LoadedAddr.Users.Count <> 2 then
      raise Exception.Create('Explicit Loading caused duplication. Expected 2, got ' + LoadedAddr.Users.Count.ToString);
      
    WriteLn('OK');
  except
    on E: Exception do
      WriteLn('FAILED: ' + E.Message);
  end;
end;

end.
