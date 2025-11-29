unit EntityDemo.Tests.FluentAPI;

interface

uses
  EntityDemo.Tests.Base,
  Dext.Specifications.Base,
  Dext.Specifications.Fluent;

type
  TFluentAPITest = class(TBaseTest)
  public
    procedure Run; override;
  end;

implementation

uses
  System.SysUtils,
  EntityDemo.Entities;

{ TFluentAPITest }

procedure TFluentAPITest.Run;
begin
  Log('🔍 Running Fluent API Tests...');
  Log('==============================');
  Log('');
  Log('This test demonstrates all available fluent operators:');
  Log('');
  
  // Setup
  FContext.Entities<TUser>;
  FContext.EnsureCreated;

  // Insert test data
  var U1 := TUser.Create;
  U1.Name := 'John Doe';
  U1.Age := 25;
  U1.Email := 'john@example.com';
  FContext.Entities<TUser>.Add(U1);

  var U2 := TUser.Create;
  U2.Name := 'Jane Smith';
  U2.Age := 30;
  U2.Email := 'jane@example.com';
  FContext.Entities<TUser>.Add(U2);

  var U3 := TUser.Create;
  U3.Name := 'Bob Johnson';
  U3.Age := 17;
  U3.Email := 'bob@example.com';
  FContext.Entities<TUser>.Add(U3);

  LogSuccess('Test data inserted (3 users)');
  Log('');

  // Test using TAdultUsersSpec which uses: UserEntity.Age >= 18
  Log('📊 Test: Using Specification with Fluent API');
  Log('---------------------------------------------');
  var AdultSpec := TAdultUsersSpec.Create;
  var Adults := FContext.Entities<TUser>.List(AdultSpec);
  LogSuccess(Format('✓ Found %d adult user(s) using: UserEntity.Age >= 18', [Adults.Count]));
  AssertTrue(Adults.Count = 2, 'Adult users spec', 'Expected 2 adult users');
  Adults.Free;
  AdultSpec.Free;

  Log('');
  Log('🚀 Test: Inline Queries (without Specification)');
  Log('------------------------------------------------');
  
  // Inline query - muito mais simples!
  var InlineAdults := FContext.Entities<TUser>.List(UserEntity.Age >= 18);
  LogSuccess(Format('✓ Inline query: Found %d adult(s)', [InlineAdults.Count]));
  AssertTrue(InlineAdults.Count = 2, 'Inline adults', 'Expected 2 adults');
  InlineAdults.Free;
  
  // FirstOrDefault inline
  var John := FContext.Entities<TUser>.FirstOrDefault(UserEntity.Name.StartsWith('John'));
  if John <> nil then
    LogSuccess(Format('✓ FirstOrDefault: Found user "%s"', [John.Name]))
  else
    LogError('FirstOrDefault failed');
  
  // Count inline
  var AdultCount := FContext.Entities<TUser>.Count(UserEntity.Age >= 18);
  LogSuccess(Format('✓ Count: %d adult user(s)', [AdultCount]));
  
  // Any inline
  var HasMinors := FContext.Entities<TUser>.Any(UserEntity.Age < 18);
  if HasMinors then
    LogSuccess('✓ Any: Found minor users')
  else
    LogError('Any: No minor users found');
  
  // Complex inline query
  var ComplexResult := FContext.Entities<TUser>.List(
    (UserEntity.Age >= 18) and UserEntity.Name.Contains('o')
  );
  LogSuccess(Format('✓ Complex inline: Found %d user(s) with Age >= 18 AND Name contains "o"', [ComplexResult.Count]));
  ComplexResult.Free;

  Log('');
  Log('✨ Test: Fluent Specification Builder');
  Log('--------------------------------------');
  
  // Managed Specification with automatic cleanup
  var FluentAdults := FContext.Entities<TUser>.List(
    Specification.Where<TUser>(UserEntity.Age >= 18)
  );
  LogSuccess(Format('✓ Fluent Spec: Found %d adult(s)', [FluentAdults.Count]));
  AssertTrue(FluentAdults.Count = 2, 'Fluent spec adults', 'Expected 2 adults');
  FluentAdults.Free;
  
  // Complex fluent with chaining
  var FluentComplex := FContext.Entities<TUser>.List(
    Specification.Where<TUser>((UserEntity.Age >= 18) and UserEntity.Name.Contains('o'))
      .Take(10)
      .Skip(0)
  );
  LogSuccess(Format('✓ Fluent Complex: Found %d user(s) with chaining', [FluentComplex.Count]));
  FluentComplex.Free;

  Log('');
  Log('� Test: OrderBy Tipado');
  Log('------------------------');
  
  // OrderBy with Asc
  var OrderedAsc := FContext.Entities<TUser>.List(
    Specification.Where<TUser>(UserEntity.Age >= 18)
      .OrderBy(UserEntity.Name.Asc)
  );
  LogSuccess(Format('✓ OrderBy Asc: Found %d user(s) ordered by Name ascending', [OrderedAsc.Count]));
  if OrderedAsc.Count > 0 then
    LogSuccess(Format('  First: %s', [OrderedAsc[0].Name]));
  OrderedAsc.Free;
  
  // OrderBy with Desc
  var OrderedDesc := FContext.Entities<TUser>.List(
    Specification.Where<TUser>(UserEntity.Age >= 18)
      .OrderBy(UserEntity.Age.Desc)
  );
  LogSuccess(Format('✓ OrderBy Desc: Found %d user(s) ordered by Age descending', [OrderedDesc.Count]));
  if OrderedDesc.Count > 0 then
    LogSuccess(Format('  First: %s (Age: %d)', [OrderedDesc[0].Name, OrderedDesc[0].Age]));
  OrderedDesc.Free;

  Log('');
  Log('🔗 Test: Include (Eager Loading)');
  Log('--------------------------------');
  
  // Create user with address
  var UWithAddr := TUser.Create;
  UWithAddr.Name := 'User With Address';
  UWithAddr.Age := 40;
  UWithAddr.Email := 'addr@example.com';
  
  var Addr := TAddress.Create;
  Addr.Street := 'Main St';
  Addr.City := 'New York';
  
  UWithAddr.Address := Addr; // Cascade insert should handle this
  FContext.Entities<TUser>.Add(UWithAddr);
  LogSuccess(Format('Inserted user with address ID: %d', [UWithAddr.AddressId]));
  
  // Fetch with Include
  var UsersWithAddr := FContext.Entities<TUser>.List(
    Specification.Where<TUser>(UserEntity.Id = UWithAddr.Id)
      .Include('Address')
  );
  
  AssertTrue(UsersWithAddr.Count = 1, 'Include count', 'Expected 1 user');
  if UsersWithAddr.Count > 0 then
  begin
    var LoadedUser := UsersWithAddr[0];
    if LoadedUser.Address <> nil then
      LogSuccess(Format('✓ Include: Address loaded: %s, %s', [LoadedUser.Address.Street, LoadedUser.Address.City]))
    else
      LogError('Include: Address NOT loaded (nil)');
  end;
  UsersWithAddr.Free;

  Log('');
  Log('�📖 Available Fluent Operators:');
  Log('------------------------------');
  Log('');
  Log('🔢 Comparison Operators:');
  Log('  • UserEntity.Age = 25');
  Log('  • UserEntity.Age <> 25');
  Log('  • UserEntity.Age > 20');
  Log('  • UserEntity.Age >= 18');
  Log('  • UserEntity.Age < 30');
  Log('  • UserEntity.Age <= 30');
  Log('');
  Log('🔤 String Operators:');
  Log('  • UserEntity.Name.StartsWith(''John'')');
  Log('  • UserEntity.Name.EndsWith(''son'')');
  Log('  • UserEntity.Name.Contains(''Smith'')');
  Log('  • UserEntity.Name.Like(''%Doe%'')');
  Log('  • UserEntity.Name.NotLike(''%Test%'')');
  Log('');
  Log('📏 Range Operators:');
  Log('  • UserEntity.Age.Between(18, 65)');
  Log('');
  Log('❓ Null Operators:');
  Log('  • UserEntity.Name.IsNull');
  Log('  • UserEntity.Name.IsNotNull');
  Log('');
  Log('🔗 Logical Operators:');
  Log('  • (UserEntity.Age >= 18) and (UserEntity.Age <= 65)');
  Log('  • (UserEntity.Age < 18) or (UserEntity.Age > 65)');
  Log('  • not (UserEntity.Age = 25)');
  Log('');
  
  LogSuccess('✅ Fluent API demonstration complete!');
  Log('');
end;

end.
