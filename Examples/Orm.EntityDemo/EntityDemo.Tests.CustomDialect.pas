unit EntityDemo.Tests.CustomDialect;

interface

uses
  Dext.Testing,
  Dext.Entity,
  Dext.Entity.Setup,
  EntityDemo.Tests.Base,
  EntityDemo.CustomDialect,
  System.SysUtils;

type
  [TestFixture]
  TCustomDialectTest = class(TBaseTest)
  public
    [Test]
    procedure TestCustomDialectInjection;
    
    procedure Run; override;
  end;

implementation

uses
  Dext.Entity.Dialects;

{ TCustomDialectTest }

procedure TCustomDialectTest.Run;
begin
  TestCustomDialectInjection;
end;

procedure TCustomDialectTest.TestCustomDialectInjection;
var
  Options: TDbContextOptions;
  Ctx: TDbContext;
begin
  // 1. Configure Context to use our Custom Dialect explicitly
  Options := TDbContextOptions.Create
    .UseDriver('SQLite')
    .UseCustomDialect(TCustomSQLiteDialect.Create);
    
  Ctx := TDbContext.Create(nil, Options.Dialect, nil, nil); // Or however we inject it
  
  try
    // 2. Verify the dialect is actually our custom class
    var DialectObj := Ctx.Dialect as TObject;
    AssertTrue(DialectObj <> nil, 'Dialect should not be nil');
    AssertTrue('TCustomSQLiteDialect' = DialectObj.ClassName, 'Should be our custom dialect class');
    
    // 3. Verify logic behavior (QuoteIdentifier)
    // Ctx.Dialect is a property returning interface, assign to local var to be safe
    var DialectIntf := Ctx.Dialect;
    var Quoted := DialectIntf.QuoteIdentifier('my_table');
    AssertTrue('[MY_TABLE]' = Quoted, 'Custom dialect should force uppercase and brackets');
  finally
    Ctx.Free;
    Options.Free;
  end;
end;

end.
