unit EntityDemo.Tests.Dialect;

interface

uses
  DUnitX.TestFramework,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.Dialects,
  FireDAC.Comp.Client,
  Dext.Entity.Drivers.FireDAC.Links,
  System.SysUtils,
  EntityDemo.Tests.Base;

type
  [TestFixture]
  TDialectTest = class(TBaseTest)
  public
    [Test]
    procedure TestSQLiteDetection;
    procedure Run; override;
  end;

implementation

procedure TDialectTest.TestSQLiteDetection;
var
  FDConn: TFDConnection;
  Conn: IDbConnection;
begin
  FDConn := TFDConnection.Create(nil);
  try
    FDConn.DriverName := 'SQLite';
    // No need to open just to detect based on driver name in our logic
    
    Conn := TFireDACConnection.Create(FDConn, False);
    
    Assert.AreEqual(TDatabaseDialect.ddSQLite, Conn.Dialect, 'Dialect should be SQLite');
  finally
    FDConn.Free;
  end;
end;

procedure TDialectTest.Run;
begin
  Log('Running Test: TestSQLiteDetection');
  TestSQLiteDetection;
end;

initialization
  TDUnitX.RegisterTestFixture(TDialectTest);

end.
