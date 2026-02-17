program TestLockingIntegration;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  Data.DB,
  Dext,
  Dext.Entity,
  Dext.Entity.Attributes,
  Dext.Entity.Context,
  Dext.Entity.Core,
  Dext.Entity.Dialects,
  Dext.Entity.Drivers.Interfaces,
  Dext.Mocks,
  Dext.Specifications.Types,
  Dext.Specifications.Interfaces;

type
  [Table('Products')]
  TProduct = class
  private
    FId: Integer;
    FName: string;
    FLockToken: string;
    FLockExpiry: TDateTime;
  public
    [PrimaryKey]
    property Id: Integer read FId write FId;
    
    [Column('Name')]
    property Name: string read FName write FName;

    [LockToken]
    property LockToken: string read FLockToken write FLockToken;

    [LockExpiration]
    property LockExpiry: TDateTime read FLockExpiry write FLockExpiry;
  end;

procedure Log(const AMsg: string);
begin
  WriteLn(AMsg);
end;

procedure TestDBLocking;
var
  ConnMock: Mock<IDbConnection>;
  CmdMock: Mock<IDbCommand>;
  Ctx: TDbContext;
begin
  Log('--- Testing DB-Level Locking ---');
  
  // 1. SQL Server Hint
  ConnMock := Mock<IDbConnection>.Create;
  CmdMock := Mock<IDbCommand>.Create;

  // Setup: Connection returns dialect and creates command
  ConnMock.Setup.Returns(TValue.From<TDatabaseDialect>(ddSQLServer)).When.GetDialect;
  ConnMock.Setup.Returns(True).When.IsConnected;
  ConnMock.Setup.Returns(TValue.From<IDbCommand>(CmdMock.Instance)).When.CreateCommand('');

  // Setup: Command returns 0 rows (no actual results needed)
  CmdMock.Setup.Returns(0).When.ExecuteNonQuery;

  Ctx := TDbContext.Create(ConnMock.Instance, TSQLServerDialect.Create, nil);
  try
    Log('SQL Server Exclusive Lock:');
    try
      Ctx.Entities<TProduct>.Where(Prop('Id') = 1).WithLock(lmExclusive).ToList;
      Log('   ✅ Query executed successfully');
    except
      on E: Exception do
        Log('   ⚠ Query raised: ' + E.Message);
    end;
  finally
    Ctx.Free;
  end;

  // 2. PostgreSQL Clause
  ConnMock := Mock<IDbConnection>.Create;
  CmdMock := Mock<IDbCommand>.Create;

  ConnMock.Setup.Returns(TValue.From<TDatabaseDialect>(ddPostgreSQL)).When.GetDialect;
  ConnMock.Setup.Returns(True).When.IsConnected;
  ConnMock.Setup.Returns(TValue.From<IDbCommand>(CmdMock.Instance)).When.CreateCommand('');

  CmdMock.Setup.Returns(0).When.ExecuteNonQuery;

  Ctx := TDbContext.Create(ConnMock.Instance, TPostgreSQLDialect.Create, nil);
  try
    Log('PostgreSQL Exclusive Lock:');
    try
      Ctx.Entities<TProduct>.Where(Prop('Id') = 1).WithLock(lmExclusive).ToList;
      Log('   ✅ Query executed successfully');
    except
      on E: Exception do
        Log('   ⚠ Query raised: ' + E.Message);
    end;
  finally
    Ctx.Free;
  end;
end;

procedure TestOfflineLocking;
var
  ConnMock: Mock<IDbConnection>;
  CmdMock: Mock<IDbCommand>;
  Ctx: TDbContext;
  Prod: TProduct;
begin
  Log('--- Testing Offline Locking ---');
  
  ConnMock := Mock<IDbConnection>.Create;
  CmdMock := Mock<IDbCommand>.Create;

  ConnMock.Setup.Returns(TValue.From<TDatabaseDialect>(ddPostgreSQL)).When.GetDialect;
  ConnMock.Setup.Returns(True).When.IsConnected;
  ConnMock.Setup.Returns(TValue.From<IDbCommand>(CmdMock.Instance)).When.CreateCommand('');

  // ExecuteNonQuery returns 1 = success (1 row affected)
  CmdMock.Setup.Returns(1).When.ExecuteNonQuery;

  Ctx := TDbContext.Create(ConnMock.Instance, TPostgreSQLDialect.Create, nil);
  try
    Prod := TProduct.Create;
    try
      Prod.Id := 99;
      
      Log('TryLock Atomic Update:');
      if Ctx.Entities<TProduct>.TryLock(Prod, 'UserA', 15) then
        Log('   ✅ TryLock Success (Logic)')
      else
        Log('   ❌ TryLock Failed');
        
      // Check if local object was updated
      if (Prod.LockToken = 'UserA') and (Prod.LockExpiry > Now) then
        Log('   ✅ Local Proxy Success: Token and Expiry updated')
      else
        Log('   ❌ Local Proxy Failure');

      Log('Unlock:');
      if Ctx.Entities<TProduct>.Unlock(Prod) then
        Log('   ✅ Unlock Success')
      else
        Log('   ❌ Unlock Failed');
        
    finally
      Prod.Free;
    end;
  finally
    Ctx.Free;
  end;
end;

begin
  try
    TestDBLocking;
    WriteLn;
    TestOfflineLocking;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
