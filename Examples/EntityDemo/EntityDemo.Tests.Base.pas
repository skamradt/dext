unit EntityDemo.Tests.Base;

interface

uses
  System.SysUtils,
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
  FireDAC.Phys.PG,
  FireDAC.Phys.PGDef,
  FireDAC.Stan.ExprFuncs,
  FireDAC.ConsoleUI.Wait,
  FireDAC.Comp.Client,
  FireDAC.DApt,
  Dext.Persistence,
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.Dialects,
  EntityDemo.Entities;

type
  TBaseTestClass = class of TBaseTest;
  TBaseTest = class
  protected
    FConn: TFDConnection;
    FContext: TDbContext;
    
    procedure Log(const Msg: string);
    procedure LogSuccess(const Msg: string);
    procedure LogError(const Msg: string);
    procedure AssertTrue(Condition: Boolean; const SuccessMsg, FailMsg: string);
    
    procedure Setup; virtual;
    procedure TearDown; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run; virtual; abstract;
  end;

implementation

{ TBaseTest }

constructor TBaseTest.Create;
begin
  inherited;
  Setup;
end;

destructor TBaseTest.Destroy;
begin
  TearDown;
  inherited;
end;

procedure TBaseTest.Setup;
begin
  // 1. Setup FireDAC Connection (PostgreSQL)
  FConn := TFDConnection.Create(nil);
  FConn.DriverName := 'PG';
  FConn.Params.Database := 'postgres';
  FConn.Params.UserName := 'postgres';
  FConn.Params.Password := 'root';
  FConn.Params.Add('Server=localhost');
  FConn.LoginPrompt := False;
  
  try
    FConn.Connected := True;
  except
    on E: Exception do
    begin
      WriteLn('❌ Failed to connect to PostgreSQL: ' + E.Message);
      WriteLn('⚠️ Make sure PostgreSQL is running and credentials are correct (user: postgres, pass: root).');
      Halt(1);
    end;
  end;

  // Drop tables to ensure clean state (Case sensitive if quoted)
  try
    // Order matters due to FKs
    FConn.ExecSQL('DROP TABLE IF EXISTS order_items CASCADE');
    FConn.ExecSQL('DROP TABLE IF EXISTS products CASCADE');
    FConn.ExecSQL('DROP TABLE IF EXISTS users CASCADE');
    FConn.ExecSQL('DROP TABLE IF EXISTS addresses CASCADE');
  except
    on E: Exception do
      WriteLn('⚠️ Warning dropping tables: ' + E.Message);
  end;

  // 2. Initialize Context
  FContext := TDbContext.Create(TFireDACConnection.Create(FConn, False), TPostgreSQLDialect.Create);
  
  // 3. Register Entities & Create Schema
  FContext.Entities<TAddress>;
  FContext.Entities<TUser>;
  FContext.Entities<TOrderItem>;
  FContext.Entities<TProduct>;
  FContext.EnsureCreated;
end;

procedure TBaseTest.TearDown;
begin
  FContext.Free;
  FConn.Free;
end;

procedure TBaseTest.Log(const Msg: string);
begin
  WriteLn(Msg);
end;

procedure TBaseTest.LogSuccess(const Msg: string);
begin
  WriteLn('   ✅ ' + Msg);
end;

procedure TBaseTest.LogError(const Msg: string);
begin
  WriteLn('   ❌ ' + Msg);
end;

procedure TBaseTest.AssertTrue(Condition: Boolean; const SuccessMsg, FailMsg: string);
begin
  if Condition then
    LogSuccess(SuccessMsg)
  else
    LogError(FailMsg);
end;

end.
