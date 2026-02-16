unit Dext.Entity.Async.Tests;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Dext.Assertions,
  Dext.Testing.Attributes,
  Dext.Entity.Core,
  Dext.Entity.Context,
  Dext.Entity.DbSet,
  Dext.Entity.Query,
  Dext.Entity.Drivers.Interfaces,
  Dext.Threading.Async,
  Dext.Collections;

type
  // Mock Connection for testing pooling checks
  TMockConnection = class(TInterfacedObject, IDbConnection)
  private
    FPooled: Boolean;
  public
    constructor Create(APooled: Boolean);
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    function BeginTransaction: IDbTransaction;
    function CreateCommand(const ASql: string): IDbCommand;
    function TableExists(const ATableName: string): Boolean;
    function RegisterCustomAggregate(const AName: string; ALimit: Integer; AHandler: TObject): Boolean;
    function IsPooled: Boolean;
    property Pooled: Boolean read IsPooled;
  end;

  // Mock DbContext for SaveChangesAsync testing
  TMockDbContext = class(TDbContext)
  public
    FSaveChangesCalled: Boolean;
    constructor Create(AConnection: IDbConnection);
    procedure OnConfiguring(Options: TDbContextOptions); override;
    function SaveChanges: Integer; override;
  end;

  [TestFixture('Async ORM Operations Tests')]
  TAsyncTests = class
  public
    [Test]
    [Description('Verify ToListAsync raises error when connection is not pooled')]
    procedure TestToListAsyncShouldFailOnNonPooledConnection;

    [Test]
    [Description('Verify SaveChangesAsync raises error when connection is not pooled')]
    procedure TestSaveChangesAsyncShouldFailOnNonPooledConnection;

    [Test]
    [Description('Verify ToListAsync executes correctly on pooled connection')]
    procedure TestToListAsyncShouldSucceedOnPooledConnection;

    [Test]
    [Description('Verify SaveChangesAsync executes correctly on pooled connection')]
    procedure TestSaveChangesAsyncShouldSucceedOnPooledConnection;
  end;

implementation

{ TMockConnection }

constructor TMockConnection.Create(APooled: Boolean);
begin
  FPooled := APooled;
end;

procedure TMockConnection.Connect; begin end;
procedure TMockConnection.Disconnect; begin end;
function TMockConnection.IsConnected: Boolean; begin Result := True; end;
function TMockConnection.BeginTransaction: IDbTransaction; begin Result := nil; end;
function TMockConnection.CreateCommand(const ASql: string): IDbCommand; begin Result := nil; end;
function TMockConnection.TableExists(const ATableName: string): Boolean; begin Result := True; end;
function TMockConnection.RegisterCustomAggregate(const AName: string; ALimit: Integer; AHandler: TObject): Boolean; begin Result := True; end;
function TMockConnection.IsPooled: Boolean; begin Result := FPooled; end;

{ TMockDbContext }

constructor TMockDbContext.Create(AConnection: IDbConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

procedure TMockDbContext.OnConfiguring(Options: TDbContextOptions);
begin
  // Do nothing
end;

function TMockDbContext.SaveChanges: Integer;
begin
  FSaveChangesCalled := True;
  Result := 42;
end;

{ TAsyncTests }

procedure TAsyncTests.TestToListAsyncShouldFailOnNonPooledConnection;
var
  Conn: IDbConnection;
  Query: TFluentQuery<TObject>;
begin
  Conn := TMockConnection.Create(False);
  // TFluentQuery.Create(IteratorFactory, Spec, ExecCount, ExecAny, ExecFirstOrDefault, Connection)
  Query := TFluentQuery<TObject>.Create(nil, nil, nil, nil, nil, Conn);

  Should(procedure
    begin
      Query.ToListAsync;
    end).Throw(Exception, 'ToListAsync requires a pooled connection');
end;

procedure TAsyncTests.TestSaveChangesAsyncShouldFailOnNonPooledConnection;
var
  Conn: IDbConnection;
  Ctx: TMockDbContext;
begin
  Conn := TMockConnection.Create(False);
  Ctx := TMockDbContext.Create(Conn);
  try
    Should(procedure
      begin
        Ctx.SaveChangesAsync;
      end).Throw(Exception, 'SaveChangesAsync requires a pooled connection');
  finally
    Ctx.Free;
  end;
end;

procedure TAsyncTests.TestToListAsyncShouldSucceedOnPooledConnection;
var
  Conn: IDbConnection;
  Query: TFluentQuery<TObject>;
  Builder: TAsyncBuilder<IList<TObject>>;
begin
  Conn := TMockConnection.Create(True);
  // We need a factory that returns something for ToList
  Query := TFluentQuery<TObject>.Create(
    function: TQueryIterator<TObject>
    begin
      Result := nil; // In a real scenario, this would return an iterator
    end,
    nil, nil, nil, nil, Conn);

  // We can't easily execute ToList without a real iterator in this mock
  // but we can at least verify it doesn't throw and returns a builder
  Builder := Query.ToListAsync;
  Should(Builder.Start).Not.BeNil;
end;

procedure TAsyncTests.TestSaveChangesAsyncShouldSucceedOnPooledConnection;
var
  Conn: IDbConnection;
  Ctx: TMockDbContext;
  Builder: TAsyncBuilder<Integer>;
  Result: Integer;
begin
  Conn := TMockConnection.Create(True);
  Ctx := TMockDbContext.Create(Conn);
  try
    Builder := Ctx.SaveChangesAsync;
    Should(Builder.Start).Not.BeNil;
    
    // Test actual async execution and result
    Result := Builder.Await;
    Should(Result).Be(42);
    Should(Ctx.FSaveChangesCalled).BeTrue;
  finally
    Ctx.Free;
  end;
end;

end.
