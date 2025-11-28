unit Dext.Entity;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  System.Generics.Collections,
  Dext.Entity.Core,
  Dext.Entity.DbSet,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Dialects;

type
  /// <summary>
  ///   Concrete implementation of DbContext.
  ///   Manages database connection, transactions, and entity sets.
  ///   
  ///   Note: This class implements IDbContext but disables reference counting.
  ///   You must manage its lifecycle manually (Free).
  /// </summary>
  TDbContext = class(TObject, IDbContext)
  private
    FConnection: IDbConnection;
    FDialect: ISQLDialect;
    FTransaction: IDbTransaction;
    FCache: TDictionary<PTypeInfo, IInterface>; // Cache for DbSets
  protected
    // IDbContext Implementation
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    
  public
    constructor Create(AConnection: IDbConnection; ADialect: ISQLDialect);
    destructor Destroy; override;
    
    function Connection: IDbConnection;
    function Dialect: ISQLDialect;
    
    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;
    
    /// <summary>
    ///   Access the DbSet for a specific entity type.
    /// </summary>
    function DataSet(AEntityType: PTypeInfo): IDbSet;
    procedure EnsureCreated;
    
    /// <summary>
    ///   Access the DbSet for a specific entity type.
    /// </summary>
    function Entities<T: class>: IDbSet<T>;
  end;

implementation

{ TDbContext }

constructor TDbContext.Create(AConnection: IDbConnection; ADialect: ISQLDialect);
begin
  inherited Create;
  FConnection := AConnection;
  FDialect := ADialect;
  FCache := TDictionary<PTypeInfo, IInterface>.Create;
end;

destructor TDbContext.Destroy;
begin
  FCache.Free;
  inherited;
end;

function TDbContext.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TDbContext._AddRef: Integer;
begin
  Result := -1; // Disable ref counting
end;

function TDbContext._Release: Integer;
begin
  Result := -1; // Disable ref counting
end;

function TDbContext.Connection: IDbConnection;
begin
  Result := FConnection;
end;

function TDbContext.Dialect: ISQLDialect;
begin
  Result := FDialect;
end;

procedure TDbContext.BeginTransaction;
begin
  FTransaction := FConnection.BeginTransaction;
end;

procedure TDbContext.Commit;
begin
  if FTransaction <> nil then
  begin
    FTransaction.Commit;
    FTransaction := nil;
  end;
end;

procedure TDbContext.Rollback;
begin
  if FTransaction <> nil then
  begin
    FTransaction.Rollback;
    FTransaction := nil;
  end;
end;

function TDbContext.DataSet(AEntityType: PTypeInfo): IDbSet;
var
  //Method: TRttiMethod;
  Ctx: TRttiContext;
  //Typ: TRttiType;
begin
  if FCache.ContainsKey(AEntityType) then
    Exit(IDbSet(FCache[AEntityType]));

  // Dynamic creation of DbSet<T> using RTTI is complex because we need to invoke a generic method.
  // However, since we are inside TDbContext, we can use a trick or just rely on the user calling Entities<T> first?
  // No, that defeats the purpose.
  
  // For now, let's assume the user has registered it or we use RTTI to invoke "Entities<T>"
  Ctx := TRttiContext.Create;
  //Typ := Ctx.GetType(TDbContext);
  //Method := Typ.GetMethod('Entities');
  
  // We need to specialize the method. Delphi RTTI doesn't support invoking generic methods easily if not instantiated.
  // BUT, we can use TActivator to create TDbSet<T> directly!
  
  // Actually, we can just create TDbSet<T> via RTTI if we know the type.
  // Let's use a helper or TActivator if it supports it.
  // Since TDbSet<T> constructor takes IDbContext, we can create it.
  
  // Simplified approach: We can't easily create generic types at runtime without some registration or heavy RTTI hacks.
  // Constraint: For this phase, let's assume we can only get Set if it was already created OR
  // we implement a non-generic way to create it.
  
  // Let's try to find the generic type TDbSet<T>
  // This is getting complicated for "Basic".
  
  // Alternative: The user calls Entities<T> which registers it.
  // But Hydrate needs it automatically.
  
  // Let's throw error if not found for now, or use a registry.
  // Better: Use TActivator to create TDbSet<T>.
  // TActivator.CreateInstance(TDbSet<T>) requires T.
  
  raise Exception.Create('Dynamic DbSet creation not fully implemented. Ensure Entities<T> is called once before using Set().');
end;

function TDbContext.Entities<T>: IDbSet<T>;
var
  TypeInfo: PTypeInfo;
  NewSet: IDbSet<T>;
begin
  TypeInfo := System.TypeInfo(T);
  
  if not FCache.ContainsKey(TypeInfo) then
  begin
    // Create the DbSet instance.
    NewSet := TDbSet<T>.Create(Self);
    FCache.Add(TypeInfo, NewSet);
  end;
  
  Result := IDbSet<T>(FCache[TypeInfo]);
end;

procedure TDbContext.EnsureCreated;
var
  SetIntf: IInterface;
  DbSet: IDbSet;
  SQL: string;
  Cmd: IDbCommand;
  CmdIntf: IInterface;
begin
  // Iterate over all registered DbSets in Cache
  // Note: This requires that Entities<T> has been called for all entities we want to create.
  // In a real app, we might want a way to scan and register all entities in a package/assembly.
  // For now, the user must register them (as done in the Demo).

  for SetIntf in FCache.Values do
  begin
    if Supports(SetIntf, IDbSet, DbSet) then
    begin
      SQL := DbSet.GenerateCreateTableScript;
      if SQL <> '' then
      begin
        // Execute creation script
        CmdIntf := FConnection.CreateCommand(SQL);
        Cmd := IDbCommand(CmdIntf);
        Cmd.ExecuteNonQuery;
      end;
    end;
  end;
end;

end.
