unit Dext.Entity;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  System.Generics.Collections,
  Dext.Entity.Naming, // Add Naming unit
  Dext.Entity.Core,
  Dext.Entity.DbSet,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Dialects,
  Dext.Entity.Attributes,
  Dext.Entity.LazyLoading,
  Dext.Specifications.Interfaces,
  Dext.Specifications.Expression,
  Dext.Specifications.Types;

type
  /// <summary>
  ///   Concrete implementation of DbContext.
  TChangeTracker = class(TInterfacedObject, IChangeTracker)
  private
    FTrackedEntities: TDictionary<TObject, TEntityState>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Track(const AEntity: TObject; AState: TEntityState);
    function GetState(const AEntity: TObject): TEntityState;
    function HasChanges: Boolean;
    procedure AcceptAllChanges;
    procedure Clear;
    function GetTrackedEntities: TEnumerable<TPair<TObject, TEntityState>>;
  end;

  TDbContext = class;

  TCollectionEntry = class(TInterfacedObject, ICollectionEntry)
  private
    FContext: TDbContext;
    FParent: TObject;
    FPropName: string;
  public
    constructor Create(AContext: TDbContext; AParent: TObject; const APropName: string);
    procedure Load;
  end;

  TReferenceEntry = class(TInterfacedObject, IReferenceEntry)
  private
    FContext: TDbContext;
    FParent: TObject;
    FPropName: string;
  public
    constructor Create(AContext: TDbContext; AParent: TObject; const APropName: string);
    procedure Load;
  end;

  TEntityEntry = class(TInterfacedObject, IEntityEntry)
  private
    FContext: TDbContext;
    FEntity: TObject;
  public
    constructor Create(AContext: TDbContext; AEntity: TObject);
    function Collection(const APropName: string): ICollectionEntry;
    function Reference(const APropName: string): IReferenceEntry;
  end;

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
    FNamingStrategy: INamingStrategy; // Add Naming Strategy field
    FTransaction: IDbTransaction;
    FCache: TDictionary<PTypeInfo, IInterface>; // Cache for DbSets
    FChangeTracker: IChangeTracker;
  protected
    // IDbContext Implementation
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    
  public
    constructor Create(AConnection: IDbConnection; ADialect: ISQLDialect; ANamingStrategy: INamingStrategy = nil);
    destructor Destroy; override;
    
    function Connection: IDbConnection;
    function Dialect: ISQLDialect;
    function NamingStrategy: INamingStrategy; // Expose Naming Strategy
    
    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;
    function InTransaction: Boolean;
    
    /// <summary>
    ///   Access the DbSet for a specific entity type.
    /// </summary>
    function DataSet(AEntityType: PTypeInfo): IDbSet;
    procedure EnsureCreated;
    
    function SaveChanges: Integer;
    procedure Clear;
    function ChangeTracker: IChangeTracker;
    
    /// <summary>
    ///   Access the DbSet for a specific entity type.
    /// </summary>
    function Entities<T: class>: IDbSet<T>;
    
    function Entry(const AEntity: TObject): IEntityEntry;
  end;

implementation

{ TDbContext }

constructor TDbContext.Create(AConnection: IDbConnection; ADialect: ISQLDialect; ANamingStrategy: INamingStrategy = nil);
begin
  inherited Create;
  FConnection := AConnection;
  FDialect := ADialect;
  if ANamingStrategy <> nil then
    FNamingStrategy := ANamingStrategy
  else
    FNamingStrategy := TDefaultNamingStrategy.Create; // Default
    
  FCache := TDictionary<PTypeInfo, IInterface>.Create;
  FChangeTracker := TChangeTracker.Create;
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

function TDbContext.NamingStrategy: INamingStrategy;
begin
  Result := FNamingStrategy;
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

function TDbContext.InTransaction: Boolean;
begin
  Result := FTransaction <> nil;
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

function TDbContext.SaveChanges: Integer;
var
  Pair: TPair<TObject, TEntityState>;
  Entity: TObject;
  DbSet: IDbSet;
  TrackedEntities: TList<TPair<TObject, TEntityState>>;
begin
  Result := 0;
  if not FChangeTracker.HasChanges then Exit;

  // Snapshot of tracked entities to avoid modification during iteration if needed
  // (Though we are iterating, and Persist methods shouldn't modify the list structure, 
  // but they might update state to Unchanged if we did it per item. 
  // Here we accept all changes at the end.)
  
  TrackedEntities := TList<TPair<TObject, TEntityState>>.Create;
  try
    TrackedEntities.AddRange(FChangeTracker.GetTrackedEntities);
    
    if not InTransaction then BeginTransaction;
    try
      // 1. Process Inserts
      for Pair in TrackedEntities do
      begin
        if Pair.Value = esAdded then
        begin
          Entity := Pair.Key;
          DbSet := DataSet(Entity.ClassInfo); // Assuming ClassInfo is PTypeInfo
          DbSet.PersistAdd(Entity);
          Inc(Result);
        end;
      end;
      
      // 2. Process Updates
      for Pair in TrackedEntities do
      begin
        if Pair.Value = esModified then
        begin
          Entity := Pair.Key;
          DbSet := DataSet(Entity.ClassInfo);
          DbSet.PersistUpdate(Entity);
          Inc(Result);
        end;
      end;
      
      // 3. Process Deletes
      for Pair in TrackedEntities do
      begin
        if Pair.Value = esDeleted then
        begin
          Entity := Pair.Key;
          DbSet := DataSet(Entity.ClassInfo);
          DbSet.PersistRemove(Entity);
          Inc(Result);
        end;
      end;
      
      Commit;
      FChangeTracker.AcceptAllChanges;
    except
      Rollback;
      raise;
    end;
  finally
    TrackedEntities.Free;
  end;
end;

procedure TDbContext.Clear;
var
  SetIntf: IInterface;
  DbSet: IDbSet;
begin
  // Clear Change Tracker
  FChangeTracker.Clear;
  
  // Clear Identity Map of all cached DbSets
  for SetIntf in FCache.Values do
  begin
    if Supports(SetIntf, IDbSet, DbSet) then
    begin
      DbSet.Clear;
    end;
  end;
end;

function TDbContext.ChangeTracker: IChangeTracker;
begin
  Result := FChangeTracker;
end;

function TDbContext.Entry(const AEntity: TObject): IEntityEntry;
begin
  Result := TEntityEntry.Create(Self, AEntity);
end;



{ TChangeTracker }

constructor TChangeTracker.Create;
begin
  inherited Create;
  FTrackedEntities := TDictionary<TObject, TEntityState>.Create;
end;

destructor TChangeTracker.Destroy;
begin
  FTrackedEntities.Free;
  inherited;
end;

procedure TChangeTracker.Track(const AEntity: TObject; AState: TEntityState);
begin
  FTrackedEntities.AddOrSetValue(AEntity, AState);
end;

function TChangeTracker.GetState(const AEntity: TObject): TEntityState;
begin
  if not FTrackedEntities.TryGetValue(AEntity, Result) then
    Result := esDetached;
end;

function TChangeTracker.HasChanges: Boolean;
var
  State: TEntityState;
begin
  Result := False;
  for State in FTrackedEntities.Values do
  begin
    if State in [esAdded, esModified, esDeleted] then
      Exit(True);
  end;
end;

procedure TChangeTracker.AcceptAllChanges;
var
  Keys: TArray<TObject>;
  Entity: TObject;
  State: TEntityState;
begin
  // Remove Deleted entities
  // Set Added/Modified to Unchanged
  
  // Cannot modify dictionary while iterating
  Keys := FTrackedEntities.Keys.ToArray;
  
  for Entity in Keys do
  begin
    State := FTrackedEntities[Entity];
    if State = esDeleted then
      FTrackedEntities.Remove(Entity)
    else if State in [esAdded, esModified] then
      FTrackedEntities[Entity] := esUnchanged;
  end;
end;

procedure TChangeTracker.Clear;
begin
  FTrackedEntities.Clear;
end;

function TChangeTracker.GetTrackedEntities: TEnumerable<TPair<TObject, TEntityState>>;
begin
  Result := FTrackedEntities;
end;

{ TCollectionEntry }

constructor TCollectionEntry.Create(AContext: TDbContext; AParent: TObject; const APropName: string);
begin
  inherited Create;
  FContext := AContext;
  FParent := AParent;
  FPropName := APropName;
end;

procedure TCollectionEntry.Load;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  Val: TValue;
begin
  Ctx := TRttiContext.Create;
  Typ := Ctx.GetType(FParent.ClassType);
  Prop := Typ.GetProperty(FPropName);
  if Prop = nil then
    raise Exception.CreateFmt('Property %s not found on %s', [FPropName, Typ.Name]);

  Val := Prop.GetValue(Pointer(FParent));
  if Val.IsEmpty or (Val.AsObject = nil) then
    raise Exception.Create('Collection must be initialized before loading.');
    
  // Determine Child Type
  var ListObj := Val.AsObject;
  var AddMethod := Ctx.GetType(ListObj.ClassType).GetMethod('Add');
  if AddMethod = nil then
    raise Exception.Create('Collection does not have Add method');
    
  var ChildType := AddMethod.GetParameters[0].ParamType;
  var ChildClass := ChildType.AsInstance.MetaclassType;
  
  // Find DbSet for ChildClass
  var DbSet := FContext.DataSet(ChildClass.ClassInfo);
  
  // Find Parent PK
  var ParentPKProp := Typ.GetProperty('Id'); // Simplified
  if ParentPKProp = nil then raise Exception.Create('PK Id not found on parent');
  var ParentPKVal := ParentPKProp.GetValue(Pointer(FParent));
  
  // Find FK on Child pointing to Parent
  var FKName := '';
  var ChildTyp := Ctx.GetType(ChildClass);
  var CProp: TRttiProperty;
  var Attr: TCustomAttribute;
  
  for CProp in ChildTyp.GetProperties do
  begin
    if CProp.PropertyType.Handle = Typ.Handle then // Found property of Parent type
    begin
       // Check for ForeignKey attribute
       for Attr in CProp.GetAttributes do
         if Attr is ForeignKeyAttribute then
         begin
           FKName := ForeignKeyAttribute(Attr).ColumnName;
           Break;
         end;
       if FKName <> '' then Break;
    end;
  end;
  
  // If not found via attribute, try convention 'ParentClassNameId'
  if FKName = '' then
  begin
    // Try 'UserId' if parent is TUser
    var Candidate := Typ.Name.Substring(1) + 'Id'; // TUser -> User + Id
    if ChildTyp.GetProperty(Candidate) <> nil then
      FKName := Candidate;
  end;
  
  if FKName = '' then
    raise Exception.CreateFmt('Could not determine Foreign Key for collection %s', [FPropName]);
  
  // IMPORTANT: FKName is the property name, we need to convert to column name!
  var FKProp := ChildTyp.GetProperty(FKName);
  if FKProp <> nil then
  begin
    // Check if property has [Column] attribute
    for Attr in FKProp.GetAttributes do
    begin
      if Attr is ColumnAttribute then
      begin
        FKName := ColumnAttribute(Attr).Name;
        Break;
      end;
    end;
  end;
  
  // Clear the collection before loading to ensure it reflects current DB state
  var ClearMethod := Ctx.GetType(ListObj.ClassType).GetMethod('Clear');
  if ClearMethod <> nil then
    ClearMethod.Invoke(ListObj, []);
    
  // Build Query: Child.FK = Parent.Id
  var Expr := TBinaryExpression.Create(
    FKName,
    boEqual,
    ParentPKVal
  );
  
  var Results := DbSet.ListObjects(Expr);
  try
    // Add results to ListObj
    for var ChildObj in Results do
    begin
      AddMethod.Invoke(ListObj, [ChildObj]);
    end;
  finally
    Results.Free;
  end;
end;

{ TReferenceEntry }

constructor TReferenceEntry.Create(AContext: TDbContext; AParent: TObject; const APropName: string);
begin
  inherited Create;
  FContext := AContext;
  FParent := AParent;
  FPropName := APropName;
end;

procedure TReferenceEntry.Load;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  ChildType: TRttiType;
  ChildClass: TClass;
  DbSet: IDbSet;
  FKProp: TRttiProperty;
  FKVal: TValue;
  FKName: string;
  ChildObj: TObject;
  Attr: TCustomAttribute;
begin
  Ctx := TRttiContext.Create;
  Typ := Ctx.GetType(FParent.ClassType);
  Prop := Typ.GetProperty(FPropName);
  if Prop = nil then
    raise Exception.CreateFmt('Property %s not found on %s', [FPropName, Typ.Name]);

  ChildType := Prop.PropertyType;
  if ChildType.TypeKind <> tkClass then
    raise Exception.Create('Reference property must be a class');
    
  ChildClass := ChildType.AsInstance.MetaclassType;
  DbSet := FContext.DataSet(ChildClass.ClassInfo);
  
  // Find FK Property on Parent
  // Look for [ForeignKey] on Prop
  FKName := '';
  for Attr in Prop.GetAttributes do
    if Attr is ForeignKeyAttribute then
    begin
      FKName := ForeignKeyAttribute(Attr).ColumnName;
      Break;
    end;
    
  if FKName = '' then
  begin
    // Convention: PropName + 'Id'
    FKName := FPropName + 'Id';
  end;
  
  FKProp := Typ.GetProperty(FKName);
  if FKProp = nil then
    raise Exception.CreateFmt('Foreign Key property %s not found for reference %s', [FKName, FPropName]);
    
  FKVal := FKProp.GetValue(Pointer(FParent));
  if FKVal.IsEmpty or (FKVal.AsInteger = 0) then Exit; // No FK, nothing to load
  
  // Find Child
  WriteLn('DEBUG: Loading Reference ' + FPropName + ' FK=' + FKVal.ToString);
  ChildObj := DbSet.FindObject(FKVal.AsVariant);
  WriteLn('DEBUG: ChildObj found: ' + BoolToStr(ChildObj <> nil, True));
  
  if ChildObj <> nil then
  begin
    WriteLn('DEBUG: Setting Value type: ' + ChildObj.ClassName);
    // The TClassToClassConverter will handle the conversion from TObject to TAddress
    Prop.SetValue(Pointer(FParent), ChildObj);
    WriteLn('DEBUG: Value Set');
  end;
end;





{ TEntityEntry }

constructor TEntityEntry.Create(AContext: TDbContext; AEntity: TObject);
begin
  inherited Create;
  FContext := AContext;
  FEntity := AEntity;
end;

function TEntityEntry.Collection(const APropName: string): ICollectionEntry;
begin
  Result := TCollectionEntry.Create(FContext, FEntity, APropName);
end;

function TEntityEntry.Reference(const APropName: string): IReferenceEntry;
begin
  Result := TReferenceEntry.Create(FContext, FEntity, APropName);
end;

end.
