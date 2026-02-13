{***************************************************************************}
{                                                                           }
{           Dext Framework                                                  }
{                                                                           }
{           Copyright (C) 2025 Cesar Romero & Dext Contributors             }
{                                                                           }
{           Licensed under the Apache License, Version 2.0 (the "License"); }
{           you may not use this file except in compliance with the License.}
{           You may obtain a copy of the License at                         }
{                                                                           }
{               http://www.apache.org/licenses/LICENSE-2.0                  }
{                                                                           }
{           Unless required by applicable law or agreed to in writing,      }
{           software distributed under the License is distributed on an     }
{           "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    }
{           either express or implied. See the License for the specific     }
{           language governing permissions and limitations under the        }
{           License.                                                        }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Author:  Cesar Romero                                                    }
{  Created: 2025-12-08                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.Entity.Context;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  System.Generics.Collections,
  System.Generics.Defaults,
  Dext.Entity.Naming,
  Dext.Entity.Mapping,
  Dext.Entity.Core,
  Dext.Entity.DbSet,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Dialects,
  Dext.Entity.Setup,
  Dext.Entity.Tenancy,
  Dext.MultiTenancy,
  Dext.Entity.Attributes,
  Dext.Entity.LazyLoading,
  Dext.Specifications.Interfaces,
  Dext,
  Dext.Entity.TypeSystem,
  Dext.Specifications.Fluent,
  Dext.Specifications.Types,
  Dext.Core.Reflection;

type
  TFluentExpression = Dext.Specifications.Types.TFluentExpression;
  
  // TypeSystem
  TPropertyInfo = Dext.Entity.TypeSystem.TPropertyInfo;

  /// <summary>
  ///   Concrete implementation of DbContext.
  TChangeTracker = class(TInterfacedObject, IChangeTracker)
  private
    FTrackedEntities: TDictionary<TObject, TEntityState>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Track(const AEntity: TObject; AState: TEntityState);
    procedure Remove(const AEntity: TObject);
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
    constructor Create(const AContext: TDbContext; const AParent: TObject; const APropName: string);
    procedure Load;
  end;

  TReferenceEntry = class(TInterfacedObject, IReferenceEntry)
  private
    FContext: TDbContext;
    FParent: TObject;
    FPropName: string;
  public
    constructor Create(const AContext: TDbContext; const AParent: TObject; const APropName: string);
    procedure Load;
  end;

  TEntityEntry = class(TInterfacedObject, IEntityEntry)
  private
    FContext: TDbContext;
    FEntity: TObject;
  public
    constructor Create(const AContext: TDbContext; const AEntity: TObject);
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
    FNamingStrategy: INamingStrategy;
    FModelBuilder: TModelBuilder; // Model Builder
    FOwnsModelBuilder: Boolean;
    FTransaction: IDbTransaction;
    FCache: TDictionary<PTypeInfo, IInterface>; // Cache for DbSets
    FChangeTracker: IChangeTracker;
    FTenantProvider: ITenantProvider;
    FTenantConfigApplied: Boolean;
    FLastAppliedTenantId: string;
    FOnLog: TProc<string>;
    procedure SetOnLog(const AValue: TProc<string>);
    function GetOnLog: TProc<string>;
    procedure ApplyTenantConfig(ACreateSchema: Boolean = False);
    function GetModelBuilder: TModelBuilder;
  protected
    // IDbContext Implementation
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    
    /// <summary>
    ///   Override this method to configure the model (Fluent Mapping).
    /// </summary>
    procedure OnModelCreating(Builder: TModelBuilder); virtual;
    
    /// <summary>
    ///   Override this method to configure the database context (e.g. Connection String, Naming Strategy).
    ///   This is called during constructor execution.
    /// </summary>
    procedure OnConfiguring(Options: TDbContextOptions); virtual;
    
  public
    class var FModelCache: TObjectDictionary<TClass, TModelBuilder>;
    class var FCriticalSection: TObject; // For thread safety
    
    constructor Create(const AConnection: IDbConnection; const ADialect: ISQLDialect = nil; const ANamingStrategy: INamingStrategy = nil; const ATenantProvider: ITenantProvider = nil); overload;
    constructor Create(const AOptions: TDbContextOptions; const ATenantProvider: ITenantProvider = nil); overload;
    destructor Destroy; override;
    
    class constructor Create;
    class destructor Destroy;
    
    function Connection: IDbConnection;
    function Dialect: ISQLDialect;
    function NamingStrategy: INamingStrategy;
    function ModelBuilder: TModelBuilder; // Expose ModelBuilder
    function GetTenantProvider: ITenantProvider;
    
    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;
    function InTransaction: Boolean;
    
    /// <summary>
    ///   Access the DbSet for a specific entity type.
    /// </summary>
    function DataSet(AEntityType: PTypeInfo): IDbSet;

    /// <summary>
    /// Preload the DBSets cache to avoid having to manually call Entities<T> for
    ///  each type which are already exposed as properties on the current TDBContext
    ///  implementation.
    /// </summary>
    procedure PreloadDbSets;
    function EnsureCreated: Boolean;
    procedure ExecuteSchemaSetup;
    
    function SaveChanges: Integer;
    procedure Clear;
    procedure DetachAll;
    procedure Detach(const AEntity: TObject);
    function ChangeTracker: IChangeTracker;
    
    function GetMapping(AType: PTypeInfo): TObject;
    
    /// <summary>
    ///   Access the DbSet for a specific entity type.
    /// </summary>
    function Entities<T: class>: IDbSet<T>;
    
    function Entry(const AEntity: TObject): IEntityEntry;
    
    property OnLog: TProc<string> read GetOnLog write SetOnLog;
  end;

implementation

uses
  Dext.Utils,
  Dext.Entity.Validator;

{ Helper Functions }

/// <summary>
///   Unwraps Nullable<T> values and validates if FK is valid (non-zero for integers, non-empty for strings)
/// </summary>
function TryUnwrapAndValidateFK(var AValue: TValue; AContext: TRttiContext): Boolean;
var
  RType: TRttiType;
  TypeName: string;
  Fields: TArray<TRttiField>;
  HasValueField, ValueField: TRttiField;
  HasValue: Boolean;
  Instance: Pointer;
begin
  Result := False;
  
  // Handle Nullable<T> unwrapping
  if AValue.Kind = tkRecord then
  begin
    RType := AContext.GetType(AValue.TypeInfo);
    if RType <> nil then
    begin
      TypeName := RType.Name;
      
      // Check if it's a Nullable<T> by name (Delphi doesn't generate RTTI for generic record properties)
      if TypeName.StartsWith('Nullable<') or TypeName.StartsWith('TNullable') then
      begin
        // Access fields directly since GetProperty won't work for generic records
        Fields := RType.GetFields;
        HasValueField := nil;
        ValueField := nil;
        
        // Find fHasValue and fValue fields
        for var Field in Fields do
        begin
          if Field.Name.ToLower.Contains('hasvalue') then
            HasValueField := Field
          else if Field.Name.ToLower = 'fvalue' then
            ValueField := Field;
        end;
        
        if (HasValueField <> nil) and (ValueField <> nil) then
        begin
          Instance := AValue.GetReferenceToRawData;
          
          // Check HasValue - it can be a string (Spring4D) or Boolean
          var HasValueVal := HasValueField.GetValue(Instance);
          if HasValueVal.Kind = tkUString then
            HasValue := HasValueVal.AsString <> ''
          else if HasValueVal.Kind = tkEnumeration then
            HasValue := HasValueVal.AsBoolean
          else
            HasValue := False;
            
          if not HasValue then Exit; // Null, nothing to load
          
          // Get the actual value
          AValue := ValueField.GetValue(Instance);
        end
        else
          Exit; // Couldn't find fields, treat as invalid
      end;
    end;
  end;

  if AValue.IsEmpty then Exit;

  // Validate based on type
  if AValue.Kind in [tkInteger, tkInt64] then
    Result := AValue.AsInt64 <> 0
  else if AValue.Kind in [tkString, tkUString, tkWString, tkLString] then
    Result := AValue.AsString <> ''
  else
    Result := True; // For other types like GUID, assume valid if not empty
end;


{ TDbContext }

type
  TEntityNode = class
  public
    TypeInfo: PTypeInfo;
    DbSet: IDbSet;
    Dependencies: TList<PTypeInfo>;
    constructor Create;
    destructor Destroy; override;
  end;

const
  // Key for caching based on class type
  CACHE_KEY = 'Model';

class constructor TDbContext.Create;
begin
  FModelCache := TObjectDictionary<TClass, TModelBuilder>.Create([doOwnsValues]);
  FCriticalSection := TObject.Create;
end;

class destructor TDbContext.Destroy;
begin
  FreeAndNil(FModelCache);
  FreeAndNil(FCriticalSection);
end;

constructor TDbContext.Create(const AConnection: IDbConnection; const ADialect: ISQLDialect; const ANamingStrategy: INamingStrategy; const ATenantProvider: ITenantProvider);
begin
  inherited Create;
  FConnection := AConnection;
  
  if ADialect <> nil then
    FDialect := ADialect
  else if (FConnection <> nil) and (FConnection.Dialect <> ddUnknown) then
    FDialect := TDialectFactory.CreateDialect(FConnection.Dialect)
  else
    FDialect := nil; // Will likely cause issues if query generation is attempted without dialect

  if FDialect = nil then
  begin
     // Optional: Raise warning or default to generic?
     // We leave it nil, it might be set later or cause runtime error if used.
  end;
  
  if ANamingStrategy <> nil then
    FNamingStrategy := ANamingStrategy
  else
    FNamingStrategy := TDefaultNamingStrategy.Create; // Default
    
  FCache := TDictionary<PTypeInfo, IInterface>.Create;
  FChangeTracker := TChangeTracker.Create;
  FTenantProvider := ATenantProvider;
  FTenantConfigApplied := False;
  
  // Model Caching Logic
  System.TMonitor.Enter(FCriticalSection);
  try
    if not FModelCache.TryGetValue(Self.ClassType, FModelBuilder) then
    begin
      FModelBuilder := TModelBuilder.Create;
      // Initialize Model
      OnModelCreating(FModelBuilder);
      
      FModelCache.Add(Self.ClassType, FModelBuilder);
    end;
    // We reuse the cached builder. Do NOT own it.
    FOwnsModelBuilder := False;
  finally
    System.TMonitor.Exit(FCriticalSection);
  end;

  PreloadDbSets;
end;

constructor TDbContext.Create(const AOptions: TDbContextOptions;
  const ATenantProvider: ITenantProvider);
begin
  // Allow derived classes to configure options (e.g. Naming Strategy)
  OnConfiguring(AOptions);
  
  Self.Create(AOptions.BuildConnection, AOptions.BuildDialect, AOptions.BuildNamingStrategy, ATenantProvider);
end;

procedure TDbContext.OnConfiguring(Options: TDbContextOptions);
begin
  // Virtual method implies do nothing by default
end;

destructor TDbContext.Destroy;
begin
  // Clear ChangeTracker before freeing DbSets (which free entities).
  // This prevents ChangeTracker from holding dangling pointers during its destruction.
  if FChangeTracker <> nil then
    FChangeTracker.Clear;
    
  FCache.Free;
  if FOwnsModelBuilder then
    FModelBuilder.Free;
  inherited;
end;

procedure TDbContext.SetOnLog(const AValue: TProc<string>);
begin
  FOnLog := AValue;
  if FConnection <> nil then
    FConnection.OnLog := AValue;
end;

function TDbContext.GetOnLog: TProc<string>;
begin
  Result := FOnLog;
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

function TDbContext.GetModelBuilder: TModelBuilder;
begin
  Result := FModelBuilder;
end;

function TDbContext.ModelBuilder: TModelBuilder;
begin
  Result := FModelBuilder;
end;

function TDbContext.GetTenantProvider: ITenantProvider;
begin
  Result := FTenantProvider;
end;

procedure TDbContext.OnModelCreating(Builder: TModelBuilder);
begin
  // Default implementation does nothing.
  // Override this in your derived context to configure mappings.
end;

procedure TDbContext.PreloadDbSets;
var
  ctx : TRttiContext;
  typ : TRttiType;
  props :  TArray<TRttiProperty>;
  Prop : TRttiProperty;
begin
   Ctx := TRttiContext.Create;
   try
     Typ := Ctx.GetType(self.classtype);
     Props := Typ.GetProperties;
     for Prop in Props do
     begin
       // ignore any properties that are [NotMapped].
       if Prop.HasAttribute<NotMappedAttribute> then
         continue;

       // assume that properties that hold an interface are likely models (IDbSet<T>)
       if Prop.IsReadable and (Prop.PropertyType.TypeKind = tkInterface) then
       begin
         // Capture the property name for naming discovery (e.g. "Customers" -> "customers")
         // We need to find the generic type T from IDbSet<T>
         var IntfType := Prop.PropertyType;
         if IntfType.Name.StartsWith('IDbSet<') then
         begin
            // This is a simple heuristic. Better: check GUID or use RTTI to find the entity type.
            // But usually, the property name is exactly what we want as a table name placeholder.
            
            // Try to extract T from IDbSet<T> RTTI if possible, 
            // but even simpler: when the getter is called by Prop.GetValue(Self), 
            // Entities<T> is called, which registers the type. 
            // We just need to associate this specific property name with the type T.
            
            // Invoke the getter to ensure Entities<T> is called and the DbSet is created/cached
            var Val := Prop.GetValue(Self);
            
            // After invocation, the DbSet for T is in FCache. 
            // We can try to peek into the IDbSet to see its Entity Type, 
            // or we use a more robust way to register the association.
            
            var DbSet: IDbSet;
            if (not Val.IsEmpty) and Supports(Val.AsInterface, IDbSet, DbSet) then
            begin
               // Register this property name as the "Discovery Name" for this entity type
               // TModelBuilder will use it if no explicit table name is provided.
               TModelBuilder.Instance.RegisterDiscoveryName(DbSet.EntityType, Prop.Name);
            end;
         end;
       end;
     end;
   finally
      Ctx.free;
   end;
end;

function TDbContext.GetMapping(AType: PTypeInfo): TObject;
begin
  if FModelBuilder.HasMap(AType) then
    Result := FModelBuilder.GetMap(AType)
  else
    Result := nil;
end;

procedure TDbContext.ApplyTenantConfig(ACreateSchema: Boolean);
var
  Sql: string;
  CurrentTenantId: string;
begin
  CurrentTenantId := '';
  if (FTenantProvider <> nil) and (FTenantProvider.Tenant <> nil) then
    CurrentTenantId := FTenantProvider.Tenant.Id;

  if (FLastAppliedTenantId = CurrentTenantId) and FTenantConfigApplied and (not ACreateSchema) then Exit;
  
  if (FTenantProvider <> nil) and (FTenantProvider.Tenant <> nil) then
  begin
    // 1. Handle Schema-based tenancy
    if FTenantProvider.Tenant.Schema <> '' then
    begin
      if ACreateSchema then
      begin
        // Sql := FDialect.GetCreateSchemaSQL(FTenantProvider.Tenant.Schema);
        Sql := '';
        if Sql <> '' then
        begin
          var Cmd := FConnection.CreateCommand(Sql);
          Cmd.ExecuteNonQuery;
        end;
      end;

      // Sql := FDialect.GetSetSchemaSQL(FTenantProvider.Tenant.Schema);
      Sql := '';
      if Sql <> '' then
      begin
        var Cmd := FConnection.CreateCommand(Sql);
        Cmd.ExecuteNonQuery;
      end;
    end;

    // 2. Handle Connection-based tenancy (Tenant per Database)
    if FTenantProvider.Tenant.ConnectionString <> '' then
    begin
      if FConnection.ConnectionString <> FTenantProvider.Tenant.ConnectionString then
      begin
        FConnection.ConnectionString := FTenantProvider.Tenant.ConnectionString;
        // If we are already connected, we need to reconnect with the new string
        if FConnection.IsConnected then
        begin
          FConnection.Disconnect;
          FConnection.Connect;
        end;
      end;
    end;
  end;
  
  FLastAppliedTenantId := CurrentTenantId;
  FTenantConfigApplied := True;
end;

procedure TDbContext.ExecuteSchemaSetup;
begin
  ApplyTenantConfig(False);
end;

procedure TDbContext.BeginTransaction;
begin
  ApplyTenantConfig(False);
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
  Ctx: TRttiContext;
begin
  ApplyTenantConfig;
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
    NewSet := TDbSet<T>.Create(IDbContext(Self));
    FCache.Add(TypeInfo, NewSet);
  end;
  
  Result := IDbSet<T>(FCache[TypeInfo]);
end;

function TDbContext.EnsureCreated: Boolean;
var
  Nodes: TObjectList<TEntityNode>;
  Created: TList<PTypeInfo>;
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  Attr: TCustomAttribute;
  Node: TEntityNode;
  Pair: TPair<PTypeInfo, IInterface>;
  DbSet: IDbSet;
  SQL: string;
  Cmd: IDbCommand;
  CmdIntf: IInterface;
  HasProgress, CanCreate: Boolean;
  i: Integer;
begin
  Result := False;
  ApplyTenantConfig(True);
  Nodes := TObjectList<TEntityNode>.Create;
  Created := TList<PTypeInfo>.Create;
  if fCache.count = 0 then
    PreloadDBSets;
  Ctx := TRttiContext.Create;
  try
    // 1. Build Dependency Graph
    for Pair in FCache do
    begin
      if not Supports(Pair.Value, IDbSet, DbSet) then Continue;
      
      Node := TEntityNode.Create;
      Node.TypeInfo := Pair.Key;
      Node.DbSet := DbSet;
      Nodes.Add(Node);
      
      // Analyze Dependencies
      Typ := Ctx.GetType(Pair.Key);
      if Typ = nil then Continue;
      
      for Prop in Typ.GetProperties do
      begin
        for Attr in Prop.GetAttributes do
        begin
          if Attr is ForeignKeyAttribute then
          begin
            // Found a FK. Check if the property type is a class we manage.
            if Prop.PropertyType.TypeKind = tkClass then
            begin
               var DepType := Prop.PropertyType.Handle;
               // Only add dependency if it's in our Cache (managed entity)
               if FCache.ContainsKey(DepType) and (DepType <> Pair.Key) then // Avoid self-dependency
               begin
                 if not Node.Dependencies.Contains(DepType) then
                   Node.Dependencies.Add(DepType);
               end;
            end;
          end;
        end;
      end;
    end;
    
    // 2. Topological Sort / Execution
    while Nodes.Count > 0 do
    begin
      HasProgress := False;
      
      for i := Nodes.Count - 1 downto 0 do
      begin
        Node := Nodes[i];
        CanCreate := True;
        
        // Check if all dependencies are created
        for var Dep in Node.Dependencies do
        begin
          if not Created.Contains(Dep) then
          begin
            CanCreate := False;
            Break;
          end;
        end;
        
        if CanCreate then
        begin
          // Execute Creation
          SQL := Node.DbSet.GenerateCreateTableScript;
          if SQL <> '' then
          begin
            // Check if table exists first (to support databases without IF NOT EXISTS like Firebird)
            // Note: GenerateCreateTableScript returns "CREATE TABLE Name ...". We need to extract the name.
            // But we already have the name in the Node/DbSet metadata.
            // Actually, we can just use FConnection.TableExists(TableName)
            
            var TableName := '';
            var Mapping := GetMapping(Node.TypeInfo);
            var MapTableName := '';
            
            if Mapping <> nil then
              MapTableName := Dext.Entity.Mapping.TEntityMap(Mapping).TableName;
              
            if MapTableName <> '' then
            begin
              TableName := MapTableName;
            end
            else
            begin
              // Fallback to Attribute or Naming Strategy if Mapping doesn't specify TableName
              var RContext := TRttiContext.Create;
              try
                var RType := RContext.GetType(Node.TypeInfo);
                if RType <> nil then
                begin
                  var TableAttr := RType.GetAttribute<TableAttribute>;
                  if TableAttr <> nil then
                    TableName := TableAttr.Name;
                end;
              finally
                RContext.Free;
              end;
              
              if TableName = '' then
                TableName := FNamingStrategy.GetTableName(Node.TypeInfo.TypeData.ClassType);
            end;
            
            // Quote identifier if needed (Dialect specific)
            // But TableExists expects the name as is (or handles quotes internally)
            // Let's pass the raw name first.
            
            if not FConnection.TableExists(TableName) then
            begin
              if Assigned(FOnLog) then
                FOnLog(SQL);

              try
                CmdIntf := FConnection.CreateCommand(SQL);
                Cmd := IDbCommand(CmdIntf);
                Cmd.ExecuteNonQuery;
                Result := True;
              except
                 on E: Exception do
                 begin
                   // Always log failed SQL if we have a logger
                   if Assigned(FOnLog) then
                     FOnLog('FAILED SQL: ' + SQL);
                 end;
              end;
            end;
          end;
          
          Created.Add(Node.TypeInfo);
          Nodes.Delete(i); // Remove from pending
          HasProgress := True;
        end;
      end;
      
      if not HasProgress then
      begin
        // Cycle detected or missing dependency.
        // For now, force create the remaining ones (might fail on FKs, but better than hanging)

        for i := Nodes.Count - 1 downto 0 do
        begin
           Node := Nodes[i];
           SQL := Node.DbSet.GenerateCreateTableScript;
            if SQL <> '' then
            begin
              try
                CmdIntf := FConnection.CreateCommand(SQL);
                Cmd := IDbCommand(CmdIntf);
                Cmd.ExecuteNonQuery;
              except
                 on E: Exception do; // Ignore errors in forced creation
              end;
            end;
           Nodes.Delete(i);
        end;
        Break;
      end;
    end;
    
  finally
    Created.Free;
    Nodes.Free;
    Ctx.Free;
  end;
end;

{ TEntityNode }

{ TEntityNode }

constructor TEntityNode.Create;
begin
  Dependencies := TList<PTypeInfo>.Create;
end;

destructor TEntityNode.Destroy;
begin
  Dependencies.Free;
  // inherited; // Removing inherited call to fix E2075
end;

function TDbContext.SaveChanges: Integer;
var
  Pair: TPair<TObject, TEntityState>;
  Entity: TObject;
  DbSet: IDbSet;
  TrackedEntities: TList<TPair<TObject, TEntityState>>;
begin
  ApplyTenantConfig(False);
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
      // 1. Process Inserts (Bulk Optimized)
      var AddedGroups := TDictionary<PTypeInfo, TList<TObject>>.Create;
      try
        for Pair in TrackedEntities do
        begin
          if Pair.Value = esAdded then
          begin
            Entity := Pair.Key;
            if not AddedGroups.ContainsKey(Entity.ClassInfo) then
              AddedGroups.Add(Entity.ClassInfo, TList<TObject>.Create);
            
             // Auto-populate TenantId if applicable (Security & Convenience)
             if (FTenantProvider <> nil) and (FTenantProvider.Tenant <> nil) then
             begin
               var TenantAware: ITenantAware;
               if Supports(Entity, ITenantAware, TenantAware) then
               begin
                  // Always enforce current tenant ID on insert
                  TenantAware.TenantId := FTenantProvider.Tenant.Id;
               end;
             end;
               

             // Validate Entity
             var Map: TEntityMap := nil;
             if FModelBuilder <> nil then
               Map := FModelBuilder.GetMap(Entity.ClassInfo);
               
             TEntityValidator.Validate(Entity, Map);

             AddedGroups[Entity.ClassInfo].Add(Entity);
          end;
        end;

        for var TypeInfo in AddedGroups.Keys do
        begin
          var List := AddedGroups[TypeInfo];
          DbSet := DataSet(TypeInfo);
          
          // Force loop to ensure IDs are retrieved for all entities.
          // Bulk Insert (PersistAddRange) does not currently support ID retrieval.
          for var Item in List do
            DbSet.PersistAdd(Item);
            
          Inc(Result, List.Count);
        end;
      finally
        for var List in AddedGroups.Values do List.Free;
        AddedGroups.Free;
      end;
      
      // 2. Process Updates
      for Pair in TrackedEntities do
      begin
        if Pair.Value = esModified then
        begin
          Entity := Pair.Key;
          
          // Validate Entity
          var Map: TEntityMap := nil;
          if FModelBuilder <> nil then
            Map := FModelBuilder.GetMap(Entity.ClassInfo);
            
          TEntityValidator.Validate(Entity, Map);

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
          
          // Remove from tracker BEFORE freeing the entity (via PersistRemove -> IdentityMap)
          // This prevents dangling pointers in the tracker.
          FChangeTracker.Remove(Entity);
          
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

procedure TDbContext.DetachAll;
var
  SetIntf: IInterface;
  DbSet: IDbSet;
begin
  // Clear Change Tracker (Stop tracking everything)
  FChangeTracker.Clear;
  
  // Detach all entities in all DbSets
  for SetIntf in FCache.Values do
  begin
    if Supports(SetIntf, IDbSet, DbSet) then
    begin
      DbSet.DetachAll;
    end;
  end;
end;

procedure TDbContext.Detach(const AEntity: TObject);
begin
  DataSet(AEntity.ClassInfo).Detach(AEntity);
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
  // Use a custom comparer that treats objects as pointers.
  // This avoids calling virtual methods (GetHashCode/Equals) on freed objects,
  // which can happen if an object is deleted (and freed by IdentityMap) before AcceptAllChanges.
  FTrackedEntities := TDictionary<TObject, TEntityState>.Create(
    TEqualityComparer<TObject>.Construct(
      function(const Left, Right: TObject): Boolean
      begin
        Result := Pointer(Left) = Pointer(Right);
      end,
      function(const Value: TObject): Integer
      begin
        Result := Integer(Pointer(Value));
      end
    )
  );
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

procedure TChangeTracker.Remove(const AEntity: TObject);
begin
  FTrackedEntities.Remove(AEntity);
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

constructor TCollectionEntry.Create(const AContext: TDbContext; const AParent: TObject; const APropName: string);
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
  ListObj: TObject;
  ListIntf: IInterface;
  IsInterface: Boolean;
  ListType: TRttiType;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(FParent.ClassType);
  Prop := Typ.GetProperty(FPropName);
  if Prop = nil then
    raise Exception.CreateFmt('Property %s not found on %s', [FPropName, Typ.Name]);

  Val := Prop.GetValue(Pointer(FParent));
  if Val.IsEmpty then
    raise Exception.Create('Collection must be initialized before loading.');
  
  // Detect if property is Interface or Class
  IsInterface := Prop.PropertyType.TypeKind = tkInterface;

  if IsInterface then
  begin
    // Handle IList<T> (interface)
    if not Val.TryAsType<IInterface>(ListIntf) or (ListIntf = nil) then
      raise Exception.Create('Collection interface is nil and must be initialized before loading.');
    
    // Get the interface type to find Add method
    ListType := Prop.PropertyType;
  end
  else
  begin
    // Handle TList<T> or TObjectList<T> (class)
    if not Val.TryAsType<TObject>(ListObj) or (ListObj = nil) then
      raise Exception.Create('Collection must be initialized before loading.');
    
    ListType := Ctx.GetType(ListObj.ClassType);
  end;
  
  // Find Add method
  var AddMethod := ListType.GetMethod('Add');
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
  var ClearMethod := ListType.GetMethod('Clear');
  if ClearMethod <> nil then
  begin
    if IsInterface then
      ClearMethod.Invoke(Val, [])
    else
      ClearMethod.Invoke(ListObj, []);
  end;
    
  // Build Query: Child.FK = Parent.Id
  var Expr := TBinaryExpression.Create(
    FKName,
    boEqual,
    ParentPKVal
  );
  
  var Results := DbSet.ListObjects(Expr);
    // Add results to collection
    for var ChildObj in Results do
    begin
      if IsInterface then
        AddMethod.Invoke(Val, [ChildObj])
      else
        AddMethod.Invoke(ListObj, [ChildObj]);
    end;
  finally
    Ctx.Free;
  end;
end;

{ TReferenceEntry }

constructor TReferenceEntry.Create(const AContext: TDbContext; const AParent: TObject; const APropName: string);
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
  
  // Unwrap Nullable<T> and validate FK value
  if not TryUnwrapAndValidateFK(FKVal, Ctx) then Exit;

  
  // Find Child
  ChildObj := DbSet.FindObject(FKVal.AsVariant);
  if ChildObj <> nil then
  begin
    // The TClassToClassConverter will handle the conversion from TObject to TAddress
    Prop.SetValue(Pointer(FParent), ChildObj);
  end;
end;

{ TEntityEntry }

constructor TEntityEntry.Create(const AContext: TDbContext; const AEntity: TObject);
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

