program TestTypeConvertersDb;

{$APPTYPE CONSOLE}
{$TYPEINFO ON}
{$METHODINFO ON}

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Phys.PG,
  FireDAC.Phys.PGDef,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  Dext,
  Dext.Collections,
  Dext.Types.UUID,
  Dext.Entity.Core,
  Dext.Entity.Attributes,
  Dext.Entity.Context,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.Mapping,
  Dext.Entity.TypeConverters,
  Dext.Entity.Dialects,
  Dext.Specifications.Interfaces,
  Dext.Specifications.Base;

type
  TUserRole = (urUser, urAdmin, urSuperAdmin);

  [Table('test_guid_entities')]
  TGuidEntity = class
  private
    FId: TGUID;
    FName: string;
  public
    [PK]
    property Id: TGUID read FId write FId;
    [Column('name')]
    property Name: string read FName write FName;
  end;

  // TUUID Entity - Tests RFC 9562 compliant UUID handling
  [Table('test_uuid_entities')]
  TUuidEntity = class
  private
    FId: TUUID;
    FName: string;
  public
    [PK]
    property Id: TUUID read FId write FId;
    [Column('name')]
    property Name: string read FName write FName;
  end;

  // NOTE: TAutoGuidEntity is for PostgreSQL-specific test (gen_random_uuid())
  // Uncomment to test auto-generated UUID endianness with PostgreSQL.
  (*
  [Table('test_autoguid_entities')]
  TAutoGuidEntity = class
  private
    FId: TGUID;
    FName: string;
  public
    [PK, AutoInc, Column('id')]
    property Id: TGUID read FId write FId;
    [Column('name')]
    property Name: string read FName write FName;
  end;
  *)

  // Composite Key: GUID + Integer
  [Table('test_composite_guid_int')]
  TCompositeGuidInt = class
  private
    FGuidKey: TGUID;
    FIntKey: Integer;
    FData: string;
  public
    [PK]
    property GuidKey: TGUID read FGuidKey write FGuidKey;
    [PK]
    [Column('int_key')]
    property IntKey: Integer read FIntKey write FIntKey;
    [Column('data')]
    property Data: string read FData write FData;
  end;

  // Composite Key: Integer + DateTime
  [Table('test_composite_int_datetime')]
  TCompositeIntDateTime = class
  private
    FId: Integer;
    FTimestamp: TDateTime;
    FValue: string;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    [PK]
    [Column('timestamp')]
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
    [Column('value')]
    property Value: string read FValue write FValue;
  end;

  [Table('test_enum_entities')]
  TEnumEntity = class
  private
    FId: Integer;
    FRole: TUserRole;
    FStatus: TUserRole;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    [Column('role_int')]
    property Role: TUserRole read FRole write FRole;
    [Column('role_name'), EnumAsString]
    property Status: TUserRole read FStatus write FStatus;
  end;

  {$M+}
  {$RTTI EXPLICIT FIELDS([vcPrivate, vcPublic]) PROPERTIES([vcPublic, vcPublished])}
  TJsonMetadata = class
  private
    FName: string;
    FValue: Integer;
  public
    property Name: string read FName write FName;
    property Value: Integer read FValue write FValue;
  end;

  [Table('test_json_entities')]
  TJsonEntity = class
  private
    FId: Integer;
    FMetadata: TJsonMetadata;
  public
    constructor Create;
    destructor Destroy; override;
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    [Column('metadata')]
    property Metadata: TJsonMetadata read FMetadata write FMetadata;
  end;

  TTestDbContext = class(TDbContext)
  private
    function GetGuidEntities: IDbSet<TGuidEntity>;
    function GetCompositeGuidInt: IDbSet<TCompositeGuidInt>;
    function GetCompositeIntDateTime: IDbSet<TCompositeIntDateTime>;
    function GetEnumEntities: IDbSet<TEnumEntity>;
    function GetJsonEntities: IDbSet<TJsonEntity>;
    function GetUuidEntities: IDbSet<TUuidEntity>;
    // function GetAutoGuidEntities: IDbSet<TAutoGuidEntity>;  // PostgreSQL-specific
  public
    property GuidEntities: IDbSet<TGuidEntity> read GetGuidEntities;
    property CompositeGuidInt: IDbSet<TCompositeGuidInt> read GetCompositeGuidInt;
    property CompositeIntDateTime: IDbSet<TCompositeIntDateTime> read GetCompositeIntDateTime;
    property EnumEntities: IDbSet<TEnumEntity> read GetEnumEntities;
    property JsonEntities: IDbSet<TJsonEntity> read GetJsonEntities;
    property UuidEntities: IDbSet<TUuidEntity> read GetUuidEntities;
    // property AutoGuidEntities: IDbSet<TAutoGuidEntity> read GetAutoGuidEntities;  // PostgreSQL-specific
  end;

{ TJsonEntity }

constructor TJsonEntity.Create;
begin
  FMetadata := TJsonMetadata.Create;
end;

destructor TJsonEntity.Destroy;
begin
  FMetadata.Free;
  inherited;
end;

{ TTestDbContext }

function TTestDbContext.GetGuidEntities: IDbSet<TGuidEntity>;
begin
  Result := Entities<TGuidEntity>;
end;

function TTestDbContext.GetCompositeGuidInt: IDbSet<TCompositeGuidInt>;
begin
  Result := Entities<TCompositeGuidInt>;
end;

function TTestDbContext.GetCompositeIntDateTime: IDbSet<TCompositeIntDateTime>;
begin
  Result := Entities<TCompositeIntDateTime>;
end;

function TTestDbContext.GetEnumEntities: IDbSet<TEnumEntity>;
begin
  Result := Entities<TEnumEntity>;
end;

function TTestDbContext.GetJsonEntities: IDbSet<TJsonEntity>;
begin
  Result := Entities<TJsonEntity>;
end;

function TTestDbContext.GetUuidEntities: IDbSet<TUuidEntity>;
begin
  Result := Entities<TUuidEntity>;
end;

// PostgreSQL-specific - uncomment to test auto-generated UUID endianness
(*
function TTestDbContext.GetAutoGuidEntities: IDbSet<TAutoGuidEntity>;
begin
  Result := Entities<TAutoGuidEntity>;
end;
*)

procedure EnsureDatabaseExists;
var
  Conn: TFDConnection;
  Qry: TFDQuery;
begin
  Conn := TFDConnection.Create(nil);
  try
    Conn.DriverName := 'PG';
    Conn.Params.Values['Server'] := 'localhost';
    Conn.Params.Values['Port'] := '5432';
    Conn.Params.Values['User_Name'] := 'postgres';
    Conn.Params.Values['Password'] := 'root';
    Conn.Params.Values['Database'] := 'postgres';
    
    try
      Conn.Connected := True;
      Qry := TFDQuery.Create(nil);
      try
        Qry.Connection := Conn;
        Qry.SQL.Text := 'SELECT 1 FROM pg_database WHERE datname = ''dext_test''';
        Qry.Open;
        if Qry.Eof then
        begin
          Conn.ExecSQL('CREATE DATABASE dext_test');
          WriteLn('  ✓ Database created');
        end;
      finally
        Qry.Free;
      end;
    except
      on E: Exception do WriteLn('  ⚠ Database failure: ', E.Message);
    end;
  finally
    Conn.Free;
  end;
end;

procedure RegisterConverters;
begin
  // We use RegisterConverterForType to validly OVERRIDE the default global converter for TGUID
  // This is required for PostgreSQL + FireDAC to handle endianness correctly
  TTypeConverterRegistry.Instance.RegisterConverterForType(TypeInfo(TGUID), TGuidConverter.Create(True));
  TTypeConverterRegistry.Instance.RegisterConverterForType(TypeInfo(TUUID), TUuidConverter.Create);
  TTypeConverterRegistry.Instance.RegisterConverter(TEnumConverter.Create(False));
  TTypeConverterRegistry.Instance.RegisterConverterForType(TypeInfo(TJsonMetadata), TJsonConverter.Create(True));
end;

procedure ExecSQL(Db: TDbContext; const SQL: string);
var
  Cmd: IDbCommand;
begin
  Cmd := Db.Connection.CreateCommand(SQL) as IDbCommand;
  Cmd.ExecuteNonQuery;
end;

procedure TestGuidFind(Db: TTestDbContext);
var
  TestGuid: TGUID;
  Entity: TGuidEntity;
  Loaded: TGuidEntity;
begin
  WriteLn('► Testing GUID Find...');
  WriteLn('  Step 1: Delete all');
  ExecSQL(Db, 'DELETE FROM test_guid_entities');
  
  TestGuid := TGuid.NewGuid;
  Entity := TGuidEntity.Create;
  Entity.Id := TestGuid;
  Entity.Name := 'Test GUID Find';
  
  WriteLn('  Step 2: Save');
  Db.GuidEntities.Add(Entity);
  Db.SaveChanges;
  
  WriteLn('  Step 3: Clear context');
  Db.Clear;
  
  WriteLn('  Step 4: Find by GUID');
  WriteLn('  Looking for: ', GUIDToString(TestGuid));
  
  Loaded := Db.GuidEntities.Find(GUIDToString(TestGuid));
  
  if Loaded = nil then
    raise Exception.Create('GUID Find returned nil!')
  else
  begin
    WriteLn('  Found GUID:  ', GUIDToString(Loaded.Id));
    WriteLn('  Found Name:  ', Loaded.Name);
    
    if not IsEqualGUID(TestGuid, Loaded.Id) then
      raise Exception.Create('GUID mismatch in Find!');
      
    WriteLn('  ✓ OK');
  end;
end;

procedure TestGuidList(Db: TTestDbContext);
var
  TestGuid: TGUID;
  Entity: TGuidEntity;
  List: IList<TGuidEntity>;
  Loaded: TGuidEntity;
begin
  WriteLn('► Testing GUID List...');
  WriteLn('  Step 1: Delete');
  ExecSQL(Db, 'DELETE FROM test_guid_entities');
  
  TestGuid := TGuid.NewGuid;
  Entity := TGuidEntity.Create;
  Entity.Id := TestGuid;
  Entity.Name := 'Test GUID List';
  
  WriteLn('  Step 2: Save');
  Db.GuidEntities.Add(Entity);
  Db.SaveChanges;
  
  WriteLn('  Step 3: Clear');
  Db.Clear; 
  
  WriteLn('  Step 4: List');
  List := Db.GuidEntities.ToList;
  
  WriteLn('  Step 5: Loaded count: ', List.Count);
  if List.Count > 0 then
  begin
    Loaded := List[0];
    WriteLn('  Original GUID: ', GUIDToString(TestGuid));
    WriteLn('  Loaded GUID:   ', GUIDToString(Loaded.Id));
    if not IsEqualGUID(TestGuid, Loaded.Id) then
    begin
       WriteLn('  Mismatch details:');
       WriteLn('    Org: ', GUIDToString(TestGuid));
       WriteLn('    Ld : ', GUIDToString(Loaded.Id));
       raise Exception.Create('GUID mismatch');
    end;
    WriteLn('  ✓ OK');
  end;
end;

procedure TestCompositeGuidInt(Db: TTestDbContext);
var
  TestGuid: TGUID;
  Entity: TCompositeGuidInt;
  Loaded: TCompositeGuidInt;
  Keys: TArray<Variant>;
begin
  WriteLn('► Testing Composite Key (GUID + Int)...');
  ExecSQL(Db, 'DELETE FROM test_composite_guid_int');
  
  TestGuid := TGuid.NewGuid;
  Entity := TCompositeGuidInt.Create;
  Entity.GuidKey := TestGuid;
  Entity.IntKey := 42;
  Entity.Data := 'Composite Test';
  
  WriteLn('  Saving with GUID: ', GUIDToString(TestGuid), ' and Int: 42');
  Db.CompositeGuidInt.Add(Entity);
  Db.SaveChanges;
  Db.Clear;
  
  WriteLn('  Finding by composite key...');
  SetLength(Keys, 2);
  Keys[0] := GUIDToString(TestGuid);
  Keys[1] := 42;
  
  Loaded := Db.CompositeGuidInt.Find(Keys);
  
  if Loaded = nil then
    raise Exception.Create('Composite GUID+Int Find returned nil!');
    
  if not IsEqualGUID(TestGuid, Loaded.GuidKey) then
    raise Exception.Create('GUID mismatch in composite key!');
    
  if Loaded.IntKey <> 42 then
    raise Exception.Create('Int mismatch in composite key!');
    
  WriteLn('  Found Data: ', Loaded.Data);
  WriteLn('  ✓ OK');
end;

procedure TestCompositeIntDateTime(Db: TTestDbContext);
var
  Entity: TCompositeIntDateTime;
  Loaded: TCompositeIntDateTime;
  Keys: TArray<Variant>;
  TestTime: TDateTime;
begin
  WriteLn('► Testing Composite Key (Int + DateTime)...');
  ExecSQL(Db, 'DELETE FROM test_composite_int_datetime');
  
  TestTime := EncodeDate(2025, 12, 20) + EncodeTime(14, 30, 0, 0);
  Entity := TCompositeIntDateTime.Create;
  Entity.Timestamp := TestTime;
  Entity.Value := 'DateTime Composite Test';
  
  WriteLn('  Saving with DateTime: ', DateTimeToStr(TestTime));
  Db.CompositeIntDateTime.Add(Entity);
  Db.SaveChanges;
  
  WriteLn('  Auto-generated ID: ', Entity.Id);
  
  Db.Clear;
  
  WriteLn('  Finding by composite key...');
  SetLength(Keys, 2);
  Keys[0] := Entity.Id;
  Keys[1] := TestTime;
  
  Loaded := Db.CompositeIntDateTime.Find(Keys);
  
  if Loaded = nil then
    raise Exception.Create('Composite Int+DateTime Find returned nil!');
    
  WriteLn('  Found Value: ', Loaded.Value);
  WriteLn('  ✓ OK');
end;

procedure TestEnum(Db: TTestDbContext);
var
  Entity: TEnumEntity;
  List: IList<TEnumEntity>;
  Loaded: TEnumEntity;
begin
  WriteLn('► Testing Enum...');
  ExecSQL(Db, 'DELETE FROM test_enum_entities');
  Entity := TEnumEntity.Create;
  Entity.Role := urSuperAdmin;
  Entity.Status := urAdmin;
  Db.EnumEntities.Add(Entity);
  Db.SaveChanges;
  Db.Clear;
  List := Db.EnumEntities.ToList;
  if List.Count > 0 then
  begin
    Loaded := List[0];
    WriteLn('  Loaded Role: ' + GetEnumName(TypeInfo(TUserRole), Ord(Loaded.Role)));
    if Loaded.Role <> urSuperAdmin then raise Exception.Create('Enum mismatch');
    WriteLn('  ✓ OK');
  end;
end;

procedure TestJson(Db: TTestDbContext);
var
  Entity: TJsonEntity;
  List: IList<TJsonEntity>;
  Loaded: TJsonEntity;
begin
  WriteLn('► Testing JSON...');
  ExecSQL(Db, 'DELETE FROM test_json_entities');
  Entity := TJsonEntity.Create;
  Entity.Metadata.Name := 'Dext';
  Entity.Metadata.Value := 10;
  
  Db.JsonEntities.Add(Entity);
  Db.SaveChanges;
  Db.Clear;
  List := Db.JsonEntities.ToList;
  if List.Count > 0 then
  begin
    Loaded := List[0];
    WriteLn('  Loaded Name: "' + Loaded.Metadata.Name + '" Value: ' + IntToStr(Loaded.Metadata.Value));
    if Loaded.Metadata.Name <> 'Dext' then raise Exception.Create('JSON mismatch');
    WriteLn('  ✓ OK');
  end;
end;

procedure TestUuid(Db: TTestDbContext);
var
  TestUuid: TUUID;
  Entity: TUuidEntity;
  List: IList<TUuidEntity>;
  Loaded: TUuidEntity;
  OriginalStr: string;
begin
  WriteLn('► Testing TUUID (RFC 9562)...');
  
  // Clean up
  ExecSQL(Db, 'DELETE FROM test_uuid_entities');
  
  // Generate a new UUID v7
  TestUuid := TUUID.NewV7;
  OriginalStr := TestUuid.ToString;
  
  WriteLn('  Original TUUID: ', OriginalStr);
  
  // Create entity
  Entity := TUuidEntity.Create;
  Entity.Id := TestUuid;
  Entity.Name := 'Test TUUID Entity';
  
  // Save
  WriteLn('  Saving entity...');
  Db.UuidEntities.Add(Entity);
  Db.SaveChanges;
  
  // Clear context
  Db.Clear;
  
  // Test List
  WriteLn('  Loading via ToList...');
  List := Db.UuidEntities.ToList;
  
  if List.Count = 0 then
    raise Exception.Create('TUUID List returned empty!');
  
  Loaded := List[0];
  WriteLn('  Loaded TUUID:   ', Loaded.Id.ToString);
  
  // Verify byte-order is correct
  if Loaded.Id.ToString <> OriginalStr then
  begin
    WriteLn('  ❌ MISMATCH!');
    WriteLn('     Original: ', OriginalStr);
    WriteLn('     Loaded:   ', Loaded.Id.ToString);
    raise Exception.Create('TUUID byte-order mismatch in List!');
  end;
  
  WriteLn('  ✓ List OK');
  
  // Test Find
  Db.Clear;
  WriteLn('  Finding by TUUID string...');
  Loaded := Db.UuidEntities.Find(OriginalStr);
  
  if Loaded = nil then
    raise Exception.Create('TUUID Find returned nil!');
    
  if Loaded.Id.ToString <> OriginalStr then
  begin
    WriteLn('  ❌ MISMATCH in Find!');
    raise Exception.Create('TUUID byte-order mismatch in Find!');
  end;
  
  WriteLn('  Found Name: ', Loaded.Name);
  WriteLn('  ✓ Find OK');
  WriteLn('  ✓ TUUID Test PASSED');
end;

// NOTE: This test is PostgreSQL-specific (uses gen_random_uuid())
// It validates that RETURNING endianness is correct when using a registered TGuidConverter.
// To run this test, you must register TGuidConverter and have PostgreSQL as the backend.
(*
procedure TestAutoGuid(Db: TTestDbContext);
var
  Entity: TAutoGuidEntity;
  List: IList<TAutoGuidEntity>;
  Loaded: TAutoGuidEntity;
  PostInsertId: TGUID;
begin
  WriteLn('► Testing Auto-Generated GUID (RETURNING endianness)...');
  
  // Clean up and create table manually with gen_random_uuid() default
  ExecSQL(Db, 'DROP TABLE IF EXISTS test_autoguid_entities');
  ExecSQL(Db, 'CREATE TABLE test_autoguid_entities (id uuid PRIMARY KEY DEFAULT gen_random_uuid(), name text)');
  
  // Create entity (without setting the ID - it will be generated by PostgreSQL)
  Entity := TAutoGuidEntity.Create;
  Entity.Name := 'Auto GUID Test';
  
  WriteLn('  Saving entity (ID will be generated by PostgreSQL)...');
  Db.AutoGuidEntities.Add(Entity);
  Db.SaveChanges;
  
  // After SaveChanges, Entity.Id should be populated with the generated UUID
  PostInsertId := Entity.Id;
  WriteLn('  ID after INSERT: ', GUIDToString(PostInsertId));
  
  if IsEqualGUID(PostInsertId, TGUID.Empty) then
    raise Exception.Create('Auto-generated ID was not populated after INSERT!');
  
  // Clear and reload from database
  Db.Clear;
  WriteLn('  Reloading from database...');
  List := Db.AutoGuidEntities.ToList;
  
  if List.Count = 0 then
    raise Exception.Create('No entities found after INSERT!');
  
  Loaded := List[0];
  WriteLn('  ID from database: ', GUIDToString(Loaded.Id));
  
  // The critical test: ID after INSERT should match ID from database
  // If endianness is wrong, these will differ!
  if not IsEqualGUID(PostInsertId, Loaded.Id) then
  begin
    WriteLn('  ❌ ENDIANNESS MISMATCH!');
    WriteLn('     After INSERT: ', GUIDToString(PostInsertId));
    WriteLn('     From DB:      ', GUIDToString(Loaded.Id));
    raise Exception.Create('UUID endianness mismatch between INSERT RETURNING and SELECT!');
  end;
  
  // Also verify by querying raw from DB
  WriteLn('  ✓ Endianness correct - POST and GET return same ID');
  WriteLn('  ✓ Auto-Generated GUID Test PASSED');
end;
*)

var
  Db: TTestDbContext;
  Connection: IDbConnection;
  FDConn: TFDConnection;
  Dialect: ISQLDialect;
begin
  try
    WriteLn('═══════════════════════════════════════════════════════════');
    WriteLn('  Dext ORM - Type Converters & Composite Keys Test Suite');
    WriteLn('═══════════════════════════════════════════════════════════');
    WriteLn;
    
    EnsureDatabaseExists;
    
    // Direct configuration without DbConfig.pas
    FDConn := TFDConnection.Create(nil);
    FDConn.DriverName := 'PG';
    FDConn.Params.Values['Server'] := 'localhost';
    FDConn.Params.Values['Port'] := '5432';
    FDConn.Params.Values['Database'] := 'dext_test';
    FDConn.Params.Values['User_Name'] := 'postgres';
    FDConn.Params.Values['Password'] := 'root';
    
    Connection := TFireDACConnection.Create(FDConn, True);
    Dialect := TPostgreSQLDialect.Create;

    RegisterConverters;
    
    Db := TTestDbContext.Create(Connection, Dialect);
    try
      // Force clean state by dropping tables
      var C: IInterface := Connection.CreateCommand('DROP TABLE IF EXISTS test_guid_entities');
      (C as IDbCommand).ExecuteNonQuery;
      
      C := Connection.CreateCommand('DROP TABLE IF EXISTS test_composite_guid_int');
      (C as IDbCommand).ExecuteNonQuery;
      
      C := Connection.CreateCommand('DROP TABLE IF EXISTS test_composite_int_datetime');
      (C as IDbCommand).ExecuteNonQuery;
      
      C := Connection.CreateCommand('DROP TABLE IF EXISTS test_enum_entities');
      (C as IDbCommand).ExecuteNonQuery;
      
      C := Connection.CreateCommand('DROP TABLE IF EXISTS test_json_entities');
      (C as IDbCommand).ExecuteNonQuery;
      
      C := Connection.CreateCommand('DROP TABLE IF EXISTS test_uuid_entities');
      (C as IDbCommand).ExecuteNonQuery;

      // Force initialization of DbSets
      if Db.GuidEntities <> nil then;
      if Db.CompositeGuidInt <> nil then;
      if Db.CompositeIntDateTime <> nil then;
      if Db.EnumEntities <> nil then;
      if Db.JsonEntities <> nil then;
      if Db.UuidEntities <> nil then;
      
      Db.EnsureCreated;
      
      WriteLn;
      WriteLn('Running Tests:');
      WriteLn('─────────────────────────────────────────────────────────');
      
      TestGuidList(Db);
      WriteLn;
      
      TestGuidFind(Db);  // ← This is the critical test!
      WriteLn;
      
      TestCompositeGuidInt(Db);
      WriteLn;
      
      TestCompositeIntDateTime(Db);
      WriteLn;
      
      TestEnum(Db);
      WriteLn;
      
      TestJson(Db);
      WriteLn;
      
      TestUuid(Db);  // TUUID RFC 9562 test
      WriteLn;
      
      // TestAutoGuid(Db);  // PostgreSQL-specific test - uncomment to run manually
      // WriteLn;
      
      WriteLn('─────────────────────────────────────────────────────────');
      WriteLn('🎉 ALL TESTS PASSED!');
      WriteLn('═══════════════════════════════════════════════════════════');
    finally
      Db.Free;
    end;
  except
    on E: Exception do 
    begin
      WriteLn;
      WriteLn('═══════════════════════════════════════════════════════════');
      WriteLn('❌ TEST FAILED: ' + E.Message);
      WriteLn('═══════════════════════════════════════════════════════════');
    end;
  end;
  WriteLn;
  WriteLn('Press ENTER to exit...');
  ReadLn;
end.
