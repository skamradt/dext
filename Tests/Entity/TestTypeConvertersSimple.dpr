program TestTypeConvertersSimple;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  System.SysUtils,
  System.Rtti,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  Dext.Entity.Drivers.FireDAC.Links,
  Dext.Utils,
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.TypeConverters,
  Dext.Entity.Dialects;

type
  TUserRole = (urGuest, urUser, urAdmin, urSuperAdmin);

procedure TestGuidConverter;
var
  Conn: TFDConnection;
  Query: TFDQuery;
  Converter: TGuidConverter;
  TestGuid, LoadedGuid: TGUID;
  Value, DbValue, Result: TValue;
begin
  WriteLn('► Testing GUID Converter with PostgreSQL...');
  
  Conn := TFDConnection.Create(nil);
  try
    Conn.DriverName := 'PG';
    Conn.Params.Values['Server'] := 'localhost';
    Conn.Params.Values['Port'] := '5432';
    Conn.Params.Values['Database'] := 'dext_test';
    Conn.Params.Values['User_Name'] := 'postgres';
    Conn.Params.Values['Password'] := 'root';
    Conn.Connected := True;
    
    WriteLn('  ✓ Connected to PostgreSQL');
    
    // Create table
    Conn.ExecSQL('DROP TABLE IF EXISTS test_guid');
    Conn.ExecSQL('CREATE TABLE test_guid (id UUID PRIMARY KEY, name VARCHAR(100))');
    WriteLn('  ✓ Table created');
    
    // Test converter
    Converter := TGuidConverter.Create(True); // PG requires endian swap
    try
      CreateGUID(TestGuid);
      TValue.Make(@TestGuid, TypeInfo(TGUID), Value);
      
      DbValue := Converter.ToDatabase(Value, ddPostgreSQL);
      WriteLn('  Original GUID: ', GUIDToString(TestGuid));
      WriteLn('  DB format:     ', DbValue.AsString);
      
      // Insert
      Query := TFDQuery.Create(nil);
      try
        Query.Connection := Conn;
        Query.SQL.Text := 'INSERT INTO test_guid (id, name) VALUES (:id::uuid, :name)';
        Query.ParamByName('id').AsString := DbValue.AsString;
        Query.ParamByName('name').AsString := 'Test Record';
        Query.ExecSQL;
        WriteLn('  ✓ Record inserted');
        
        // Select
        Query.SQL.Text := 'SELECT id FROM test_guid LIMIT 1';
        Query.Open;
        if not Query.Eof then
        begin
          var GuidStr := Query.Fields[0].AsString;
          WriteLn('  Read from DB:  ', GuidStr);
          
          Result := Converter.FromDatabase(TValue.From<string>(GuidStr), TypeInfo(TGUID));
          LoadedGuid := Result.AsType<TGUID>;
          
          if IsEqualGUID(TestGuid, LoadedGuid) then
            WriteLn('  ✓ GUID round-trip successful!')
          else
            raise Exception.Create('GUID mismatch');
        end;
        Query.Close;
      finally
        Query.Free;
      end;
    finally
      Converter.Free;
    end;
    
    // Cleanup
    Conn.ExecSQL('DROP TABLE test_guid');
    WriteLn('✓ GUID Converter test passed');
    WriteLn('');
  finally
    Conn.Free;
  end;
end;

procedure TestEnumConverter;
var
  Conn: TFDConnection;
  Query: TFDQuery;
  Converter: TEnumConverter;
  TestRole, LoadedRole: TUserRole;
  Value, DbValue, Result: TValue;
begin
  WriteLn('► Testing Enum Converter with PostgreSQL (String mode)...');
  
  Conn := TFDConnection.Create(nil);
  try
    Conn.DriverName := 'PG';
    Conn.Params.Values['Server'] := 'localhost';
    Conn.Params.Values['Port'] := '5432';
    Conn.Params.Values['Database'] := 'dext_test';
    Conn.Params.Values['User_Name'] := 'postgres';
    Conn.Params.Values['Password'] := 'root';
    Conn.Connected := True;
    
    // Create table
    Conn.ExecSQL('DROP TABLE IF EXISTS test_enum');
    Conn.ExecSQL('CREATE TABLE test_enum (id SERIAL PRIMARY KEY, role VARCHAR(50))');
    WriteLn('  ✓ Table created');
    
    // Test converter
    Converter := TEnumConverter.Create(True); // String mode
    try
      TestRole := urSuperAdmin;
      TValue.Make(@TestRole, TypeInfo(TUserRole), Value);
      
      DbValue := Converter.ToDatabase(Value, ddPostgreSQL);
      WriteLn('  Enum value: urSuperAdmin');
      WriteLn('  DB format:  ', DbValue.AsString);
      
      // Insert
      Query := TFDQuery.Create(nil);
      try
        Query.Connection := Conn;
        Query.SQL.Text := 'INSERT INTO test_enum (role) VALUES (:role)';
        Query.ParamByName('role').AsString := DbValue.AsString;
        Query.ExecSQL;
        WriteLn('  ✓ Record inserted');
        
        // Select
        Query.SQL.Text := 'SELECT role FROM test_enum LIMIT 1';
        Query.Open;
        if not Query.Eof then
        begin
          var RoleStr := Query.Fields[0].AsString;
          WriteLn('  Read from DB: ', RoleStr);
          
          Result := Converter.FromDatabase(TValue.From<string>(RoleStr), TypeInfo(TUserRole));
          LoadedRole := Result.AsType<TUserRole>;
          
          if LoadedRole = urSuperAdmin then
            WriteLn('  ✓ Enum round-trip successful!')
          else
            raise Exception.Create('Enum mismatch');
        end;
        Query.Close;
      finally
        Query.Free;
      end;
    finally
      Converter.Free;
    end;
    
    // Cleanup
    Conn.ExecSQL('DROP TABLE test_enum');
    WriteLn('✓ Enum Converter test passed');
    WriteLn('');
  finally
    Conn.Free;
  end;
end;

begin
  SetConsoleCharSet(65001);
  try
    WriteLn('📊 Dext Type Converters PostgreSQL Integration Test');
    WriteLn('====================================================');
    WriteLn('');
    
    TestGuidConverter;
    TestEnumConverter;
    
    WriteLn('');
    WriteLn('✅ All PostgreSQL integration tests passed!');
    WriteLn('');
  except
    on E: Exception do
    begin
      WriteLn('');
      WriteLn('❌ Test failed: ', E.Message);
      WriteLn('');
      ExitCode := 1;
    end;
  end;
  
  WriteLn('Press ENTER to exit...');
  ConsolePause;
end.
