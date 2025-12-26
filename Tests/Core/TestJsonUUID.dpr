program TestJsonUUID;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Dext.Types.UUID,
  Dext.Json,
  Dext.Json.Utf8.Serializer,
  Dext.Core.Span;

type
  TTestRecord = record
    Id: TUUID;
    Name: string;
  end;

  TTestRecordWithGuid = record
    Id: TGUID;
    Name: string;
  end;

procedure TestRecordSerialization;
var
  Rec: TTestRecord;
  Json: string;
begin
  WriteLn('► Testing Record Serialization...');
  
  Rec.Id := TUUID.FromString('a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11');
  Rec.Name := 'Test Item';
  
  Json := TDextJson.Serialize(Rec);
  WriteLn('  Serialized: ', Json);
  
  // Minimal check (Case Insensitive)
  if Pos('a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11', LowerCase(Json)) = 0 then
    raise Exception.Create('UUID value missing in JSON');
    
  WriteLn('  ✓ Record Serialization OK');
end;

procedure TestRecordDeserialization;
var
  Json: string;
  Rec: TTestRecord;
begin
  WriteLn('► Testing Record Deserialization...');
  
  Json := '{"Id": "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11", "Name": "Restored Item"}';
  Rec := TDextJson.Deserialize<TTestRecord>(Json);
  
  WriteLn('  Deserialized ID: ', Rec.Id.ToString);
  WriteLn('  Deserialized Name: ', Rec.Name);
  
  if LowerCase(Rec.Id.ToString) <> 'a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11' then
    raise Exception.Create('UUID deserialization mismatch (Expected lowercase)');
    
  WriteLn('  ✓ Record Deserialization OK');
end;

procedure TestArraySerialization;
var
  Arr: TArray<TUUID>;
  Json: string;
begin
  WriteLn('► Testing Array Serialization...');
  
  Arr := [TUUID.FromString('a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11'), 
          TUUID.FromString('b0eebc99-9c0b-4ef8-bb6d-6bb9bd380a22')];
          
  Json := TDextJson.Serialize<TArray<TUUID>>(Arr);
  WriteLn('  Serialized Array: ', Json);
  
  if Pos('a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11', LowerCase(Json)) = 0 then
    raise Exception.Create('First UUID missing');
  if Pos('b0eebc99-9c0b-4ef8-bb6d-6bb9bd380a22', LowerCase(Json)) = 0 then
    raise Exception.Create('Second UUID missing');
    
  WriteLn('  ✓ Array Serialization OK');
end;

procedure TestArrayDeserialization;
var
  Json: string;
  Arr: TArray<TUUID>;
begin
  WriteLn('► Testing Array Deserialization...');
  
  Json := '["a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11", "b0eebc99-9c0b-4ef8-bb6d-6bb9bd380a22"]';
  Arr := TDextJson.Deserialize<TArray<TUUID>>(Json);
  
  WriteLn('  Length: ', Length(Arr));
  if Length(Arr) <> 2 then
    raise Exception.Create('Array length mismatch');
    
  if LowerCase(Arr[0].ToString) <> 'a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11' then
    raise Exception.Create('First element mismatch');
  
  if LowerCase(Arr[1].ToString) <> 'b0eebc99-9c0b-4ef8-bb6d-6bb9bd380a22' then
    raise Exception.Create('Second element mismatch');
    
  WriteLn('  ✓ Array Deserialization OK');
end;

procedure TestUtf8DeserializationTUUID;
var
  Json: string;
  JsonBytes: TBytes;
  Span: TByteSpan;
  Rec: TTestRecord;
begin
  WriteLn('► Testing UTF-8 Deserializer (TUUID)...');
  
  // This tests the TUtf8JsonSerializer used in ModelBinding (POST requests)
  Json := '{\"Id\": \"3A4E1A9B-A4AD-D844-8920-E8DD00916690\", \"Name\": \"Test User\"}';
  JsonBytes := TEncoding.UTF8.GetBytes(Json);
  Span := TByteSpan.FromBytes(JsonBytes);
  
  Rec := TUtf8JsonSerializer.Deserialize<TTestRecord>(Span);
  
  WriteLn('  Deserialized ID: ', Rec.Id.ToString);
  WriteLn('  Deserialized Name: ', Rec.Name);
  
  if LowerCase(Rec.Id.ToString) <> '3a4e1a9b-a4ad-d844-8920-e8dd00916690' then
    raise Exception.Create('TUUID UTF-8 deserialization mismatch');
    
  if Rec.Name <> 'Test User' then
    raise Exception.Create('Name field mismatch');
    
  WriteLn('  ✓ UTF-8 Deserializer (TUUID) OK');
end;

procedure TestUtf8DeserializationTGUID;
var
  Json: string;
  JsonBytes: TBytes;
  Span: TByteSpan;
  Rec: TTestRecordWithGuid;
begin
  WriteLn('► Testing UTF-8 Deserializer (TGUID)...');
  
  Json := '{\"Id\": \"5B1A1E5A-ADA4-4448-892D-E8DD00916690\", \"Name\": \"GUID Test\"}';
  JsonBytes := TEncoding.UTF8.GetBytes(Json);
  Span := TByteSpan.FromBytes(JsonBytes);
  
  Rec := TUtf8JsonSerializer.Deserialize<TTestRecordWithGuid>(Span);
  
  WriteLn('  Deserialized ID: ', GUIDToString(Rec.Id));
  WriteLn('  Deserialized Name: ', Rec.Name);
  
  // TGUID.ToString includes braces, so compare without them
  var GuidStr := GUIDToString(Rec.Id).ToLower;
  if GuidStr <> '{5b1a1e5a-ada4-4448-892d-e8dd00916690}' then
    raise Exception.Create('TGUID UTF-8 deserialization mismatch');
    
  if Rec.Name <> 'GUID Test' then
    raise Exception.Create('Name field mismatch');
    
  WriteLn('  ✓ UTF-8 Deserializer (TGUID) OK');
end;

begin
  try
    WriteLn('═══════════════════════════════════════════════════════════');
    WriteLn('  Dext.Json + TUUID Integration Test');
    WriteLn('═══════════════════════════════════════════════════════════');
    WriteLn;
    
    TestRecordSerialization;
    WriteLn;
    
    TestRecordDeserialization;
    WriteLn;
    
    TestArraySerialization;
    WriteLn;
    
    TestArrayDeserialization;
    WriteLn;
    
    TestUtf8DeserializationTUUID;
    WriteLn;
    
    TestUtf8DeserializationTGUID;
    WriteLn;
    
    WriteLn('═══════════════════════════════════════════════════════════');
    WriteLn('🎉 ALL TESTS PASSED!');
    WriteLn('═══════════════════════════════════════════════════════════');
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('❌ TEST FAILED: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
