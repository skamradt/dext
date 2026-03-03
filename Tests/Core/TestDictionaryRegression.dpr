program TestDictionaryRegression;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.IOUtils,
  Dext.Collections,
  Dext.Collections.Dict,
  Dext.Configuration.Core,
  Dext.Configuration.Json,
  Dext.Configuration.Interfaces,
  Dext.Utils;

procedure TestDictionaryRehash;
var
  Dict: IDictionary<string, string>;
  V: string;
  I: Integer;
begin
  Writeln('--- Test: Dictionary Rehash and Metadata Preservation ---');
  Dict := TCollections.CreateDictionary<string, string>;
  
  // Fill dictionary to trigger rehash (default capacity is 4, 75% load factor = 3 items)
  Writeln('Adding 3 items...');
  Dict.Add('Key1', 'Value1');
  Dict.Add('Key2', 'Value2');
  Dict.Add('Key3', 'Value3');
  
  Writeln('Adding 4th item to trigger Rehash (4 -> 8)...');
  Dict.Add('Key4', 'Value4');

  Writeln('Verifying all keys after rehash:');
  for I := 1 to 4 do
  begin
    var K := 'Key' + I.ToString;
    if Dict.TryGetValue(K, V) then
      Writeln('  ' + K + ': ' + V)
    else
      Writeln('  ' + K + ': NOT FOUND (FAILURE)');
  end;

  if Dict.Count <> 4 then
    Writeln('FAILURE: Expected count 4, but got ', Dict.Count);
end;

procedure TestDictionaryCaseInsensitivity;
var
  Dict: IDictionary<string, string>;
  V: string;
begin
  Writeln('--- Test: Dictionary Case Insensitivity ---');
  Dict := TCollections.CreateDictionaryIgnoreCase<string, string>;
  Dict.Add('MyKey', 'MyValue');
  
  if Dict.TryGetValue('MYKEY', V) then
    Writeln('SUCCESS: Found "MYKEY" -> ', V)
  else
    Writeln('FAILURE: Could not find "MYKEY" in ignore-case dictionary');
    
  if Dict.TryGetValue('mykey', V) then
    Writeln('SUCCESS: Found "mykey" -> ', V)
  else
    Writeln('FAILURE: Could not find "mykey" in ignore-case dictionary');
end;


procedure TestJsonConfiguration;
var
  Config: IConfigurationRoot;
  Json: string;
  Path: string;
begin
  Writeln('--- Test: JSON Configuration Loading ---');
  Path := 'test_regression.json';
  Json := 
    '{' +
    '  "Database": {' +
    '    "Driver": "FB",' +
    '    "Server": "localhost",' +
    '    "Port": 3050' +
    '  }' +
    '}';
    
  TFile.WriteAllText(Path, Json);
  try
    Config := TDextConfiguration.New
      .AddJsonFile(Path)
      .Build;

    Writeln('Reading Database:Driver   (Exact) = "', Config['Database:Driver'], '"');
    Writeln('Reading database:driver   (Lower) = "', Config['database:driver'], '"');
    Writeln('Reading DATABASE:DRIVER   (Upper) = "', Config['DATABASE:DRIVER'], '"');
    
    if (Config['database:driver'] = 'FB') and (Config['DATABASE:DRIVER'] = 'FB') then
      Writeln('  SUCCESS: Configuration is case-insensitive')
    else
      Writeln('  FAILURE: Configuration is still case-sensitive');

  finally


    if TFile.Exists(Path) then
      TFile.Delete(Path);
  end;
end;

begin
  try
    TestDictionaryRehash;
    Writeln;
    TestDictionaryCaseInsensitivity;
    Writeln;
    TestJsonConfiguration;
    Writeln;
    Writeln('Test Execution Finished.');

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ConsolePause;
end.
