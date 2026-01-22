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
{  Created: 2026-01-07                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.Hosting.CLI.Commands.Scaffold;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.StrUtils,
  System.Generics.Collections,
  FireDAC.Comp.Client,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.PG,
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.FB,
  Dext.Hosting.CLI.Args,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.Scaffolding,
  Dext.Utils;

type
  TScaffoldCommand = class(TInterfacedObject, IConsoleCommand)
  private
    procedure ShowUsage;
  public
    function GetName: string;
    function GetDescription: string;
    procedure Execute(const Args: TCommandLineArgs);
  end;

implementation

{ TScaffoldCommand }

function TScaffoldCommand.GetName: string;
begin
  Result := 'scaffold';
end;

function TScaffoldCommand.GetDescription: string;
begin
  Result := 'Generates entity classes from database schema. Usage: scaffold --connection <string> --driver <driver>';
end;

procedure TScaffoldCommand.ShowUsage;
begin
  SafeWriteLn('');
  SafeWriteLn('Dext Scaffold - Entity Generator from Database');
  SafeWriteLn('===============================================');
  SafeWriteLn('');
  SafeWriteLn('Usage: dext scaffold --connection <string> --driver <driver> [options]');
  SafeWriteLn('');
  SafeWriteLn('Required:');
  SafeWriteLn('  --connection, -c   FireDAC connection string or database path');
  SafeWriteLn('  --driver, -d       Database driver: sqlite, pg, mssql, firebird');
  SafeWriteLn('');
  SafeWriteLn('Options:');
  SafeWriteLn('  --output, -o       Output file path (default: Entities.pas)');
  SafeWriteLn('  --unit, -u         Unit name (default: derived from output file)');
  SafeWriteLn('  --fluent           Use fluent mapping instead of attributes');
  SafeWriteLn('  --tables, -t       Comma-separated table names (default: all)');
  SafeWriteLn('  --help             Show this help message');
  SafeWriteLn('');
  SafeWriteLn('Examples:');
  SafeWriteLn('  dext scaffold -c "mydb.db" -d sqlite -o MyEntities.pas');
  SafeWriteLn('  dext scaffold -c "host=localhost;database=mydb;user=postgres;password=123" -d pg --fluent');
  SafeWriteLn('  dext scaffold -c "Server=.;Database=MyDB;Trusted_Connection=yes" -d mssql -t "users,orders,products"');
  SafeWriteLn('');
end;

procedure TScaffoldCommand.Execute(const Args: TCommandLineArgs);
var
  ConnectionStr: string;
  DriverName: string;
  OutputFile: string;
  UnitName: string;
  UseFluent: Boolean;
  TableFilter: string;
  TableList: TArray<string>;
  
  FDConnection: TFDConnection;
  Connection: IDbConnection;
  Provider: ISchemaProvider;
  Generator: IEntityGenerator;
  Tables: TArray<string>;
  MetaList: TArray<TMetaTable>;
  Code: string;
  MappingStyle: TMappingStyle;
  I: Integer;
begin
  // Check for help
  if Args.HasOption('help') or Args.HasOption('h') then
  begin
    ShowUsage;
    Exit;
  end;

  // Parse required arguments
  ConnectionStr := Args.GetOption('connection');
  if ConnectionStr.IsEmpty then
    ConnectionStr := Args.GetOption('c');
    
  DriverName := Args.GetOption('driver');
  if DriverName.IsEmpty then
    DriverName := Args.GetOption('d');
    
  if ConnectionStr.IsEmpty or DriverName.IsEmpty then
  begin
    SafeWriteLn('Error: --connection and --driver are required.');
    ShowUsage;
    Exit;
  end;
  
  // Parse optional arguments
  OutputFile := Args.GetOption('output');
  if OutputFile.IsEmpty then
    OutputFile := Args.GetOption('o');
  if OutputFile.IsEmpty then
    OutputFile := 'Entities.pas';
    
  UnitName := Args.GetOption('unit');
  if UnitName.IsEmpty then
    UnitName := Args.GetOption('u');
  if UnitName.IsEmpty then
    UnitName := TPath.GetFileNameWithoutExtension(OutputFile);
    
  UseFluent := Args.HasOption('fluent');
  
  TableFilter := Args.GetOption('tables');
  if TableFilter.IsEmpty then
    TableFilter := Args.GetOption('t');
  if not TableFilter.IsEmpty then
    TableList := TableFilter.Split([','])
  else
    TableList := [];
    
  // Determine mapping style
  if UseFluent then
    MappingStyle := msFluent
  else
    MappingStyle := msAttributes;
    
  SafeWriteLn('');
  SafeWriteLn('Dext Scaffold');
  SafeWriteLn('=============');
  SafeWriteLn('Driver: ' + DriverName);
  SafeWriteLn('Connection: ' + ConnectionStr);
  SafeWriteLn('Output: ' + OutputFile);
  SafeWriteLn('Mapping: ' + IfThen(UseFluent, 'Fluent', 'Attributes'));
  SafeWriteLn('');
  
  // Create FireDAC connection
  FDConnection := TFDConnection.Create(nil);
  try
    FDConnection.LoginPrompt := False;
    
    // Configure driver
    DriverName := DriverName.ToLower;
    if DriverName = 'sqlite' then
    begin
      FDConnection.DriverName := 'SQLite';
      if not ConnectionStr.Contains('=') then
        FDConnection.Params.Add('Database=' + ConnectionStr)
      else
        FDConnection.Params.Text := ConnectionStr;
    end
    else if (DriverName = 'pg') or (DriverName = 'postgres') or (DriverName = 'postgresql') then
    begin
      FDConnection.DriverName := 'PG';
      FDConnection.Params.Text := ConnectionStr;
    end
    else if (DriverName = 'mssql') or (DriverName = 'sqlserver') then
    begin
      FDConnection.DriverName := 'MSSQL';
      FDConnection.Params.Text := ConnectionStr;
    end
    else if (DriverName = 'fb') or (DriverName = 'firebird') then
    begin
      FDConnection.DriverName := 'FB';
      FDConnection.Params.Text := ConnectionStr;
    end
    else
    begin
      SafeWriteLn('Error: Unknown driver "' + DriverName + '". Supported: sqlite, pg, mssql, firebird');
      Exit;
    end;
    
    SafeWriteLn('Connecting to database...');
    try
      FDConnection.Open;
    except
      on E: Exception do
      begin
        SafeWriteLn('Error: Failed to connect: ' + E.Message);
        Exit;
      end;
    end;
    SafeWriteLn('Connected!');
    
    // Create schema provider
    Connection := TFireDACConnection.Create(FDConnection);
    Provider := TFireDACSchemaProvider.Create(Connection);
    
    // Get tables
    SafeWriteLn('Reading schema...');
    Tables := Provider.GetTables;
    
    // Apply table filter if specified
    if Length(TableList) > 0 then
    begin
      var FilteredTables: TList<string> := TList<string>.Create;
      try
        for var T in Tables do
        begin
          for var F in TableList do
          begin
            if T.ToLower = Trim(F).ToLower then
            begin
              FilteredTables.Add(T);
              Break;
            end;
          end;
        end;
        Tables := FilteredTables.ToArray;
      finally
        FilteredTables.Free;
      end;
    end;
    
    SafeWriteLn('Found ' + Length(Tables).ToString + ' tables:');
    for var T in Tables do
      SafeWriteLn('  - ' + T);
    SafeWriteLn('');
    
    if Length(Tables) = 0 then
    begin
      SafeWriteLn('No tables found. Nothing to generate.');
      Exit;
    end;
    
    // Get metadata for each table
    SafeWriteLn('Extracting metadata...');
    SetLength(MetaList, Length(Tables));
    for I := 0 to High(Tables) do
    begin
      MetaList[I] := Provider.GetTableMetadata(Tables[I]);
      SafeWriteLn('  ' + Tables[I] + ': ' + Length(MetaList[I].Columns).ToString + ' columns, ' + 
                  Length(MetaList[I].ForeignKeys).ToString + ' FKs');
    end;
    
    // Generate code
    SafeWriteLn('');
    SafeWriteLn('Generating Delphi code...');
    Generator := TDelphiEntityGenerator.Create;
    Code := Generator.GenerateUnit(UnitName, MetaList, MappingStyle);
    
    // Write to file
    TFile.WriteAllText(OutputFile, Code);
    SafeWriteLn('');
    SafeWriteLn('SUCCESS! Generated: ' + TPath.GetFullPath(OutputFile));
    SafeWriteLn('');
    
  finally
    FDConnection.Free;
  end;
end;

end.
