unit Dext.Hosting.CLI.Commands.MigrateGenerate;

interface

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  System.SysUtils,
  System.IOUtils,
  Dext.Hosting.CLI,
  Dext.Hosting.CLI.Args,
  Dext.Utils;

type
  TMigrateGenerateCommand = class(TInterfacedObject, IConsoleCommand)
  public
    function GetName: string;
    function GetDescription: string;
    procedure Execute(const Args: TCommandLineArgs);
  end;

implementation

{ TMigrateGenerateCommand }

function TMigrateGenerateCommand.GetName: string;
begin
  Result := 'migrate:generate';
end;

function TMigrateGenerateCommand.GetDescription: string;
begin
  Result := 'Generates a new JSON migration file. Usage: migrate:generate <name> [--path <dir>]';
end;

procedure TMigrateGenerateCommand.Execute(const Args: TCommandLineArgs);
var
  Name, Path, FileName, Id, Content: string;
begin
  if Args.Values.Count = 0 then
  begin
    SafeWriteLn('Error: Migration name is required.');
    Exit;
  end;

  Name := Args.Values[0];
  Path := Args.GetOption('path');
  if Path = '' then Path := Args.GetOption('p');
  if Path = '' then Path := TDirectory.GetCurrentDirectory;

  Id := FormatDateTime('yyyyMMddHHmmss', Now) + '_' + Name;
  FileName := Id + '.json';
  
  if not TDirectory.Exists(Path) then
    TDirectory.CreateDirectory(Path);

  Content := 
    '{' + sLineBreak +
    '  "id": "' + Id + '",' + sLineBreak +
    '  "up": [],' + sLineBreak +
    '  "down": []' + sLineBreak +
    '}';

  TFile.WriteAllText(TPath.Combine(Path, FileName), Content);
  SafeWriteLn('   ✨ Generated migration: ' + FileName);
end;

end.
