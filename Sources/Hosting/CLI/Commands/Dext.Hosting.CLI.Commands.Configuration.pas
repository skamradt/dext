unit Dext.Hosting.CLI.Commands.Configuration;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Dext.Hosting.CLI.Args,
  Dext.Hosting.CLI.Config;

type
  TConfigInitCommand = class(TInterfacedObject, IConsoleCommand)
  public
    function GetName: string;
    function GetDescription: string;
    procedure Execute(const Args: TCommandLineArgs);
  end;

  TEnvScanCommand = class(TInterfacedObject, IConsoleCommand)
  public
    function GetName: string;
    function GetDescription: string;
    procedure Execute(const Args: TCommandLineArgs);
  end;

implementation

{ TConfigInitCommand }

function TConfigInitCommand.GetName: string;
begin
  Result := 'config';
end;

function TConfigInitCommand.GetDescription: string;
begin
  Result := 'Manages configuration. Usage: dext config init';
end;

procedure TConfigInitCommand.Execute(const Args: TCommandLineArgs);
var
  FileName: string;
  Content: string;
begin
  if (Args.Values.Count < 1) or (Args.Values[0] <> 'init') then
  begin
    WriteLn('Usage: dext config init');
    Exit;
  end;

  FileName := '.dext.config'; 
  if FileExists(FileName) then
  begin
    WriteLn('Configuration file already exists: ' + FileName);
    Exit;
  end;

  Content := 
    'project:' + sLineBreak +
    '  name: MyProject' + sLineBreak +
    '  type: application' + sLineBreak +
    '' + sLineBreak +
    'test:' + sLineBreak +
    '  auto_add: false' + sLineBreak +
    '  include:' + sLineBreak +
    '    - Tests/**/*.pas' + sLineBreak +
    '  exclude:' + sLineBreak +
    '    - Tests/Integration/**';

  TFile.WriteAllText(FileName, Content);
  WriteLn('Created configuration file: ' + FileName);
end;

{ TEnvScanCommand }

function TEnvScanCommand.GetName: string;
begin
  Result := 'env';
end;

function TEnvScanCommand.GetDescription: string;
begin
  Result := 'Manages Delphi environments. Usage: dext env scan';
end;

procedure TEnvScanCommand.Execute(const Args: TCommandLineArgs);
var
  Scanner: TDextGlobalConfig;
  Log: string;
begin
  if (Args.Values.Count < 1) or (Args.Values[0] <> 'scan') then
  begin
    WriteLn('Usage: dext env scan');
    Exit;
  end;

  WriteLn('Scanning for Delphi installations...');
  
  Scanner := TDextGlobalConfig.Create;
  try
    Log := Scanner.ScanEnvironments;
    Write(Log);
  finally
    Scanner.Free;
  end;
end;

end.
