unit Dext.Hosting.CLI.Commands.Facade;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  Dext.Hosting.CLI.Args,
  Dext.Hosting.CLI.Tools.FacadeGenerator,
  Dext.Utils;

type
  TFacadeCommand = class(TInterfacedObject, IConsoleCommand)
  public
    function GetName: string;
    function GetDescription: string;
    procedure Execute(const Args: TCommandLineArgs);
  end;

implementation

{ TFacadeCommand }

function TFacadeCommand.GetName: string;
begin
  Result := 'facade';
end;

function TFacadeCommand.GetDescription: string;
begin
  Result := 'Generates Dext facade unit (Dext.pas) from core units.';
end;

procedure TFacadeCommand.Execute(const Args: TCommandLineArgs);
var
  SourcePath: string;
  TargetFile: string;
  Excluded: TArray<string>;
  Generator: TFacadeGenerator;
begin
  // Parse arguments
  // -p, --path: Source path (default: current dir)
  if Args.HasOption('path') then
    SourcePath := Args.GetOption('path')
  else if Args.HasOption('p') then
    SourcePath := Args.GetOption('p')
  else
    SourcePath := GetCurrentDir;

  // -t, --target: Target file (Dext.pas)
  if Args.HasOption('target') then
    TargetFile := Args.GetOption('target')
  else if Args.HasOption('t') then
    TargetFile := Args.GetOption('t')
  else
  begin
    // Try to guess default if in standard Dext repo structure
    // But better to error if not explicit or simple fallback
    TargetFile := TPath.Combine(SourcePath, 'Dext.pas');
  end;

  // -x, --exclude: Exclusions (comma separated)
  Excluded := [];
  if Args.HasOption('exclude') then
    Excluded := Args.GetOption('exclude').Split([','])
  else if Args.HasOption('x') then
    Excluded := Args.GetOption('x').Split([',']);

  SourcePath := TPath.GetFullPath(SourcePath);
  TargetFile := TPath.GetFullPath(TargetFile);

  SafeWriteLn('Dext Facade Generator');
  SafeWriteLn('---------------------');
  SafeWriteLn('Source Path : ' + SourcePath);
  SafeWriteLn('Target File : ' + TargetFile);
  if Length(Excluded) > 0 then
    SafeWriteLn('Excluded    : ' + string.Join(', ', Excluded));
    
  if not TDirectory.Exists(SourcePath) then
  begin
    SafeWriteLn('Error: Source path does not exist.');
    Exit;
  end;

  Generator := TFacadeGenerator.Create(SourcePath, '*.pas', Excluded);
  // Delimiters
  if Args.HasOption('start-alias') then Generator.StartAliasTag := Args.GetOption('start-alias');
  if Args.HasOption('end-alias') then Generator.EndAliasTag := Args.GetOption('end-alias');
  if Args.HasOption('start-uses') then Generator.StartUsesTag := Args.GetOption('start-uses');
  if Args.HasOption('end-uses') then Generator.EndUsesTag := Args.GetOption('end-uses');
  
  if TargetFile <> '' then
  begin
    Generator.TargetUnitName := TPath.GetFileNameWithoutExtension(TargetFile);
    SafeWriteLn('Debug: Target Unit Name determined as: ' + Generator.TargetUnitName);
  end;
  
  // Validation
  if Args.HasOption('no-validate') then Generator.ValidateTags := False;
  if Args.HasOption('verbose') or Args.HasOption('v') then Generator.Verbose := True;

  try
    SafeWriteLn('Scanning...');
    Generator.Execute;
    
    // Backup
    if Args.HasOption('backup') then
      Generator.BackupTargetFile(TargetFile);
      
    SafeWriteLn('Injecting into ' + TargetFile + '...');
    var DryRun := Args.HasOption('dry-run');
    Generator.InjectIntoFile(TargetFile, DryRun);
    
    if not DryRun then
      SafeWriteLn('Done!');
  finally
    Generator.Free;
  end;
end;

end.
