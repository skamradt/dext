program DextFacadeGenerator;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  FacadeGenerator in 'FacadeGenerator.pas';

procedure PrintUsage;
begin
  Writeln('DextFacadeGenerator - Generates aliases for Dext Framework');
  Writeln('Usage:');
  Writeln('  DextFacadeGenerator -p <source_path> -t <target_file> [-x <excluded_unit1,unit2>]');
  Writeln('');
  Writeln('Options:');
  Writeln('  -p, --project   Root folder of source code to scan');
  Writeln('  -t, --target    Target unit file (e.g. Dext.pas) to inject aliases into');
  Writeln('  -x, --exclude   Comma-separated list of units to exclude');
end;

var
  SourcePath: string;
  TargetFile: string;
  Excluded: string;
  ExcludedArray: TArray<string>;
  Generator: TFacadeGenerator;
  I: Integer;
begin
  try
    if ParamCount < 2 then
    begin
      PrintUsage;
      Exit;
    end;
    
    // Simple argument parsing
    for I := 1 to ParamCount do
    begin
      if (ParamStr(I) = '-p') or (ParamStr(I) = '--project') then
        SourcePath := ParamStr(I + 1)
      else if (ParamStr(I) = '-t') or (ParamStr(I) = '--target') then
        TargetFile := ParamStr(I + 1)
      else if (ParamStr(I) = '-x') or (ParamStr(I) = '--exclude') then
        Excluded := ParamStr(I + 1);
    end;
    
    if (SourcePath = '') or (TargetFile = '') then
    begin
      Writeln('Error: Source path (-p) and Target file (-t) are required.');
      Exit;
    end;
    
    SourcePath := TPath.GetFullPath(SourcePath);
    TargetFile := TPath.GetFullPath(TargetFile);
    
    Writeln('Source Path: ' + SourcePath);
    Writeln('Target File: ' + TargetFile);
    
    if Excluded <> '' then
      ExcludedArray := Excluded.Split([','])
    else
      ExcludedArray := [];
      
    Generator := TFacadeGenerator.Create(SourcePath, '*.pas', ExcludedArray);
    try
      Writeln('Scanning...');
      Generator.Execute;
      
      Writeln('Injecting...');
      Generator.InjectIntoFile(TargetFile);
      
      Writeln('Done.');
    finally
      Generator.Free;
    end;
    
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
