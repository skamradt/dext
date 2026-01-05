unit Dext.Hosting.CLI.Commands.Test;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Diagnostics,
  System.Masks,
  Winapi.Windows,
  Dext.Hosting.CLI.Args,
  Dext.Hosting.CLI.Config,
  Dext.Hosting.CLI.Tools.Sonar;

type
  TTestCommand = class(TInterfacedObject, IConsoleCommand)
  private
    const
      BUILD_DIR = 'TestOutput';
    
    function FindProjectFile(const Directory: string): string;
    function GetMapFile(const ProjectName: string): string;
    function GetExeFile(const ProjectName: string): string;
    
    function BuildProject(const ProjectFile: string; EnableMap: Boolean): Boolean;
    function RunProcess(const Exe, Params: string): Boolean;
    
    function GetSourceDirectory(const BaseDir: string): string;
    procedure GenerateCoverageLists(const BaseDir, SourceDir: string; const Excludes: TArray<string>; out UnitFile, SourcePathFile: string);
    procedure GenerateAutoInclude(const BaseDir, SourceDir: string);
    function FindCodeCoverageExe(const TargetPlatform: string): string;
    function FindRSVars: string;
    
    procedure RunTests(const ProjectFile: string; const Args: TCommandLineArgs; Config: TDextConfig);
    procedure RunWithCoverage(const ProjectFile: string; const Args: TCommandLineArgs; Config: TDextConfig);
  public
    function GetName: string;
    function GetDescription: string;
    procedure Execute(const Args: TCommandLineArgs);
  end;

implementation

{ TTestCommand }

function TTestCommand.GetName: string;
begin
  Result := 'test';
end;

function TTestCommand.GetDescription: string;
begin
  Result := 'Runs project tests with optional code coverage analysis.';
end;

procedure TTestCommand.Execute(const Args: TCommandLineArgs);
var
  ProjectFile: string;
  WorkDir: string;
  Config: TDextConfig;
begin
  WorkDir := GetCurrentDir;
  Config := TDextConfig.Create;
  try
    Config.LoadFromFile(TPath.Combine(WorkDir, 'dext.json'));
    
    // 1. Find Project File
    if Args.HasOption('project') then
      ProjectFile := Args.GetOption('project')
    else if Config.Test.Project <> '' then
      ProjectFile := TPath.GetFullPath(TPath.Combine(WorkDir, Config.Test.Project))
    else
      ProjectFile := FindProjectFile(WorkDir);
  
    if ProjectFile = '' then
    begin
      WriteLn('Error: No Delphi project file (.dproj) found in current directory.');
      WriteLn('Use --project=<path> to specify one.');
      Exit;
    end;
  
    ProjectFile := TPath.GetFullPath(ProjectFile);
    WriteLn('Testing Project: ' + ExtractFileName(ProjectFile));
  
    // 2. Determine Mode
    if Args.HasOption('coverage') then
    begin
      RunWithCoverage(ProjectFile, Args, Config);
    end
    else
    begin
      RunTests(ProjectFile, Args, Config);
    end;
  finally
    Config.Free;
  end;
end;

function TTestCommand.FindProjectFile(const Directory: string): string;
var
  Files: TArray<string>;
  Candidate: string;
begin
  Result := '';
  Files := TDirectory.GetFiles(Directory, '*.dproj', TSearchOption.SoTopDirectoryOnly);
  
  if Length(Files) = 0 then Exit;
  
  if Length(Files) = 1 then
    Exit(Files[0]);

  // Priority: Contains "Test" -> First Found
  for Candidate in Files do
  begin
    if Candidate.ToLower.Contains('test') then
      Exit(Candidate);
  end;
  
  Result := Files[0];
end;

function TTestCommand.GetExeFile(const ProjectName: string): string;
begin
  Result := TPath.Combine(BUILD_DIR, ProjectName + '.exe');
end;

function TTestCommand.GetMapFile(const ProjectName: string): string;
begin
  Result := TPath.Combine(BUILD_DIR, ProjectName + '.map');
end;

function TTestCommand.RunProcess(const Exe, Params: string): Boolean;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  CmdLine: string;
  Res: Boolean;
  ExitCode: Cardinal;
begin
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  
  // Use existing console
  CmdLine := Format('"%s" %s', [Exe, Params]);
  
  WriteLn(Format('Running: %s %s', [Exe, Params]));
  
  Res := CreateProcess(nil, PChar(CmdLine), nil, nil, True, 0, nil, nil, SI, PI);
  if not Res then
  begin
    WriteLn('Failed to start process: ' + SysErrorMessage(GetLastError));
    Exit(False);
  end;
  
  WaitForSingleObject(PI.hProcess, INFINITE);
  GetExitCodeProcess(PI.hProcess, ExitCode);
  
  CloseHandle(PI.hProcess);
  CloseHandle(PI.hThread);
  
  Result := ExitCode = 0;
end;

function TTestCommand.FindRSVars: string;
var
  Paths: TArray<string>;
  Path: string;
begin
  if GetEnvironmentVariable('BDS') <> '' then
  begin
     Result := TPath.Combine(GetEnvironmentVariable('BDS'), 'bin\rsvars.bat');
     if FileExists(Result) then Exit;
  end;

  // Typical paths - Prioritize User's 37.0
  Paths := [
    'C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat',
    'C:\Program Files (x86)\Embarcadero\Studio\24.0\bin\rsvars.bat',
    'C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat', // Delphi 12
    'C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat', // Delphi 11
    'C:\Program Files (x86)\Embarcadero\Studio\21.0\bin\rsvars.bat'  // Delphi 10.4
  ];

  for Path in Paths do
    if FileExists(Path) then Exit(Path);
    
  Result := '';
end;

function TTestCommand.BuildProject(const ProjectFile: string; EnableMap: Boolean): Boolean;
var
  Args: string;
  ProjectName: string;
  OutDir: string;
  RSVars: string;
begin
  ProjectName := TPath.GetFileNameWithoutExtension(ProjectFile);
  OutDir := TPath.Combine(GetCurrentDir, BUILD_DIR);
  ForceDirectories(OutDir);
  
  // MSBuild arguments to enforce configuration
  Args := Format('"%s" /t:Build /p:Config=Debug /p:Platform=Win32 /p:DCC_ExeOutput="%s" /p:DCC_DcuOutput="%s"', 
    [ProjectFile, OutDir, TPath.Combine(OutDir, 'dcu')]);
    
  // Enforce Map file generation for coverage
  if EnableMap then
    Args := Args + ' /p:DCC_MapFile=3 /p:DCC_GenerateStackFrames=true /p:DCC_Define="DEBUG;TESTING;COVERAGE"';

  RSVars := FindRSVars;
  if RSVars <> '' then
  begin
    WriteLn('Using Environment: ' + RSVars);
    WriteLn('Building project...');
    // Invoke cmd /c "call rsvars && msbuild ..."
    Result := RunProcess('cmd', Format('/c "call "%s" && msbuild %s"', [RSVars, Args]));
  end
  else
  begin
    if GetEnvironmentVariable('BDS') = '' then
    begin
       WriteLn('WARNING: "BDS" environment variable not set and "rsvars.bat" not found.');
       WriteLn('         Build will likely fail.');
    end;
  
    WriteLn('Building project...');
    Result := RunProcess('msbuild', Args);
  end;

  if not Result then
    WriteLn('Error: Build failed.');
end;

procedure TTestCommand.RunTests(const ProjectFile: string; const Args: TCommandLineArgs; Config: TDextConfig);
var
  ExePath: string;
  ProjectName: string;
begin
  if not BuildProject(ProjectFile, False) then Exit;
  
  ProjectName := TPath.GetFileNameWithoutExtension(ProjectFile);
  ExePath := GetExeFile(ProjectName);
  
  if not FileExists(ExePath) then
  begin
    WriteLn('Error: Executable not found at ' + ExePath);
    Exit;
  end;
  
  WriteLn('Running tests...');
  RunProcess(ExePath, '');
end;

procedure TTestCommand.GenerateAutoInclude(const BaseDir, SourceDir: string);
var
  UnitName: string;
  SB: TStringBuilder;
  Files: TArray<string>;
  FileName: string;
  DestFile: string;
begin
  DestFile := TPath.Combine(BaseDir, 'AutoInclude.pas');
  // WriteLn('Generating ' + DestFile + ' to force linkage of all units...');
  
  SB := TStringBuilder.Create;
  try
    SB.AppendLine('unit AutoInclude;');
    SB.AppendLine;
    SB.AppendLine('interface');
    SB.AppendLine;
    SB.AppendLine('uses');
    
    Files := TDirectory.GetFiles(SourceDir, '*.pas', TSearchOption.SoAllDirectories);
    
    for FileName in Files do
    begin
       if FileName.ToLower.Contains('\tests\') then Continue;
       if TPath.GetFileName(FileName).ToLower = 'autoinclude.pas' then Continue;
       
       UnitName := TPath.GetFileNameWithoutExtension(FileName); // Beware of duplicate unit names in diff folders!
       // Assuming unique unit names as per Delphi requirement
       SB.AppendLine('  ' + UnitName + ',');
    end;
    
    SB.AppendLine('  System.SysUtils;'); // Closer
    
    SB.AppendLine;
    SB.AppendLine('implementation');
    SB.AppendLine;
    SB.AppendLine('end.');
    
    TFile.WriteAllText(DestFile, SB.ToString);
  finally
    SB.Free;
  end;
end;

function TTestCommand.GetSourceDirectory(const BaseDir: string): string;
begin
  // Assume standard Dext structure: ProjectRoot/Sources
  // Resolve canonical path to handle '../../' correctly
  Result := TPath.GetFullPath(TPath.Combine(BaseDir, 'Sources'));
  
  if not TDirectory.Exists(Result) then
  begin
    // Try Dext Structure (Tests/Testing -> ../../Sources)
    Result := TPath.GetFullPath(TPath.Combine(BaseDir, '../../Sources'));
    
    if not TDirectory.Exists(Result) then
    begin
       // Try Sibling (Tests -> Sources)
       Result := TPath.GetFullPath(TPath.Combine(BaseDir, '../Sources'));
       
       if not TDirectory.Exists(Result) then
          Result := BaseDir;
    end;
  end;
end;

procedure TTestCommand.GenerateCoverageLists(const BaseDir, SourceDir: string; const Excludes: TArray<string>; out UnitFile, SourcePathFile: string);
var
  Units, Paths: TStringList;
  Files: TArray<string>;
  FileName: string;
  UnitName: string;
  Mask: string;
  Excluded: Boolean;
begin
  WriteLn('Scanning sources in: ' + SourceDir);

  Units := TStringList.Create;
  Paths := TStringList.Create;
  try
    Files := TDirectory.GetFiles(SourceDir, '*.pas', TSearchOption.SoAllDirectories);
    
    for FileName in Files do
    begin
      // Skip Tests and Dext.Testing framework itself if present
      if FileName.ToLower.Contains('\tests\') then Continue;
      
      UnitName := TPath.GetFileNameWithoutExtension(FileName);
      
      // Check Exclusions
      Excluded := False;
      for Mask in Excludes do
      begin
        if MatchesMask(FileName, Mask) or MatchesMask(UnitName, Mask) then
        begin
          Excluded := True;
          Break;
        end;
      end;
      if Excluded then Continue;
      
      Units.Add(UnitName);
      
      // Add directory to paths if unique
      var Dir := ExtractFilePath(FileName);
      if Paths.IndexOf(Dir) = -1 then
        Paths.Add(Dir);
    end;
    
    UnitFile := TPath.Combine(BaseDir, BUILD_DIR, 'units.lst');
    SourcePathFile := TPath.Combine(BaseDir, BUILD_DIR, 'sources.lst');
    
    // Ensure Build Dir
    ForceDirectories(ExtractFilePath(UnitFile));

    Units.SaveToFile(UnitFile);
    Paths.SaveToFile(SourcePathFile);
    
    WriteLn(Format('Coverage Lists Generated: %d units, %d paths.', [Units.Count, Paths.Count]));
  finally
    Units.Free;
    Paths.Free;
  end;
end;

function TTestCommand.FindCodeCoverageExe(const TargetPlatform: string): string;
var
  AppDir: string;
  Candidate: string;
  Current: string;
  I: Integer;
const
  // DextRepository is usually C:\dev\Dext\DextRepository
  // Libs is C:\dev\Dext\Libs
  // So we need to go up from DextTool to DextRepository, then up one more, then down to Libs
  LIBS_PATH = 'Libs\DelphiCodeCoverage'; 
begin
  Result := 'CodeCoverage.exe'; // Default to PATH
  
  AppDir := ExtractFileDir(ParamStr(0));
  
  // 1. Check same directory as CLI executable
  Candidate := TPath.Combine(AppDir, 'CodeCoverage.exe');
  if FileExists(Candidate) then Exit(Candidate);
  
  // Search structure:
  // 1. ../Tools/DelphiCodeCoverage (Repo localized)
  // 2. ../../Libs/DelphiCodeCoverage/{Platform} (Sibling localized)
  
  Current := AppDir;
  for I := 1 to 8 do // Traverse up to 8 levels
  begin
    // Check Repo Tools
    Candidate := TPath.Combine(Current, 'Tools\DelphiCodeCoverage\CodeCoverage.exe');
    if FileExists(Candidate) then Exit(Candidate);
    
    // Check Sibling Libs
    Candidate := TPath.Combine(Current, Format('%s\%s\CodeCoverage.exe', [LIBS_PATH, TargetPlatform]));
    if FileExists(Candidate) then Exit(Candidate);
    
    Current := TPath.GetDirectoryName(Current);
    if (Current = '') or (Current = TPath.GetPathRoot(Current)) then Break;
  end;
end;

procedure TTestCommand.RunWithCoverage(const ProjectFile: string; const Args: TCommandLineArgs; Config: TDextConfig);
var
  ExePath, MapPath: string;
  ProjectName, UnitLst, SourceLst: string;
  CoverageCmd: string;
  DCCExe: string;
  SourceDir: string;
begin
  // 1. Resolve Source Dir & Generate AutoInclude
  SourceDir := GetSourceDirectory(GetCurrentDir);
  GenerateAutoInclude(GetCurrentDir, SourceDir);

  // 2. Generate lists
  GenerateCoverageLists(GetCurrentDir, SourceDir, Config.Test.CoverageExclude, UnitLst, SourceLst);
  
  // 3. Build (After AutoInclude is updated)
  // Currently forcing Win32
  if not BuildProject(ProjectFile, True) then Exit;
  
  ProjectName := TPath.GetFileNameWithoutExtension(ProjectFile);
  ExePath := GetExeFile(ProjectName);
  MapPath := GetMapFile(ProjectName);

  if not FileExists(MapPath) then
  begin
    WriteLn('Error: MAP file not generated. Coverage analysis impossible.');
    Exit;
  end;
  
  DCCExe := FindCodeCoverageExe('Win32');
  if not FileExists(DCCExe) and (ExtractFileDir(DCCExe) <> '') then
  begin
     WriteLn('Error: CodeCoverage.exe not found in Libs or Tools.');
     WriteLn('Checked: ' + DCCExe);
     WriteLn('Please download DelphiCodeCoverage release and unpack to C:\dev\Dext\Libs\DelphiCodeCoverage');
     Exit;
  end;

  WriteLn('Executing Code Coverage using: ' + DCCExe);
  
  // Create Report Directory
  var ReportDir := TPath.Combine(TPath.GetDirectoryName(ExePath), 'report');
  
  // Override if config present
  if Config.Test.ReportDir <> '' then
     ReportDir := TPath.GetFullPath(TPath.Combine(ExtractFileDir(ProjectFile), Config.Test.ReportDir));

  ForceDirectories(ReportDir);

  // Construct DCC command matchin best practices:
  // -lt (Lazy Thread), -html, -xml, -xmllines (Detailed XML for Sonar)
  CoverageCmd := Format('-e "%s" -m "%s" -uf "%s" -spf "%s" -od "%s" -lt -html -xml -xmllines', 
    [ExePath, MapPath, UnitLst, SourceLst, ReportDir]);
    
  if not RunProcess(DCCExe, CoverageCmd) then
    WriteLn('Coverage analysis failed (or tests failed).')
  else
  begin
    WriteLn('Coverage analysis complete. Check output in ' + ReportDir);
    
    // Convert to Sonar
    var DccXml := TPath.Combine(ReportDir, 'CodeCoverage_Summary.xml');
    var SonarXml := TPath.Combine(ReportDir, 'dext_coverage.xml');
    TSonarConverter.Convert(DccXml, SonarXml, SourceDir, Config.Test.CoverageThreshold);
  end;
end;

end.
