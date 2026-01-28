unit Dext.Dashboard.TestRunner;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  WinApi.Windows,
  WinApi.ShellAPI;

type
  TTestRunner = class
  private
    class function FindExecutable(const AProjectPath: string): string;
    class function ExecuteProcess(const AExePath, AParams: string): Boolean;

  public
    class function RunProject(const AProjectPath: string): TJSONObject;
  end;

implementation

{ TTestRunner }



class function TTestRunner.ExecuteProcess(const AExePath, AParams: string): Boolean;
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  CmdLine: string;
begin
  Result := False;
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_HIDE; 
  
  // We use -no-wait argument to tell the runner not to pause at the end
  CmdLine := Format('"%s" %s', [AExePath, AParams]);
  UniqueString(CmdLine);

  if CreateProcess(nil, PChar(CmdLine), nil, nil, False, 0, nil, PChar(TPath.GetDirectoryName(AExePath)), SI, PI) then
  begin
    // We do NOT wait for completion. Fire and forget.
    // The runner will report back via Telemetry.
    CloseHandle(PI.hProcess);
    CloseHandle(PI.hThread);
    Result := True;
  end;
end;

class function TTestRunner.FindExecutable(const AProjectPath: string): string;
var
  BaseName, ProjectDir: string;
  Candidates: TArray<string>;
  Path: string;
begin
  Result := '';
  BaseName := TPath.GetFileNameWithoutExtension(AProjectPath);
  ProjectDir := TPath.GetDirectoryName(AProjectPath);
  
  // Potential locations
  Candidates := [
    TPath.Combine(ProjectDir, BaseName + '.exe'),
    TPath.Combine(TPath.Combine(ProjectDir, 'TestOutput'), BaseName + '.exe'),
    TPath.Combine(TPath.Combine(ProjectDir, 'Output'), BaseName + '.exe'),
    // Repo Root Output (approximate)
    TPath.Combine(TPath.GetFullPath(TPath.Combine(ProjectDir, '..\..\Output')), BaseName + '.exe'),
    // Common Win32 Debug output
    TPath.Combine(TPath.Combine(ProjectDir, 'Win32\Debug'), BaseName + '.exe'),
    // Tests/Output (Relative to Tests/Testing)
    TPath.Combine(TPath.Combine(ProjectDir, '..\Output'), BaseName + '.exe')
  ];
  
  for Path in Candidates do
  begin

    if FileExists(Path) then Exit(Path);
  end;

end;

class function TTestRunner.RunProject(const AProjectPath: string): TJSONObject;
var
  ExePath: string;
  ResultsFile: string;
begin
  ExePath := FindExecutable(AProjectPath);

  
  if ExePath = '' then
  begin

    Result := TJSONObject.Create;
    Result.AddPair('error', 'Test executable not found. Please build the project first.');
    Exit;
  end;
  

  
  // Delete previous results...
  ResultsFile := TPath.Combine(TPath.GetDirectoryName(ExePath), 'test-results.json');
  if FileExists(ResultsFile) then TFile.Delete(ResultsFile);
  
  // Launch Async
  if ExecuteProcess(ExePath, '-no-wait') then
  begin

      Result := TJSONObject.Create;
      Result.AddPair('status', 'started');
      Result.AddPair('message', 'Tests are running in background. Check dashboard for real-time progress.');
  end
  else
  begin

      Result := TJSONObject.Create;
      Result.AddPair('error', 'Failed to execute test process.');
  end;
end;

end.
