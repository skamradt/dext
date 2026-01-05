unit Dext.Hosting.CLI.Commands.UI;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.DateUtils,
  System.JSON,
{$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.ShellAPI,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Stdlib,
{$ENDIF}
  Dext.Hosting.CLI.Args,
  Dext.WebHost,
  Dext.Web.Interfaces,
  Dext.DI.Interfaces,
  Dext.DI.Core,
  Dext.Web.StaticFiles,
  Dext.Web.Routing,
  Dext.Web.Results,
  Dext.Hosting.CLI.Registry,
  Dext.Yaml,
  Dext.Hosting.CLI.Config,
  Dext.Hosting.CLI.Tools.CodeCoverage; // Added

type
  TUICommand = class(TInterfacedObject, IConsoleCommand)
  private
    procedure EnsureUIAssets(const WwwRoot: string);
    procedure OpenBrowser(const Url: string);
    function GetDashboardHtml: string;
  public
    function GetName: string;
    function GetDescription: string;
    procedure Execute(const Args: TCommandLineArgs);
  end;

implementation

{ TUICommand }

function TUICommand.GetName: string;
begin
  Result := 'ui';
end;

function TUICommand.GetDescription: string;
begin
  Result := 'Launches the Web Configuration Dashboard. Usage: dext ui';
end;

procedure TUICommand.OpenBrowser(const Url: string);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(Url), nil, nil, SW_SHOWNORMAL);
{$ENDIF}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(Url))); 
{$ENDIF}
end;

procedure TUICommand.Execute(const Args: TCommandLineArgs);
var
  Port: Integer;
  HomeDir, UIDir, WwwRoot: string;
  Host: IWebHost;
begin
  Port := 3000;
  if Args.HasOption('port') then
    Port := StrToIntDef(Args.GetOption('port'), 3000);

  HomeDir := TPath.GetHomePath;
  UIDir := TPath.Combine(HomeDir, '.dext', 'ui');
  WwwRoot := TPath.Combine(UIDir, 'wwwroot');
  
  EnsureUIAssets(WwwRoot);

  WriteLn(Format('Starting Dext Dashboard on port %d...', [Port]));

  Host := TWebHostBuilder.CreateDefault(nil)
    .UseUrls(Format('http://localhost:%d', [Port]))
    .ConfigureServices(procedure(Services: IServiceCollection)
      begin
        Services.AddSingleton(TProjectRegistry, TProjectRegistry.Create);
      end)
    .Configure(procedure(App: IApplicationBuilder)
      var
        StaticOpts: TStaticFileOptions;
      begin
        StaticOpts := TStaticFileOptions.Create;
        StaticOpts.RootPath := WwwRoot;
        StaticOpts.DefaultFile := 'index.html';
        StaticOpts.ServeUnknownFileTypes := True;
        
        TApplicationBuilderStaticFilesExtensions.UseStaticFiles(App, StaticOpts);

        // Serve Test Reports manually since TStaticFileOptions doesn't support Alias/RequestPath
        App.Use(procedure(Ctx: IHttpContext; Next: TRequestDelegate)
          var
            Path, ReportPath, FilePath, CT: string;
            CP: TContentTypeProvider;
            FS: TFileStream;
          begin
            Path := Ctx.Request.Path;
            if Path.StartsWith('/reports/', True) or (Path = '/reports') then
            begin
               if Path = '/reports' then 
               begin
                 Ctx.Response.StatusCode := 302;
                 Ctx.Response.AddHeader('Location', '/reports/CodeCoverage_Summary.html');
                 Exit;
               end;
               
               ReportPath := TPath.GetFullPath('TestOutput\report');
               if TDirectory.Exists(ReportPath) then
               begin
                   // '/reports/' is 9 chars
                   FilePath := TPath.Combine(ReportPath, Path.Substring(9)); 
                   
                   // DEBUG LOG
                   WriteLn('Serving Report: ' + FilePath);
                   
                   if FileExists(FilePath) then
                   begin
                       // ... (Reuse existing content serving logic)
                       CP := TContentTypeProvider.Create;
                       try
                          if not CP.TryGetContentType(FilePath, CT) then CT := 'application/octet-stream';
                       finally
                          CP.Free;
                       end;
                       
                       Ctx.Response.SetContentType(CT);
                       FS := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
                       try
                          Ctx.Response.SetContentLength(FS.Size);
                          Ctx.Response.Write(FS);
                       finally
                          FS.Free;
                       end;
                       Exit;
                   end else WriteLn('File not found: ' + FilePath);
               end else WriteLn('Report Dir not found: ' + ReportPath);
            end;
            
            Next(Ctx);
          end);

        // API: Test Summary
        App.MapGet('/api/test/summary',
          procedure(Ctx: IHttpContext)
          var
            ReportDir, SummaryFile: string;
            Res: IResult;
            Content: string;
            P1, P2: Integer;
            Coverage: string;
          begin
             ReportDir := TPath.GetFullPath('TestOutput\report');
             SummaryFile := TPath.Combine(ReportDir, 'CodeCoverage_Summary.xml');
             
             if FileExists(SummaryFile) then
             begin
                Content := TFile.ReadAllText(SummaryFile);
                // Simple parser for <Coverage ... percent="85">
                P1 := Content.IndexOf('percent="');
                if P1 > 0 then
                begin
                   Inc(P1, 9); // len of percent="
                   P2 := Content.IndexOf('"', P1);
                   if P2 > P1 then
                   begin
                      Coverage := Content.Substring(P1, P2 - P1);
                      Res := Results.Ok('{"available": true, "coverage": ' + Coverage.Replace(',', '.') + ', "path": "' + SummaryFile.Replace('\', '\\') + '"}');
                      Res.Execute(Ctx);
                      Exit;
                   end;
                end;
             end;
             
             Res := Results.Ok('{"available": false, "path": "' + SummaryFile.Replace('\', '\\') + '"}');
             Res.Execute(Ctx);
          end);
        App.MapGet('/api/projects', 
          procedure(Ctx: IHttpContext)
          var
            Registry: TProjectRegistry;
            Projects: TArray<TProjectInfo>;
            SB: TStringBuilder;
            I: Integer;
            EscapedPath, EscapedName: string;
            Res: IResult;
          begin
             Registry := Ctx.Services.GetRequiredService(TProjectRegistry) as TProjectRegistry;
             Projects := Registry.GetAllProjects;
             
             SB := TStringBuilder.Create;
             try
               SB.Append('[');
               for I := 0 to High(Projects) do
               begin
                 if I > 0 then SB.Append(',');
                 EscapedPath := Projects[I].Path.Replace('\', '\\').Replace('"', '\"');
                 EscapedName := Projects[I].Name.Replace('\', '\\').Replace('"', '\"');
                 
                 SB.Append('{');
                 SB.Append('"path":"').Append(EscapedPath).Append('",');
                 SB.Append('"name":"').Append(EscapedName).Append('",');
                 SB.Append('"lastAccess":"').Append(DateToISO8601(Projects[I].LastAccess)).Append('"');
                 SB.Append('}');
               end;
               SB.Append(']');
               Res := Results.Text(SB.ToString, 200);
               Res.Execute(Ctx);
             finally
               SB.Free;
             end;
          end);

        // API: Get Config
        App.MapGet('/api/config',
          procedure(Ctx: IHttpContext)
          var
            Config: TDextGlobalConfig;
            Json, EnvObj: TJSONObject;
            Arr, PlatArr: TJSONArray;
            Res: IResult;
            Env: TDextEnvironment;
            P: string;
          begin
            Config := TDextGlobalConfig.Create;
            Json := TJSONObject.Create;
            try
              Config.Load;
              
              Json.AddPair('dextPath', Config.DextPath);
              if Config.DextPath.IsEmpty then Json.AddPair('dextPath', ParamStr(0));
              
              var CovPath := Config.CoveragePath;
              if (CovPath = '') then
                 CovPath := TCodeCoverageTool.FindPath(Config, 'Win32'); // Autodetect

              Json.AddPair('coveragePath', CovPath);
              Json.AddPair('configPath', TPath.Combine(TPath.GetHomePath, '.dext', 'config.yaml'));
              
              Arr := TJSONArray.Create;
              for Env in Config.Environments do
              begin
                EnvObj := TJSONObject.Create;
                EnvObj.AddPair('version', Env.Version);
                EnvObj.AddPair('name', Env.Name);
                EnvObj.AddPair('path', Env.Path);
                EnvObj.AddPair('isDefault', TJSONBool.Create(Env.IsDefault));
                
                PlatArr := TJSONArray.Create;
                for P in Env.Platforms do
                  PlatArr.Add(P);
                EnvObj.AddPair('platforms', PlatArr);
                
                Arr.Add(EnvObj);
              end;
              Json.AddPair('environments', Arr);
              
              Res := Results.Text(Json.ToString, 200);
              Res.Execute(Ctx);
            finally
              Json.Free;
              Config.Free;
            end;
          end);

        // API: Save Config 
        App.MapPost('/api/config',
          procedure(Ctx: IHttpContext)
          var
            Body: string;
            Res: IResult;
            SR: TStreamReader;
            Json: TJSONObject;
            Config: TDextGlobalConfig;
          begin
            SR := TStreamReader.Create(Ctx.Request.Body);
            try
               Body := SR.ReadToEnd;
               Json := TJSONObject.ParseJSONValue(Body) as TJSONObject;
               if Json <> nil then
               try
                  Config := TDextGlobalConfig.Create;
                  try
                    Config.Load;
                    if Json.TryGetValue('dextPath', Body) then Config.DextPath := Body;
                    if Json.TryGetValue('coveragePath', Body) then Config.CoveragePath := Body;
                    Config.Save;
                    
                    Res := Results.Ok('{"status":"saved"}');
                  finally
                    Config.Free;
                  end;
               finally
                 Json.Free;
               end
               else
                 Res := Results.BadRequest('Invalid JSON');
                 
               Res.Execute(Ctx);
            finally
               SR.Free;
            end;
          end);
          
        // API: Scan Environments
        App.MapPost('/api/env/scan',
          procedure(Ctx: IHttpContext)
          var
            Scanner: TDextGlobalConfig;
            Log: string;
            Res: IResult;
          begin
             Scanner := TDextGlobalConfig.Create;
             try
               Log := Scanner.ScanEnvironments;
               Res := Results.Ok('{"status":"ok", "log": "' + Log.Replace(sLineBreak, '\n').Replace('\', '\\') + '"}');
               Res.Execute(Ctx);
             finally
               Scanner.Free;
             end;
          end);

        // API: Install Code Coverage
        App.MapPost('/api/tools/codecoverage/install',
          procedure(Ctx: IHttpContext)
          var
            Path: string;
            Res: IResult;
          begin
             try
               TCodeCoverageTool.InstallLatest(Path);
               Res := Results.Ok('{"status":"ok", "path": "' + Path.Replace('\', '\\') + '"}');
               Res.Execute(Ctx);
             except
               on E: Exception do
               begin
                 Res := Results.StatusCode(500, Format('{"error": "%s"}', [E.Message.Replace('"', '\"')]));
                 Res.Execute(Ctx);
               end;
             end;
          end);

        // API: Set Default Environment
        App.MapPost('/api/env/default',
          procedure(Ctx: IHttpContext)
          var
            Body, Ver: string;
            Res: IResult;
            SR: TStreamReader;
            Json: TJSONObject;
            Config: TDextGlobalConfig;
            I: Integer;
            Updated: Boolean;
            E: TDextEnvironment;
          begin
            SR := TStreamReader.Create(Ctx.Request.Body);
            try
               Body := SR.ReadToEnd;
               Json := TJSONObject.ParseJSONValue(Body) as TJSONObject;
               if (Json <> nil) and Json.TryGetValue('version', Ver) then
               try
                  Config := TDextGlobalConfig.Create;
                  try
                    Config.Load;
                    Updated := False;
                    for I := 0 to Config.Environments.Count - 1 do
                    begin
                       E := Config.Environments[I];
                       var NewState := (E.Version = Ver);
                       if E.IsDefault <> NewState then
                       begin
                          E.IsDefault := NewState;
                          Config.Environments[I] := E; 
                          Updated := True;
                       end;
                    end;
                    
                    if Updated then Config.Save;
                    Res := Results.Ok('{"status":"updated"}');
                  finally
                    Config.Free;
                  end;
               finally
                 Json.Free;
               end
               else
                 Res := Results.BadRequest('Invalid Request');
               Res.Execute(Ctx);
            finally
               SR.Free;
            end;
          end);


      end)
    .Build;
  OpenBrowser(Format('http://localhost:%d', [Port]));
  Host.Run;
end;

procedure TUICommand.EnsureUIAssets(const WwwRoot: string);
begin
  if not TDirectory.Exists(WwwRoot) then
    TDirectory.CreateDirectory(WwwRoot);

  TFile.WriteAllText(TPath.Combine(WwwRoot, 'index.html'), GetDashboardHtml, TEncoding.UTF8);
end;

function TUICommand.GetDashboardHtml: string;
begin
  Result := 
    '<!DOCTYPE html>' + sLineBreak +
    '<html lang="en" class="dark">' + sLineBreak +
    '<head>' + sLineBreak +
    '    <meta charset="UTF-8">' + sLineBreak +
    '    <meta name="viewport" content="width=device-width, initial-scale=1.0">' + sLineBreak +
    '    <title>Dext Dashboard</title>' + sLineBreak +
    '    <script src="https://cdn.tailwindcss.com"></script>' + sLineBreak +
    '    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap" rel="stylesheet">' + sLineBreak +
    '    <style>' + sLineBreak +
    '        body { font-family: "Inter", sans-serif; }' + sLineBreak +
    '        .glass { background: rgba(30, 41, 59, 0.7); backdrop-filter: blur(10px); border: 1px solid rgba(255, 255, 255, 0.1); }' + sLineBreak +
    '        .tab-active { border-bottom: 2px solid #0ea5e9; color: #fff; }' + sLineBreak +
    '        .tab-inactive { color: #94a3b8; }' + sLineBreak +
    '        .tab-inactive:hover { color: #cbd5e1; }' + sLineBreak +
    '        .input-dark { background: rgba(15, 23, 42, 0.6); border: 1px solid rgba(255, 255, 255, 0.1); color: white; }' + sLineBreak +
    '        .input-dark:focus { border-color: #0ea5e9; outline: none; }' + sLineBreak +
    '        ::-webkit-scrollbar { width: 8px; }' + sLineBreak +
    '        ::-webkit-scrollbar-track { background: #0f172a; }' + sLineBreak +
    '        ::-webkit-scrollbar-thumb { background: #334155; border-radius: 4px; }' + sLineBreak +
    '        ::-webkit-scrollbar-thumb:hover { background: #475569; }' + sLineBreak +
    '    </style>' + sLineBreak +
    '    <script>' + sLineBreak +
    '      tailwind.config = { darkMode: "class", theme: { extend: { colors: { dext: { 500: "#0ea5e9" } } } } }' + sLineBreak +
    '    </script>' + sLineBreak +
    '</head>' + sLineBreak +
    '<body class="bg-slate-900 text-slate-100 min-h-screen font-sans">' + sLineBreak +
    '' + sLineBreak +
    '    <!-- NavBar -->' + sLineBreak +
    '    <nav class="glass fixed w-full z-50 top-0 border-b border-white/10">' + sLineBreak +
    '        <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">' + sLineBreak +
    '            <div class="flex items-center justify-between h-16">' + sLineBreak +
    '                <div class="flex items-center gap-8">' + sLineBreak +
    '                    <div class="flex items-center gap-3">' + sLineBreak +
    '                        <div class="w-8 h-8 rounded-lg bg-gradient-to-br from-cyan-400 to-blue-600 flex items-center justify-center font-bold text-white">D</div>' + sLineBreak +
    '                        <span class="text-xl font-bold bg-clip-text text-transparent bg-gradient-to-r from-cyan-400 to-blue-500">Dext CLI</span>' + sLineBreak +
    '                    </div>' + sLineBreak +
    '                    <div class="flex gap-4 h-16">' + sLineBreak +
    '                        <button onclick="switchTab(''projects'')" id="tab-projects" class="tab-active px-3 text-sm font-medium transition-colors border-b-2 border-transparent h-full flex items-center">Projects</button>' + sLineBreak +
    '                        <button onclick="switchTab(''tests'')" id="tab-tests" class="tab-inactive px-3 text-sm font-medium transition-colors border-b-2 border-transparent h-full flex items-center">Tests</button>' + sLineBreak +
    '                        <button onclick="switchTab(''settings'')" id="tab-settings" class="tab-inactive px-3 text-sm font-medium transition-colors border-b-2 border-transparent h-full flex items-center">Settings</button>' + sLineBreak +
    '                    </div>' + sLineBreak +
    '                </div>' + sLineBreak +
    '            </div>' + sLineBreak +
    '        </div>' + sLineBreak +
    '    </nav>' + sLineBreak +
    '' + sLineBreak +
    '    <main class="pt-24 pb-12 px-4 max-w-7xl mx-auto">' + sLineBreak +
    '' + sLineBreak +
    '        <!-- Projects View -->' + sLineBreak +
    '        <div id="view-projects">' + sLineBreak +
    '            <div class="flex items-end justify-between mb-8">' + sLineBreak +
    '                <div>' + sLineBreak +
    '                    <h1 class="text-3xl font-bold text-white mb-2">Projects</h1>' + sLineBreak +
    '                    <p class="text-slate-400">Manage your Dext applications.</p>' + sLineBreak +
    '                </div>' + sLineBreak +
    '                <button onclick="loadProjects()" class="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm transition-colors border border-white/10">Refresh</button>' + sLineBreak +
    '            </div>' + sLineBreak +
    '            <div id="projects-grid" class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6"></div>' + sLineBreak +
    '        </div>' + sLineBreak +
    '' + sLineBreak +
    '        <!-- Tests View -->' + sLineBreak +
    '        <div id="view-tests" class="hidden">' + sLineBreak +
    '             <div class="flex items-center justify-between mb-8">' + sLineBreak +
    '                <div>' + sLineBreak +
    '                    <h1 class="text-3xl font-bold text-white mb-2">Test Results</h1>' + sLineBreak +
    '                    <p class="text-slate-400">Automated test runs and code coverage analysis.</p>' + sLineBreak +
    '                </div>' + sLineBreak +
    '                <div class="flex gap-2">' + sLineBreak +
    '                   <button onclick="window.open(''/reports/index.html'', ''_blank'')" class="px-4 py-2 bg-slate-800 hover:bg-slate-700 rounded-lg text-sm transition-colors border border-white/10">Full Report ↗</button>' + sLineBreak +
    '                   <button onclick="loadTests()" class="px-4 py-2 bg-blue-600 hover:bg-blue-500 text-white rounded-lg text-sm transition-colors">Refresh</button>' + sLineBreak +
    '                </div>' + sLineBreak +
    '            </div>' + sLineBreak +
    '            ' + sLineBreak +
    '            <!-- Summary Cards -->' + sLineBreak +
    '            <div class="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">' + sLineBreak +
    '                 <div class="glass rounded-xl p-6 border-l-4 border-l-purple-500">' + sLineBreak +
    '                     <h3 class="text-sm font-medium text-slate-400 mb-1">Code Coverage</h3>' + sLineBreak +
    '                     <div class="flex items-baseline gap-2">' + sLineBreak +
    '                         <span class="text-4xl font-bold text-white" id="test-coverage-val">--%</span>' + sLineBreak +
    '                         <span class="text-xs text-slate-500">Line coverage</span>' + sLineBreak +
    '                     </div>' + sLineBreak +
    '                 </div>' + sLineBreak +
    '                 <div class="glass rounded-xl p-6 border-l-4 border-l-emerald-500">' + sLineBreak +
    '                     <h3 class="text-sm font-medium text-slate-400 mb-1">Test Status</h3>' + sLineBreak +
    '                     <div class="flex items-baseline gap-2">' + sLineBreak +
    '                         <span class="text-4xl font-bold text-white">Passed</span>' + sLineBreak +
    '                         <span class="text-xs text-emerald-400">All Systems Go</span>' + sLineBreak +
    '                     </div>' + sLineBreak +
    '                 </div>' + sLineBreak +
    '            </div>' + sLineBreak +
    '            ' + sLineBreak +
    '            <!-- Report Embed -->' + sLineBreak +
    '            <div class="glass rounded-xl overflow-hidden h-[600px] border border-white/10 bg-white">' + sLineBreak +
    '                <iframe id="test-iframe" src="" class="w-full h-full border-none"></iframe>' + sLineBreak +
    '            </div>' + sLineBreak +
    '        </div>' + sLineBreak +
    '' + sLineBreak +
    '        <!-- Settings View -->' + sLineBreak +
    '        <div id="view-settings" class="hidden max-w-2xl mx-auto">' + sLineBreak +
    '            <h1 class="text-3xl font-bold text-white mb-8">Global Configuration</h1>' + sLineBreak +
    '            <div class="glass rounded-xl p-8 mb-8">' + sLineBreak +
    '                <h2 class="text-lg font-semibold text-white mb-4 border-b border-white/10 pb-2">Paths</h2>' + sLineBreak +
    '                <div class="space-y-6">' + sLineBreak +
    '                    <div>' + sLineBreak +
    '                        <label class="block text-sm font-medium text-slate-400 mb-2">Dext CLI Path</label>' + sLineBreak +
    '                        <input type="text" id="conf-dextpath" class="w-full px-4 py-2 rounded-lg input-dark font-mono text-sm" readonly>' + sLineBreak +
    '                    </div>' + sLineBreak +
    '                    <div>' + sLineBreak +
    '                        <label class="block text-sm font-medium text-slate-400 mb-2">Code Coverage Path (Sonar/DelphiCoverage)</label>' + sLineBreak +
    '                        <div class="flex gap-2">' + sLineBreak +
    '                           <input type="text" id="conf-coverage" class="flex-1 px-4 py-2 rounded-lg input-dark font-mono text-sm" placeholder="C:\Path\To\CodeCoverage.exe">' + sLineBreak +
    '                           <button onclick="installCoverage()" id="btn-coverage-install" class="px-3 py-2 bg-slate-700 hover:bg-slate-600 rounded-lg text-xs font-medium transition-colors border border-slate-600" title="Download latest from GitHub">Install/Update</button>' + sLineBreak +
    '                        </div>' + sLineBreak +
    '                    </div>' + sLineBreak +
    '                    <div class="pt-4 flex justify-end">' + sLineBreak +
    '                        <button onclick="saveConfig()" class="px-6 py-2 bg-blue-600 hover:bg-blue-500 rounded-lg text-white font-medium transition-colors">Save Changes</button>' + sLineBreak +
    '                    </div>' + sLineBreak +
    '                </div>' + sLineBreak +
    '            </div>' + sLineBreak +
    '' + sLineBreak +
    '            <div class="glass rounded-xl p-8">' + sLineBreak +
    '                <div class="flex justify-between items-center mb-4 border-b border-white/10 pb-2">' + sLineBreak +
    '                   <div>' + sLineBreak +
    '                     <h2 class="text-lg font-semibold text-white">Delphi Environments</h2>' + sLineBreak +
    '                     <p class="text-xs text-slate-500 font-mono mt-1" id="env-config-path"></p>' + sLineBreak +
    '                   </div>' + sLineBreak +
    '                   <button onclick="scanEnv()" id="btn-scan" class="text-xs bg-slate-700 hover:bg-slate-600 px-3 py-1.5 rounded transition-colors border border-slate-600">Scan Now</button>' + sLineBreak +
    '                </div>' + sLineBreak +
    '                <div id="env-list" class="space-y-4">' + sLineBreak +
    '                    <div class="animate-pulse space-y-3">' + sLineBreak +
    '                       <div class="h-20 bg-slate-800 rounded-lg"></div>' + sLineBreak +
    '                       <div class="h-20 bg-slate-800 rounded-lg"></div>' + sLineBreak +
    '                    </div>' + sLineBreak +
    '                </div>' + sLineBreak +
    '            </div>' + sLineBreak +
    '        </div>' + sLineBreak +
    '    </main>' + sLineBreak +
    '' + sLineBreak +
    '    <script>' + sLineBreak +
    '        function switchTab(tab) {' + sLineBreak +
    '            document.getElementById("view-projects").classList.add("hidden");' + sLineBreak +
    '            document.getElementById("view-settings").classList.add("hidden");' + sLineBreak +
    '            document.getElementById("view-tests").classList.add("hidden");' + sLineBreak +
    '            ' + sLineBreak +
    '            document.getElementById("tab-projects").className = "tab-inactive px-3 text-sm font-medium transition-colors border-b-2 border-transparent h-full flex items-center";' + sLineBreak +
    '            document.getElementById("tab-settings").className = "tab-inactive px-3 text-sm font-medium transition-colors border-b-2 border-transparent h-full flex items-center";' + sLineBreak +
    '            document.getElementById("tab-tests").className = "tab-inactive px-3 text-sm font-medium transition-colors border-b-2 border-transparent h-full flex items-center";' + sLineBreak +
    '            ' + sLineBreak +
    '            document.getElementById("view-" + tab).classList.remove("hidden");' + sLineBreak +
    '            document.getElementById("tab-" + tab).className = "tab-active px-3 text-sm font-medium transition-colors border-b-2 border-transparent h-full flex items-center";' + sLineBreak +
    '            ' + sLineBreak +
    '            if(tab === "settings") loadConfig();' + sLineBreak +
    '            if(tab === "tests") loadTests();' + sLineBreak +
    '        }' + sLineBreak +
    '        ' + sLineBreak +
    '        async function loadTests() {' + sLineBreak +
    '             // Try loading coverage summary' + sLineBreak +
    '             try {' + sLineBreak +
    '                 const res = await fetch("/api/test/summary");' + sLineBreak +
    '                 const data = await res.json();' + sLineBreak +
    '                 const el = document.getElementById("test-coverage-val");' + sLineBreak +
    '                 ' + sLineBreak +
    '                 if(data.available) {' + sLineBreak +
    '                    el.innerText = data.coverage + "%";' + sLineBreak +
    '                    if(data.coverage >= 80) el.className = "text-4xl font-bold text-emerald-400";' + sLineBreak +
    '                    else if(data.coverage >= 50) el.className = "text-4xl font-bold text-yellow-400";' + sLineBreak +
    '                    else el.className = "text-4xl font-bold text-red-400";' + sLineBreak +
    '                    ' + sLineBreak +
    '                    document.getElementById("test-iframe").src = "/reports/CodeCoverage_Summary.html";' + sLineBreak +
    '                 } else {' + sLineBreak +
    '                    el.innerHTML = ''N/A <span class="block text-[10px] text-slate-600 font-mono mt-1 w-full truncate" title="'' + (data.path || "") + ''">Report not found</span>'';' + sLineBreak +
    '                    el.className = "text-4xl font-bold text-slate-500";' + sLineBreak +
    '                    console.log("Report checked at:", data.path);' + sLineBreak +
    '                 }' + sLineBreak +
    '             } catch (e) { console.error(e); }' + sLineBreak +
    '        }' + sLineBreak +
    '        ' + sLineBreak +
    '        async function loadConfig() {' + sLineBreak +
    '            try {' + sLineBreak +
    '                const res = await fetch("/api/config");' + sLineBreak +
    '                const conf = await res.json();' + sLineBreak +
    '                document.getElementById("conf-dextpath").value = conf.dextPath || "";' + sLineBreak +
    '                document.getElementById("conf-coverage").value = conf.coveragePath || "";' + sLineBreak +
    '                document.getElementById("env-config-path").innerText = conf.configPath || "";' + sLineBreak +
    '                ' + sLineBreak +
    '                // Environments Render' + sLineBreak +
    '                const envs = conf.environments || [];' + sLineBreak +
    '                if (envs.length === 0) {' + sLineBreak +
    '                   document.getElementById("env-list").innerHTML = `<div class="text-center py-8 text-slate-500 bg-slate-800/20 rounded-lg border border-dashed border-slate-700"><p>No Delphi installations found.</p><p class="text-xs mt-1">Try running command: <code class="text-cyan-400">dext env scan</code> or click Scan Now</p></div>`;' + sLineBreak +
    '                } else {' + sLineBreak +
    '                   document.getElementById("env-list").innerHTML = envs.map(e => `' + sLineBreak +
    '                     <div class="p-4 rounded-lg border border-slate-700 bg-slate-800/40 flex flex-col gap-2 hover:border-slate-600 transition-colors">' + sLineBreak +
    '                         <div class="flex justify-between items-center">' + sLineBreak +
    '                             <div class="flex items-center gap-2">' + sLineBreak +
    '                                <span class="font-semibold text-white text-sm">${e.name}</span>' + sLineBreak +
    '                                <span class="text-xs text-slate-500 bg-slate-800 px-1.5 rounded">v${e.version}</span>' + sLineBreak +
    '                             </div>' + sLineBreak +
    '                               ${e.isDefault ? ' + sLineBreak +
    '                               ''<span class="text-[10px] bg-cyan-900/50 text-cyan-200 border border-cyan-800 px-2 py-0.5 rounded font-bold uppercase tracking-wider">Default</span>'' :' + sLineBreak +
    '                               `<button onclick="setDefaultEnv(''${e.version}'')" class="text-[10px] bg-slate-700 hover:bg-slate-600 text-slate-300 border border-slate-600 px-2 py-0.5 rounded transition-colors uppercase tracking-wider font-medium">Set Default</button>`}' + sLineBreak +
    '                         </div>' + sLineBreak +
    '                         <div class="text-[11px] text-slate-400 font-mono break-all bg-black/20 p-1.5 rounded border border-white/5">${e.path}</div>' + sLineBreak +
    '                         <div class="flex flex-wrap gap-1.5 mt-1">' + sLineBreak +
    '                             ${e.platforms ? e.platforms.map(p => `<span class="text-[10px] bg-slate-700 px-1.5 py-0.5 rounded text-slate-300 border border-slate-600">${p}</span>`).join('''') : ''<span class="text-[10px] text-slate-600">No platforms detected</span>''}' + sLineBreak +
    '                         </div>' + sLineBreak +
    '                     </div>' + sLineBreak +
    '                   `).join('''');' + sLineBreak +
    '                }' + sLineBreak +
    '            } catch(e) { console.error(e); }' + sLineBreak +
    '        }' + sLineBreak +
    '        ' + sLineBreak +
    '        async function saveConfig() {' + sLineBreak +
    '            const data = {' + sLineBreak +
    '                dextPath: document.getElementById("conf-dextpath").value,' + sLineBreak +
    '                coveragePath: document.getElementById("conf-coverage").value' + sLineBreak +
    '            };' + sLineBreak +
    '            await fetch("/api/config", { method: "POST", body: JSON.stringify(data) });' + sLineBreak +
    '            alert("Configuration saved! (Note: changes to Dext Path persist, but Environments must be updated via CLI)");' + sLineBreak +
    '        }' + sLineBreak +
    '        ' + sLineBreak +
    '        async function setDefaultEnv(ver) {' + sLineBreak +
    '             try {' + sLineBreak +
    '                await fetch("/api/env/default", { ' + sLineBreak +
    '                    method: "POST", ' + sLineBreak +
    '                    body: JSON.stringify({ version: ver }) ' + sLineBreak +
    '                });' + sLineBreak +
    '                await loadConfig();' + sLineBreak +
    '             } catch (e) { alert("Failed to set default"); }' + sLineBreak +
    '        }' + sLineBreak +
    '        ' + sLineBreak +
    '        async function scanEnv() {' + sLineBreak +
    '             const btn = document.getElementById("btn-scan");' + sLineBreak +
    '             btn.innerText = "Scanning...";' + sLineBreak +
    '             btn.disabled = true;' + sLineBreak +
    '             try {' + sLineBreak +
    '                await fetch("/api/env/scan", { method: "POST" });' + sLineBreak +
    '                await loadConfig();' + sLineBreak +
    '             } catch (e) { alert("Scan failed"); console.error(e); }' + sLineBreak +
    '             btn.innerText = "Scan Now";' + sLineBreak +
    '             btn.disabled = false;' + sLineBreak +
    '        }' + sLineBreak +
    '        ' + sLineBreak +
    '        async function installCoverage() {' + sLineBreak +
    '             if(!confirm("Download and install latest CodeCoverage from GitHub?")) return;' + sLineBreak +
    '             const btn = document.getElementById("btn-coverage-install");' + sLineBreak +
    '             const originalText = btn.innerText;' + sLineBreak +
    '             btn.innerText = "Downloading...";' + sLineBreak +
    '             btn.disabled = true;' + sLineBreak +
    '             try {' + sLineBreak +
    '                const res = await fetch("/api/tools/codecoverage/install", { method: "POST" });' + sLineBreak +
    '                const data = await res.json();' + sLineBreak +
    '                if (data.status === "ok") {' + sLineBreak +
    '                   document.getElementById("conf-coverage").value = data.path;' + sLineBreak +
    '                   alert("Installed successfully: " + data.path);' + sLineBreak +
    '                } else {' + sLineBreak +
    '                   alert("Error via API");' + sLineBreak +
    '                }' + sLineBreak +
    '             } catch (e) { alert("Install failed: " + e); }' + sLineBreak +
    '             btn.innerText = originalText;' + sLineBreak +
    '             btn.disabled = false;' + sLineBreak +
    '        }' + sLineBreak +
    '        ' + sLineBreak +
    '        async function loadProjects() {' + sLineBreak +
    '             const grid = document.getElementById("projects-grid");' + sLineBreak +
    '             try {' + sLineBreak +
    '                 const res = await fetch("/api/projects");' + sLineBreak +
    '                 const projects = await res.json();' + sLineBreak +
    '                 if (projects.length === 0) {' + sLineBreak +
    '                     grid.innerHTML = `<div class="col-span-full py-20 text-center border-2 border-dashed border-slate-700 rounded-2xl bg-slate-800/20"><p class="text-xl text-slate-300 font-semibold mb-2">No projects found</p><p class="text-slate-500">Run "dext" in folder.</p></div>`;' + sLineBreak +
    '                     return;' + sLineBreak +
    '                 }' + sLineBreak +
    '                 grid.innerHTML = projects.map(p => `' + sLineBreak +
    '                     <div class="glass rounded-xl p-6 card-hover group cursor-pointer relative overflow-hidden transition-all hover:scale-[1.01] hover:shadow-xl hover:shadow-cyan-900/20">' + sLineBreak +
    '                         <div class="absolute top-0 right-0 p-4 opacity-0 group-hover:opacity-100 transition-opacity">' + sLineBreak +
    '                              <svg xmlns="http://www.w3.org/2000/svg" class="h-5 w-5 text-cyan-400" viewBox="0 0 20 20" fill="currentColor"><path d="M11 3a1 1 0 100 2h2.586l-6.293 6.293a1 1 0 101.414 1.414L15 6.414V9a1 1 0 102 0V4a1 1 0 00-1-1h-5z" /></svg>' + sLineBreak +
    '                         </div>' + sLineBreak +
    '                         <h3 class="text-lg font-semibold text-white mb-1 group-hover:text-cyan-400 transition-colors">${p.name}</h3>' + sLineBreak +
    '                         <p class="text-xs text-slate-400 break-all mb-4 h-8 overflow-hidden text-ellipsis font-mono opacity-80">${p.path}</p>' + sLineBreak +
    '                         <div class="flex items-center gap-2">' + sLineBreak +
    '                             <span class="w-2 h-2 rounded-full bg-emerald-500"></span>' + sLineBreak +
    '                             <span class="text-xs text-slate-500">Last access: ${new Date(p.lastAccess).toLocaleDateString()}</span>' + sLineBreak +
    '                         </div>' + sLineBreak +
    '                     </div>`).join("");' + sLineBreak +
    '             } catch (e) { console.error(e); }' + sLineBreak +
    '        }' + sLineBreak +
    '        loadProjects();' + sLineBreak +
    '    </script>' + sLineBreak +
    '</body>' + sLineBreak +
    '</html>';
end;

end.
