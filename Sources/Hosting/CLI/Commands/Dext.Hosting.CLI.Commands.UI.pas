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
  Dext.Web.Hubs.Extensions,
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
  Dext.Hosting.CLI.Tools.CodeCoverage,
  Dext.Web.Hubs, // SignalR Support
  Dext.Hosting.CLI.Hubs.Dashboard, // Our Dashboard Hub
  Dext.Logging,
  Dext.Hosting.CLI.Logger,
  Dext.Utils;

type
  TUICommand = class(TInterfacedObject, IConsoleCommand)
  private
    procedure EnsureUIAssets(const WwwRoot: string);
    procedure OpenBrowser(const Url: string);

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

  {$IFDEF MSWINDOWS}
  HomeDir := GetEnvironmentVariable('USERPROFILE');
  {$ELSE}
  HomeDir := TPath.GetHomePath;
  {$ENDIF}
  UIDir := TPath.Combine(HomeDir, '.dext', 'ui');
  WwwRoot := TPath.Combine(UIDir, 'wwwroot');
  
  EnsureUIAssets(WwwRoot);

  SafeWriteLn(Format('Starting Dext Dashboard V2 (Vue) on port %d...', [Port]));

  Host := TWebHostBuilder.CreateDefault(nil)
    .UseUrls(Format('http://localhost:%d', [Port]))
    .ConfigureServices(procedure(Services: IServiceCollection)
      begin
        Services.AddSingleton(TProjectRegistry, TProjectRegistry.Create);
        
        // Logging Setup
        Services.AddSingleton(TypeInfo(ILoggerFactory), nil,
           function(Provider: IServiceProvider): TObject
           var
             Factory: TLoggerFactory;
           begin
              Factory := TLoggerFactory.Create;
              Factory.AddProvider(TConsoleHubLoggerProvider.Create);
              Result := Factory;
           end);
      end)
    .Configure(procedure(App: IApplicationBuilder)
      var
        StaticOpts: TStaticFileOptions;
      begin
        // Register Hubs
        THubExtensions.MapHub(App, '/hubs/dashboard', TDashboardHub);

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
                   SafeWriteLn('Serving Report: ' + FilePath);
                   
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
                   end else SafeWriteLn('File not found: ' + FilePath);
               end else SafeWriteLn('Report Dir not found: ' + ReportPath);
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
            Logger: ILogger;
            Factory: ILoggerFactory;
          begin
             // Log this request to Dashboard Hub (Best effort)
             Factory := Ctx.Services.GetServiceAsInterface(TypeInfo(ILoggerFactory)) as ILoggerFactory;
             if Factory <> nil then
             begin
                Logger := Factory.CreateLogger('Api');
                Logger.LogInformation('Client requested Project List from %s', [Ctx.Request.RemoteIpAddress]);
             end;

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
    
  if not TFile.Exists(TPath.Combine(WwwRoot, 'index.html')) then
  begin
     SafeWriteLn('[WARN] Dashboard frontend not found in ' + WwwRoot);
     SafeWriteLn('       Running "dext ui" will show 404 until you build the vue-app.');
  end;
end;

end.

