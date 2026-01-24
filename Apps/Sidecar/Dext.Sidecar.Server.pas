unit Dext.Sidecar.Server;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.DateUtils,
  System.JSON,
  System.Types,
  System.SyncObjs,
  Winapi.Windows,
  Dext.DI.Core,
  Dext.DI.Interfaces,
  Dext.Hosting.CLI.Config,
  Dext.Hosting.CLI.Hubs.Dashboard,
  Dext.Hosting.CLI.Logger,
  Dext.Hosting.CLI.Registry,
  Dext.Hosting.CLI.Tools.CodeCoverage,
  Dext.Http.Executor,
  Dext.Http.Parser,
  Dext.Http.Request,
  Dext.Logging,
  Dext.Hosting.ApplicationLifetime,
  Dext.Web.Hubs,
  Dext.Web.Hubs.Extensions,
  Dext.Web.Interfaces,
  Dext.Web.Results,
  Dext.Web.Routing,
  Dext.Web.StaticFiles,
  Dext.WebHost;

type
  TSidecarServer = class
  private
    FHost: IWebHost;
    FPort: Integer;
    FRunning: Boolean;
  public
    constructor Create(APort: Integer = 3030);
    destructor Destroy; override;
    
    procedure Start;
    procedure Stop;
    
    function GetUrl: string;
    property Port: Integer read FPort;
    property Running: Boolean read FRunning;
  end;

implementation

{$R '..\..\Sources\Dashboard\Dext.Dashboard.res'}

{ TSidecarServer }

constructor TSidecarServer.Create(APort: Integer);
begin
  inherited Create;
  FPort := APort;
  FRunning := False;
end;

destructor TSidecarServer.Destroy;
begin
  Stop;
  inherited;
end;

function TSidecarServer.GetUrl: string;
begin
  Result := Format('http://localhost:%d', [FPort]);
end;

procedure TSidecarServer.Start;
begin
  if FRunning then Exit;
  FRunning := True;

  // Build the host in the main thread
  // Using Start (non-blocking) allows us to run on Main Thread without freezing UI
  FHost := TWebHostBuilder.CreateDefault(nil)
    .UseUrls(Format('http://localhost:%d', [FPort]))
    .ConfigureServices(procedure(Services: IServiceCollection)
      var
        RegistryType: TServiceType;
        LoggerType: TServiceType;
        FactoryFunc: TFunc<IServiceProvider, TObject>;
      begin
        RegistryType := TServiceType.FromClass(TProjectRegistry);
        Services.AddSingleton(RegistryType, TProjectRegistry, nil);
        
        LoggerType := TServiceType.FromInterface(TypeInfo(ILoggerFactory));
        FactoryFunc := function(Provider: IServiceProvider): TObject
           begin
              Result := TLoggerFactory.Create; // Silent logger for Sidecar
           end;
        Services.AddSingleton(LoggerType, TClass(nil), FactoryFunc);
      end)
    .Configure(procedure(App: IApplicationBuilder)
      begin
        THubExtensions.MapHub(App, '/hubs/dashboard', TDashboardHub);

        // --- Static Files / Dashboard ---
        App.Use(procedure(Ctx: IHttpContext; Next: TRequestDelegate)
          var Path, ResName, CT: string; RS: TResourceStream;
          begin
            Path := Ctx.Request.Path;
            ResName := ''; CT := '';

            if (Path = '/') or (Path = '/index.html') then begin ResName := 'MAIN_HTML'; CT := 'text/html; charset=utf-8'; end
            else if Path = '/main.css' then begin ResName := 'MAIN_CSS'; CT := 'text/css'; end
            else if Path = '/main.js' then begin ResName := 'MAIN_JS'; CT := 'text/javascript'; end;

            if ResName <> '' then begin
              Ctx.Response.SetContentType(CT);
              RS := TResourceStream.Create(HInstance, ResName, RT_RCDATA);
              try Ctx.Response.SetContentLength(RS.Size); Ctx.Response.Write(RS); finally RS.Free; end;
              Exit;
            end;
            Next(Ctx);
          end);

        // --- Test Reports ---
        App.Use(procedure(Ctx: IHttpContext; Next: TRequestDelegate)
          var Path, ReportPath, FilePath, CT: string; CP: TContentTypeProvider; FS: TFileStream;
          begin
            Path := Ctx.Request.Path;
            if Path.StartsWith('/reports/', True) or (Path = '/reports') then begin
               if Path = '/reports' then begin
                 Ctx.Response.StatusCode := 302;
                 Ctx.Response.AddHeader('Location', '/reports/CodeCoverage_Summary.html');
                 Exit;
               end;
               ReportPath := TPath.GetFullPath('TestOutput\report');
               if TDirectory.Exists(ReportPath) then begin
                   FilePath := TPath.Combine(ReportPath, Path.Substring(9)); 
                   if FileExists(FilePath) then begin
                       CP := TContentTypeProvider.Create;
                       try if not CP.TryGetContentType(FilePath, CT) then CT := 'application/octet-stream'; finally CP.Free; end;
                       Ctx.Response.SetContentType(CT);
                       FS := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
                       try Ctx.Response.SetContentLength(FS.Size); Ctx.Response.Write(FS); finally FS.Free; end;
                       Exit;
                   end;
               end;
            end;
            Next(Ctx);
          end);

        // --- API: Test Summary ---
        App.MapGet('/api/test/summary', procedure(Ctx: IHttpContext)
          var ReportDir, SummaryFile, Content: string; Res: IResult; P1, P2: Integer; Coverage: string;
          begin
             ReportDir := TPath.GetFullPath('TestOutput\report');
             SummaryFile := TPath.Combine(ReportDir, 'CodeCoverage_Summary.xml');
             if FileExists(SummaryFile) then begin
                Content := TFile.ReadAllText(SummaryFile);
                P1 := Content.IndexOf('percent="');
                if P1 > 0 then begin
                   Inc(P1, 9); P2 := Content.IndexOf('"', P1);
                   if P2 > P1 then begin
                      Coverage := Content.Substring(P1, P2 - P1);
                      Res := Results.Ok('{"available": true, "coverage": ' + Coverage.Replace(',', '.') + '}');
                      Res.Execute(Ctx); Exit;
                   end;
                end;
             end;
             Res := Results.Ok('{"available": false}'); Res.Execute(Ctx);
          end);
          
        // --- API: Projects ---
        App.MapGet('/api/projects', procedure(Ctx: IHttpContext)
          var Registry: TProjectRegistry; Projects: TArray<TProjectInfo>; SB: TStringBuilder; I: Integer; Res: IResult;
          begin
             Registry := Ctx.Services.GetRequiredService(TProjectRegistry) as TProjectRegistry;
             Projects := Registry.GetAllProjects;
             SB := TStringBuilder.Create;
             try
               SB.Append('[');
               for I := 0 to High(Projects) do begin
                 if I > 0 then SB.Append(',');
                 SB.Append('{"path":"').Append(Projects[I].Path.Replace('\', '\\')).Append('",');
                 SB.Append('"name":"').Append(Projects[I].Name.Replace('\', '\\')).Append('",');
                 SB.Append('"lastAccess":"').Append(DateToISO8601(Projects[I].LastAccess)).Append('"}');
               end;
               SB.Append(']'); Res := Results.Text(SB.ToString, 200); Res.Execute(Ctx);
             finally SB.Free; end;
          end);

        // --- API: Config ---
        App.MapGet('/api/config', procedure(Ctx: IHttpContext)
          var Config: TDextGlobalConfig; Json, EnvObj: TJSONObject; Arr, PlatArr: TJSONArray; Res: IResult; Env: TDextEnvironment; P, CovPath: string;
          begin
            Config := TDextGlobalConfig.Create; Json := TJSONObject.Create;
            try
              Config.Load;
              Json.AddPair('dextPath', Config.DextPath);
              if Config.DextPath.IsEmpty then Json.AddPair('dextPath', ParamStr(0));
              CovPath := Config.CoveragePath;
              if (CovPath = '') then CovPath := TCodeCoverageTool.FindPath(Config, 'Win32');
              Json.AddPair('coveragePath', CovPath);
              Json.AddPair('configPath', TPath.Combine(TPath.Combine(TPath.GetHomePath, '.dext'), 'config.yaml'));
              Arr := TJSONArray.Create;
              for Env in Config.Environments do begin
                EnvObj := TJSONObject.Create; EnvObj.AddPair('version', Env.Version); EnvObj.AddPair('name', Env.Name); EnvObj.AddPair('path', Env.Path); EnvObj.AddPair('isDefault', TJSONBool.Create(Env.IsDefault));
                PlatArr := TJSONArray.Create; for P in Env.Platforms do PlatArr.Add(P); EnvObj.AddPair('platforms', PlatArr);
                Arr.Add(EnvObj);
              end;
              Json.AddPair('environments', Arr);
              Res := Results.Text(Json.ToString, 200); Res.Execute(Ctx);
            finally Json.Free; Config.Free; end;
          end);

        App.MapPost('/api/config', procedure(Ctx: IHttpContext)
          var Body: string; Res: IResult; SR: TStreamReader; Json: TJSONObject; Config: TDextGlobalConfig;
          begin
            SR := TStreamReader.Create(Ctx.Request.Body);
            try
               Body := SR.ReadToEnd; Json := TJSONObject.ParseJSONValue(Body) as TJSONObject;
               if Json <> nil then try
                  Config := TDextGlobalConfig.Create;
                  try
                    Config.Load;
                    if Json.TryGetValue('dextPath', Body) then Config.DextPath := Body;
                    if Json.TryGetValue('coveragePath', Body) then Config.CoveragePath := Body;
                    Config.Save; Res := Results.Ok('{"status":"saved"}');
                  finally Config.Free; end;
               finally Json.Free; end
               else Res := Results.BadRequest('Invalid JSON');
               Res.Execute(Ctx);
            finally SR.Free; end;
          end);
          
        App.MapPost('/api/env/scan', procedure(Ctx: IHttpContext)
          var Scanner: TDextGlobalConfig; Res: IResult;
          begin
             Scanner := TDextGlobalConfig.Create;
             try Scanner.ScanEnvironments; Res := Results.Ok('{"status":"ok"}'); Res.Execute(Ctx); finally Scanner.Free; end;
          end);

        App.MapPost('/api/tools/codecoverage/install', procedure(Ctx: IHttpContext)
          var Path: string; Res: IResult;
          begin
             try TCodeCoverageTool.InstallLatest(Path); Res := Results.Ok('{"status":"ok", "path": "' + Path.Replace('\', '\\') + '"}'); Res.Execute(Ctx);
             except on E: Exception do begin
                 Res := Results.StatusCode(500, Format('{"error": "%s"}', [E.Message.Replace('"', '\"')])); Res.Execute(Ctx);
             end; end;
          end);

        // --- API: HTTP Client ---
        App.MapPost('/api/http/parse', procedure(Ctx: IHttpContext)
          var Body: string; Res: IResult; SR: TStreamReader; Json, ResJson: TJSONObject; Collection: THttpRequestCollection; ReqArr, VarArr: TJSONArray; ReqObj, VarObj: TJSONObject; I: Integer;
          begin
            SR := TStreamReader.Create(Ctx.Request.Body);
            try
              Body := SR.ReadToEnd; Json := TJSONObject.ParseJSONValue(Body) as TJSONObject;
              if (Json <> nil) then try
                if Json.TryGetValue('content', Body) then begin
                  Collection := THttpRequestParser.Parse(Body);
                  try
                    ResJson := TJSONObject.Create; try
                      ReqArr := TJSONArray.Create;
                      for I := 0 to Collection.Requests.Count - 1 do begin
                        ReqObj := TJSONObject.Create; ReqObj.AddPair('name', Collection.Requests[I].Name); ReqObj.AddPair('method', Collection.Requests[I].Method); ReqObj.AddPair('url', Collection.Requests[I].Url); ReqArr.Add(ReqObj);
                      end;
                      ResJson.AddPair('requests', ReqArr);
                      VarArr := TJSONArray.Create;
                      for I := 0 to Collection.Variables.Count - 1 do begin
                        VarObj := TJSONObject.Create; VarObj.AddPair('name', Collection.Variables[I].Name); VarObj.AddPair('value', Collection.Variables[I].Value); VarArr.Add(VarObj);
                      end;
                      ResJson.AddPair('variables', VarArr); Res := Results.Text(ResJson.ToString, 200);
                    finally ResJson.Free; end;
                  finally Collection.Free; end;
                end else Res := Results.BadRequest('Missing content field');
              finally Json.Free; end else Res := Results.BadRequest('Invalid JSON');
              Res.Execute(Ctx);
            finally SR.Free; end;
          end);

        App.MapPost('/api/http/execute', procedure(Ctx: IHttpContext)
          var Body: string; Res: IResult; SR: TStreamReader; Json, ResJson, HeadersObj: TJSONObject; Collection: THttpRequestCollection; RequestIndex: Integer; ExResult: THttpExecutionResult; Pair: TPair<string, string>;
          begin
            SR := TStreamReader.Create(Ctx.Request.Body);
            try
              Body := SR.ReadToEnd; Json := TJSONObject.ParseJSONValue(Body) as TJSONObject;
              if (Json <> nil) then try
                RequestIndex := 0; Json.TryGetValue('requestIndex', RequestIndex);
                if Json.TryGetValue('content', Body) then begin
                  Collection := THttpRequestParser.Parse(Body);
                  try
                    if (RequestIndex >= 0) and (RequestIndex < Collection.Requests.Count) then begin
                      ExResult := THttpExecutor.ExecuteSync(Collection.Requests[RequestIndex], Collection.Variables);
                      ResJson := TJSONObject.Create;
                      try
                        ResJson.AddPair('statusCode', TJSONNumber.Create(ExResult.StatusCode)); ResJson.AddPair('responseBody', ExResult.ResponseBody);
                        HeadersObj := TJSONObject.Create;
                        if ExResult.ResponseHeaders <> nil then for Pair in ExResult.ResponseHeaders do HeadersObj.AddPair(Pair.Key, Pair.Value);
                        ResJson.AddPair('responseHeaders', HeadersObj); Res := Results.Text(ResJson.ToString, 200);
                      finally ResJson.Free; end;
                    end else Res := Results.BadRequest('Invalid Request');
                  finally Collection.Free; end;
                end else Res := Results.BadRequest('Missing content field');
              finally Json.Free; end else Res := Results.BadRequest('Invalid JSON');
              Res.Execute(Ctx);
            finally SR.Free; end;
          end);
      end)
    .Build;

  try
    FHost.Start;
  except
    on E: Exception do
      OutputDebugString(PChar('DextSidecar: Run Exception: ' + E.Message));
  end;
end;

procedure TSidecarServer.Stop;
var
  WebHost: IWebHost;
begin
  if not FRunning then Exit;
  
  // 1. Capture local reference and clear field
  WebHost := FHost;
  FHost := nil;
  
  // 2. First, shutdown Hub connections (SSE loops need to exit before Indy stops)
  THubExtensions.ShutdownHubs;
  
  // 3. Signal stop on the local reference
  // This now triggers StopApplication inside TDextApplication
  if WebHost <> nil then
  begin
    try
      WebHost.Stop;
    except
      on E: Exception do
        OutputDebugString(PChar('DextSidecar: Stop Signal Error: ' + E.Message));
    end;
  end;
  
  // 4. Finally release the interface
  WebHost := nil;
  FRunning := False;
end;

end.
