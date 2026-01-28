unit Dext.Dashboard.Routes;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.DateUtils,
  System.JSON,
  System.Types,
  Dext.DI.Interfaces,
  Dext.Dashboard.TestScanner,
  Dext.Dashboard.TestRunner,
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  Dext.DI.Core,
  Dext.Web.Interfaces,
  Dext.Web.Routing,
  Dext.Web.Results,
  Dext.Web.StaticFiles,
  Dext.Web.Hubs.Interfaces,
  Dext.Web.Hubs.Extensions,
  // Note: These dependencies suggest the dashboard logic is tightly coupled with CLI infrastructure for now.
  // Ideally these should be moved to a shared Dext.Dashboard namespace in the future.
  Dext.Hosting.CLI.Hubs.Dashboard,
  Dext.Hosting.CLI.Registry,
  Dext.Hosting.CLI.Config,
  Dext.Hosting.CLI.Tools.CodeCoverage,
  Dext.Http.Parser,
  Dext.Http.Executor,
  Dext.Http.Request;

type
  TDashboardRoutes = class
  public
    class procedure Configure(App: IApplicationBuilder);
  end;

  THttpHistoryItem = class
  public
    Id: string;
    Method: string;
    Url: string;
    StatusCode: Integer;
    DurationMs: Int64;
    Timestamp: TDateTime;
    Content: string; // The .http content used
  end;

var
  FHttpHistory: TObjectList<THttpHistoryItem>;

implementation

uses
  Dext.Web.Indy, // Access to TIndyHttpContext
  IdContext,     // Access to TIdContext
  IdGlobal;      // Access to ToBytes/IndyTextEncoding_UTF8

var
  FSSEClients: TList<IHttpContext>;
  FLock: TObject;

procedure BroadcastSSE(const EventName, Data: string); forward;

{$R 'Dext.Dashboard.res'}

{ TDashboardRoutes }

class procedure TDashboardRoutes.Configure(App: IApplicationBuilder);
begin
  // Map SignalR/WebSocket Hub
  THubExtensions.MapHub(App, '/hubs/dashboard', TDashboardHub);

  // Serve embedded dashboard HTML/CSS/JS
  App.Use(procedure(Ctx: IHttpContext; Next: TRequestDelegate)
    var
      Path, ResName, CT: string;
      RS: TResourceStream;
    begin
      Path := Ctx.Request.Path;
      ResName := '';
      CT := '';

      if (Path = '/') or (Path = '/index.html') then
      begin
        ResName := 'MAIN_HTML';
        CT := 'text/html; charset=utf-8';
      end
      else if Path = '/main.css' then
      begin
        ResName := 'MAIN_CSS';
        CT := 'text/css; charset=utf-8';
      end
      else if Path = '/main.js' then
      begin
        ResName := 'MAIN_JS';
        CT := 'text/javascript; charset=utf-8';
      end
      else if Path = '/i18n.js' then
      begin
        ResName := 'I18N_JS';
        CT := 'text/javascript; charset=utf-8';
      end;

      if ResName <> '' then
      begin
        Ctx.Response.SetContentType(CT);
        RS := TResourceStream.Create(HInstance, ResName, RT_RCDATA);
        try
          Ctx.Response.SetContentLength(RS.Size);
          Ctx.Response.Write(RS);
        finally
          RS.Free;
        end;
        Exit;
      end;
      
      Next(Ctx);
    end);

  // Serve Test Reports
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
             FilePath := TPath.Combine(ReportPath, Path.Substring(9)); 
             
             if FileExists(FilePath) then
             begin
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
             end;
         end;
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
          P1 := Content.IndexOf('percent="');
          if P1 > 0 then
          begin
             Inc(P1, 9);
             P2 := Content.IndexOf('"', P1);
             if P2 > P1 then
             begin
                Coverage := Content.Substring(P1, P2 - P1);
                Res := Results.Ok('{"available": true, "coverage": ' + Coverage.Replace(',', '.') + '}');
                Res.Execute(Ctx);
                Exit;
             end;
          end;
       end;
       
       Res := Results.Ok('{"available": false}');
       Res.Execute(Ctx);
    end);
    
  // API: HTTP Client - History
  App.MapGet('/api/http/history',
    procedure(Ctx: IHttpContext)
    var
      Res: IResult;
      Arr: TJSONArray;
      Item: THttpHistoryItem;
      Obj: TJSONObject;
    begin
      if FHttpHistory = nil then
      begin
        Res := Results.Ok('[]');
        Res.Execute(Ctx);
        Exit;
      end;

      Arr := TJSONArray.Create;
      TMonitor.Enter(FHttpHistory);
      try
        for Item in FHttpHistory do
        begin
          Obj := TJSONObject.Create;
          Obj.AddPair('id', Item.Id);
          Obj.AddPair('method', Item.Method);
          Obj.AddPair('url', Item.Url);
          Obj.AddPair('statusCode', Item.StatusCode);
          Obj.AddPair('durationMs', Item.DurationMs);
          Obj.AddPair('timestamp', DateToISO8601(Item.Timestamp));
          // Don't send full content to list to save bandwidth, maybe logic to fetch detail later?
          // For now send it, text is small.
          Obj.AddPair('content', Item.Content);
          Arr.Add(Obj);
        end;
      finally
        TMonitor.Exit(FHttpHistory);
      end;
      
      Res := Results.Json(Arr.ToString);
      Arr.Free;
      Res.Execute(Ctx);
    end);
    
  // API: Projects
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

  // API: File System List
  App.MapGet('/api/fs/list',
    procedure(Ctx: IHttpContext)
    var
      ScanPath: string;
      Entries: TArray<string>;
      Entry: string;
      Arr: TJSONArray;
      Obj: TJSONObject;
      Res: IResult;
    begin
      ScanPath := Ctx.Request.Query.Values['path'];
      if ScanPath = '' then ScanPath := 'C:\'; 
      
      if (Length(ScanPath) > 3) and ScanPath.EndsWith('\') then 
        ScanPath := ScanPath.Substring(0, Length(ScanPath)-1);
      
      try
        Arr := TJSONArray.Create;
        try
           if TDirectory.Exists(ScanPath) then
           begin
               Entries := TDirectory.GetDirectories(ScanPath);
               for Entry in Entries do
               begin
                  Obj := TJSONObject.Create;
                  Obj.AddPair('name', TPath.GetFileName(Entry));
                  Obj.AddPair('type', 'dir');
                  Arr.Add(Obj);
               end;
               
               Entries := TDirectory.GetFiles(ScanPath);
               for Entry in Entries do
               begin
                  Obj := TJSONObject.Create;
                  Obj.AddPair('name', TPath.GetFileName(Entry));
                  Obj.AddPair('type', 'file');
                  Arr.Add(Obj);
               end;
           end;
           
           Res := Results.Json(Arr.ToString);
           Res.Execute(Ctx);
        finally
           Arr.Free;
        end;
      except
         on E: Exception do Results.BadRequest(E.Message).Execute(Ctx);
      end;
    end);

  // API: File System Read
  App.MapGet('/api/fs/read',
    procedure(Ctx: IHttpContext)
    var
      FilePath: string;
      Content: string;
    begin
      FilePath := Ctx.Request.Query.Values['path'];
      if (FilePath = '') or not FileExists(FilePath) then
      begin
        Results.BadRequest('Valid file path required').Execute(Ctx);
        Exit;
      end;

      try
        Content := TFile.ReadAllText(FilePath, TEncoding.UTF8);
        Results.Text(Content, 200).Execute(Ctx);
      except
        on E: Exception do Results.InternalServerError(E.Message).Execute(Ctx);
      end;
    end);

  // API: Workspace Scan
  App.MapGet('/api/workspace/scan',
    procedure(Ctx: IHttpContext)
    var
       ScanPath: string;
       Json: TJSONObject;
       Projects, Tests, HttpFiles, Docs: TJSONArray;
       Files: TArray<string>;
       F: string;
       Res: IResult;
    begin
       ScanPath := Ctx.Request.Query.Values['path'];
       if ScanPath = '' then 
       begin
          Results.BadRequest('Path required').Execute(Ctx);
          Exit;
       end;
       
       Json := TJSONObject.Create;
       Projects := TJSONArray.Create;
       Tests := TJSONArray.Create;
       HttpFiles := TJSONArray.Create;
       Docs := TJSONArray.Create;
       
       try
          if TDirectory.Exists(ScanPath) then
          begin
              Files := TDirectory.GetFiles(ScanPath, '*.dproj', TSearchOption.soAllDirectories);
              for F in Files do Projects.Add(TPath.GetFileNameWithoutExtension(F));

              // Also scan for .dpr (console apps/tests that are not in dproj)
              Files := TDirectory.GetFiles(ScanPath, '*.dpr', TSearchOption.soAllDirectories);
              for F in Files do 
              begin
                 var Name := TPath.GetFileNameWithoutExtension(F);
                 // Avoid duplicates if dproj already found
                 var Found := False;
                 for var I := 0 to Projects.Count - 1 do
                   if Projects.Items[I].Value.Equals(Name) then
                   begin
                     Found := True;
                     Break;
                   end;
                 
                 if not Found then Projects.Add(Name);
              end;
              
              Files := TDirectory.GetFiles(ScanPath, '*.http', TSearchOption.soAllDirectories);
              for F in Files do HttpFiles.Add(TPath.GetFileName(F));
              
              // Scan for Test Projects (.dpr) instead of units (.pas)
              // Convention: Starts with "Test" or ends with "Tests"
              var TestFiles := TDirectory.GetFiles(ScanPath, 'Test*.dpr', TSearchOption.soAllDirectories);
              TestFiles := TestFiles + TDirectory.GetFiles(ScanPath, '*Tests.dpr', TSearchOption.soAllDirectories);
              
              for F in TestFiles do 
              begin
                 var Name := TPath.GetFileNameWithoutExtension(F);
                 // Avoid duplicates
                 var Found := False;
                 for var I := 0 to Tests.Count - 1 do
                   if (Tests.Items[I] is TJSONObject) and (Tests.Items[I] as TJSONObject).GetValue('name').Value.Equals(Name) then
                   begin
                     Found := True;
                     Break;
                   end;
                 
                 if not Found then 
                 begin
                   var TestObj := TJSONObject.Create;
                   TestObj.AddPair('name', Name);
                   TestObj.AddPair('path', F);
                   Tests.Add(TestObj);
                 end;
              end;
          end;
          
          Json.AddPair('projects', Projects);
          Json.AddPair('tests', Tests);
          Json.AddPair('httpFiles', HttpFiles);
          Json.AddPair('docs', Docs);
          
          Res := Results.Json(Json.ToString);
          Res.Execute(Ctx);
       finally
          Json.Free;
       end;
    end);


  // API: Discover Tests in Project
  App.MapGet('/api/tests/discover',
    procedure(Ctx: IHttpContext)
    var
       ProjectPath: string;
       ProjectInfo: TTestProjectInfo;
       Fixture: TTestFixtureInfo;
       Method: TTestMethodInfo;
       
       ResJson, FixtureObj, MethodObj: TJSONObject;
       FixturesArr, MethodsArr: TJSONArray;
       Res: IResult;
    begin
       ProjectPath := Ctx.Request.Query.Values['project'];
       // If only name provided, try to find in current workspace (context needed, assuming full path for now or scan)
       // For simple validaton, assume User passes full path or relative to known root.
       // But better: Receive Full Path from the UI which already knows it from previous scan.
       
       if (ProjectPath = '') or not FileExists(ProjectPath) then
       begin
          // Fallback: try to find in last scanned folder? Too complex for now.
          Results.BadRequest('Valid project path required').Execute(Ctx);
          Exit;
       end;
       
       try
         ProjectInfo := TTestScanner.ScanProject(ProjectPath);
         ResJson := nil;
         try
            ResJson := TJSONObject.Create;
            ResJson.AddPair('project', TPath.GetFileNameWithoutExtension(ProjectPath));
            ResJson.AddPair('path', ProjectPath);
            
            FixturesArr := TJSONArray.Create;
            for Fixture in ProjectInfo.Fixtures do
            begin
                FixtureObj := TJSONObject.Create;
                FixtureObj.AddPair('name', Fixture.Name);
                FixtureObj.AddPair('unit', Fixture.UnitName);
                FixtureObj.AddPair('line', TJSONNumber.Create(Fixture.LineNumber));
                
                MethodsArr := TJSONArray.Create;
                for Method in Fixture.Methods do
                begin
                    MethodObj := TJSONObject.Create;
                    MethodObj.AddPair('name', Method.Name);
                    MethodObj.AddPair('line', TJSONNumber.Create(Method.LineNumber));
                    MethodsArr.Add(MethodObj);
                end;
                FixtureObj.AddPair('tests', MethodsArr);
                
                FixturesArr.Add(FixtureObj);
            end;
            ResJson.AddPair('fixtures', FixturesArr);
            
            Res := Results.Json(ResJson.ToString);
            Res.Execute(Ctx);
         finally
            ResJson.Free;
            ProjectInfo.Free;
         end;
       except
         on E: Exception do Results.StatusCode(500, E.Message).Execute(Ctx);
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
      CovPath: string;
    begin
      Config := TDextGlobalConfig.Create;
      Json := TJSONObject.Create;
      try
        Config.Load;
        
        Json.AddPair('dextPath', Config.DextPath);
        if Config.DextPath.IsEmpty then Json.AddPair('dextPath', ParamStr(0));
        
        CovPath := Config.CoveragePath;
        if (CovPath = '') then
           CovPath := TCodeCoverageTool.FindPath(Config, 'Win32');

        Json.AddPair('coveragePath', CovPath);
        Json.AddPair('configPath', TPath.Combine(TPath.Combine(TPath.GetHomePath, '.dext'), 'config.yaml'));
        
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
      Res: IResult;
    begin
       Scanner := TDextGlobalConfig.Create;
       try
         Scanner.ScanEnvironments;
         Res := Results.Ok('{"status":"ok"}');
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
      NewState: Boolean;
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
                 NewState := (E.Version = Ver);
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

  // API: HTTP Client - Parse
  App.MapPost('/api/http/parse',
    procedure(Ctx: IHttpContext)
    var
      Body: string;
      Res: IResult;
      SR: TStreamReader;
      Json, ResJson: TJSONObject;
      Collection: THttpRequestCollection;
      ReqArr, VarArr: TJSONArray;
      ReqObj, VarObj: TJSONObject;
      I: Integer;
    begin
      SR := TStreamReader.Create(Ctx.Request.Body);
      try
        Body := SR.ReadToEnd;
        Json := TJSONObject.ParseJSONValue(Body) as TJSONObject;
        if (Json <> nil) then
        try
          if Json.TryGetValue('content', Body) then
          begin
            Collection := THttpRequestParser.Parse(Body);
            try
              ResJson := TJSONObject.Create;
              try
                ReqArr := TJSONArray.Create;
                for I := 0 to Collection.Requests.Count - 1 do
                begin
                  ReqObj := TJSONObject.Create;
                  ReqObj.AddPair('name', Collection.Requests[I].Name);
                  ReqObj.AddPair('method', Collection.Requests[I].Method);
                  ReqObj.AddPair('url', Collection.Requests[I].Url);
                  ReqObj.AddPair('lineNumber', TJSONNumber.Create(Collection.Requests[I].LineNumber));
                  ReqObj.AddPair('body', Collection.Requests[I].Body);
                  ReqArr.Add(ReqObj);
                end;
                ResJson.AddPair('requests', ReqArr);
                
                VarArr := TJSONArray.Create;
                for I := 0 to Collection.Variables.Count - 1 do
                begin
                  VarObj := TJSONObject.Create;
                  VarObj.AddPair('name', Collection.Variables[I].Name);
                  VarObj.AddPair('value', Collection.Variables[I].Value);
                  VarObj.AddPair('isEnvVar', TJSONBool.Create(Collection.Variables[I].IsEnvVar));
                  VarObj.AddPair('envVarName', Collection.Variables[I].EnvVarName);
                  VarArr.Add(VarObj);
                end;
                ResJson.AddPair('variables', VarArr);
                
                Res := Results.Text(ResJson.ToString, 200);
              finally
                ResJson.Free;
              end;
            finally
              Collection.Free;
            end;
          end
          else
            Res := Results.BadRequest('Missing content field');
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

  // API: HTTP Client - Execute
  App.MapPost('/api/http/execute',
    procedure(Ctx: IHttpContext)
    var
      Body: string;
      Res: IResult;
      SR: TStreamReader;
      Json, ResJson, HeadersObj: TJSONObject;
      Collection: THttpRequestCollection;
      RequestIndex: Integer;
      ExResult: THttpExecutionResult;
      Pair: TPair<string, string>;
    begin
      SR := TStreamReader.Create(Ctx.Request.Body);
      try
        Body := SR.ReadToEnd;
        Json := TJSONObject.ParseJSONValue(Body) as TJSONObject;
        if (Json <> nil) then
        try
          RequestIndex := 0;
          Json.TryGetValue('requestIndex', RequestIndex);
          
          if Json.TryGetValue('content', Body) then
          begin
            Collection := THttpRequestParser.Parse(Body);
            try
              if (RequestIndex >= 0) and (RequestIndex < Collection.Requests.Count) then
              begin
                ExResult := THttpExecutor.ExecuteSync(Collection.Requests[RequestIndex], Collection.Variables);
                
                ResJson := TJSONObject.Create;
                try
                  ResJson.AddPair('requestName', ExResult.RequestName);
                  ResJson.AddPair('requestMethod', ExResult.RequestMethod);
                  ResJson.AddPair('requestUrl', ExResult.RequestUrl);
                  ResJson.AddPair('statusCode', TJSONNumber.Create(ExResult.StatusCode));
                  ResJson.AddPair('statusText', ExResult.StatusText);
                  ResJson.AddPair('responseBody', ExResult.ResponseBody);
                  ResJson.AddPair('durationMs', TJSONNumber.Create(ExResult.DurationMs));
                  ResJson.AddPair('success', TJSONBool.Create(ExResult.Success));
                  ResJson.AddPair('errorMessage', ExResult.ErrorMessage);
                  
                  HeadersObj := TJSONObject.Create;
                  if ExResult.ResponseHeaders <> nil then
                    for Pair in ExResult.ResponseHeaders do
                      HeadersObj.AddPair(Pair.Key, Pair.Value);
                  ResJson.AddPair('responseHeaders', HeadersObj);
                  
                  Res := Results.Text(ResJson.ToString, 200);
                  Res.Execute(Ctx);
                  
                  // Save to History
                  if FHttpHistory = nil then
                     FHttpHistory := TObjectList<THttpHistoryItem>.Create(True);
                  
                  TMonitor.Enter(FHttpHistory);
                  try
                    var HistoryItem := THttpHistoryItem.Create;
                    HistoryItem.Id := TGUID.NewGuid.ToString;
                    HistoryItem.Method := Collection.Requests[RequestIndex].Method;
                    HistoryItem.Url := Collection.Requests[RequestIndex].Url;
                    HistoryItem.StatusCode := ExResult.StatusCode;
                    HistoryItem.DurationMs := ExResult.DurationMs;
                    HistoryItem.Timestamp := Now;
                    HistoryItem.Content := Body; 
                    
                    FHttpHistory.Insert(0, HistoryItem);
                    
                    // Limit to 50
                    while FHttpHistory.Count > 50 do
                      FHttpHistory.Delete(FHttpHistory.Count - 1);
                  finally
                    TMonitor.Exit(FHttpHistory);
                  end;
                finally
                  ResJson.Free;
                end;
              end
              else
                Res := Results.BadRequest('Invalid request index');
            finally
              Collection.Free;
            end;
          end
          else
            Res := Results.BadRequest('Missing content field');
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

  // API: Telemetry Logs Ingestion
  App.MapPost('/api/telemetry/logs',
    procedure(Ctx: IHttpContext)
    var
      Body: string;
      SR: TStreamReader;
    begin
      // Read logs
      SR := TStreamReader.Create(Ctx.Request.Body);
      try
        Body := SR.ReadToEnd;
        


        if Body.IsEmpty then
        begin
          Ctx.Response.StatusCode := 204; // No Content
          Exit;
        end;

        // ADAPTER: Telemetry to Dashboard
        // Only SSE (Server-Sent Events) is used as IHubContext is not registered.
             
        // PROCESS LOGS ALWAYS (Parsing JSON)
        var JV: TJSONValue := TJSONObject.ParseJSONValue(Body);
        try
           if (JV <> nil) and (JV is TJSONObject) then
           begin
               var JO := JV as TJSONObject;
               var Val: string;
               var EventType := '';
               if JO.TryGetValue('event', Val) then EventType := Val;
                    
               // SSE Adapter (Primary Channel)
               var SseEvent := '';
               if EventType = 'RunStart' then SseEvent := 'run_start'
               else if EventType = 'TestStart' then SseEvent := 'test_start'
               else if EventType = 'TestComplete' then SseEvent := 'test_complete'
               else if EventType = 'RunComplete' then SseEvent := 'run_complete';
               
               if SseEvent <> '' then
               begin
                    BroadcastSSE(SseEvent, JO.ToString);
               end;
           end;
        finally
           JV.Free;
        end;
        
        Ctx.Response.StatusCode := 202; // Accepted
      finally
        SR.Free;
      end;
    end);

  // API: Run Tests
  App.MapPost('/api/tests/run',
    procedure(Ctx: IHttpContext)
    var
       Body: string;
       Json, TestRunResult: TJSONObject;
       Project: string;
       SR: TStreamReader;
    begin
       SR := TStreamReader.Create(Ctx.Request.Body);
       try
         Body := SR.ReadToEnd;
       finally
         SR.Free;
       end;

       try
         Json := TJSONObject.ParseJSONValue(Body) as TJSONObject;
         if Json = nil then
         begin
            Results.BadRequest('Invalid JSON').Execute(Ctx);
            Exit;
         end;
         
         try
           if not Json.TryGetValue<string>('project', Project) then
           begin
              Results.BadRequest('Missing "project" field').Execute(Ctx);
              Exit;
           end;
           
           TestRunResult := TTestRunner.RunProject(Project);
           if TestRunResult <> nil then
           begin
              try
                Results.Json(TestRunResult.ToString).Execute(Ctx);
              finally
                TestRunResult.Free;
              end;
           end
           else
           begin
              // If nil is returned, it means FindExecutable failed or similar handled error in RunProject
              // But currently RunProject returns a JSON with "error" field on failure.
              // So nil means unexpected.
              Results.InternalServerError('Failed to run tests (Result is nil)').Execute(Ctx);
           end;
              
         finally
           Json.Free;
         end;
       except
          on E: Exception do
            Results.InternalServerError(E.Message).Execute(Ctx);
       end;
    end);

  // API: SSE Events Endpoint (Fallback)
  App.MapGet('/events',
    procedure(Ctx: IHttpContext)
    var
      IndyCtx: TIdContext;
    begin
         IndyCtx := nil;
         if Ctx is TIndyHttpContext then
            IndyCtx := TIndyHttpContext(Ctx).Context;



         if IndyCtx <> nil then
         begin
             // Manually write headers to flush immediately
             IndyCtx.Connection.IOHandler.WriteLn('HTTP/1.1 200 OK');
             IndyCtx.Connection.IOHandler.WriteLn('Content-Type: text/event-stream; charset=utf-8');
             IndyCtx.Connection.IOHandler.WriteLn('Cache-Control: no-cache');
             IndyCtx.Connection.IOHandler.WriteLn('Connection: keep-alive');
             IndyCtx.Connection.IOHandler.WriteLn(''); // End of headers
             
             // Initial Handshake
             IndyCtx.Connection.IOHandler.Write('event: connected'#10'data: {"msg":"welcome"}'#10#10);
         end
         else
         begin
             // Fallback
             Ctx.Response.SetContentType('text/event-stream; charset=utf-8');
             Ctx.Response.AddHeader('Cache-Control', 'no-cache');
             Ctx.Response.AddHeader('Connection', 'keep-alive');
             Ctx.Response.Write('event: connected'#10'data: {"msg":"welcome"}'#10#10);
         end;
         
         // Add to active clients list
         TMonitor.Enter(FLock);
         try
            FSSEClients.Add(Ctx);

         finally
            TMonitor.Exit(FLock);
         end;
         
         // Keep connection open
         try
            while True do
            begin
               if (IndyCtx <> nil) and (not IndyCtx.Connection.Connected) then Break;
               Sleep(500); 
            end;
         finally
            TMonitor.Enter(FLock);
            try
               FSSEClients.Remove(Ctx);
            finally
               TMonitor.Exit(FLock);
            end;
         end;
    end);
    
end;

procedure BroadcastSSE(const EventName, Data: string);
var
  Ctx: IHttpContext;
  Msg: string;
  IndyCtx: TIdContext;
begin
  if (FSSEClients = nil) then Exit;

  TMonitor.Enter(FLock);
  try


    if FSSEClients.Count = 0 then Exit;

    Msg := Format('event: %s'#10'data: %s'#10#10, [EventName, Data]);
    
    // Iterate backwards so we can remove dead clients if needed (though we just ignore errors here)
    for Ctx in FSSEClients do
    begin
      try
        if Ctx is TIndyHttpContext then
        begin
           IndyCtx := TIndyHttpContext(Ctx).Context;
           if (IndyCtx <> nil) and (IndyCtx.Connection <> nil) and IndyCtx.Connection.Connected then
           begin
               IndyCtx.Connection.IOHandler.Write(ToBytes(Msg, IndyTextEncoding_UTF8));
           end;
        end;
      except
        // Handle disconnection?

      end;
    end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

initialization
  FHttpHistory := TObjectList<THttpHistoryItem>.Create(True);
  FSSEClients := TList<IHttpContext>.Create;
  FLock := TObject.Create;

finalization
  FSSEClients.Free;
  FLock.Free;
  FHttpHistory.Free;


end.
