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
  Dext.Web.Hubs,
  Dext.Hosting.CLI.Hubs.Dashboard,
  Dext.Logging,
  Dext.Hosting.CLI.Logger,
  Dext.Utils;

type
  TUICommand = class(TInterfacedObject, IConsoleCommand)
  private
    procedure OpenBrowser(const Url: string);
    function GetDashboardHTML: string;
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

function TUICommand.GetDashboardHTML: string;
begin
  Result :=
'<!DOCTYPE html><html lang="en"><head>' + #13#10 +
'<meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0">' + #13#10 +
'<title>Dext Dashboard</title>' + #13#10 +
'<link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap" rel="stylesheet">' + #13#10 +
'<link href="https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@24,400,0,0" rel="stylesheet">' + #13#10 +
'<style>' + #13#10 +
':root{--bg:#0D0B10;--surface:#171520;--surface-c:#1E1B26;--surface-h:#292634;--on-surface:#E8E3EC;--on-surface-v:#A8A3B2;--outline:#555065;' + 
'--outline-v:#3A3546;--primary:#C4A9FF;--on-primary:#2A1258;--primary-c:#3D2470;--success:#7CD992;--success-c:#1C3D28;--warn:#FFB74D;--warn-c:#3D2800}' + #13#10 +
':root.light{--bg:#F5F3F7;--surface:#FFFFFF;--surface-c:#F0EDF4;--surface-h:#E8E4EE;--on-surface:#1C1B1F;--on-surface-v:#49454F;--outline:#79747E;' + 
'--outline-v:#CAC4D0;--primary:#6750A4;--on-primary:#FFFFFF;--primary-c:#EADDFF;--success:#1B873A;--success-c:#D4EDDA;--warn:#B25E02;--warn-c:#FFF3CD}' + #13#10 +
'*{margin:0;padding:0;box-sizing:border-box}body{font-family:Inter,sans-serif;background:var(--bg);color:var(--on-surface);min-height:100vh;display:flex}' + #13#10 +
'.ms{font-variation-settings:"FILL"0,"wght"400,"GRAD"0,"opsz"24}' + #13#10 +
'.nav{width:90px;background:var(--surface);display:flex;flex-direction:column;align-items:center;padding:16px 0;gap:8px;border-right:1px solid var(--outline-v)}' + #13#10 +
'.logo{width:52px;height:52px;background:linear-gradient(135deg,#9D7BF7,#6C4AB6);border-radius:14px;display:flex;align-items:center;justify-content:center;font-weight:700;' + 
'font-size:24px;color:#fff;margin-bottom:24px;box-shadow:0 4px 20px rgba(157,123,247,0.4)}' + #13#10 +
'.logo span{font-size:14px;margin-left:2px}' + #13#10 +
'.ni{width:64px;height:64px;display:flex;flex-direction:column;align-items:center;justify-content:center;gap:4px;border-radius:18px;cursor:pointer;transition:all .2s;' + 
'text-decoration:none;color:var(--on-surface-v)}' + #13#10 +
'.ni:hover{background:var(--surface-h)}.ni.on{background:linear-gradient(135deg,rgba(157,123,247,0.3),rgba(157,123,247,0.1));color:var(--primary)}' + #13#10 +
'.ni .ms{font-size:26px}.ni .lb{font-size:10px;font-weight:500}' + #13#10 +
'.main{flex:1;padding:24px 32px;overflow-y:auto}' + #13#10 +
'.hd{display:flex;justify-content:space-between;align-items:center;margin-bottom:28px}' + #13#10 +
'.hd h1{font-size:26px;font-weight:600;color:#fff}' + #13#10 +
'.hd-act{display:flex;gap:10px}' + #13#10 +
'.hd-ic{width:38px;height:38px;border-radius:10px;display:flex;align-items:center;justify-content:center;background:var(--surface-c);color:var(--on-surface-v);cursor:pointer;border:1px solid var(--outline-v);transition:all .15s}' + #13#10 +
'.hd-ic:hover{border-color:var(--primary);color:var(--primary)}' + #13#10 +
'.av{width:38px;height:38px;border-radius:50%;background:linear-gradient(135deg,#9D7BF7,#E040FB);display:flex;align-items:center;justify-content:center;font-weight:600;font-size:13px;color:#fff}' + #13#10 +
'.cg{display:grid;grid-template-columns:repeat(3,1fr);gap:20px;margin-bottom:28px}' + #13#10 +
'.cd{background:var(--surface-c);border-radius:20px;padding:20px;border:1px solid var(--outline-v);min-height:180px;display:flex;flex-direction:column;transition:all .25s ease;cursor:pointer}' + #13#10 +
'.cd:hover{border-color:var(--primary);box-shadow:0 0 30px rgba(157,123,247,0.25),0 0 60px rgba(157,123,247,0.1);transform:translateY(-2px)}' + #13#10 +
'.cd-hd{display:flex;justify-content:space-between;align-items:flex-start;margin-bottom:12px}' + #13#10 +
'.cd-tt{font-size:16px;font-weight:600;color:var(--on-surface)}' + #13#10 +
'.cd-ic{width:42px;height:42px;border-radius:12px;display:flex;align-items:center;justify-content:center;background:rgba(157,123,247,0.15);color:var(--primary)}' + #13#10 +
'.cd-val{font-size:48px;font-weight:700;color:var(--primary);display:flex;align-items:baseline;gap:6px}' + #13#10 +
'.cd-val .arr{font-size:24px;color:var(--success)}' + #13#10 +
'.cd-pct{font-size:13px;color:var(--success);margin-top:4px}' + #13#10 +
'.cd-sub{font-size:12px;color:var(--on-surface-v);margin-top:auto}' + #13#10 +
'.cd-grph{flex:1;display:flex;align-items:flex-end;margin-top:12px}' + #13#10 +
'.cd-circ{flex:1;display:flex;flex-direction:column;align-items:center;justify-content:center;position:relative}' + #13#10 +
'.cd-circ svg{width:100px;height:100px}' + #13#10 +
'.cd-circ .cv{position:absolute;font-size:28px;font-weight:700;color:var(--primary)}' + #13#10 +
'.cd-circ .cs{font-size:11px;color:var(--on-surface-v);text-align:center;margin-top:8px}' + #13#10 +
'.pj-list{margin-top:8px;font-size:12px}' + #13#10 +
'.pj-row{display:flex;align-items:center;justify-content:space-between;padding:6px 0;border-bottom:1px solid var(--outline-v)}' + #13#10 +
'.pj-row:last-child{border:none}.pj-nm{color:var(--on-surface)}' + #13#10 +
'.pj-st{font-size:11px;display:flex;align-items:center;gap:6px}' + #13#10 +
'.pj-bar{width:30px;height:4px;border-radius:2px;background:var(--outline-v)}.pj-bar.g{background:var(--success)}.pj-bar.y{background:var(--warn)}.pj-bar.p{background:var(--primary)}' + #13#10 +
'.sec{background:var(--surface-c);border-radius:18px;padding:20px;margin-bottom:20px;border:1px solid var(--outline-v)}' + #13#10 +
'.sec-tt{font-size:16px;font-weight:600;margin-bottom:16px;display:flex;align-items:center;gap:8px}' + #13#10 +
'.env{list-style:none}' + #13#10 +
'.env-i{display:flex;align-items:center;justify-content:space-between;padding:12px 14px;background:var(--surface);border-radius:12px;margin-bottom:8px;transition:all .15s}' + #13#10 +
'.env-i:hover{transform:translateX(3px);border-left:2px solid var(--primary)}.env-i:last-child{margin:0}' + #13#10 +
'.env-nm{font-weight:500;display:flex;align-items:center;gap:8px;font-size:14px}' + #13#10 +
'.env-bg{font-size:10px;padding:3px 8px;border-radius:50px;background:var(--primary-c);color:var(--primary);font-weight:500}' + #13#10 +
'.env-pt{font-size:11px;color:var(--on-surface-v);font-family:Consolas,monospace}' + #13#10 +
'.at{width:100%;border-collapse:collapse}' + #13#10 +
'.at th{text-align:left;font-size:10px;font-weight:600;color:var(--on-surface-v);text-transform:uppercase;letter-spacing:.5px;padding:10px 12px;border-bottom:1px solid var(--outline-v)}' + #13#10 +
'.at td{padding:10px 12px;font-size:13px;border-bottom:1px solid var(--outline-v)}' + #13#10 +
'.at tr:last-child td{border:none}.at tr:hover td{background:var(--surface)}' + #13#10 +
'.st{font-size:10px;padding:3px 8px;border-radius:50px;font-weight:500}' + #13#10 +
'.st-s{background:var(--success-c);color:var(--success)}.st-w{background:var(--warn-c);color:var(--warn)}.st-p{background:var(--primary-c);color:var(--primary)}' + #13#10 +
'.logs{background:var(--surface);border-radius:12px;padding:14px;font-family:Consolas,monospace;font-size:12px;max-height:200px;overflow-y:auto}' + #13#10 +
'.log{padding:5px 0;border-bottom:1px solid var(--outline-v)}.log:last-child{border:none}' + #13#10 +
'.log-t{color:var(--outline);margin-right:8px}.log-i{color:var(--primary)}.log-w{color:var(--warn)}.log-e{color:#F2B8B5}' + #13#10 +
'.btn{display:inline-flex;align-items:center;gap:6px;padding:10px 20px;border-radius:50px;font-size:13px;font-weight:500;cursor:pointer;border:none;transition:all .15s}' + #13#10 +
'.btn-t{background:var(--primary-c);color:var(--primary)}.btn-t:hover{filter:brightness(1.2)}' + #13#10 +
'.grid2{display:grid;grid-template-columns:1fr 1fr;gap:20px}@media(max-width:1000px){.grid2,.cg{grid-template-columns:1fr}}' + #13#10 +
'</style></head><body>' + #13#10 +
'<nav class="nav"><div class="logo">D<span>ext</span></div>' + #13#10 +
'<a href="#" class="ni on"><span class="ms material-symbols-outlined">home</span><span class="lb">Dashboard</span></a>' + #13#10 +
'<a href="#" class="ni"><span class="ms material-symbols-outlined">folder</span><span class="lb">Projects</span></a>' + #13#10 +
'<a href="#" class="ni"><span class="ms material-symbols-outlined">settings</span><span class="lb">Settings</span></a>' + #13#10 +
'<a href="#" class="ni"><span class="ms material-symbols-outlined">terminal</span><span class="lb">Logs</span></a></nav>' + #13#10 +
'<main class="main"><div class="hd"><h1>Dext Dashboard</h1>' + #13#10 +
'<div class="hd-act"><div class="hd-ic" id="theme-toggle" onclick="toggleTheme()"><span class="ms material-symbols-outlined" id="theme-icon">dark_mode</span></div>' + #13#10 +
'<div class="hd-ic"><span class="ms material-symbols-outlined">notifications</span></div>' + #13#10 +
'<div class="hd-ic"><span class="ms material-symbols-outlined">help</span></div><div class="av">CR</div></div></div>' + #13#10 +
'<div class="cg">' + #13#10 +
'<div class="cd"><div class="cd-hd"><span class="cd-tt">Environments</span><div class="cd-ic"><span class="ms material-symbols-outlined">cloud</span></div></div>' + #13#10 +
'<div class="cd-val"><span id="env-count">3</span><span class="arr ms material-symbols-outlined">north_east</span></div>' + #13#10 +
'<div class="cd-pct">+10% from last week</div>' + #13#10 +
'<div class="cd-sub">Active development & staging environments</div>' + #13#10 +
'<div class="cd-grph"><svg viewBox="0 0 200 50" preserveAspectRatio="none" style="width:100%;height:40px"><path d="M0,45 C20,42 40,35 60,30 S100,20 140,18 ' + 
'S180,15 200,10" fill="none" stroke="var(--primary)" stroke-width="2"/></svg></div></div>' + #13#10 +
'<div class="cd"><div class="cd-hd"><span class="cd-tt">Coverage</span><div class="cd-ic"><span class="ms material-symbols-outlined">verified</span></div></div>' + #13#10 +
'<div class="cd-circ"><svg viewBox="0 0 100 100"><circle cx="50" cy="50" r="42" fill="none" stroke="var(--outline-v)" stroke-width="8"/><circle id="cov-ring" ' + 
'cx="50" cy="50" r="42" fill="none" stroke="var(--primary)" stroke-width="8" stroke-linecap="round" stroke-dasharray="0 264" transform="rotate(-90 50 50)"/></svg><span class="cv" id="coverage-value">-</span></div>' + #13#10 +
'<div class="cd-circ cs">Code test coverage across all projects</div></div>' + #13#10 +
'<div class="cd"><div class="cd-hd"><span class="cd-tt">Projects</span><div class="cd-ic"><span class="ms material-symbols-outlined">folder_copy</span></div></div>' + #13#10 +
'<div class="cd-val" id="project-count">0</div>' + #13#10 +
'<div class="cd-sub">Active Delphi projects currently managed</div>' + #13#10 +
'<div class="pj-list" id="pj-list"></div></div>' + #13#10 +
'</div>' + #13#10 +
'<div class="grid2"><section class="sec"><h2 class="sec-tt"><span class="ms material-symbols-outlined">dns</span> Environments</h2>' + #13#10 +
'<ul class="env" id="env-list"><li class="env-i">Loading...</li></ul>' + #13#10 +
'<button class="btn btn-t" style="margin-top:14px" onclick="scan()"><span class="ms material-symbols-outlined">search</span>Scan Environments</button></section>' + #13#10 +
'<section class="sec"><h2 class="sec-tt"><span class="ms material-symbols-outlined">history</span> Recent Activity</h2>' + #13#10 +
'<table class="at"><thead><tr><th>Action</th><th>Time</th><th>Status</th></tr></thead>' + #13#10 +
'<tbody id="at-body"><tr><td>Dashboard started</td><td>Just now</td><td><span class="st st-p">Active</span></td></tr></tbody></table></section></div>' + #13#10 +
'<section class="sec"><h2 class="sec-tt"><span class="ms material-symbols-outlined">terminal</span> Live Logs</h2>' + #13#10 +
'<div class="logs" id="logs"><div class="log"><span class="log-t">[--:--:--]</span> Waiting for logs...</div></div></section>' + #13#10 +
'</main>' + #13#10 +
'<script src="https://cdn.jsdelivr.net/npm/@microsoft/signalr@8.0.0/dist/browser/signalr.min.js"></script>' + #13#10 +
'<script>' + #13#10 +
'var cfg=null;' + #13#10 +
'async function load(){try{var r=await Promise.all([fetch("/api/config").then(function(x){return x.json()}),fetch("/api/projects").then(function(x){return x.json()}),' + 
'fetch("/api/test/summary").then(function(x){return x.json()})]);' + #13#10 +
'cfg=r[0];document.getElementById("env-count").textContent=cfg.environments?cfg.environments.length:0;' + #13#10 +
'document.getElementById("project-count").textContent=r[1].length||0;' + #13#10 +
'var cov=r[2].available?r[2].coverage:0;document.getElementById("coverage-value").textContent=cov?cov+"%":"N/A";' + #13#10 +
'var ring=document.getElementById("cov-ring");ring.style.strokeDasharray=Math.round(cov*2.64)+" 264";' + #13#10 +
'envList(cfg.environments||[]);act("Data loaded","Just now","Done");}catch(e){console.error(e);}}' + #13#10 +
'function envList(a){var l=document.getElementById("env-list");if(!a.length){l.innerHTML="<li class=\"env-i\">No environments</li>";return;}' + #13#10 +
'var h="";for(var i=0;i<a.length;i++){var e=a[i];h+="<li class=\"env-i\"><div class=\"env-nm\">"+e.name+(e.isDefault?" <span class=\"env-bg\">Default</span>":"")+"</div><span class=\"env-pt\">"+e.path+"</span></li>";}l.innerHTML=h;}' + #13#10 +
'function act(a,t,s){var b=document.getElementById("at-body");var c=s=="Done"?"st-s":s=="Active"?"st-p":"st-w";' + #13#10 +
'b.innerHTML="<tr><td>"+a+"</td><td>"+t+"</td><td><span class=\"st "+c+"\">"+s+"</span></td></tr>"+b.innerHTML;}' + #13#10 +
'async function scan(){act("Scanning...","Now","Active");try{await fetch("/api/env/scan",{method:"POST"});load();act("Scan done","Just now","Done");}catch(e){console.error(e);}}' + #13#10 +
'function hub(){var c=new signalR.HubConnectionBuilder().withUrl("/hubs/dashboard").withAutomaticReconnect().build();' + #13#10 +
'c.on("ReceiveLog",function(l,m){var p=document.getElementById("logs");var t=new Date().toLocaleTimeString();' + #13#10 +
'var x=l.toLowerCase().indexOf("error")>=0?"log-e":l.toLowerCase().indexOf("warn")>=0?"log-w":"log-i";' + #13#10 +
'p.innerHTML+="<div class=\"log\"><span class=\"log-t\">["+t+"]</span><span class=\""+x+"\">"+m+"</span></div>";p.scrollTop=p.scrollHeight;});' + #13#10 +
'c.start().catch(function(e){console.error(e);});}' + #13#10 +
'document.querySelectorAll(".ni").forEach(function(x){x.addEventListener("click",function(e){e.preventDefault();document.querySelectorAll(".ni").forEach(function(y){' + 
'y.classList.remove("on");});x.classList.add("on");});});' + #13#10 +
'function toggleTheme(){var r=document.documentElement;var isLight=r.classList.toggle("light");localStorage.setItem("dext-theme",isLight?"light":"dark");document.getElementById("theme-icon").textContent=isLight?"light_mode":"dark_mode";}' + #13#10 +
'(function(){var t=localStorage.getItem("dext-theme");if(t=="light"){document.documentElement.classList.add("light");document.getElementById("theme-icon").textContent="light_mode";}})();' + #13#10 +
'load();hub();' + #13#10 +
'</script></body></html>';
end;



procedure TUICommand.Execute(const Args: TCommandLineArgs);
var
  Port: Integer;
  Host: IWebHost;
  DashboardHTML: string;
begin
  Port := 3000;
  if Args.HasOption('port') then
    Port := StrToIntDef(Args.GetOption('port'), 3000);

  DashboardHTML := GetDashboardHTML;

  SafeWriteLn(Format('Starting Dext Dashboard (Material 3) on port %d...', [Port]));

  Host := TWebHostBuilder.CreateDefault(nil)
    .UseUrls(Format('http://localhost:%d', [Port]))
    .ConfigureServices(procedure(Services: IServiceCollection)
      var
        RegistryType: TServiceType;
        RegistryInstance: TObject;
        LoggerType: TServiceType;
        FactoryFunc: TFunc<IServiceProvider, TObject>;
      begin
        RegistryType := TServiceType.FromClass(TProjectRegistry);
        RegistryInstance := TProjectRegistry.Create;
        Services.AddSingleton(RegistryType, RegistryInstance);
        
        LoggerType := TServiceType.FromInterface(TypeInfo(ILoggerFactory));
        
        FactoryFunc := function(Provider: IServiceProvider): TObject
           var
             Factory: TLoggerFactory;
           begin
              Factory := TLoggerFactory.Create;
              Factory.AddProvider(TConsoleHubLoggerProvider.Create);
              Result := Factory;
           end;
           
        Services.AddSingleton(LoggerType, TClass(nil), FactoryFunc);
      end)
    .Configure(procedure(App: IApplicationBuilder)
      var
        CapturedHTML: string;
      begin
        CapturedHTML := DashboardHTML;
        
        THubExtensions.MapHub(App, '/hubs/dashboard', TDashboardHub);

        // Serve embedded dashboard HTML
        App.Use(procedure(Ctx: IHttpContext; Next: TRequestDelegate)
          var
            Path: string;
            Content: TBytes;
          begin
            Path := Ctx.Request.Path;
            if (Path = '/') or (Path = '/index.html') then
            begin
              Ctx.Response.SetContentType('text/html; charset=utf-8');
              Content := TEncoding.UTF8.GetBytes(CapturedHTML);
              Ctx.Response.SetContentLength(Length(Content));
              Ctx.Response.Write(Content);
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

      end)
    .Build;
  OpenBrowser(Format('http://localhost:%d', [Port]));
  Host.Run;
end;

end.
