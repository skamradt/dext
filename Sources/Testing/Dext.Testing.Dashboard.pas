unit Dext.Testing.Dashboard;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  IdHTTPServer,
  IdContext,
  IdCustomHTTPServer,
  IdGlobal, // Added for ToBytes/TIdBytes
  Dext.Testing.Runner,
  Dext.Testing.History;

type
  { TDashboardListener }
  TDashboardListener = class(TInterfacedObject, ITestListener)
  private
    FServer: TIdHTTPServer;
    FClients: TList<TIdContext>;
    FLock: TObject;
    FPort: Integer;
    FEventBuffer: TList<string>; 
    
    // Server events
    procedure OnConnect(AContext: TIdContext);
    procedure OnDisconnect(AContext: TIdContext);
    procedure OnCommandGet(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    
    // ITestListener implementation
    procedure OnRunStart(TotalTests: Integer);
    procedure OnRunComplete(const Summary: TTestSummary);
    procedure OnFixtureStart(const FixtureName: string; TestCount: Integer);
    procedure OnFixtureComplete(const FixtureName: string);
    procedure OnTestStart(const Fixture, Test: string);
    procedure OnTestComplete(const Info: TTestInfo);
    
    // Helper
    procedure BroadcastEvent(const EventType: string; const DataJson: string);
  public
    constructor Create(Port: Integer = 9000);
    destructor Destroy; override;
    
    procedure Start;
    procedure Stop;
  end;

implementation

uses
  Dext.Utils;

const
  HTML_TEMPLATE = 
    '<!DOCTYPE html>' +
    '<html lang="en">' +
    '<head>' +
    '    <meta charset="UTF-8">' +
    '    <meta name="viewport" content="width=device-width, initial-scale=1.0">' +
    '    <title>Dext Dashboard</title>' +
    '    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=JetBrains+Mono:wght@400&display=swap" rel="stylesheet">' +
    '    <style>' +
    '        :root { --bg: #1a1a1a; --sidebar: #252526; --card: #2d2d2d; --text: #e0e0e0; --text-dim: #999; --accent: #007acc; --pass: #4caf50; --fail: #f44336; --skip: #ffeb3b; }' +
    '        body { font-family: "Inter", sans-serif; background-color: var(--bg); color: var(--text); margin: 0; display: flex; height: 100vh; overflow: hidden; }' +
    '        /* Layout */' +
    '        .sidebar { width: 250px; background: var(--sidebar); display: flex; flex-direction: column; border-right: 1px solid #333; flex-shrink: 0; }' +
    '        .brand { padding: 20px; font-size: 1.2rem; font-weight: 600; border-bottom: 1px solid #333; display: flex; align-items: center; gap: 10px; }' +
    '        .brand span { color: var(--accent); }' +
    '        .nav-item { padding: 15px 20px; cursor: pointer; color: var(--text-dim); transition: 0.2s; display: flex; align-items: center; gap: 10px; }' +
    '        .nav-item:hover, .nav-item.active { background: #333; color: white; border-left: 3px solid var(--accent); }' +
    '        .main { flex: 1; display: flex; flex-direction: column; overflow: hidden; position: relative; }' +
    '        .view { display: none; flex: 1; flex-direction: column; overflow: hidden; width: 100%; height: 100%; }' +
    '        .view.active { display: flex; }' +
    '        /* Dashboard View */' +
    '        .doughnut-container { padding: 30px; display: flex; flex-direction: column; align-items: center; border-bottom: 1px solid #333; }' +
    '        .doughnut { width: 160px; height: 160px; border-radius: 50%; background: conic-gradient(var(--pass) 0% 0%, var(--fail) 0% 0%, #444 0% 100%); position: relative; transition: background 0.5s; box-shadow: 0 0 20px rgba(0,0,0,0.3); }' +
    '        .doughnut::after { content: ""; position: absolute; inset: 15px; background: var(--sidebar); border-radius: 50%; }' +
    '        .doughnut-label { position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); text-align: center; z-index: 10; }' +
    '        .doughnut-label .value { font-size: 2.5rem; font-weight: bold; }' +
    '        .doughnut-label .label { font-size: 0.8rem; color: var(--text-dim); letter-spacing: 1px; }' +
    '        .toolbar { padding: 15px 20px; border-bottom: 1px solid #333; display: flex; justify-content: space-between; align-items: center; background: var(--bg); flex-shrink: 0; }' +
    '        .status-badge { padding: 4px 8px; border-radius: 4px; font-size: 0.8rem; font-weight: 500; }' +
    '        .status-running { background: #007acc33; color: #64b5f6; }' +
    '        .content { flex: 1; overflow-y: auto; padding: 20px; }' +
    '        /* Timeline View */' +
    '        .chart-container { flex: 1; padding: 20px; display: flex; flex-direction: column; }' +
    '        svg { width: 100%; height: 100%; overflow: visible; }' +
    '        .axis-line { stroke: #444; stroke-width: 1; }' +
    '        .grid-line { stroke: #333; stroke-width: 1; stroke-dasharray: 4; }' +
    '        .line-path { fill: none; stroke-width: 3; stroke-linecap: round; stroke-linejoin: round; }' +
    '        .area-path { stroke: none; opacity: 0.2; }' +
    '        .dot { r: 4; stroke-width: 2; stroke: var(--bg); cursor: pointer; transition: r 0.2s; }' +
    '        .dot:hover { r: 7; }' +
    '        .tooltip { position: absolute; background: #333; padding: 8px; border-radius: 4px; border: 1px solid #555; pointer-events: none; opacity: 0; transition: opacity 0.2s; font-size: 0.9em; box-shadow: 0 4px 10px rgba(0,0,0,0.5); }' +
    '        /* Test List */' +
    '        .test-card { background: var(--card); border-radius: 6px; margin-bottom: 8px; overflow: hidden; display: flex; flex-direction: column; border: 1px solid transparent; transition: border-color 0.2s; }' +
    '        .test-card:hover { border-color: #444; }' +
    '        .test-header { padding: 12px 15px; display: flex; align-items: center; cursor: pointer; user-select: none; }' +
    '        .test-icon { margin-right: 12px; width: 50px; font-weight: bold; font-size: 0.8em; text-align: center; }' +
    '        .test-info { flex: 1; }' +
    '        .test-name { font-weight: 500; }' +
    '        .test-fixture { font-size: 0.8em; color: var(--text-dim); margin-top: 2px; }' +
    '        .test-meta { font-family: "JetBrains Mono", monospace; font-size: 0.85em; color: var(--text-dim); }' +
    '        .test-details { background: #222; padding: 15px; border-top: 1px solid #333; display: none; }' +
    '        .test-details.open { display: block; animation: slideDown 0.2s; }' +
    '        .error-msg { color: var(--fail); white-space: pre-wrap; font-family: "JetBrains Mono", monospace; font-size: 0.9em; line-height: 1.5; }' +
    '        /* Animations */' +
    '        @keyframes pulse { 0% { opacity: 0.6; transform: scale(0.95); } 100% { opacity: 1; transform: scale(1); } }' +
    '        @keyframes slideDown { from { opacity: 0; transform: translateY(-5px); } to { opacity: 1; transform: translateY(0); } }' +
    '        .running .test-icon { animation: pulse 1s infinite alternate; color: var(--accent); }' +
    '        .passed .test-icon { color: var(--pass); }' +
    '        .failed .test-icon { color: var(--fail); }' +
    '        .skipped .test-icon { color: var(--skip); }' +
    '        .test-card.failed { border-left: 3px solid var(--fail); }' +
    '        /* Run List */' +
    '        .run-list { flex: 1; overflow-y: auto; border-top: 1px solid #333; }' +
    '        .run-item { padding: 12px 20px; border-bottom: 1px solid #333; display: flex; justify-content: space-between; align-items: center; }' +
    '        .run-item:hover { background: #222; }' +
    '        .run-meta { display: flex; flex-direction: column; gap: 4px; }' +
    '        .run-date { font-size: 0.9em; color: var(--text); }' +
    '        .run-duration { font-size: 0.8em; color: var(--text-dim); font-family: "JetBrains Mono", monospace; }' +
    '        .run-stats { display: flex; gap: 15px; font-size: 0.85em; font-family: "JetBrains Mono", monospace; }' +
    '        .stat-pill { font-weight: 500; }' +
    '        .stat-pill.pass { color: var(--pass); }' +
    '        .stat-pill.fail { color: var(--fail); font-weight: bold; }' +
    '        .stat-pill.skip { color: var(--skip); }' +
    '    </style>' +
    '</head>' +
    '<body>' +
    '    <div class="sidebar">' +
    '        <div class="brand">Dext Dashboard</div>' +
    '        <div class="doughnut-container">' +
    '            <div class="doughnut" id="chart">' +
    '                <div class="doughnut-label">' +
    '                    <div class="value" id="pass-percent">0%</div>' +
    '                    <div class="label">PASS RATE</div>' +
    '                </div>' +
    '            </div>' +
    '        </div>' +
    '        <div class="nav-item active" onclick="switchView(&quot;dashboard&quot;)">' +
    '            <span>Current Run</span>' +
    '        </div>' +
    '        <div class="nav-item" onclick="switchView(&quot;timeline&quot;)">' +
    '            <span>History</span>' +
    '        </div>' +
    '    </div>' +
    '    ' +
    '    <div class="main">' +
    '        <!-- Dashboard View -->' +
    '        <div id="view-dashboard" class="view active">' +
    '            <div class="toolbar">' +
    '                <div id="status-text">Waiting for test run...</div>' +
    '                <div class="status-badge status-running" id="connection-status">Connecting...</div>' +
    '            </div>' +
    '            <div class="content" id="test-list"></div>' +
    '        </div>' +
    '        ' +
    '        <!-- Timeline View -->' +
    '        <div id="view-timeline" class="view">' +
    '             <div class="toolbar"><div>Test Quality Trend</div></div>' +
    '             <div class="chart-container" id="timeline-container" style="height: 300px; flex-shrink: 0;">Loading...</div>' +
    '             <div class="toolbar" style="background:var(--sidebar); border-top:1px solid #333;"><div>Executions</div></div>' +
    '             <div id="run-list" class="run-list"></div>' +
    '             <div id="tooltip" class="tooltip"></div>' +
    '        </div>' +
    '    </div>' +
    '    ' +
    '    <script>' +
    '        /* --- Logic --- */' +
    '        const els = {' +
    '            list: document.getElementById("test-list"),' +
    '            chart: document.getElementById("chart"),' +
    '            percent: document.getElementById("pass-percent"),' +
    '            status: document.getElementById("status-text"),' +
    '            conn: document.getElementById("connection-status"),' +
    '            timeline: document.getElementById("timeline-container"),' +
    '            tooltip: document.getElementById("tooltip")' +
    '        };' +
    '        let stats = { total: 0, passed: 0, failed: 0, skipped: 0 };' +
    '        ' +
    '        function switchView(name) {' +
    '            document.querySelectorAll(".view").forEach(el => el.classList.remove("active"));' +
    '            document.getElementById("view-" + name).classList.add("active");' +
    '            document.querySelectorAll(".nav-item").forEach(el => el.classList.remove("active"));' +
    '            event.currentTarget.classList.add("active");' +
    '            if(name === "timeline") loadHistory();' +
    '        }' +
    '        ' +
    '        function updateChart() {' +
    '            const total = stats.passed + stats.failed + stats.skipped;' +
    '            if (total === 0) return;' +
    '            const pPass = (stats.passed / total) * 100;' +
    '            const pFail = (stats.failed / total) * 100;' +
    '            const pSkip = (stats.skipped / total) * 100;' +
    '            stats.total = total;' +
    '            ' +
    '            /* Conic gradient visual */' +
    '            els.chart.style.background = `conic-gradient(var(--pass) 0% ${pPass}%, var(--fail) ${pPass}% ${pPass+pFail}%, var(--skip) ${pPass+pFail}% 100%)`;' +
    '            els.percent.innerText = Math.round(pPass) + "%";' +
    '            els.status.innerText = `Running... ${stats.passed} Passed · ${stats.failed} Failed`;' +
    '            document.title = `(${stats.failed} Fails) Dext Dashboard`;' +
    '        }' +
    '        ' +
    '        /* SSE Handling */' +
    '        const source = new EventSource("/events");' +
    '        source.onopen = () => { els.conn.innerText = "Live"; els.conn.className = "status-badge status-running"; };' +
    '        source.onerror = () => { els.conn.innerText = "Disconnected"; els.conn.className = "status-badge"; };' +
    '        ' +
    '        source.addEventListener("run_start", (e) => {' +
    '            els.list.innerHTML = ""; stats = { total: 0, passed: 0, failed: 0, skipped: 0 }; updateChart();' +
    '        });' +
    '        ' +
    '        source.addEventListener("test_start", (e) => {' +
    '            const msg = JSON.parse(e.data);' +
    '            const id = ("t-" + msg.fixture + "-" + msg.test).replace(/[^a-zA-Z0-9-_]/g, "_");' +
    '            const html = `' +
    '              <div class="test-card" id="${id}">' +
    '                 <div class="test-header">' +
    '                     <div class="test-icon running">RUN</div>' +
    '                     <div class="test-info">' +
    '                         <div class="test-name">${msg.test}</div>' +
    '                         <div class="test-fixture">${msg.fixture}</div>' +
    '                     </div>' +
    '                     <div class="test-meta">Running...</div>' +
    '                 </div>' +
    '              </div>`;' +
    '            els.list.insertAdjacentHTML("afterbegin", html);' +
    '        });' +
    '        ' +
    '        source.addEventListener("test_complete", (e) => {' +
    '             const msg = JSON.parse(e.data);' +
    '             const id = ("t-" + msg.fixture + "-" + msg.test).replace(/[^a-zA-Z0-9-_]/g, "_");' +
    '             const el = document.getElementById(id);' +
    '             if (el) {' +
    '                 let icon = "FAIL";' +
    '                 if (msg.status === "passed") icon = "PASS";' +
    '                 else if (msg.status === "skipped") icon = "SKIP";' +
    '                 el.className = "test-card " + msg.status;' +
    '                 el.querySelector(".test-icon").className = "test-icon"; /* remove running */' +
    '                 el.querySelector(".test-icon").innerText = icon;' +
    '                 el.querySelector(".test-meta").innerText = msg.duration + "ms";' +
    '                 if (stats[msg.status] !== undefined) stats[msg.status]++;' +
    '                 updateChart();' +
    '                 if (msg.status === "failed") {' +
    '                     const detailsId = id + "-details";' +
    '                     el.querySelector(".test-header").setAttribute("onclick", `document.getElementById("${detailsId}").classList.toggle("open")`);' +
    '                     const detailsHtml = `<div class="test-details" id="${detailsId}"><div class="error-msg">${msg.error}</div></div>`;' +
    '                     el.insertAdjacentHTML("beforeend", detailsHtml);' +
    '                 }' +
    '             }' +
    '        });' +
    '        ' +
    '        source.addEventListener("run_complete", (e) => {' +
    '             els.status.innerText = "Run Complete";' +
    '             loadHistory(); /* refresh history */' +
    '        });' +
    '        ' +
    '        /* History Logic (Native SVG Chart code) */' +
    '        async function loadHistory() {' +
    '            const res = await fetch("/api/history");' +
    '            const data = await res.json();' +
    '            drawChart(data);' +
    '            drawRunList(data);' +
    '        }' +
    '        ' +
    '        function drawChart(data) {' +
    '            if(!data || data.length === 0) { els.timeline.innerHTML = "No history available"; return; }' +
    '            const padding = 40; const w = els.timeline.clientWidth; const h = els.timeline.clientHeight || 400;' +
    '            const maxVal = Math.max(...data.map(d => d.total)) * 1.1 || 10;' +
    '            ' +
    '            const scaleX = (i) => padding + (i / (data.length - 1 || 1)) * (w - 2 * padding);' +
    '            const scaleY = (v) => h - padding - (v / maxVal) * (h - 2 * padding);' +
    '            ' +
    '            /* Generate paths */' +
    '            const passPoints = data.map((d, i) => `${scaleX(i)},${scaleY(d.passed)}`).join(" ");' +
    '            const failPoints = data.map((d, i) => `${scaleX(i)},${scaleY(d.failed)}`).join(" ");' +
    '            ' +
    '            let svg = `<svg viewBox="0 0 ${w} ${h}">`;' +
    '            ' +
    '            /* Axes */' +
    '            svg += `<line x1="${padding}" y1="${h-padding}" x2="${w-padding}" y2="${h-padding}" class="axis-line"/>`;' +
    '            ' +
    '            if (data.length > 1) {' +
    '                /* Pass Line (Green) */' +
    '                svg += `<polyline points="${passPoints}" class="line-path" stroke="#4caf50" />`;' +
    '                /* Fail Line (Red) */' +
    '                svg += `<polyline points="${failPoints}" class="line-path" stroke="#f44336" />`;' +
    '                ' +
    '                /* Dots */' +
    '                data.forEach((d, i) => {' +
    '                   svg += `<circle cx="${scaleX(i)}" cy="${scaleY(d.passed)}" class="dot" fill="#4caf50" onmouseover="showTooltip(evt, ''Passed: ${d.passed}\nTotal: ${d.total}'')" onmouseout="hideTooltip()" />`;' +
    '                   if (d.failed > 0) svg += `<circle cx="${scaleX(i)}" cy="${scaleY(d.failed)}" class="dot" fill="#f44336" onmouseover="showTooltip(evt, ''Failed: ${d.failed}'')" onmouseout="hideTooltip()" />`;' +
    '                });' +
    '            }' +
    '            svg += `</svg>`;' +
    '            els.timeline.innerHTML = svg;' +
    '        }' +
    '        ' +
    '        function drawRunList(data) {' +
    '            const container = document.getElementById("run-list");' +
    '            if (!container) return;' +
    '            container.innerHTML = "";' +
    '            if(!data || data.length === 0) { container.innerHTML = "<div style=''padding:20px;color:#999''>No history yet</div>"; return; }' +
    '            /* Clone and reverse to show newest first */' +
    '            const list = [...data].reverse();' +
    '            list.forEach(run => {' +
    '                 const date = new Date(run.date).toLocaleString();' +
    '                 const html = `' +
    '                     <div class="run-item">' +
    '                         <div class="run-meta">' +
    '                             <div class="run-date">${date}</div>' +
    '                             <div class="run-duration">Duration: ${run.duration}ms</div>' +
    '                         </div>' +
    '                         <div class="run-stats">' +
    '                             <span class="stat-pill pass">PASS: ${run.passed}</span>' +
    '                             ${run.failed > 0 ? `<span class="stat-pill fail">FAIL: ${run.failed}</span>` : ""}' +
    '                             ${run.skipped > 0 ? `<span class="stat-pill skip">SKIP: ${run.skipped}</span>` : ""}' +
    '                         </div>' +
    '                     </div>`;' +
    '                 container.insertAdjacentHTML("beforeend", html);' +
    '            });' +
    '        }' +
    '        ' +
    '        function showTooltip(evt, text) {' +
    '            els.tooltip.innerText = text;' +
    '            els.tooltip.style.left = (evt.clientX + 10) + "px";' +
    '            els.tooltip.style.top = (evt.clientY - 30) + "px";' +
    '            els.tooltip.style.opacity = 1;' +
    '        }' +
    '        function hideTooltip() { els.tooltip.style.opacity = 0; }' +
    '    </script>' +
    '</body>' +
    '</html>';

{ TDashboardListener }

constructor TDashboardListener.Create(Port: Integer);
begin
  inherited Create;
  FPort := Port;
  FLock := TObject.Create;
  FClients := TList<TIdContext>.Create;
  FEventBuffer := TList<string>.Create;
  
  FServer := TIdHTTPServer.Create(nil);
  FServer.OnConnect := OnConnect;
  FServer.OnDisconnect := OnDisconnect;
  FServer.OnCommandGet := OnCommandGet;
  FServer.DefaultPort := FPort;
end;

destructor TDashboardListener.Destroy;
begin
  Stop;
  FServer.Free;
  FClients.Free;
  FEventBuffer.Free;
  FLock.Free;
  inherited;
end;

procedure TDashboardListener.Start;
begin
  if not FServer.Active then
  begin
    FServer.Active := True;
    SafeWriteLn('Dext Dashboard running at http://localhost:' + FPort.ToString);
    TTestRunner.RegisterListener(Self);
  end;
end;

procedure TDashboardListener.Stop;
begin
  if FServer.Active then
    FServer.Active := False;
end;

procedure TDashboardListener.OnConnect(AContext: TIdContext);
begin
end;

procedure TDashboardListener.OnDisconnect(AContext: TIdContext);
begin
  TMonitor.Enter(FLock);
  try
    FClients.Remove(AContext);
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TDashboardListener.OnCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Msg: string;
begin
  if SameText(ARequestInfo.URI, '/') then
  begin
    AResponseInfo.ContentStream := TStringStream.Create(HTML_TEMPLATE, TEncoding.UTF8); 
    AResponseInfo.ContentType := 'text/html; charset=utf-8';
    AResponseInfo.ResponseNo := 200;
  end
  else if SameText(ARequestInfo.URI, '/favicon.ico') then
  begin
    AResponseInfo.ResponseNo := 204; // No Content
  end
  else if SameText(ARequestInfo.URI, '/api/history') then
  begin
    AResponseInfo.ContentText := TTestHistoryManager.LoadHistoryJson;
    AResponseInfo.ContentType := 'application/json';
    AResponseInfo.ResponseNo := 200;
  end
  else if SameText(ARequestInfo.URI, '/events') then
  begin
    // Write Raw Headers to force Content-Type and avoid defaults    
    AContext.Connection.IOHandler.WriteLn('HTTP/1.1 200 OK');
    AContext.Connection.IOHandler.WriteLn('Content-Type: text/event-stream; charset=utf-8');
    AContext.Connection.IOHandler.WriteLn('Cache-Control: no-cache');
    AContext.Connection.IOHandler.WriteLn('Connection: keep-alive');
    AContext.Connection.IOHandler.WriteLn('');

    
    TMonitor.Enter(FLock);
    try
      // if FEventBuffer.Count > 0 then
      //   SafeWriteLn('Replaying ' + FEventBuffer.Count.ToString + ' events to new client');

      // Replay existing events to the new client
      for Msg in FEventBuffer do
      begin
        try
           // Write raw bytes to avoid Indy string length prefix
           AContext.Connection.IOHandler.Write(ToBytes(Msg, IndyTextEncoding_UTF8));
        except
        end;
      end;
      
      if not FClients.Contains(AContext) then
        FClients.Add(AContext);
    finally
      TMonitor.Exit(FLock);
    end;
    
    // Hold for SSE
    try
       while AContext.Connection.Connected and FServer.Active do
         Sleep(100); 
    except
    end;
    
    TMonitor.Enter(FLock);
    try
      FClients.Remove(AContext);
    finally
      TMonitor.Exit(FLock);
    end;
  end
  else
  begin
    AResponseInfo.ResponseNo := 404;
  end;
end;

procedure TDashboardListener.BroadcastEvent(const EventType: string; const DataJson: string);
var
  Ctx: TIdContext;
  I: Integer;
  FullMsg: string;
begin
  // Format message once
  FullMsg := 'event: ' + EventType + sLineBreak + 
             'data: ' + DataJson + sLineBreak + 
             sLineBreak;

  TMonitor.Enter(FLock);
  try
    // Store in buffer
    FEventBuffer.Add(FullMsg);
    
    // Send to active clients
    for I := FClients.Count - 1 downto 0 do
    begin
      Ctx := FClients[I];
      try
        // Write raw bytes to avoid Indy string length prefix
        Ctx.Connection.IOHandler.Write(ToBytes(FullMsg, IndyTextEncoding_UTF8));
      except
        FClients.Remove(Ctx);
      end;
    end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TDashboardListener.OnRunStart(TotalTests: Integer);
begin
  // Clear buffer on new run
  TMonitor.Enter(FLock);
  try
    FEventBuffer.Clear;
  finally
    TMonitor.Exit(FLock);
  end;
  BroadcastEvent('run_start', Format('{"total": %d}', [TotalTests]));
end;

procedure TDashboardListener.OnRunComplete(const Summary: TTestSummary);
begin
  // Save history
  TTestHistoryManager.AppendRun(Summary);
  
  BroadcastEvent('run_complete', '{}');
  
  SafeWriteLn;
  SafeWriteLn('📊 Dext Dashboard: http://localhost:' + FPort.ToString);
  SafeWriteLn;
end;

procedure TDashboardListener.OnFixtureStart(const FixtureName: string; TestCount: Integer);
begin
  // Not used in frontend yet
end;

procedure TDashboardListener.OnFixtureComplete(const FixtureName: string);
begin
  // Not used
end;

procedure TDashboardListener.OnTestStart(const Fixture, Test: string);
begin
  BroadcastEvent('test_start', Format('{"fixture": "%s", "test": "%s"}', [Fixture, Test]));
end;

procedure TDashboardListener.OnTestComplete(const Info: TTestInfo);
var
  Status: string;
  ErrMsg: string;
begin
  if Info.Result = trPassed then Status := 'passed'
  else if Info.Result = trSkipped then Status := 'skipped'
  else Status := 'failed';

  ErrMsg := Info.ErrorMessage.Replace('"', '\"').Replace(#13#10, '\n').Replace(#10, '\n').Replace(#13, '');
  
  BroadcastEvent('test_complete', Format('{"fixture": "%s", "test": "%s", "status": "%s", "passed": %s, "duration": %d, "error": "%s"}', 
    [Info.FixtureName, Info.DisplayName, Status, BoolToStr(Info.Result = trPassed, True).ToLower, Round(Info.Duration.TotalMilliseconds), ErrMsg]));
end;

end.
