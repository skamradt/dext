// ==================== i18n ====================
// i18n object is now defined in i18n.js


var currentLang = localStorage.getItem("dext-lang");
if (!currentLang) {
    var browserLang = (navigator.language || navigator.userLanguage || "en").toLowerCase().split("-")[0];
    currentLang = i18n[browserLang] ? browserLang : "en";
}

function t(key) { return i18n[currentLang][key] || key; }

function applyI18n() {
    document.querySelectorAll("[data-i18n]").forEach(function (el) {
        var key = el.getAttribute("data-i18n");
        var val = t(key);
        if ((el.tagName == "INPUT" || el.tagName == "TEXTAREA") && el.placeholder) el.placeholder = val;
        else if (el.tagName == "OPTION") el.textContent = val;
        else el.textContent = val;
    });
    // Update dynamic things
    var p = document.querySelector(".ni.on").getAttribute("data-page");
    document.getElementById("page-title").textContent = t(p);
}

function changeLang(lang) {
    currentLang = lang;
    localStorage.setItem("dext-lang", currentLang);
    document.getElementById("lang-select").value = currentLang;
    applyI18n();
}


// ==================== Dashboard Functions ====================
async function load() {
    try {
        // Sync language selector
        document.getElementById("lang-select").value = currentLang;

        var r = await Promise.all([fetch("/api/config").then(function (x) { return x.json() }), fetch("/api/projects").then(function (x) { return x.json() }),
        fetch("/api/test/summary").then(function (x) { return x.json() })]);
        cfg = r[0]; document.getElementById("env-count").textContent = cfg.environments ? cfg.environments.length : 0;
        document.getElementById("project-count").textContent = r[1].length || 0;
        var cov = r[2].available ? r[2].coverage : 0; document.getElementById("coverage-value").textContent = cov ? cov + "%" : "N/A";
        var ring = document.getElementById("cov-ring"); ring.style.strokeDasharray = Math.round(cov * 2.64) + " 264";
        envList(cfg.environments || []); act(t("data_loaded"), t("just_now"), t("done"));

        applyI18n(); // Initial translation
        document.getElementById("lang-label").textContent = currentLang.toUpperCase();

        // Restore active project if exists
        var savedPj = localStorage.getItem("activeProject");
        if (savedPj) selectActiveProject(savedPj, true);
    } catch (e) { console.error(e); }
}

function envList(a) {
    var l = document.getElementById("env-list"); if (!a.length) { l.innerHTML = "<li class=\"env-i\">" + t("no_env") + "</li>"; return; }
    var h = ""; for (var i = 0; i < a.length; i++) { var e = a[i]; h += "<li class=\"env-i\"><div class=\"env-nm\">" + e.name + (e.isDefault ? " <span class=\"env-bg\">Default</span>" : "") + "</div><span class=\"env-pt\">" + e.path + "</span></li>"; } l.innerHTML = h;
}

function act(a, t, s) {
    var b = document.getElementById("at-body"); var c = s == "Done" ? "st-s" : s == "Active" ? "st-p" : "st-w";
    b.innerHTML = "<tr><td>" + a + "</td><td>" + t + "</td><td><span class=\"st " + c + "\">" + s + "</span></td></tr>" + b.innerHTML;
}

async function scan() { act(t("active"), t("just_now"), t("active")); try { await fetch("/api/env/scan", { method: "POST" }); load(); act(t("done"), t("just_now"), t("done")); } catch (e) { console.error(e); } }

// Test Runner State
var trState = { total: 0, current: 0, passed: 0, failed: 0, open: false };

// Connection Logic (SignalR vs SSE)
function hub() {
    // Try SignalR first (Sidecar / CLI Dashboard)
    var c = new signalR.HubConnectionBuilder().withUrl("/hubs/dashboard")
        .withAutomaticReconnect()
        .build();

    c.start()
        .then(function () {
            // SignalR Connected
            console.log("Connected via SignalR");
            c.on("ReceiveLog", function (l, m) {
                processLog(l, m);
            });
        })
        .catch(function (e) {
            console.log("SignalR failed, trying SSE (Standalone Mode)...");
            connectSSE();
        });
}


function connectSSE() {
    console.log("Starting SSE Connection...");
    if (window.dashboardSSE) window.dashboardSSE.close();
    window.dashboardSSE = new EventSource("/events");
    var evtSource = window.dashboardSSE;

    evtSource.onopen = function () { console.log("SSE Connection Opened (Global)"); };
    evtSource.onerror = function (e) {
        if (e.target.readyState == EventSource.CLOSED) {
            console.log("SSE Closed. Reconnecting in 5s...");
            setTimeout(connectSSE, 5000);
        }
    };
    // Generic message listener removed (too verbose)

    evtSource.addEventListener("run_start", function (e) {
        var d = JSON.parse(e.data);
        // Payload has 'totalTests', JS expects 'total'
        var total = d.total || d.totalTests || 0;
        trState = { total: total, current: 0, passed: 0, failed: 0, open: true };
        updateTestProgress();
    });

    evtSource.addEventListener("test_start", function (e) {
        // var d = JSON.parse(e.data);
    });

    evtSource.addEventListener("test_complete", function (e) {
        var info = JSON.parse(e.data);
        trState.current++;

        // Derive passed boolean from status string
        var isPassed = (info.status === "Passed");
        if (isPassed) trState.passed++; else trState.failed++;

        // Log equivalent - MUST match format expected by updateTestState (Fixture.Test)
        var testFullName = (info.fixture ? info.fixture + "." : "") + info.test;
        var msg = (isPassed ? "Passed" : "Failed") + " Test: " + testFullName;

        var lvl = isPassed ? "Info" : "Error";
        processLog(lvl, msg);

        updateTestProgress();
    });

    evtSource.addEventListener("run_complete", function (e) {
        trState.open = false;
        updateTestProgress(true);
    });
}

function processLog(level, message) {
    var logs1 = document.getElementById("logs");
    var logs2 = document.getElementById("logs-full");
    var t = new Date().toLocaleTimeString();
    var x = level.toLowerCase().indexOf("error") >= 0 ? "log-e" : level.toLowerCase().indexOf("warn") >= 0 ? "log-w" : "log-i";

    var html = "<div class=\"log\"><span class=\"log-t\">[" + t + "]</span><span class=\"" + x + "\">" + message + "</span></div>";

    if (logs1) {
        logs1.innerHTML += html;
        logs1.scrollTop = logs1.scrollHeight;
    }
    if (logs2) {
        logs2.innerHTML += html;
        logs2.scrollTop = logs2.scrollHeight;
    }

    // --- Test Runner Progress Parsing (For SignalR/Log Mode) ---
    // If we are in SSE mode, this parsing is redundant but harmless as logs from SSE are synthetic here.
    // However, if we receive Raw Logs via SignalR (Sidecar), we MUST parse them here.
    if (message.indexOf("Run Started") >= 0) {
        trState = { total: 0, current: 0, passed: 0, failed: 0, open: true };
        var match = message.match(/(\d+) tests/);
        if (match) trState.total = parseInt(match[1]);
        updateTestProgress();
    } else if (trState.open && !window.sseActive) { // Only parse logs if NOT using SSE events
        if (message.indexOf("Passed Test:") >= 0) {
            trState.current++; trState.passed++;
            updateTestProgress();
        } else if (message.indexOf("Failed Test:") >= 0) {
            trState.current++; trState.failed++;
            updateTestProgress();
        } else if (message.indexOf("Skipped Test:") >= 0) {
            trState.current++;
            updateTestProgress();
        } else if (message.indexOf("Run Completed:") >= 0) {
            trState.open = false;
            updateTestProgress(true);
        }
    }

    // Update Test Tree (New Feature)
    updateTestState(message);
}

function updateTestProgress(done) {
    // We need a UI element for this.
    // For now, let's inject a status card into the Dashboard if not exists, or update it.
    var el = document.getElementById("tr-status");
    if (!el) {
        // Create card dynamically in the first grid
        var grid = document.querySelector(".cg");
        if (grid) {
            el = document.createElement("div");
            el.id = "tr-status";
            el.className = "cd";
            el.innerHTML = `
                <div class="cd-hd"><span class="cd-tt">Test Runner</span>
                    <div class="cd-ic"><span class="ms material-symbols-outlined">play_circle</span></div>
                </div>
                <div class="cd-val" id="tr-main">Ready</div>
                <div class="cd-sub" id="tr-sub">Waiting for execution...</div>
                <div class="cd-bar" style="margin-top:10px;height:4px;background:#333;border-radius:2px;overflow:hidden">
                    <div id="tr-bar" style="width:0%;height:100%;background:var(--primary);transition:width 0.3s"></div>
                </div>
            `;
            grid.insertBefore(el, grid.firstChild);
        } else return;
    }

    var pct = trState.total > 0 ? (trState.current / trState.total) * 100 : 0;
    if (pct > 100) pct = 100;

    document.getElementById("tr-bar").style.width = pct + "%";

    if (trState.open) {
        document.getElementById("tr-main").textContent = "Running...";
        document.getElementById("tr-main").style.color = "var(--primary)";
        document.getElementById("tr-sub").textContent = trState.current + " / " + trState.total + " (" + trState.failed + " failed)";
    } else if (done) {
        if (trState.failed > 0) {
            document.getElementById("tr-main").textContent = "Failed";
            document.getElementById("tr-main").style.color = "var(--error)";
            document.getElementById("tr-bar").style.background = "var(--error)";
        } else {
            document.getElementById("tr-main").textContent = "Passed";
            document.getElementById("tr-main").style.color = "var(--success)";
            document.getElementById("tr-bar").style.background = "var(--success)";
        }
        document.getElementById("tr-sub").textContent = trState.passed + " passed, " + trState.failed + " failed.";
    }
}

// ==================== Page Navigation ====================
var pageTitles = {
    dashboard: "dashboard",
    http: "http",
    projects: "projects",
    settings: "settings",
    logs: "logs"
};

document.querySelectorAll(".ni").forEach(function (x) {
    x.addEventListener("click", function (e) {
        e.preventDefault();
        document.querySelectorAll(".ni").forEach(function (y) { y.classList.remove("on"); });
        x.classList.add("on");

        var page = x.getAttribute("data-page");
        document.querySelectorAll(".page").forEach(function (p) { p.classList.remove("active"); });
        var target = document.getElementById("page-" + page);
        if (target) target.classList.add("active");

        document.getElementById("page-title").textContent = t(page);
    });
});

// ==================== Theme Toggle ====================
function toggleTheme() { var r = document.documentElement; var isLight = r.classList.toggle("light"); localStorage.setItem("dext-theme", isLight ? "light" : "dark"); document.getElementById("theme-icon").textContent = isLight ? "light_mode" : "dark_mode"; }
(function () { var t = localStorage.getItem("dext-theme"); if (t == "light") { document.documentElement.classList.add("light"); document.getElementById("theme-icon").textContent = "light_mode"; } })();

// ==================== HTTP Client Functions ====================
function httpNew() {
    document.getElementById("http-editor").value = "### New Request\nGET https://api.example.com/resource\nContent-Type: application/json\n\n";
    httpParse();
}

function httpUpload(event) {
    var file = event.target.files[0];
    if (!file) return;
    var reader = new FileReader();
    reader.onload = function (e) {
        document.getElementById("http-editor").value = e.target.result;
        httpParse();
    };
    reader.readAsText(file);
    event.target.value = ""; // Reset for re-upload
}

function httpDownload() {
    var content = document.getElementById("http-editor").value;
    var blob = new Blob([content], { type: "text/plain" });
    var a = document.createElement("a");
    a.href = URL.createObjectURL(blob);
    a.download = "requests.http";
    a.click();
}

function httpParse() {
    var content = document.getElementById("http-editor").value;
    // Parse using backend API
    fetch("/api/http/parse", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ content: content })
    })
        .then(function (r) { return r.json(); })
        .then(function (data) {
            httpParsedRequests = data.requests || [];
            httpParsedVars = data.variables || [];
            httpResults = {}; // Reset results on parse
            httpRenderChips();
            httpRenderVars();
        })
        .catch(function (err) {
            console.error("Parse error:", err);
            // Fallback: simple local parse
            httpLocalParse(content);
        });
}

function httpLocalParse(content) {
    // Fallback simple parser
    var lines = content.split("\n");
    var requests = [];
    var currentName = "";
    var methods = ["GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS"];

    for (var i = 0; i < lines.length; i++) {
        var line = lines[i].trim();
        if (line.startsWith("###")) {
            currentName = line.substring(3).trim();
        } else {
            for (var j = 0; j < methods.length; j++) {
                if (line.startsWith(methods[j] + " ")) {
                    requests.push({
                        name: currentName || "Request " + (requests.length + 1),
                        method: methods[j],
                        url: line.substring(methods[j].length + 1).trim(),
                        lineNumber: i + 1
                    });
                    currentName = "";
                    break;
                }
            }
        }
    }
    httpParsedRequests = requests;
    httpRenderChips();
}

// HTTP Client State
var httpParsedRequests = [];
var httpParsedVars = [];
var httpSelectedIndex = 0;
var httpHistoryList = [];
var httpResults = {}; // Index -> Result Map

function httpRenderChips() {
    var container = document.getElementById("http-requests");
    if (httpParsedRequests.length === 0) {
        container.innerHTML = "<span style='color:var(--on-surface-v);font-size:11px'>No requests detected</span>";
        return;
    }
    var html = "";
    for (var i = 0; i < httpParsedRequests.length; i++) {
        var r = httpParsedRequests[i];
        var active = i === httpSelectedIndex ? " on" : "";
        var methodClass = r.method.toLowerCase();

        // Status dot if we have a result
        var dot = "";
        if (httpResults[i]) {
            var stClass = getStatusClass(httpResults[i].statusCode);
            dot = `<span class="http-dot ${stClass}"></span>`;
        }

        html += `<div class="http-chip${active}" onclick="httpSelectRequest(${i})">
                    ${dot}
                    <span class="method ${methodClass}">${r.method}</span>
                    ${r.name || "Unnamed"}
                 </div>`;
    }
    container.innerHTML = html;
}

function httpRenderVars() {
    var container = document.getElementById("http-vars-list");
    if (!httpParsedVars || httpParsedVars.length === 0) {
        container.innerHTML = "<span style='color:var(--outline);font-size:11px'>No variables defined</span>";
        return;
    }
    var html = "";
    for (var i = 0; i < httpParsedVars.length; i++) {
        var v = httpParsedVars[i];
        html += '<div class="http-var">';
        html += '<span class="http-var-name">@' + v.name + '</span>';
        html += '<span class="http-var-value">' + (v.value || v.envVarName || '') + '</span>';
        html += '</div>';
    }
    container.innerHTML = html;
}

function httpSelectRequest(index) {
    httpSelectedIndex = index;
    httpRenderChips();

    // Show previous result if exists
    if (httpResults[index]) {
        renderHttpResult(httpResults[index]);
    } else {
        // Clear UI
        document.getElementById("http-status").textContent = "--";
        document.getElementById("http-status").className = "http-status";
        document.getElementById("http-time").textContent = "--";
        document.getElementById("http-body").textContent = "";
        document.getElementById("http-headers").textContent = "";
    }
}

function httpTab(tab) {
    document.querySelectorAll(".http-tab").forEach(function (t) { t.classList.remove("on"); });
    document.querySelector('.http-tab[data-tab="' + tab + '"]').classList.add("on");
    document.getElementById("http-body").style.display = tab === "body" ? "block" : "none";
    document.getElementById("http-headers").style.display = tab === "headers" ? "block" : "none";
}

async function httpExecute() {
    if (httpParsedRequests.length === 0) {
        httpParse();
        await new Promise(function (r) { setTimeout(r, 300); });
    }

    if (httpParsedRequests.length === 0) {
        alert("No requests to execute. Check your .http syntax.");
        return;
    }

    var request = httpParsedRequests[httpSelectedIndex];
    document.getElementById("http-status").textContent = "Loading...";
    document.getElementById("http-status").className = "http-status";
    document.getElementById("http-time").textContent = "--";
    document.getElementById("http-body").textContent = "Executing request...";
    document.getElementById("http-headers").textContent = "";

    try {
        var response = await fetch("/api/http/execute", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({
                content: document.getElementById("http-editor").value,
                requestIndex: httpSelectedIndex
            })
        });
        var result = await response.json();

        // Save result
        httpResults[httpSelectedIndex] = result;
        renderHttpResult(result);
        httpRenderChips();

    } catch (err) {
        document.getElementById("http-status").textContent = "Error";
        document.getElementById("http-status").className = "http-status err";
        document.getElementById("http-body").textContent = err.message || "Request failed";
    }
}

function renderHttpResult(result) {
    // Update UI
    var statusClass = getStatusClass(result.statusCode);
    document.getElementById("http-status").textContent = result.statusCode + " " + result.statusText;
    document.getElementById("http-status").className = "http-status " + statusClass;
    document.getElementById("http-time").textContent = result.durationMs + "ms";

    // Format body
    var body = result.responseBody || "";
    try {
        var parsed = JSON.parse(body);
        body = JSON.stringify(parsed, null, 2);
        body = syntaxHighlight(body);
    } catch (e) { /* Not JSON */ }
    document.getElementById("http-body").innerHTML = body;

    // Headers
    var headers = "";
    if (result.responseHeaders) {
        for (var key in result.responseHeaders) {
            headers += key + ": " + result.responseHeaders[key] + "\n";
        }
    }
    document.getElementById("http-headers").textContent = headers || "No headers";
}

async function httpExecuteAll() {
    for (var i = 0; i < httpParsedRequests.length; i++) {
        httpSelectedIndex = i;
        httpRenderChips();
        await httpExecute();
    }
}

function getStatusClass(code) {
    if (code >= 200 && code < 300) return "s2";
    if (code >= 300 && code < 400) return "s3";
    if (code >= 400 && code < 500) return "s4";
    if (code >= 500) return "s5";
    return "err";
}

function syntaxHighlight(json) {
    json = json.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    return json.replace(/("(\\u[a-zA-Z0-9]{4}|\\[^u]|[^\\"])*"(\s*:)?|\b(true|false|null)\b|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?)/g, function (match) {
        var cls = 'json-number';
        if (/^"/.test(match)) {
            if (/:$/.test(match)) {
                cls = 'json-key';
            } else {
                cls = 'json-string';
            }
        } else if (/true|false/.test(match)) {
            cls = 'json-boolean';
        } else if (/null/.test(match)) {
            cls = 'json-null';
        }
        return '<span class="' + cls + '">' + match + '</span>';
    });
}

// Editor change listener
document.getElementById("http-editor").addEventListener("input", function () {
    clearTimeout(window.httpParseTimeout);
    window.httpParseTimeout = setTimeout(httpParse, 500);
});

function httpToggleHistory() {
    var p = document.getElementById("http-history");
    if (p.style.display == "none") {
        p.style.display = "block";
        httpLoadHistory();
    } else {
        p.style.display = "none";
    }
}

function httpLoadHistory() {
    fetch("/api/http/history")
        .then(function (r) { return r.json() })
        .then(function (d) {
            httpHistoryList = d;
            var c = document.getElementById("http-history-list");
            if (d.length == 0) { c.innerHTML = "<div style='padding:20px;color:#666'>No history</div>"; return; }
            var h = "";
            for (var i = 0; i < d.length; i++) {
                var item = d[i];
                var date = new Date(item.timestamp).toLocaleString();
                var stClass = getStatusClass(item.statusCode);
                h += `<div class="http-hist-item" onclick="httpRestoreHistory(${i})">
                        <div class="http-hist-top">
                            <span class="method ${item.method.toLowerCase()}">${item.method}</span>
                            <span class="url">${item.url}</span>
                        </div>
                        <div class="http-hist-bot">
                            <span class="status ${stClass}">${item.statusCode}</span>
                            <span class="time">${item.durationMs}ms</span>
                            <span class="date">${date}</span>
                        </div>
                      </div>`;
            }
            c.innerHTML = h;
        });
}

function httpRestoreHistory(idx) {
    if (httpHistoryList[idx]) {
        var item = httpHistoryList[idx];
        document.getElementById("http-editor").value = item.content;
        document.getElementById("http-history").style.display = "none";
        httpParse();
    }
}

// ==================== Test Tab Logic ====================
var testSuite = {};
var currentTestDetail = null;

// Debug logging for parser
function updateTestState(msg) {
    if (msg.indexOf("Run Started") >= 0) {
        console.log("Resetting testSuite (Run Started detected)");
        testSuite = {};
        renderTestTree();
        return;
    }

    var fixtureName = "", testName = "";
    var status = "";

    if (msg.indexOf("Started Test:") >= 0) {
        var parts = msg.split(":")[1].trim().split(".");
        if (parts.length >= 2) { fixtureName = parts[0]; testName = parts.slice(1).join("."); status = "running"; }
    } else if (msg.indexOf("Passed Test:") >= 0) {
        var parts = msg.split(":")[1].trim().split(" ")[0].split("."); // Split space to handle potential extra text
        if (parts.length >= 2) { fixtureName = parts[0]; testName = parts.slice(1).join("."); status = "passed"; }
    } else if (msg.indexOf("Failed Test:") >= 0) {
        var parts = msg.split(":")[1].trim().split(" ")[0].split(".");
        if (parts.length >= 2) { fixtureName = parts[0]; testName = parts.slice(1).join("."); status = "failed"; }
    } else if (msg.indexOf("Skipped Test:") >= 0) {
        var parts = msg.split(":")[1].trim().split(" ")[0].split(".");
        if (parts.length >= 2) { fixtureName = parts[0]; testName = parts.slice(1).join("."); status = "skipped"; }
    }

    if (fixtureName && testName) {
        // console.log("Parsed:", fixtureName, testName, status);
        if (!testSuite[fixtureName]) testSuite[fixtureName] = { status: "none", tests: {} };
        if (!testSuite[fixtureName].tests[testName]) testSuite[fixtureName].tests[testName] = { status: "none", logs: "" };

        testSuite[fixtureName].tests[testName].status = status;
        testSuite[fixtureName].tests[testName].logs += msg + "\n";

        if (status == "failed") testSuite[fixtureName].status = "failed";

        renderTestTree();

        if (currentTestDetail && currentTestDetail.fixture == fixtureName && currentTestDetail.test == testName) {
            showTestDetail(fixtureName, testName);
        }
    } else {
        if (msg.indexOf("Test:") >= 0 && msg.indexOf("Run Completed") < 0) {
            console.warn("Failed to parse test msg:", msg);
        }
    }
}

function renderTestTree() {
    var c = document.getElementById("test-tree");
    if (!c) return; // Tab might not be loaded yet

    var h = "";
    var fKeys = Object.keys(testSuite).sort();

    if (fKeys.length === 0) return;

    for (var i = 0; i < fKeys.length; i++) {
        var fName = fKeys[i];
        var fixture = testSuite[fName];
        var fStatusClass = fixture.status == "failed" ? "st-fail" : "st-none";

        h += `<div class="test-tree-item test-fixture"><i class="test-status-icon ${fStatusClass}">folder</i>${fName}</div>`;

        var tKeys = Object.keys(fixture.tests).sort();
        for (var j = 0; j < tKeys.length; j++) {
            var tName = tKeys[j];
            var test = fixture.tests[tName];
            var icon = "radio_button_unchecked";
            var stClass = "st-none";

            if (test.status == "passed") { icon = "check_circle"; stClass = "st-pass"; }
            else if (test.status == "failed") { icon = "cancel"; stClass = "st-fail"; }
            else if (test.status == "skipped") { icon = "remove_circle"; stClass = "st-skip"; }
            else if (test.status == "running") { icon = "hourglass_empty"; stClass = "st-none"; }

            var selected = (currentTestDetail && currentTestDetail.fixture == fName && currentTestDetail.test == tName) ? "selected" : "";

            h += `<div class="test-tree-item test-method ${selected}" onclick="showTestDetail('${fName}', '${tName}')">
                    <i class="test-status-icon ${stClass}">${icon}</i>${tName}
                  </div>`;
        }
    }
    c.innerHTML = h;

    // Update Summaries
    var tot = document.getElementById("tests-total");
    if (tot) tot.textContent = trState.total + " Tests";
    var pass = document.getElementById("tests-passed");
    if (pass) pass.textContent = trState.passed + " Pass";
    var fail = document.getElementById("tests-failed");
    if (fail) fail.textContent = trState.failed + " Fail";
}

function showTestDetail(fixtureName, testName) {
    console.log("showTestDetail clicked:", fixtureName, testName);
    currentTestDetail = { fixture: fixtureName, test: testName };

    var fixtureDocs = testSuite[fixtureName];
    if (!fixtureDocs) {
        console.error("Fixture not found in testSuite:", fixtureName, Object.keys(testSuite));
        return;
    }

    var t = fixtureDocs.tests[testName];
    if (!t) {
        console.error("Test not found in fixture:", testName, Object.keys(fixtureDocs.tests));
        return;
    }

    document.getElementById("test-detail-title").textContent = fixtureName + "." + testName;
    document.getElementById("test-detail-logs").textContent = t.logs || "No logs captured.";
    renderTestTree();
}

async function testsRunAll() {
    // Auto-select first test project if none selected
    if (!window.currentTestProjectPath) {
        var scan = window.lastWorkspaceScan || {};
        if (scan.tests && scan.tests.length > 0) {
            var first = scan.tests[0];
            var p = (typeof first === 'string') ? "" : (first.path || "");
            if (p) {
                await discoverTestProject(p);
            }
        }
    }

    if (!window.currentTestProjectPath) {
        alert("No project selected. Please go to Projects tab and click on a test project.");
        return;
    }

    // Show running state
    var tree = document.getElementById("test-tree");
    tree.innerHTML = '<div style="padding:20px"><span class="loader"></span> Running Tests...<br><span style="font-size:12px;color:var(--secondary)">This may take a moment.</span></div>';

    var projectPath = window.currentTestProjectPath;
    // Ensure dropdown is in sync
    var sel = document.getElementById("tests-pj-select");
    if (sel && projectPath) sel.value = projectPath;

    try {
        var r = await fetch("/api/tests/run", {
            method: "POST",
            body: JSON.stringify({ project: window.currentTestProjectPath })
        });

        if (r.ok) {
            var results = await r.json();
            // Re-render tree with results
            if (window.lastDiscoveryData) {
                renderTestDiscovery(window.lastDiscoveryData, results);
            }
        } else {
            tree.innerHTML = '<div style="padding:20px;color:var(--error)">Run Failed: ' + (await r.text()) + '</div>';
        }
    } catch (e) {
        tree.innerHTML = '<div style="padding:20px;color:var(--error)">Error: ' + e.message + '</div>';
    }
}

function testsRunFailed() {
    alert("Not implemented");
}

function testsFilter() {
    var txt = document.getElementById("test-filter").value.toLowerCase();
    var items = document.querySelectorAll(".test-tree-item");
    for (var i = 0; i < items.length; i++) {
        var t = items[i].textContent.toLowerCase();
        items[i].style.display = t.indexOf(txt) >= 0 ? "flex" : "none";
    }
}

// ==================== Projects / Workspace Logic ====================
// ==================== Projects / Workspace Logic ====================
var activeWorkspace = "";
var feCurrentPath = "C:\\";

// Persist active workspace
if (localStorage.getItem("activeWorkspace")) {
    activeWorkspace = localStorage.getItem("activeWorkspace");
    updateWorkspaceUI(activeWorkspace);
    refreshWorkspace();
}

function openWorkspaceModal() {
    var el = document.getElementById("wk-modal");
    el.style.display = "flex"; // flex for centering
    loadFolder(activeWorkspace || "C:\\");
}

function closeWorkspaceModal() {
    document.getElementById("wk-modal").style.display = "none";
}

async function loadFolder(path) {
    try {
        if (!path) path = "C:\\";
        path = path.replace(/\\\\/g, "\\");
        if (path.endsWith("\\") && path.length > 3) path = path.substring(0, path.length - 1);

        // Handle Drive Letter Only (e.g. "C")
        if (path.length === 1 && path.match(/[a-z]/i)) path = path + ":\\";

        feCurrentPath = path;
        document.getElementById("fe-path").value = path;

        var list = [];
        try {
            var r = await fetch("/api/fs/list?path=" + encodeURIComponent(path));
            if (r.ok) list = await r.json();
            else list = [{ name: "Error loading folder", type: "file" }];
        } catch (e) {
            // Mock Fallback
            if (path == "C:\\") list = [{ name: "dev", type: "dir" }, { name: "Users", type: "dir" }];
            else if (path == "C:\\dev") list = [{ name: "Dext", type: "dir" }];
            else list = [{ name: "MockProject.dproj", type: "file" }];
        }

        var h = "";
        // Back item if not root
        if (path.length > 3) {
            h += `<div class="fe-item" onclick="navUp()"><i class="material-symbols-outlined" style="margin-right:10px;color:var(--primary)">arrow_back</i>..</div>`;
        }

        for (var i = 0; i < list.length; i++) {
            var it = list[i];
            var icon = it.type == "dir" ? "folder" : "description";
            var iconColor = it.type == "dir" ? "var(--primary)" : "var(--on-surface-v)";
            var nextPath = path.endsWith("\\") ? path + it.name : path + "\\" + it.name;
            // Escape backslashes for JS string
            var jsPath = nextPath.replace(/\\/g, "\\\\");

            var action = it.type == "dir" ? `onclick="loadFolder('${jsPath}')"` : "";

            h += `<div class="fe-item" ${action}>
                    <i class="material-symbols-outlined" style="margin-right:10px;color:${iconColor}">${icon}</i>${it.name}
                  </div>`;
        }
        document.getElementById("fe-list").innerHTML = h;
    } catch (e) { console.error(e); }
}

function navUp() {
    if (feCurrentPath.length <= 3) return; // At root
    var p = feCurrentPath.lastIndexOf("\\");
    if (p <= 2) loadFolder("C:\\");
    else loadFolder(feCurrentPath.substring(0, p));
}

function selectCurrentFolder() {
    updateWorkspaceUI(feCurrentPath);
    closeWorkspaceModal();
    refreshWorkspace();
}

function updateWorkspaceUI(path) {
    activeWorkspace = path;
    localStorage.setItem("activeWorkspace", activeWorkspace);

    // Header
    var headerEl = document.getElementById("header-wk-path");
    if (headerEl) {
        headerEl.textContent = path;
        headerEl.title = path;
    }

    // Card (if exists on page)
    var cardEl = document.getElementById("wk-path-card");
    if (cardEl) cardEl.textContent = path;
}

// Drag and Drop (Simple Paste of Path)
var modalContent = document.querySelector(".modal-content");
if (modalContent) {
    var overlay = document.getElementById("fe-drag-overlay");

    document.addEventListener("dragover", function (e) {
        e.preventDefault();
        if (document.getElementById("wk-modal").style.display === "flex") {
            overlay.style.display = "flex";
        }
    });

    overlay.addEventListener("dragleave", function (e) {
        overlay.style.display = "none";
    });

    overlay.addEventListener("drop", function (e) {
        e.preventDefault();
        overlay.style.display = "none";

        // Try to get file/folder
        if (e.dataTransfer.items) {
            // We can't get full path due to security, BUT
            // if user drags multiple items or files, we might just use the Name to filter?
            // Actually, standard drag drop is useless for Full Path.
            // However, if we support pasting TEXT (which some OS do when dragging folder to text editor),
            // we can check dataTransfer.getData("text").
            var text = e.dataTransfer.getData("text/plain");
            if (text && text.indexOf("\\") > 0) {
                loadFolder(text.trim());
            } else {
                // Fallback: Show message
                alert("Browser security prevents reading the full path of dropped folders.\nPlease paste the path into the input box manually.");
            }
        }
    });
}

async function refreshWorkspace() {
    if (!activeWorkspace) return;

    // Reset indicators
    var ids = ["scan-dproj", "scan-tests", "scan-http", "scan-docs"];
    ids.forEach(id => document.getElementById(id).innerHTML = '<span class="st-w">Scanning...</span>');

    try {
        var r = await fetch("/api/workspace/scan?path=" + encodeURIComponent(activeWorkspace));
        var res = await r.json();
        window.lastWorkspaceScan = res; // Save for fallback use

        // Render Results
        renderScanList("scan-dproj", res.projects, "folder");
        renderScanList("scan-tests", res.tests, "science");
        renderScanList("scan-http", res.httpFiles, "http");
        renderScanList("scan-docs", res.docs, "description");

        // Populate Dropdowns
        populateSelect("header-pj-select", res.projects, "activeProject");
        populateSelect("tests-pj-select", res.tests, "activeTestProject");
        populateSelect("http-file-select", res.httpFiles, "activeHttpFile");

    } catch (e) {
        ids.forEach(id => document.getElementById(id).innerHTML = '<span class="st-fail">Error scanning</span>');
    }
}

function populateSelect(id, items, storageKey) {
    var sel = document.getElementById(id);
    if (!sel) return;
    var current = sel.value;
    var saved = localStorage.getItem(storageKey);

    var h = `<option value="">Select...</option>`;
    if (items) {
        items.forEach(it => {
            var name = (typeof it === 'string') ? it : (it.name || "Unknown");
            var path = (typeof it === 'string') ? name : (it.path || name);
            // If it's just name and we are in workspace, maybe it's relative?
            // Usually res.projects are just names. res.tests are objects with paths.
            // res.httpFiles are just names (filenames in root/subdirs?) 
            // Actually Dext sidecar scan usually returns relative paths or full paths.
            h += `<option value="${path}">${name}</option>`;
        });
    }
    sel.innerHTML = h;

    // Restore selection
    if (saved) {
        // Find if saved exists in items
        var exists = items && items.some(it => {
            var val = (typeof it === 'string') ? it : (it.path || it.name);
            return val === saved;
        });
        if (exists) {
            sel.value = saved;
            // Trigger change logic if needed
            if (id === "tests-pj-select") discoverTestProject(saved);
            else if (id === "http-file-select") httpLoadFile(saved);
        } else if (items && items.length > 0) {
            // Select first by default if nothing saved
            var first = items[0];
            var firstVal = (typeof first === 'string') ? first : (first.path || first.name);
            sel.value = firstVal;
            if (id === "tests-pj-select") discoverTestProject(firstVal);
            else if (id === "http-file-select") httpLoadFile(firstVal);
        }
    } else if (items && items.length > 0) {
        var first = items[0];
        var firstVal = (typeof first === 'string') ? first : (first.path || first.name);
        sel.value = firstVal;
        if (id === "tests-pj-select") discoverTestProject(firstVal);
        else if (id === "http-file-select") httpLoadFile(firstVal);
    }
}

function selectActiveProject(path, skipSave) {
    if (!skipSave) localStorage.setItem("activeProject", path);
    var sel = document.getElementById("header-pj-select");
    if (sel) sel.value = path;
}

async function httpLoadFile(filename) {
    if (!filename) return;
    localStorage.setItem("activeHttpFile", filename);

    // Check if filename is path or just name
    var path = filename;
    if (activeWorkspace && !path.includes(":\\") && !path.startsWith("/")) {
        path = activeWorkspace + "\\" + filename;
    }

    try {
        var r = await fetch("/api/fs/read?path=" + encodeURIComponent(path));
        if (r.ok) {
            var text = await r.text();
            document.getElementById("http-editor").value = text;
            httpParse();
        }
    } catch (e) { console.error("Error loading http file:", e); }
}


function renderScanList(elId, items, icon) {
    var el = document.getElementById(elId);
    if (!items || items.length == 0) { el.innerHTML = '<span class="st-none">None found</span>'; return; }

    var h = "";
    var limit = 8;
    for (var i = 0; i < Math.min(items.length, limit); i++) {
        var it = items[i];
        var name = (typeof it === 'string') ? it : (it.name || "Unknown");
        var path = (typeof it === 'string') ? "" : (it.path || "");
        var action = "";

        // If it's a test project, allowing clicking to load details
        if (elId === 'scan-tests' && path) {
            // Escape backslashes and quotes for safe inclusion in onclick handler
            var safePath = path.replace(/\\/g, "\\\\").replace(/'/g, "\\'");
            action = `style="cursor:pointer;color:var(--primary)" onclick="discoverTestProject('${safePath}')" title="${path}"`;
        }

        h += `<div class="pj-i" ${action}>
                <span class="ms material-symbols-outlined" style="font-size:16px;margin-right:5px">${icon}</span>${name}
              </div>`;
    }
    if (items.length > limit) h += `<div class="pj-i" style="font-style:italic">+${items.length - limit} more...</div>`;
    el.innerHTML = h;
}

async function discoverTestProject(path) {
    console.log("Discovering tests for:", path);
    if (!path) return;

    window.currentTestProjectPath = path;

    // Switch to Tests tab
    var tab = document.querySelector(".nav-item[onclick*='Tests']");
    if (tab) tab.click();

    document.getElementById("test-tree").innerHTML = '<div style="padding:20px"><span class="loader"></span> Loading test metrics...</div>';

    try {
        var r = await fetch("/api/tests/discover?project=" + encodeURIComponent(path));
        if (r.ok) {
            window.lastDiscoveryData = await r.json();
            renderTestDiscovery(window.lastDiscoveryData);
        } else {
            document.getElementById("test-tree").innerHTML = '<div style="padding:20px;color:var(--error)">Failed to load tests: ' + (await r.text()) + '</div>';
        }
    } catch (e) {
        document.getElementById("test-tree").innerHTML = '<div style="padding:20px;color:var(--error)">Error: ' + e.message + '</div>';
    }
}

function renderTestDiscovery(data, results) {
    var h = `<div style="padding:10px;font-weight:bold;border-bottom:1px solid var(--outline-v);display:flex;justify-content:space-between;align-items:center">
                <div style="overflow:hidden;text-overflow:ellipsis">
                    <span>${data.project}</span><br>
                    <span style="font-size:10px;color:var(--on-surface-v)">${data.path}</span>
                </div>
                <button class="btn btn-sm" onclick="testsRunAll()">Run All</button>
             </div>`;

    if (!data.fixtures || data.fixtures.length == 0) {
        h += '<div style="padding:20px">No [TestFixture] found in this project.</div>';
    } else {
        data.fixtures.forEach(f => {
            // Ensure fixture exists in testSuite to avoid crash on click
            if (!testSuite[f.name]) testSuite[f.name] = { status: "none", tests: {} };

            h += `<div class="test-tree-item" style="font-weight:600;background:var(--surface-h)">
                    <span class="material-symbols-outlined" style="font-size:16px;margin-right:5px;color:var(--secondary)">folder</span>${f.name}
                  </div>`;
            f.tests.forEach(t => {
                // Ensure test exists in testSuite
                if (!testSuite[f.name].tests[t.name]) testSuite[f.name].tests[t.name] = { status: "none", logs: "" };

                var statusIcon = "science";
                var statusColor = "var(--on-surface-v)";
                var title = "";

                // Map results if available
                if (results) {
                    // Find result for this test
                    // Simplified matching by name for now
                    var res = findTestResult(results, f.name, t.name);
                    if (res) {
                        if (res.status == "Passed") { statusIcon = "check_circle"; statusColor = "var(--success)"; }
                        else if (res.status == "Failed") { statusIcon = "cancel"; statusColor = "var(--error)"; title = res.message; }
                        else { statusIcon = "help"; statusColor = "var(--warning)"; }
                    }
                }

                h += `<div class="test-tree-item" style="padding-left:35px;font-size:13px;cursor:pointer" onclick="showTestDetail('${f.name}', '${t.name}')" title="${title}">
                        <span class="material-symbols-outlined" style="font-size:14px;margin-right:5px;color:${statusColor}">${statusIcon}</span>${t.name}
                      </div>`;
            });
        });
    }

    document.getElementById("test-tree").innerHTML = h;

    // Update summary if results provided
    if (results) {
        // ... simple stats update can go here or in testsRunAll
    }
}

function findTestResult(results, fixtureName, testName) {
    if (!results || !results.results) return null;
    for (var i = 0; i < results.results.length; i++) {
        var r = results.results[i];
        if (r.fixture == fixtureName && r.test == testName) return r;
    }
    return null;
}

// ==================== Init ====================
load();
// hub(); // SignalR Disabled - forcing SSE
connectSSE();
