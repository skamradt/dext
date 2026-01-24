var cfg = null;
var httpParsedRequests = [];
var httpParsedVars = [];
var httpSelectedIndex = 0;

// ==================== Dashboard Functions ====================
async function load() {
    try {
        var r = await Promise.all([fetch("/api/config").then(function (x) { return x.json() }), fetch("/api/projects").then(function (x) { return x.json() }),
        fetch("/api/test/summary").then(function (x) { return x.json() })]);
        cfg = r[0]; document.getElementById("env-count").textContent = cfg.environments ? cfg.environments.length : 0;
        document.getElementById("project-count").textContent = r[1].length || 0;
        var cov = r[2].available ? r[2].coverage : 0; document.getElementById("coverage-value").textContent = cov ? cov + "%" : "N/A";
        var ring = document.getElementById("cov-ring"); ring.style.strokeDasharray = Math.round(cov * 2.64) + " 264";
        envList(cfg.environments || []); act("Data loaded", "Just now", "Done");
    } catch (e) { console.error(e); }
}

function envList(a) {
    var l = document.getElementById("env-list"); if (!a.length) { l.innerHTML = "<li class=\"env-i\">No environments</li>"; return; }
    var h = ""; for (var i = 0; i < a.length; i++) { var e = a[i]; h += "<li class=\"env-i\"><div class=\"env-nm\">" + e.name + (e.isDefault ? " <span class=\"env-bg\">Default</span>" : "") + "</div><span class=\"env-pt\">" + e.path + "</span></li>"; } l.innerHTML = h;
}

function act(a, t, s) {
    var b = document.getElementById("at-body"); var c = s == "Done" ? "st-s" : s == "Active" ? "st-p" : "st-w";
    b.innerHTML = "<tr><td>" + a + "</td><td>" + t + "</td><td><span class=\"st " + c + "\">" + s + "</span></td></tr>" + b.innerHTML;
}

async function scan() { act("Scanning...", "Now", "Active"); try { await fetch("/api/env/scan", { method: "POST" }); load(); act("Scan done", "Just now", "Done"); } catch (e) { console.error(e); } }

function hub() {
    var c = new signalR.HubConnectionBuilder().withUrl("/hubs/dashboard").withAutomaticReconnect().build();
    c.on("ReceiveLog", function (l, m) {
        var p = document.getElementById("logs"); var t = new Date().toLocaleTimeString();
        var x = l.toLowerCase().indexOf("error") >= 0 ? "log-e" : l.toLowerCase().indexOf("warn") >= 0 ? "log-w" : "log-i";
        p.innerHTML += "<div class=\"log\"><span class=\"log-t\">[" + t + "]</span><span class=\"" + x + "\">" + m + "</span></div>"; p.scrollTop = p.scrollHeight;
    });
    c.start().catch(function (e) { console.error(e); });
}

// ==================== Page Navigation ====================
var pageTitles = {
    dashboard: "Dext Dashboard",
    http: "HTTP Client",
    projects: "Projects",
    settings: "Settings",
    logs: "Logs"
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

        document.getElementById("page-title").textContent = pageTitles[page] || "Dashboard";
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
        html += '<div class="http-chip' + active + '" onclick="httpSelectRequest(' + i + ')">';
        html += '<span class="method ' + methodClass + '">' + r.method + '</span>';
        html += (r.name || "Unnamed");
        html += '</div>';
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
    document.getElementById("http-body").textContent = "";
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

    } catch (err) {
        document.getElementById("http-status").textContent = "Error";
        document.getElementById("http-status").className = "http-status err";
        document.getElementById("http-body").textContent = err.message || "Request failed";
    }
}

async function httpExecuteAll() {
    for (var i = 0; i < httpParsedRequests.length; i++) {
        httpSelectedIndex = i;
        httpRenderChips();
        await httpExecute();
        await new Promise(function (r) { setTimeout(r, 500); });
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

// ==================== Init ====================
load();
hub();
