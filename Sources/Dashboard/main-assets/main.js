var cfg = null;
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
document.querySelectorAll(".ni").forEach(function (x) {
    x.addEventListener("click", function (e) {
        e.preventDefault(); document.querySelectorAll(".ni").forEach(function (y) {
            y.classList.remove("on");
        }); x.classList.add("on");
    });
});
function toggleTheme() { var r = document.documentElement; var isLight = r.classList.toggle("light"); localStorage.setItem("dext-theme", isLight ? "light" : "dark"); document.getElementById("theme-icon").textContent = isLight ? "light_mode" : "dark_mode"; }
(function () { var t = localStorage.getItem("dext-theme"); if (t == "light") { document.documentElement.classList.add("light"); document.getElementById("theme-icon").textContent = "light_mode"; } })();
load(); hub();
