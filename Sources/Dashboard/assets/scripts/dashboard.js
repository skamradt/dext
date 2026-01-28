/* --- Logic --- */
const els = {
    list: document.getElementById("test-list"),
    chart: document.getElementById("chart"),
    percent: document.getElementById("pass-percent"),
    status: document.getElementById("status-text"),
    conn: document.getElementById("connection-status"),
    timeline: document.getElementById("timeline-container"),
    tooltip: document.getElementById("tooltip")
};
let stats = { total: 0, passed: 0, failed: 0, skipped: 0 };

function switchView(name) {
    document.querySelectorAll(".view").forEach(el => el.classList.remove("active"));
    document.getElementById("view-" + name).classList.add("active");
    document.querySelectorAll(".nav-item").forEach(el => el.classList.remove("active"));
    event.currentTarget.classList.add("active");
    if (name === "timeline") loadHistory();
}

function updateChart() {
    const total = stats.passed + stats.failed + stats.skipped;
    if (total === 0) return;
    const pPass = (stats.passed / total) * 100;
    const pFail = (stats.failed / total) * 100;
    const pSkip = (stats.skipped / total) * 100;
    stats.total = total;

    /* Conic gradient visual */
    els.chart.style.background = `conic-gradient(var(--pass) 0% ${pPass}%, var(--fail) ${pPass}% ${pPass + pFail}%, var(--skip) ${pPass + pFail}% 100%)`;
    els.percent.innerText = Math.round(pPass) + "%";
    els.status.innerText = `Running... ${stats.passed} Passed · ${stats.failed} Failed`;
    document.title = `(${stats.failed} Fails) Dext Dashboard`;
}

/* SSE Handling */
const source = new EventSource("/events");
source.onopen = () => { els.conn.innerText = "Live"; els.conn.className = "status-badge status-running"; };
source.onerror = () => { els.conn.innerText = "Disconnected"; els.conn.className = "status-badge"; };

source.addEventListener("run_start", (e) => {
    els.list.innerHTML = ""; stats = { total: 0, passed: 0, failed: 0, skipped: 0 }; updateChart();
});

source.addEventListener("test_start", (e) => {
    const msg = JSON.parse(e.data);
    const id = ("t-" + msg.fixture + "-" + msg.test).replace(/[^a-zA-Z0-9-_]/g, "_");
    const html = `
      <div class="test-card" id="${id}">
         <div class="test-header">
             <div class="test-icon running">RUN</div>
             <div class="test-info">
                 <div class="test-name">${msg.test}</div>
                 <div class="test-fixture">${msg.fixture}</div>
             </div>
             <div class="test-meta">Running...</div>
         </div>
      </div>`;
    els.list.insertAdjacentHTML("afterbegin", html);
});

source.addEventListener("test_complete", (e) => {
    const msg = JSON.parse(e.data);
    const id = ("t-" + msg.fixture + "-" + msg.test).replace(/[^a-zA-Z0-9-_]/g, "_");
    const el = document.getElementById(id);

    // Normalize status to lowercase to handle "Passed" vs "passed"
    const statusLower = (msg.status || "").toLowerCase();

    if (el) {
        let icon = "FAIL";
        if (statusLower === "passed") icon = "PASS";
        else if (statusLower === "skipped") icon = "SKIP";

        // Remove old classes and set new status class
        el.classList.remove("running", "passed", "failed", "skipped");
        el.classList.add("test-card", statusLower);

        el.querySelector(".test-icon").className = "test-icon"; /* remove running */
        el.querySelector(".test-icon").innerText = icon;
        el.querySelector(".test-meta").innerText = msg.duration + "ms";

        // Use statusLower for stats counting
        if (stats[statusLower] !== undefined) stats[statusLower]++;

        updateChart();

        if (statusLower === "failed") {
            const detailsId = id + "-details";
            el.querySelector(".test-header").setAttribute("onclick", `document.getElementById("${detailsId}").classList.toggle("open")`);
            const detailsHtml = `<div class="test-details" id="${detailsId}"><div class="error-msg">${msg.error || "Unknown Error"}</div></div>`;
            el.insertAdjacentHTML("beforeend", detailsHtml);
        }
    } else {
        console.warn("Test card not found for update:", id);
    }
});

source.addEventListener("run_complete", (e) => {
    els.status.innerText = "Run Complete";
    loadHistory(); /* refresh history */
});

/* History Logic (Native SVG Chart code) */
async function loadHistory() {
    const res = await fetch("/api/history");
    const data = await res.json();
    drawChart(data);
    drawRunList(data);
}

function drawChart(data) {
    if (!data || data.length === 0) { els.timeline.innerHTML = "No history available"; return; }
    const padding = 40; const w = els.timeline.clientWidth; const h = els.timeline.clientHeight || 400;
    const maxVal = Math.max(...data.map(d => d.total)) * 1.1 || 10;

    const scaleX = (i) => padding + (i / (data.length - 1 || 1)) * (w - 2 * padding);
    const scaleY = (v) => h - padding - (v / maxVal) * (h - 2 * padding);

    /* Generate paths */
    const passPoints = data.map((d, i) => `${scaleX(i)},${scaleY(d.passed)}`).join(" ");
    const failPoints = data.map((d, i) => `${scaleX(i)},${scaleY(d.failed)}`).join(" ");

    let svg = `<svg viewBox="0 0 ${w} ${h}">`;

    /* Axes */
    svg += `<line x1="${padding}" y1="${h - padding}" x2="${w - padding}" y2="${h - padding}" class="axis-line"/>`;

    if (data.length > 1) {
        /* Pass Line (Green) */
        svg += `<polyline points="${passPoints}" class="line-path" stroke="#4caf50" />`;
        /* Fail Line (Red) */
        svg += `<polyline points="${failPoints}" class="line-path" stroke="#f44336" />`;

        /* Dots */
        data.forEach((d, i) => {
            svg += `<circle cx="${scaleX(i)}" cy="${scaleY(d.passed)}" class="dot" fill="#4caf50" onmouseover="showTooltip(evt, 'Passed: ${d.passed}\nTotal: ${d.total}')" onmouseout="hideTooltip()" />`;
            if (d.failed > 0) svg += `<circle cx="${scaleX(i)}" cy="${scaleY(d.failed)}" class="dot" fill="#f44336" onmouseover="showTooltip(evt, 'Failed: ${d.failed}')" onmouseout="hideTooltip()" />`;
        });
    }
    svg += `</svg>`;
    els.timeline.innerHTML = svg;
}

function drawRunList(data) {
    const container = document.getElementById("run-list");
    if (!container) return;
    container.innerHTML = "";
    if (!data || data.length === 0) { container.innerHTML = "<div style='padding:20px;color:#999'>No history yet</div>"; return; }
    /* Clone and reverse to show newest first */
    const list = [...data].reverse();
    list.forEach(run => {
        const date = new Date(run.date).toLocaleString();
        const html = `
             <div class="run-item">
                 <div class="run-meta">
                     <div class="run-date">${date}</div>
                     <div class="run-duration">Duration: ${run.duration}ms</div>
                 </div>
                 <div class="run-stats">
                     <span class="stat-pill pass">PASS: ${run.passed}</span>
                     ${run.failed > 0 ? `<span class="stat-pill fail">FAIL: ${run.failed}</span>` : ""}
                     ${run.skipped > 0 ? `<span class="stat-pill skip">SKIP: ${run.skipped}</span>` : ""}
                 </div>
             </div>`;
        container.insertAdjacentHTML("beforeend", html);
    });
}

function showTooltip(evt, text) {
    els.tooltip.innerText = text;
    els.tooltip.style.left = (evt.clientX + 10) + "px";
    els.tooltip.style.top = (evt.clientY - 30) + "px";
    els.tooltip.style.opacity = 1;
}
function hideTooltip() { els.tooltip.style.opacity = 0; }
