# 🚀 Dext Testing Live Dashboard

The **Live Dashboard** is a real-time web interface for monitoring your unit tests as they execute. It provides immediate visual feedback, detailed error reporting, and historical trend analysis.

![Dashboard Concept](https://via.placeholder.com/800x400?text=Dext+Dashboard+UI)

## ✨ Features

- **Real-Time Updates**: Watch tests pass or fail instantly via Server-Sent Events (SSE). No page refreshes required.
- **Modern UI**: Dark-themed, Allure-inspired design with a responsive layout.
- **Interactive Charts**: Dynamic donut chart showing pass/fail rates.
- **Historical Timeline**: Tracks your test quality over time.
  - Automatically saves execution summaries to `dext_test_history.json`.
  - Visualizes trends (Pass vs Fail) on an SVG graph.
- **Detailed Reporting**: Click on any failed test to see the full error message and stack trace.

## 🛠️ How it Works

The Dashboard runs as a lightweight HTTP server embedded within your test runner application.
1. When you start your tests, the runner starts an HTTP server (default port 9000).
2. You open your browser to `http://localhost:9000`.
3. As the runner executes tests, it pushes JSON events to the browser.
4. The dashboard updates the UI instantly.
5. Upon completion, the run statistics are appended to a local JSON file for historical analysis.

## 🚦 Quick Start Guide

### 1. Enable the Dashboard
In your test project's main file (`.dpr`), typically where you configure the runner:

```pascal
program MyTests;

uses
  Dext.Testing.Fluent, 
  MyTestUnits;

begin
  if TTest.Configure
      .Verbose
      // Enable the dashboard on port 9000
      .UseDashboard(9000) 
      // Optional: Wait for Enter after run so you can keep viewing results
      // (Default is True if UseDashboard is called)
      .RegisterFixture(TMyTests)
      .Run then
    ExitCode := 0
  else
    ExitCode := 1;
end.
```

### 2. Run Your Application
Compile and run your test project (Console Application).
You will see a message in the console:

```text
🚀 Dext Dashboard running at http://localhost:9000
Press ENTER to close dashboard and exit...
```

### 3. Open the Dashboard
Open your web browser and navigate to:
👉 **http://localhost:9000**

You will see the "Waiting for test run..." status if the tests haven't started (or if they finished extremely fast before you opened it). *Note: The dashboard listens to the current run. If you miss the run, just run it again!*

### 4. View History
Click on the **📈 History** tab in the sidebar to view the trend of your test executions over time.

## 📊 What does it Monitor?

- **Execution Status**: Which fixture and test is currently running.
- **Pass/Fail/Skip Rates**: Instant visual breakdown.
- **Performance**: Duration of each test (in ms).
- **Errors**: Full exception messages for failed assertions.
- **Trends**: How your codebase stability is evolving (are failures increasing?).

## ⚙️ Configuration Options

| Option | Description | Default |
|--------|-------------|---------|
| `Port` | The HTTP port to listen on. | `9000` |
| `WaitAfterRun` | If `True`, the console app pauses after execution, keeping the server alive. | `True` |

```pascal
.UseDashboard(Port: 8080, WaitAfterRun: False)
```

## 📝 Technical Details

- **Technology**: Built seamlessly on top of `Indy` (IdHTTPServer).
- **Protocol**: Server-Sent Events (SSE) `text/event-stream`.
- **Storage**: `dext_test_history.json` (stored in the application directory).
- **Dependencies**: None (Embedded HTML/CSS/JS).
