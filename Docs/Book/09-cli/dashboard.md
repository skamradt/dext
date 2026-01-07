# CLI Dashboard

Monitor your application with the web-based dashboard.

## Starting the Dashboard

```bash
dext ui
```

Open `http://localhost:3000` in your browser.

### Custom Port

```bash
dext ui --port 8080
```

## Features

### Overview Dashboard

- **Code Coverage**: Current test coverage percentage
- **Environments**: Detected Delphi versions
- **Projects**: Registered project list
- **Recent Activity**: Latest operations

### Test Reports

View HTML test reports at `/reports`:
- Code coverage summary
- Test results by module
- Coverage trends

### Real-Time Logs

Live log streaming via SignalR:
- Migration output
- Test execution
- Error messages

## Theme Toggle

Click the sun/moon icon in the header to switch between dark and light themes. Your preference is saved in localStorage.

## API Endpoints

The dashboard exposes REST APIs:

| Endpoint | Description |
|----------|-------------|
| `GET /api/config` | Global configuration |
| `GET /api/projects` | Project registry |
| `GET /api/test/summary` | Coverage summary |
| `POST /api/env/scan` | Scan for Delphi environments |

## Integration

Embed dashboard in your application:

```pascal
uses
  Dext.Hosting.CLI.Commands.UI;

var
  UICommand: TUICommand;
begin
  UICommand := TUICommand.Create;
  UICommand.Execute(Args);
end;
```

---

[← Scaffolding](scaffolding.md) | [Next: Advanced Topics →](../10-advanced/README.md)
