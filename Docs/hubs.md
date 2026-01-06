# 🔌 Dext.Hubs - Real-Time Communication

**Dext.Hubs** provides real-time, bidirectional communication between server and clients, similar to ASP.NET Core SignalR. It enables scenarios like live dashboards, chat applications, notifications, and collaborative features.

> 📋 **Implementation Plan**: See [dext-hubs-implementation-plan.md](./Plans/dext-hubs-implementation-plan.md) for roadmap and technical details.

---

## 📋 Table of Contents

- [Overview](#overview)
- [Quick Start](#quick-start)
- [Creating a Hub](#creating-a-hub)
- [Registering Hubs](#registering-hubs)
- [Client Connections](#client-connections)
- [Sending Messages](#sending-messages)
- [Groups](#groups)
- [JavaScript Client](#javascript-client)
- [API Reference](#api-reference)
- [Examples](#examples)

---

## Overview

### Key Features

- ✅ **SignalR-Compatible Protocol** - JSON-based message format
- ✅ **Polling Transport** - Reliable real-time via HTTP polling
- ✅ **Groups** - Broadcast to subsets of connected clients
- ✅ **Dependency Injection** - Access Hubs from anywhere via `IHubContext`
- ✅ **Connection Management** - Track and manage client connections
- ✅ **Auto-Reconnection** - JavaScript client handles reconnection
- ✅ **JavaScript Client** - Easy-to-use `dext-hubs.js` client library

### Architecture

```
┌─────────────────────────────────────────┐
│              Client (Browser)            │
│  ┌─────────────────────────────────────┐ │
│  │     DextHubConnection (JS)          │ │
│  └───────────────┬─────────────────────┘ │
└──────────────────┼───────────────────────┘
                   │ Polling / HTTP POST
                   ▼
┌─────────────────────────────────────────┐
│              Server (Delphi)             │
│  ┌─────────────────────────────────────┐ │
│  │     THubMiddleware                   │ │
│  │  ┌────────────────────────────────┐ │ │
│  │  │    TMyHub : THub               │ │ │
│  │  │    - Clients.All.SendAsync()   │ │ │
│  │  │    - Groups.AddToGroupAsync()  │ │ │
│  │  └────────────────────────────────┘ │ │
│  └─────────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

---

## Quick Start

### 1. Create a Hub

```pascal
uses
  Dext.Web.Hubs;

type
  TNotificationHub = class(THub)
  public
    procedure Subscribe(const Topic: string);
    procedure Unsubscribe(const Topic: string);
  end;

procedure TNotificationHub.Subscribe(const Topic: string);
begin
  Groups.AddToGroupAsync(Context.ConnectionId, 'topic:' + Topic);
  Clients.Caller.SendAsync('Subscribed', [Topic]);
end;

procedure TNotificationHub.Unsubscribe(const Topic: string);
begin
  Groups.RemoveFromGroupAsync(Context.ConnectionId, 'topic:' + Topic);
end;
```

### 2. Register the Hub

```pascal
uses
  Dext.Web.Hubs.Extensions;

// In your application startup:
App.UseHubs;
THubExtensions.MapHub(App, '/hubs/notifications', TNotificationHub);
```

### 3. Connect from JavaScript

```html
<script src="/dext-hubs.js"></script>
<script>
  const hub = new DextHubConnection('/hubs/notifications');
  
  hub.on('Subscribed', (topic) => {
    console.log('Subscribed to:', topic);
  });
  
  hub.on('NewNotification', (notification) => {
    showNotification(notification);
  });
  
  await hub.start();
  await hub.invoke('Subscribe', 'alerts');
</script>
```

### 4. Send Messages from Server

```pascal
uses
  Dext.Web.Hubs.Extensions;

// From anywhere in your application:
var HubContext := THubExtensions.GetHubContext;

// Send to all clients
HubContext.Clients.All.SendAsync('NewNotification', [
  TValue.From('New alert received!')
]);

// Send to specific topic group
HubContext.Clients.Group('topic:alerts').SendAsync('NewNotification', [
  TValue.From(AlertData)
]);
```

---

## Creating a Hub

### Hub Base Class

All Hubs inherit from `THub`:

```pascal
type
  TMyHub = class(THub)
  protected
    property Context: IHubCallerContext read FContext;  // Current connection info
    property Clients: IHubClients read FClients;        // Send to clients
    property Groups: IGroupManager read FGroups;        // Manage groups
  public
    procedure OnConnectedAsync; override;               // Called on connect
    procedure OnDisconnectedAsync(const Error: Exception); override; // Called on disconnect
  end;
```

### Hub Methods

Public methods on your Hub can be invoked by clients:

```pascal
type
  TChatHub = class(THub)
  public
    // Clients can call: hub.invoke('SendMessage', 'John', 'Hello!')
    procedure SendMessage(const User, Message: string);
    
    // Clients can call: hub.invoke('JoinRoom', 'general')
    procedure JoinRoom(const RoomName: string);
    
    // Clients can call: hub.invoke('LeaveRoom', 'general')
    procedure LeaveRoom(const RoomName: string);
  end;

procedure TChatHub.SendMessage(const User, Message: string);
begin
  // Broadcast to all connected clients
  Clients.All.SendAsync('ReceiveMessage', [User, Message]);
end;

procedure TChatHub.JoinRoom(const RoomName: string);
begin
  Groups.AddToGroupAsync(Context.ConnectionId, RoomName);
  Clients.Group(RoomName).SendAsync('UserJoined', [User]);
end;
```

### Lifecycle Events

```pascal
procedure TMyHub.OnConnectedAsync;
begin
  // Called when a client connects
  WriteLn('Client connected: ', Context.ConnectionId);
  
  // Add to a default group
  Groups.AddToGroupAsync(Context.ConnectionId, 'all-users');
end;

procedure TMyHub.OnDisconnectedAsync(const Error: Exception);
begin
  // Called when a client disconnects
  if Error <> nil then
    WriteLn('Client disconnected with error: ', Error.Message)
  else
    WriteLn('Client disconnected: ', Context.ConnectionId);
end;
```

---

## Registering Hubs

### Using THubExtensions

```pascal
uses
  Dext.Web.Hubs.Extensions;

// Basic registration
App.UseHubs;
THubExtensions.MapHub(App, '/hubs/chat', TChatHub);
THubExtensions.MapHub(App, '/hubs/notifications', TNotificationHub);

// Or use the convenience function
MapHub(App, '/hubs/dashboard', TDashboardHub);
```

### HTTP Endpoints

Each Hub creates these endpoints:

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/hubs/{name}/negotiate` | POST | Returns connectionId and available transports |
| `/hubs/{name}/poll?id=xxx` | GET | Poll for pending messages (returns JSON array) |
| `/hubs/{name}?id=xxx` | POST | Invoke Hub method |

---

## Client Connections

### Connection Context

Access connection information within Hub methods:

```pascal
procedure TMyHub.WhoAmI;
begin
  var ConnectionId := Context.ConnectionId;      // Unique connection ID
  var UserId := Context.UserIdentifier;          // User ID from claims
  var Transport := Context.TransportType;        // SSE, WebSocket, etc.
  
  Clients.Caller.SendAsync('Identity', [ConnectionId, UserId]);
end;
```

### Connection Items

Store per-connection data:

```pascal
procedure TMyHub.SetNickname(const Nickname: string);
begin
  Context.Items.AddOrSetValue('nickname', TValue.From(Nickname));
end;

function TMyHub.GetNickname: string;
begin
  if Context.Items.TryGetValue('nickname', Result) then
    // Found
  else
    Result := 'Anonymous';
end;
```

---

## Sending Messages

### To All Clients

```pascal
Clients.All.SendAsync('MethodName', [Arg1, Arg2]);
```

### To Specific Client

```pascal
Clients.Client('connection-id').SendAsync('MethodName', [Arg1]);
```

### To Caller (Current Connection)

```pascal
Clients.Caller.SendAsync('MethodName', [Arg1]);
```

### To All Except Caller

```pascal
Clients.Others.SendAsync('MethodName', [Arg1]);
```

### To Group

```pascal
Clients.Group('room-name').SendAsync('MethodName', [Arg1]);
```

### To Multiple Groups

```pascal
Clients.Groups(['room1', 'room2']).SendAsync('MethodName', [Arg1]);
```

### To User (All Connections)

```pascal
Clients.User('user-id').SendAsync('MethodName', [Arg1]);
```

### Excluding Connections

```pascal
Clients.AllExcept(['conn1', 'conn2']).SendAsync('MethodName', [Arg1]);
Clients.GroupExcept('room', ['conn1']).SendAsync('MethodName', [Arg1]);
```

---

## Groups

Groups allow you to broadcast to subsets of connections:

```pascal
// Add to group
Groups.AddToGroupAsync(Context.ConnectionId, 'room:general');

// Remove from group
Groups.RemoveFromGroupAsync(Context.ConnectionId, 'room:general');

// Check membership
if Groups.IsInGroup(Context.ConnectionId, 'room:general') then
  // ...

// Get all groups for a connection
var UserGroups := Groups.GetGroupsForConnection(Context.ConnectionId);
```

---

## JavaScript Client

### Installation

Include the script:

```html
<script src="/dext-hubs.js"></script>
```

### Basic Usage

```javascript
const connection = new DextHubConnection('/hubs/chat');

// Register handlers
connection.on('ReceiveMessage', (user, message) => {
  console.log(`${user}: ${message}`);
});

// Connect
await connection.start();

// Invoke server method
await connection.invoke('SendMessage', 'John', 'Hello!');

// Send without waiting for result
await connection.send('Ping');

// Disconnect
await connection.stop();
```

### Connection Events

```javascript
connection.on('connected', (data) => {
  console.log('Connected with ID:', data.connectionId);
});

connection.on('disconnected', () => {
  console.log('Disconnected');
});
```

### Options

```javascript
const connection = new DextHubConnection('/hubs/chat', {
  transport: 'serverSentEvents',  // or 'longPolling'
  reconnect: true,                // Auto-reconnect on disconnect
  reconnectDelay: 3000            // Delay between reconnection attempts
});
```

---

## API Reference

### THub

| Property/Method | Description |
|-----------------|-------------|
| `Context: IHubCallerContext` | Current connection information |
| `Clients: IHubClients` | Client proxy for sending messages |
| `Groups: IGroupManager` | Group management |
| `OnConnectedAsync` | Override to handle connection |
| `OnDisconnectedAsync(Error)` | Override to handle disconnection |

### IHubClients

| Method | Description |
|--------|-------------|
| `All` | All connected clients |
| `Caller` | The calling client |
| `Client(id)` | Specific client by connection ID |
| `Group(name)` | All clients in a group |
| `Groups(names)` | All clients in multiple groups |
| `User(userId)` | All connections of a user |
| `Others` | All except caller |
| `AllExcept(ids)` | All except specified connections |

### IClientProxy

| Method | Description |
|--------|-------------|
| `SendAsync(method, args)` | Send message to client(s) |

### IGroupManager

| Method | Description |
|--------|-------------|
| `AddToGroupAsync(connId, group)` | Add connection to group |
| `RemoveFromGroupAsync(connId, group)` | Remove connection from group |
| `IsInGroup(connId, group)` | Check if in group |

### IHubContext

| Property | Description |
|----------|-------------|
| `Clients` | Client proxies for sending |
| `Groups` | Group management |

---

## Examples

### Live Dashboard

```pascal
type
  TDashboardHub = class(THub)
  public
    procedure SubscribeToProject(const ProjectId: string);
  end;

procedure TDashboardHub.SubscribeToProject(const ProjectId: string);
begin
  Groups.AddToGroupAsync(Context.ConnectionId, 'project:' + ProjectId);
end;

// From build service:
procedure TBuildService.NotifyProgress(const ProjectId: string; Progress: Integer);
begin
  var Hub := THubExtensions.GetHubContext;
  Hub.Clients.Group('project:' + ProjectId).SendAsync('BuildProgress', [Progress]);
end;
```

### Chat Room

```pascal
type
  TChatHub = class(THub)
  public
    procedure Join(const Room, Username: string);
    procedure Send(const Room, Message: string);
  end;

procedure TChatHub.Join(const Room, Username: string);
begin
  Context.Items.AddOrSetValue('username', TValue.From(Username));
  Groups.AddToGroupAsync(Context.ConnectionId, Room);
  Clients.Group(Room).SendAsync('UserJoined', [Username]);
end;

procedure TChatHub.Send(const Room, Message: string);
var
  Username: TValue;
begin
  Context.Items.TryGetValue('username', Username);
  Clients.Group(Room).SendAsync('Message', [Username.AsString, Message]);
end;
```

---

## Troubleshooting

### Common Issues

1. **Connection fails immediately**
   - Check that `UseHubs` is called before `MapHub`
   - Verify the Hub path matches client URL

2. **Messages not received**
   - Verify handler is registered before `start()`
   - Check method name matches exactly (case-sensitive)

3. **SSE connection drops**
   - Check for proxy/firewall issues
   - Increase `reconnectDelay` if server is under load

---

## See Also

- [Implementation Plan](./Plans/dext-hubs-implementation-plan.md)
- [CLI & Dashboard Unified Plan](./Plans/cli-dashboard-unified-plan.md)
- [Web Roadmap](./Roadmap/web-roadmap.md)

---

*Last updated: January 6, 2026*
