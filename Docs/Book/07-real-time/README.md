# 7. Real-Time Communication

Build real-time features with SignalR-compatible Hubs.

> 📦 **Example**: [Hubs](../../../Examples/Hubs/)

## Quick Start

### 1. Define Hub

```pascal
type
  [HubName('chat')]
  TChatHub = class(THub)
  public
    procedure SendMessage(User, Message: string);
    procedure JoinRoom(RoomName: string);
  end;

procedure TChatHub.SendMessage(User, Message: string);
begin
  // Broadcast to all connected clients
  Clients.All.Invoke('ReceiveMessage', [User, Message]);
end;

procedure TChatHub.JoinRoom(RoomName: string);
begin
  // Add caller to a group
  Groups.Add(Context.ConnectionId, RoomName);
  
  // Notify room
  Clients.Group(RoomName).Invoke('UserJoined', [User]);
end;
```

### 2. Map Hub

```pascal
App.Configure(procedure(App: IApplicationBuilder)
  begin
    THubExtensions.MapHub(App, '/hubs/chat', TChatHub);
  end);
```

### 3. JavaScript Client

```javascript
const connection = new signalR.HubConnectionBuilder()
    .withUrl('/hubs/chat')
    .build();

connection.on('ReceiveMessage', (user, message) => {
    console.log(`${user}: ${message}`);
});

await connection.start();
await connection.invoke('SendMessage', 'John', 'Hello!');
```

## Client Methods

```pascal
// All connected clients
Clients.All.Invoke('Method', [Args]);

// Specific client
Clients.Client(ConnectionId).Invoke('Method', [Args]);

// All except caller
Clients.Others.Invoke('Method', [Args]);

// Group of clients
Clients.Group('RoomName').Invoke('Method', [Args]);

// Caller only
Clients.Caller.Invoke('Method', [Args]);
```

---

[← Database as API](../06-database-as-api/README.md) | [Next: Testing →](../08-testing/README.md)
