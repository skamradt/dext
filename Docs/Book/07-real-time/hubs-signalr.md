# Hubs (SignalR)

Two-way real-time communication between client and server.

> 📦 **Example**: [Hubs](../../../Examples/Hubs/)

## What are Hubs?

Hubs are a high-level abstraction for WebSockets that allow:
- Server to call methods on the client (browser/mobile).
- Client to call methods on the server.
- Broadcast to everyone or specific groups.

## Defining a Hub

```pascal
type
  [HubName('notifications')]
  TNotificationHub = class(THub)
  public
    // Method called by the client
    procedure SendGlobal(Msg: string);
  end;

procedure TNotificationHub.SendGlobal(Msg: string);
begin
  // Calls 'ReceiveNotification' on all connected clients
  Clients.All.Invoke('ReceiveNotification', [Msg]);
end;
```

## Groups and Users

You can segment messages:

```pascal
// Send only to the caller
Clients.Caller.Invoke('Confirmation', ['Received']);

// Send to a group (e.g., chat room)
Clients.Group('room-123').Invoke('NewMessage', [User, Msg]);

// Send to a specific user
Clients.User('user-guid').Invoke('Private', [Msg]);
```

## Lifecycle

Hubs have connection events:

```pascal
procedure TNotificationHub.OnConnected;
begin
  Log('Client connected: ' + Context.ConnectionId);
end;

procedure TNotificationHub.OnDisconnected(Exception: Exception);
begin
  Log('Client disconnected');
end;
```

## Pipeline Mapping

```pascal
App.Configure(procedure(App: IApplicationBuilder)
  begin
    App.MapHub<TNotificationHub>('/hubs/notifications');
  end);
```

---

[← Real-Time](README.md) | [Next: Testing →](../08-testing/README.md)
