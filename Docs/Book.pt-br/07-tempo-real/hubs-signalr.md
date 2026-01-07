# Hubs (SignalR)

Comunicação bidirecional em tempo real entre cliente e servidor.

> 📦 **Exemplo**: [Hubs](../../../Examples/Hubs/)

## O que são Hubs?

Hubs são uma abstração de alto nível para WebSockets que permitem:
- O servidor chamar métodos no cliente (browser/mobile).
- O cliente chamar métodos no servidor.
- Broadcast para todos ou grupos específicos.

## Definindo um Hub

```pascal
type
  [HubName('notificacoes')]
  TNotificationHub = class(THub)
  public
    // Método chamado pelo cliente
    procedure EnviarGlobal(Msg: string);
  end;

procedure TNotificationHub.EnviarGlobal(Msg: string);
begin
  // Chama 'ReceberNotificacao' em todos os clientes conectados
  Clients.All.Invoke('ReceberNotificacao', [Msg]);
end;
```

## Grupos e Usuários

Você pode segmentar as mensagens:

```pascal
// Enviar apenas para o remetente
Clients.Caller.Invoke('Confirmacao', ['Recebido']);

// Enviar para um grupo (ex: sala de chat)
Clients.Group('sala-123').Invoke('NovaMensagem', [User, Msg]);

// Enviar para um usuário específico
Clients.User('user-guid').Invoke('Privada', [Msg]);
```

## Ciclo de Vida

Hubs possuem eventos de conexão:

```pascal
procedure TNotificationHub.OnConnected;
begin
  Log('Cliente conectado: ' + Context.ConnectionId);
end;

procedure TNotificationHub.OnDisconnected(Exception: Exception);
begin
  Log('Cliente desconectado');
end;
```

## Mapeamento no Pipeline

```pascal
App.Configure(procedure(App: IApplicationBuilder)
  begin
    App.MapHub<TNotificationHub>('/hubs/notificacoes');
  end);
```

---

[← Tempo Real](README.md) | [Próximo: Testes →](../08-testes/README.md)
