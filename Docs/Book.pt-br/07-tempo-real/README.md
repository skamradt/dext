# 7. Comunicação em Tempo Real

Construa recursos em tempo real com Hubs compatíveis com SignalR.

> 📦 **Exemplo**: [Hubs](../../../Examples/Hubs/)

## Início Rápido

### 1. Definir Hub

```pascal
type
  [HubName('chat')]
  TChatHub = class(THub)
  public
    procedure EnviarMensagem(Usuario, Mensagem: string);
    procedure EntrarSala(NomeSala: string);
  end;

procedure TChatHub.EnviarMensagem(Usuario, Mensagem: string);
begin
  // Broadcast para todos os clientes conectados
  Clients.All.Invoke('ReceberMensagem', [Usuario, Mensagem]);
end;

procedure TChatHub.EntrarSala(NomeSala: string);
begin
  // Adicionar chamador a um grupo
  Groups.Add(Context.ConnectionId, NomeSala);
  
  // Notificar sala
  Clients.Group(NomeSala).Invoke('UsuarioEntrou', [Usuario]);
end;
```

### 2. Mapear Hub

```pascal
App.Configure(procedure(App: IApplicationBuilder)
  begin
    THubExtensions.MapHub(App, '/hubs/chat', TChatHub);
  end);
```

### 3. Cliente JavaScript

```javascript
const connection = new signalR.HubConnectionBuilder()
    .withUrl('/hubs/chat')
    .build();

connection.on('ReceberMensagem', (usuario, mensagem) => {
    console.log(`${usuario}: ${mensagem}`);
});

await connection.start();
await connection.invoke('EnviarMensagem', 'João', 'Olá!');
```

---

[← Database as API](../06-database-as-api/README.md) | [Próximo: Testes →](../08-testes/README.md)
