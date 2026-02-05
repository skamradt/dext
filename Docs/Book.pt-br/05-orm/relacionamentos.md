# Relacionamentos

Defina relacionamentos entre entidades com suporte a lazy loading.

## Um-para-Muitos (1:N)

Um usuário possui muitos pedidos:

```pascal
type
  TUser = class;
  
  [Table('orders')]
  TOrder = class
  private
    FId: Integer;
    FUserId: Integer;
    FUser: ILazy<TUser>;
    function GetUser: TUser;
    procedure SetUser(Value: TUser);
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    [ForeignKey('user_id')]
    property UserId: Integer read FUserId write FUserId;
    
    // Navegação lazy-loaded
    property User: TUser read GetUser write SetUser;
  end;
  
  [Table('users')]
  TUser = class
  private
    FId: Integer;
    FName: string;
    FOrders: ILazy<TList<TOrder>>;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    
    [InverseProperty('User')]
    property Orders: TList<TOrder> read GetOrders;
  end;
```

## Lazy Loading

Propriedades de navegação são carregadas no primeiro acesso:

```pascal
var Pedido := Context.Orders.Find(1);

// Usuário NÃO carregado ainda
WriteLn('Pedido ID: ', Pedido.Id);

// Usuário carregado AQUI (no primeiro acesso)
WriteLn('Usuário: ', Pedido.User.Name);
```

## Muitos-para-Muitos (N:N)

Relacionamentos Muitos-para-Muitos requerem uma tabela de ligação. Use o atributo `[ManyToMany]`:

```pascal
type
  [Table('roles')]
  TRole = class
  private
    FId: Integer;
    FName: string;
    FUsers: ILazy<TList<TUser>>;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    
    [ManyToMany('user_roles', 'role_id', 'user_id')]
    property Users: TList<TUser> read GetUsers;
  end;

  [Table('users')]
  TUser = class
  private
    FId: Integer;
    FRoles: ILazy<TList<TRole>>;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    [ManyToMany('user_roles', 'user_id', 'role_id')]
    property Roles: TList<TRole> read GetRoles;
  end;
```

### Vinculando Entidades Gerenciadas

Para vincular ou desvincular entidades em um relacionamento Many-to-Many:

```pascal
// Adicionar um novo vínculo
Context.Users.LinkManyToMany(User, 'Roles', AdminRole);

// Remover um vínculo
Context.Users.UnlinkManyToMany(User, 'Roles', AdminRole);

// Sync (substitui todos os vínculos pelo array fornecido)
Context.Users.SyncManyToMany(User, 'Roles', [Role1, Role2, Role3]);

Context.SaveChanges;
```

## Include (Eager Loading)

Carregue entidades relacionadas antecipadamente para evitar consultas N+1:

```pascal
// Sem Include: N+1 consultas
var Pedidos := Context.Orders.ToList;
for var P in Pedidos do
  WriteLn(P.User.Name);  // Cada acesso = 1 consulta!

// Com Include: 2 consultas no total
var Pedidos := Context.Orders
  .Include('User')
  .ToList;
for var P in Pedidos do
  WriteLn(P.User.Name);  // Já carregado!
```

---

[← Specifications](specifications.md) | [Próximo: Migrations →](migrations.md)
