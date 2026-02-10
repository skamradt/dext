# Relationships

Define relationships between entities with lazy loading support.

## One-to-Many (1:N)

A user has many orders:

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
    
    // Lazy-loaded navigation
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

Navigation properties are loaded on first access:

```pascal
var Order := Context.Orders.Find(1);

// User NOT loaded yet
WriteLn('Order ID: ', Order.Id);

// User loaded HERE (on first access)
WriteLn('User: ', Order.User.Name);
```

### ILazy<T> Implementation

```pascal
function TOrder.GetUser: TUser;
begin
  if FUser = nil then
    FUser := TLazy<TUser>.Create;
  Result := FUser.Value;  // Loads from DB if needed
end;

procedure TOrder.SetUser(Value: TUser);
begin
  if FUser = nil then
    FUser := TLazy<TUser>.Create;
  FUser.Value := Value;
  if Value <> nil then
    FUserId := Value.Id;
end;
```

## Many-to-One (N:1)

Many orders belong to one user (inverse of above):

```pascal
[Table('orders')]
TOrder = class
public
  [ForeignKey('user_id')]
  property User: TUser read GetUser write SetUser;
end;
```

## One-to-One (1:1)

User has one profile:

```pascal
[Table('profiles')]
TProfile = class
public
  [PK]  // Same ID as User (shared primary key)
  property UserId: Integer;
  
  [ForeignKey('user_id')]
  property User: TUser;
end;

[Table('users')]
TUser = class
public
  property Profile: TProfile;
end;
```

## Many-to-Many (N:N)

Many-to-Many relationships require a join table. Use the `[ManyToMany]` attribute:

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

### Linking Managed Entities

To link or unlink entities in a Many-to-Many relationship:

```pascal
// Add a new link
Context.Users.LinkManyToMany(User, 'Roles', AdminRole);

// Remove a link
Context.Users.UnlinkManyToMany(User, 'Roles', AdminRole);

// Sync (replaces all links with the provided array)
Context.Users.SyncManyToMany(User, 'Roles', [Role1, Role2, Role3]);

Context.SaveChanges;
```

## Lazy Loading

Load related entities upfront to avoid N+1 queries:

```pascal
// Without Include: N+1 queries
var Orders := Context.Orders.ToList;
for var O in Orders do
  WriteLn(O.User.Name);  // Each access = 1 query!

// With Include: 2 queries total
var Orders := Context.Orders
  .Include('User')
  .ToList;
for var O in Orders do
  WriteLn(O.User.Name);  // Already loaded!
```

### Multiple Includes

```pascal
var Orders := Context.Orders
  .Include('User')
  .Include('Items')
  .Include('Items.Product')  // Nested
  .ToList;
```

## Cascade Delete

Configure cascade behavior:

```pascal
[ForeignKey('user_id'), OnDeleteCascade]
property UserId: Integer;

// Or in fluent mapping
Builder.Entity<TOrder>
  .HasOne('User')
  .WithMany('Orders')
  .OnDeleteCascade;
```

---

[← Specifications](specifications.md) | [Next: Migrations →](migrations.md)
