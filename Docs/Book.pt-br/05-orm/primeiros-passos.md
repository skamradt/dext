# ORM: Primeiros Passos

Aprenda a criar sua primeira entidade e contexto de banco de dados.

> 📦 **Exemplo**: [Orm.EntityDemo](../../../Examples/Orm.EntityDemo/)

## 1. Criar uma Entidade

```pascal
unit User;

interface

uses
  Dext.Entity.Attributes;

type
  [Table('users')]
  TUser = class
  private
    FId: Integer;
    FName: string;
    FEmail: string;
    FCreatedAt: TDateTime;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    [Column('name')]
    property Name: string read FName write FName;
    
    [Column('email')]
    property Email: string read FEmail write FEmail;
    
    [Column('created_at')]
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
  end;

implementation

end.
```

## 2. Criar um DbContext

```pascal
unit AppDbContext;

interface

uses
  Dext.Entity.Context,
  Dext.Entity.Core,
  User;

type
  TAppDbContext = class(TDbContext)
  private
    function GetUsers: IDbSet<TUser>;
  public
    property Users: IDbSet<TUser> read GetUsers;
  end;

implementation

function TAppDbContext.GetUsers: IDbSet<TUser>;
begin
  Result := Entities<TUser>;
end;

end.
```

## 3. Configurar Conexão

```pascal
uses
  FireDAC.Comp.Client,
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Dialects;

var
  FDConn: TFDConnection;
  Connection: IDbConnection;
  Dialect: ISQLDialect;
  Ctx: TAppDbContext;
begin
  // Configurar conexão FireDAC
  FDConn := TFDConnection.Create(nil);
  FDConn.DriverName := 'SQLite';
  FDConn.Params.Add('Database=meuapp.db');
  FDConn.Connected := True;
  
  // Encapsular para Dext
  Connection := TFireDACConnection.Create(FDConn, True);
  
  // Criar contexto (Dialeto é detectado automaticamente da Conexão)
  Ctx := TAppDbContext.Create(Connection); 
  
  // Opcional: Especificando o dialeto explicitamente
  // Dialect := TSQLiteDialect.Create;
  // Ctx := TAppDbContext.Create(Connection, Dialect);
end;
```

### 3.1. Otimizações de Conexão

O Dext aplica otimizações de performance automaticamente por padrão (ex: desabilitar Macros e Escapes para velocidade).
Se você precisar de comportamento legado (ex: se você depende de SQL Macros), você pode **desabilitar** essas otimizações configurando o conjunto explicitamente.

```pascal
var
  Options: TDbContextOptions;
begin
  Options := TDbContextOptions.Create
    .UseDriver('PostgreSQL')
    // Exemplo: Re-abilitar Macros excluindo 'optDisableMacros' do conjunto
    // O padrão inclui: [optDisableMacros, optDisableEscapes, optDirectExecute]
    .ConfigureOptimizations([optDirectExecute, optDisableEscapes]); 

  Ctx := TAppDbContext.Create(Options);
end;
```

## 4. Criar Tabelas

```pascal
Ctx.EnsureCreated;   // Cria tabelas se elas não existirem
```

## 5. Operações CRUD

### Create

```pascal
var
  User: TUser;
begin
  User := TUser.Create;
  User.Name := 'João Silva';
  User.Email := 'joao@exemplo.com';
  User.CreatedAt := Now;
  
  Ctx.Users.Add(User);
  Ctx.SaveChanges;
  
  WriteLn('Usuário criado com ID: ', User.Id);
end;
```

### Read

```pascal
// Buscar por ID
var User := Ctx.Users.Find(1);

// Obter todos
var TodosUsuarios := Ctx.Users.ToList;

// Query com filtro
var Joaos := Ctx.Users
  .Where(function(U: TUser): Boolean
    begin
      Result := U.Name.StartsWith('João');
    end)
  .ToList;
```

### Update

```pascal
var User := Ctx.Users.Find(1);
User.Name := 'João Santos';
Ctx.SaveChanges;
```

### Delete

```pascal
var User := Ctx.Users.Find(1);
Ctx.Users.Remove(User);
Ctx.SaveChanges;
```

---

[← Visão Geral do ORM](README.md) | [Próximo: Entidades & Mapeamento →](entidades.md)
