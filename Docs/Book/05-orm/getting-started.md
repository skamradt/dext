# ORM: Getting Started

Learn how to create your first entity and database context.

> 📦 **Example**: [Orm.EntityDemo](../../../Examples/Orm.EntityDemo/)

## 1. Create an Entity

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

## 2. Create a DbContext

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

## 3. Configure Connection

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
  // Setup FireDAC connection
  FDConn := TFDConnection.Create(nil);
  FDConn.DriverName := 'SQLite';
  FDConn.Params.Add('Database=myapp.db');
  FDConn.Connected := True;
  
  // Wrap for Dext
  Connection := TFireDACConnection.Create(FDConn, True);
  
  // Create context (Dialect is auto-detected from Connection)
  Ctx := TAppDbContext.Create(Connection); 
  
  // Optional: Explicitly specifying dialect
  // Dialect := TSQLiteDialect.Create;
  // Ctx := TAppDbContext.Create(Connection, Dialect);
end;
```

### 3.1. Connection Optimizations

Dext automatically applies performance optimizations by default (e.g., disabling Macros and Escapes for speed). 
If you need legacy behavior (e.g., you rely on SQL Macros), you can **disable** these optimizations by configuring the set explicitly.

```pascal
var
  Options: TDbContextOptions;
begin
  Options := TDbContextOptions.Create
    .UseDriver('PostgreSQL')
    // Example: Re-enable Macros by excluding 'optDisableMacros' from the set
    // Default includes: [optDisableMacros, optDisableEscapes, optDirectExecute]
    .ConfigureOptimizations([optDirectExecute, optDisableEscapes]); 

  Ctx := TAppDbContext.Create(Options);
end;
```

## 4. Create Tables

```pascal
Ctx.EnsureCreated;   // Creates tables if they don't exist
```

## 5. CRUD Operations

### Create

```pascal
var
  User: TUser;
begin
  User := TUser.Create;
  User.Name := 'John Doe';
  User.Email := 'john@example.com';
  User.CreatedAt := Now;
  
  Ctx.Users.Add(User);
  Ctx.SaveChanges;
  
  WriteLn('Created user with ID: ', User.Id);
end;
```

### Read

```pascal
// Find by ID
var User := Ctx.Users.Find(1);

// Get all
var AllUsers := Ctx.Users.ToList;

// Query with filter
var JohnUsers := Ctx.Users
  .Where(function(U: TUser): Boolean
    begin
      Result := U.Name.StartsWith('John');
    end)
  .ToList;
```

### Update

```pascal
var User := Ctx.Users.Find(1);
User.Name := 'Jane Doe';
Ctx.SaveChanges;
```

### Delete

```pascal
var User := Ctx.Users.Find(1);
Ctx.Users.Remove(User);
Ctx.SaveChanges;
```

---

[← ORM Overview](README.md) | [Next: Entities & Mapping →](entities.md)
