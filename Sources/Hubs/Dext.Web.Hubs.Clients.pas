{***************************************************************************}
{                                                                           }
{           Dext Framework                                                  }
{                                                                           }
{           Copyright (C) 2025-2026 Cesar Romero & Dext Contributors        }
{                                                                           }
{           Licensed under the Apache License, Version 2.0 (the "License"); }
{           you may not use this file except in compliance with the License.}
{           You may obtain a copy of the License at                         }
{                                                                           }
{               http://www.apache.org/licenses/LICENSE-2.0                  }
{                                                                           }
{           Unless required by applicable law or agreed to in writing,      }
{           software distributed under the License is distributed on an     }
{           "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    }
{           either express or implied. See the License for the specific     }
{           language governing permissions and limitations under the        }
{           License.                                                        }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Author:  Cesar Romero                                                    }
{  Created: 2026-01-06                                                      }
{                                                                           }
{  Description:                                                             }
{    Client proxy implementations for sending messages to Hub clients.      }
{                                                                           }
{***************************************************************************}
unit Dext.Web.Hubs.Clients;

{$I ..\Dext.inc}

interface

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  Dext.Web.Hubs.Interfaces,
  Dext.Web.Hubs.Types,
  Dext.Web.Hubs.Protocol.Json;

type
  /// <summary>
  /// Client proxy that sends to specific connection(s).
  /// </summary>
  TClientProxy = class(TInterfacedObject, IClientProxy)
  private
    FConnectionManager: IConnectionManager;
    FConnectionIds: TArray<string>;
    FExcludeIds: TArray<string>;
    FProtocol: TJsonHubProtocol;
  public
    constructor Create(const AConnectionManager: IConnectionManager;
                       const AConnectionIds: TArray<string>;
                       const AExcludeIds: TArray<string> = nil);
    destructor Destroy; override;
    
    procedure SendAsync(const Method: string; const Args: TArray<TValue>); overload;
    procedure SendAsync(const Method: string); overload;
    procedure SendAsync(const Method: string; const Arg: TValue); overload;
  end;
  
  /// <summary>
  /// Client proxy that sends to all connections.
  /// </summary>
  TAllClientsProxy = class(TInterfacedObject, IClientProxy)
  private
    FConnectionManager: IConnectionManager;
    FExcludeIds: TArray<string>;
    FProtocol: TJsonHubProtocol;
  public
    constructor Create(const AConnectionManager: IConnectionManager;
                       const AExcludeIds: TArray<string> = nil);
    destructor Destroy; override;
    
    procedure SendAsync(const Method: string; const Args: TArray<TValue>); overload;
    procedure SendAsync(const Method: string); overload;
    procedure SendAsync(const Method: string; const Arg: TValue); overload;
  end;
  
  /// <summary>
  /// Client proxy that sends to connections in a group.
  /// </summary>
  TGroupClientsProxy = class(TInterfacedObject, IClientProxy)
  private
    FConnectionManager: IConnectionManager;
    FGroupName: string;
    FExcludeIds: TArray<string>;
    FProtocol: TJsonHubProtocol;
  public
    constructor Create(const AConnectionManager: IConnectionManager;
                       const AGroupName: string;
                       const AExcludeIds: TArray<string> = nil);
    destructor Destroy; override;
    
    procedure SendAsync(const Method: string; const Args: TArray<TValue>); overload;
    procedure SendAsync(const Method: string); overload;
    procedure SendAsync(const Method: string; const Arg: TValue); overload;
  end;
  
  /// <summary>
  /// Client proxy that sends to connections of a specific user.
  /// </summary>
  TUserClientsProxy = class(TInterfacedObject, IClientProxy)
  private
    FConnectionManager: IConnectionManager;
    FUserId: string;
    FProtocol: TJsonHubProtocol;
  public
    constructor Create(const AConnectionManager: IConnectionManager;
                       const AUserId: string);
    destructor Destroy; override;
    
    procedure SendAsync(const Method: string; const Args: TArray<TValue>); overload;
    procedure SendAsync(const Method: string); overload;
    procedure SendAsync(const Method: string; const Arg: TValue); overload;
  end;
  
  /// <summary>
  /// Implementation of IHubClients providing access to client proxies.
  /// </summary>
  THubClients = class(TInterfacedObject, IHubClients)
  private
    FConnectionManager: IConnectionManager;
    FCallerConnectionId: string;
  public
    constructor Create(const AConnectionManager: IConnectionManager;
                       const ACallerConnectionId: string = '');
    
    function All: IClientProxy;
    function Client(const ConnectionId: string): IClientProxy;
    function Group(const GroupName: string): IClientProxy;
    function Groups(const GroupNames: TArray<string>): IClientProxy;
    function User(const UserId: string): IClientProxy;
    function Users(const UserIds: TArray<string>): IClientProxy;
    function AllExcept(const ExcludedConnectionIds: TArray<string>): IClientProxy;
    function GroupExcept(const GroupName: string; const ExcludedConnectionIds: TArray<string>): IClientProxy;
    function Caller: IClientProxy;
    function Others: IClientProxy;
    function OthersInGroup(const GroupName: string): IClientProxy;
  end;

implementation

{ Helper }

function Contains(const Arr: TArray<string>; const Value: string): Boolean;
var
  S: string;
begin
  Result := False;
  for S in Arr do
    if SameText(S, Value) then
      Exit(True);
end;

{ TClientProxy }

constructor TClientProxy.Create(const AConnectionManager: IConnectionManager;
  const AConnectionIds, AExcludeIds: TArray<string>);
begin
  inherited Create;
  FConnectionManager := AConnectionManager;
  FConnectionIds := AConnectionIds;
  FExcludeIds := AExcludeIds;
  FProtocol := TJsonHubProtocol.Create;
end;

destructor TClientProxy.Destroy;
begin
  FProtocol.Free;
  inherited;
end;

procedure TClientProxy.SendAsync(const Method: string; const Args: TArray<TValue>);
var
  Msg: string;
  Id: string;
  Conn: IHubConnection;
begin
  Msg := TJsonHubProtocol.SerializeInvocation(Method, Args);
  
  for Id in FConnectionIds do
  begin
    if (Length(FExcludeIds) > 0) and Contains(FExcludeIds, Id) then
      Continue;
      
    if FConnectionManager.TryGet(Id, Conn) then
      Conn.SendAsync(Msg);
  end;
end;

procedure TClientProxy.SendAsync(const Method: string);
begin
  SendAsync(Method, []);
end;

procedure TClientProxy.SendAsync(const Method: string; const Arg: TValue);
begin
  SendAsync(Method, [Arg]);
end;

{ TAllClientsProxy }

constructor TAllClientsProxy.Create(const AConnectionManager: IConnectionManager;
  const AExcludeIds: TArray<string>);
begin
  inherited Create;
  FConnectionManager := AConnectionManager;
  FExcludeIds := AExcludeIds;
  FProtocol := TJsonHubProtocol.Create;
end;

destructor TAllClientsProxy.Destroy;
begin
  FProtocol.Free;
  inherited;
end;

procedure TAllClientsProxy.SendAsync(const Method: string; const Args: TArray<TValue>);
var
  Msg: string;
  Conn: IHubConnection;
  Connections: TArray<IHubConnection>;
begin
  Msg := TJsonHubProtocol.SerializeInvocation(Method, Args);
  Connections := FConnectionManager.GetAll;
  
  for Conn in Connections do
  begin
    if (Length(FExcludeIds) > 0) and Contains(FExcludeIds, Conn.ConnectionId) then
      Continue;
      
    Conn.SendAsync(Msg);
  end;
end;

procedure TAllClientsProxy.SendAsync(const Method: string);
begin
  SendAsync(Method, []);
end;

procedure TAllClientsProxy.SendAsync(const Method: string; const Arg: TValue);
begin
  SendAsync(Method, [Arg]);
end;

{ TGroupClientsProxy }

constructor TGroupClientsProxy.Create(const AConnectionManager: IConnectionManager;
  const AGroupName: string; const AExcludeIds: TArray<string>);
begin
  inherited Create;
  FConnectionManager := AConnectionManager;
  FGroupName := AGroupName;
  FExcludeIds := AExcludeIds;
  FProtocol := TJsonHubProtocol.Create;
end;

destructor TGroupClientsProxy.Destroy;
begin
  FProtocol.Free;
  inherited;
end;

procedure TGroupClientsProxy.SendAsync(const Method: string; const Args: TArray<TValue>);
var
  Msg: string;
  Conn: IHubConnection;
  Connections: TArray<IHubConnection>;
begin
  Msg := TJsonHubProtocol.SerializeInvocation(Method, Args);
  Connections := FConnectionManager.GetByGroup(FGroupName);
  
  for Conn in Connections do
  begin
    if (Length(FExcludeIds) > 0) and Contains(FExcludeIds, Conn.ConnectionId) then
      Continue;
      
    Conn.SendAsync(Msg);
  end;
end;

procedure TGroupClientsProxy.SendAsync(const Method: string);
begin
  SendAsync(Method, []);
end;

procedure TGroupClientsProxy.SendAsync(const Method: string; const Arg: TValue);
begin
  SendAsync(Method, [Arg]);
end;

{ TUserClientsProxy }

constructor TUserClientsProxy.Create(const AConnectionManager: IConnectionManager;
  const AUserId: string);
begin
  inherited Create;
  FConnectionManager := AConnectionManager;
  FUserId := AUserId;
  FProtocol := TJsonHubProtocol.Create;
end;

destructor TUserClientsProxy.Destroy;
begin
  FProtocol.Free;
  inherited;
end;

procedure TUserClientsProxy.SendAsync(const Method: string; const Args: TArray<TValue>);
var
  Msg: string;
  Conn: IHubConnection;
  Connections: TArray<IHubConnection>;
begin
  Msg := TJsonHubProtocol.SerializeInvocation(Method, Args);
  Connections := FConnectionManager.GetByUser(FUserId);
  
  for Conn in Connections do
    Conn.SendAsync(Msg);
end;

procedure TUserClientsProxy.SendAsync(const Method: string);
begin
  SendAsync(Method, []);
end;

procedure TUserClientsProxy.SendAsync(const Method: string; const Arg: TValue);
begin
  SendAsync(Method, [Arg]);
end;

{ THubClients }

constructor THubClients.Create(const AConnectionManager: IConnectionManager;
  const ACallerConnectionId: string);
begin
  inherited Create;
  FConnectionManager := AConnectionManager;
  FCallerConnectionId := ACallerConnectionId;
end;

function THubClients.All: IClientProxy;
begin
  Result := TAllClientsProxy.Create(FConnectionManager);
end;

function THubClients.Client(const ConnectionId: string): IClientProxy;
begin
  Result := TClientProxy.Create(FConnectionManager, [ConnectionId]);
end;

function THubClients.Group(const GroupName: string): IClientProxy;
begin
  Result := TGroupClientsProxy.Create(FConnectionManager, GroupName);
end;

function THubClients.Groups(const GroupNames: TArray<string>): IClientProxy;
var
  AllConnections: TList<string>;
  GroupName: string;
  Connections: TArray<IHubConnection>;
  Conn: IHubConnection;
begin
  // Collect all unique connection IDs from all groups
  AllConnections := TList<string>.Create;
  try
    for GroupName in GroupNames do
    begin
      Connections := FConnectionManager.GetByGroup(GroupName);
      for Conn in Connections do
        if not AllConnections.Contains(Conn.ConnectionId) then
          AllConnections.Add(Conn.ConnectionId);
    end;
    Result := TClientProxy.Create(FConnectionManager, AllConnections.ToArray);
  finally
    AllConnections.Free;
  end;
end;

function THubClients.User(const UserId: string): IClientProxy;
begin
  Result := TUserClientsProxy.Create(FConnectionManager, UserId);
end;

function THubClients.Users(const UserIds: TArray<string>): IClientProxy;
var
  AllConnections: TList<string>;
  UserId: string;
  Connections: TArray<IHubConnection>;
  Conn: IHubConnection;
begin
  AllConnections := TList<string>.Create;
  try
    for UserId in UserIds do
    begin
      Connections := FConnectionManager.GetByUser(UserId);
      for Conn in Connections do
        if not AllConnections.Contains(Conn.ConnectionId) then
          AllConnections.Add(Conn.ConnectionId);
    end;
    Result := TClientProxy.Create(FConnectionManager, AllConnections.ToArray);
  finally
    AllConnections.Free;
  end;
end;

function THubClients.AllExcept(const ExcludedConnectionIds: TArray<string>): IClientProxy;
begin
  Result := TAllClientsProxy.Create(FConnectionManager, ExcludedConnectionIds);
end;

function THubClients.GroupExcept(const GroupName: string;
  const ExcludedConnectionIds: TArray<string>): IClientProxy;
begin
  Result := TGroupClientsProxy.Create(FConnectionManager, GroupName, ExcludedConnectionIds);
end;

function THubClients.Caller: IClientProxy;
begin
  if FCallerConnectionId = '' then
    raise EHubException.Create('Caller is only available during Hub method invocation');
  Result := TClientProxy.Create(FConnectionManager, [FCallerConnectionId]);
end;

function THubClients.Others: IClientProxy;
begin
  if FCallerConnectionId = '' then
    raise EHubException.Create('Others is only available during Hub method invocation');
  Result := TAllClientsProxy.Create(FConnectionManager, [FCallerConnectionId]);
end;

function THubClients.OthersInGroup(const GroupName: string): IClientProxy;
begin
  if FCallerConnectionId = '' then
    raise EHubException.Create('OthersInGroup is only available during Hub method invocation');
  Result := TGroupClientsProxy.Create(FConnectionManager, GroupName, [FCallerConnectionId]);
end;

end.
