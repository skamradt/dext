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
{    Connection management for Hub clients.                                 }
{    Thread-safe storage and retrieval of active connections.               }
{                                                                           }
{***************************************************************************}
unit Dext.Web.Hubs.Connections;

{$I ..\Dext.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.SyncObjs,
  System.Generics.Collections,
  Dext.Web.Hubs.Interfaces,
  Dext.Web.Hubs.Types,
  Dext.Core.CancellationToken,
  Dext.Auth.Identity;

type
  /// <summary>
  /// Default implementation of IHubConnection.
  /// Represents a single client connection to a Hub.
  /// </summary>
  THubConnection = class(TInterfacedObject, IHubConnection)
  private
    FConnectionId: string;
    FTransportType: TTransportType;
    FState: TConnectionState;
    FUser: IClaimsPrincipal;
    FItems: TDictionary<string, TValue>;
    FAbortTokenSource: TCancellationTokenSource;
    FOnSend: TProc<string>;
    FOnClose: TProc<string>;
  public
    constructor Create(const AConnectionId: string; 
                       ATransportType: TTransportType;
                       const AUser: IClaimsPrincipal = nil);
    destructor Destroy; override;
    
    // IHubConnection
    function GetConnectionId: string;
    function GetTransportType: TTransportType;
    function GetState: TConnectionState;
    function GetUser: IClaimsPrincipal;
    function GetUserIdentifier: string;
    function GetItems: TDictionary<string, TValue>;
    function GetAbortToken: ICancellationToken;
    
    procedure SendAsync(const Message: string);
    procedure Close(const Reason: string = '');
    
    // Configuration
    procedure SetOnSend(const Handler: TProc<string>);
    procedure SetOnClose(const Handler: TProc<string>);
    procedure SetState(AState: TConnectionState);
    
    property ConnectionId: string read GetConnectionId;
    property TransportType: TTransportType read GetTransportType;
    property State: TConnectionState read GetState;
  end;
  
  /// <summary>
  /// Thread-safe manager for all active Hub connections.
  /// </summary>
  TConnectionManager = class(TInterfacedObject, IConnectionManager)
  private
    FConnections: TDictionary<string, IHubConnection>;
    FLock: TCriticalSection;
    FGroupManager: IGroupManager;
  public
    constructor Create;
    destructor Destroy; override;
    
    // IConnectionManager
    procedure Add(const Connection: IHubConnection);
    procedure Remove(const ConnectionId: string);
    function TryGet(const ConnectionId: string; out Connection: IHubConnection): Boolean;
    function Get(const ConnectionId: string): IHubConnection;
    function GetAll: TArray<IHubConnection>;
    function GetByGroup(const GroupName: string): TArray<IHubConnection>;
    function GetByUser(const UserId: string): TArray<IHubConnection>;
    function Count: Integer;
    function Contains(const ConnectionId: string): Boolean;
    
    // Configuration
    procedure SetGroupManager(const AGroupManager: IGroupManager);
  end;
  
  /// <summary>
  /// Thread-safe manager for connection groups.
  /// </summary>
  TGroupManager = class(TInterfacedObject, IGroupManager)
  private
    // GroupName -> Set of ConnectionIds
    FGroups: TDictionary<string, TList<string>>;
    // ConnectionId -> Set of GroupNames
    FConnectionGroups: TDictionary<string, TList<string>>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    
    // IGroupManager
    procedure AddToGroupAsync(const ConnectionId, GroupName: string);
    procedure RemoveFromGroupAsync(const ConnectionId, GroupName: string);
    procedure RemoveFromAllGroupsAsync(const ConnectionId: string);
    function GetGroupsForConnection(const ConnectionId: string): TArray<string>;
    function IsInGroup(const ConnectionId, GroupName: string): Boolean;
    
    // Additional methods
    function GetConnectionsInGroup(const GroupName: string): TArray<string>;
  end;

implementation

{ THubConnection }

constructor THubConnection.Create(const AConnectionId: string;
  ATransportType: TTransportType; const AUser: IClaimsPrincipal);
begin
  inherited Create;
  FConnectionId := AConnectionId;
  FTransportType := ATransportType;
  FState := csConnecting;
  FUser := AUser;
  FItems := TDictionary<string, TValue>.Create;
  FAbortTokenSource := TCancellationTokenSource.Create;
end;

destructor THubConnection.Destroy;
begin
  FAbortTokenSource.Free;
  FItems.Free;
  inherited;
end;

function THubConnection.GetConnectionId: string;
begin
  Result := FConnectionId;
end;

function THubConnection.GetTransportType: TTransportType;
begin
  Result := FTransportType;
end;

function THubConnection.GetState: TConnectionState;
begin
  Result := FState;
end;

function THubConnection.GetUser: IClaimsPrincipal;
begin
  Result := FUser;
end;

function THubConnection.GetUserIdentifier: string;
begin
  if FUser <> nil then
    Result := FUser.FindClaim('sub').Value // Standard claim for user ID
  else
    Result := '';
end;

function THubConnection.GetItems: TDictionary<string, TValue>;
begin
  Result := FItems;
end;

function THubConnection.GetAbortToken: ICancellationToken;
begin
  Result := FAbortTokenSource.Token;
end;

procedure THubConnection.SendAsync(const Message: string);
begin
  if Assigned(FOnSend) and (FState = csConnected) then
    FOnSend(Message);
end;

procedure THubConnection.Close(const Reason: string);
begin
  if FState <> csDisconnected then
  begin
    FState := csDisconnected;
    FAbortTokenSource.Cancel;
    if Assigned(FOnClose) then
      FOnClose(Reason);
  end;
end;

procedure THubConnection.SetOnSend(const Handler: TProc<string>);
begin
  FOnSend := Handler;
end;

procedure THubConnection.SetOnClose(const Handler: TProc<string>);
begin
  FOnClose := Handler;
end;

procedure THubConnection.SetState(AState: TConnectionState);
begin
  FState := AState;
end;

{ TConnectionManager }

constructor TConnectionManager.Create;
begin
  inherited Create;
  FConnections := TDictionary<string, IHubConnection>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TConnectionManager.Destroy;
begin
  FLock.Enter;
  try
    FConnections.Free;
  finally
    FLock.Leave;
  end;
  FLock.Free;
  inherited;
end;

procedure TConnectionManager.Add(const Connection: IHubConnection);
begin
  FLock.Enter;
  try
    FConnections.AddOrSetValue(Connection.ConnectionId, Connection);
  finally
    FLock.Leave;
  end;
end;

procedure TConnectionManager.Remove(const ConnectionId: string);
begin
  FLock.Enter;
  try
    // Remove from all groups first
    if FGroupManager <> nil then
      FGroupManager.RemoveFromAllGroupsAsync(ConnectionId);
      
    FConnections.Remove(ConnectionId);
  finally
    FLock.Leave;
  end;
end;

function TConnectionManager.TryGet(const ConnectionId: string;
  out Connection: IHubConnection): Boolean;
begin
  FLock.Enter;
  try
    Result := FConnections.TryGetValue(ConnectionId, Connection);
  finally
    FLock.Leave;
  end;
end;

function TConnectionManager.Get(const ConnectionId: string): IHubConnection;
begin
  if not TryGet(ConnectionId, Result) then
    raise EConnectionNotFoundException.CreateFmt('Connection not found: %s', [ConnectionId]);
end;

function TConnectionManager.GetAll: TArray<IHubConnection>;
begin
  FLock.Enter;
  try
    Result := FConnections.Values.ToArray;
  finally
    FLock.Leave;
  end;
end;

function TConnectionManager.GetByGroup(const GroupName: string): TArray<IHubConnection>;
var
  ConnectionIds: TArray<string>;
  Id: string;
  Conn: IHubConnection;
  ResultList: TList<IHubConnection>;
begin
  if FGroupManager = nil then
    Exit(nil);
    
  ConnectionIds := (FGroupManager as TGroupManager).GetConnectionsInGroup(GroupName);
  ResultList := TList<IHubConnection>.Create;
  try
    FLock.Enter;
    try
      for Id in ConnectionIds do
      begin
        if FConnections.TryGetValue(Id, Conn) then
          ResultList.Add(Conn);
      end;
    finally
      FLock.Leave;
    end;
    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

function TConnectionManager.GetByUser(const UserId: string): TArray<IHubConnection>;
var
  Pair: TPair<string, IHubConnection>;
  ResultList: TList<IHubConnection>;
begin
  ResultList := TList<IHubConnection>.Create;
  try
    FLock.Enter;
    try
      for Pair in FConnections do
      begin
        if Pair.Value.UserIdentifier = UserId then
          ResultList.Add(Pair.Value);
      end;
    finally
      FLock.Leave;
    end;
    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

function TConnectionManager.Count: Integer;
begin
  FLock.Enter;
  try
    Result := FConnections.Count;
  finally
    FLock.Leave;
  end;
end;

function TConnectionManager.Contains(const ConnectionId: string): Boolean;
begin
  FLock.Enter;
  try
    Result := FConnections.ContainsKey(ConnectionId);
  finally
    FLock.Leave;
  end;
end;

procedure TConnectionManager.SetGroupManager(const AGroupManager: IGroupManager);
begin
  FGroupManager := AGroupManager;
end;

{ TGroupManager }

constructor TGroupManager.Create;
begin
  inherited Create;
  FGroups := TDictionary<string, TList<string>>.Create;
  FConnectionGroups := TDictionary<string, TList<string>>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TGroupManager.Destroy;
var
  List: TList<string>;
begin
  FLock.Enter;
  try
    for List in FGroups.Values do
      List.Free;
    FGroups.Free;
    
    for List in FConnectionGroups.Values do
      List.Free;
    FConnectionGroups.Free;
  finally
    FLock.Leave;
  end;
  FLock.Free;
  inherited;
end;

procedure TGroupManager.AddToGroupAsync(const ConnectionId, GroupName: string);
var
  GroupConnections, ConnectionGroupsList: TList<string>;
begin
  FLock.Enter;
  try
    // Add to group's connection list
    if not FGroups.TryGetValue(GroupName, GroupConnections) then
    begin
      GroupConnections := TList<string>.Create;
      FGroups.Add(GroupName, GroupConnections);
    end;
    if not GroupConnections.Contains(ConnectionId) then
      GroupConnections.Add(ConnectionId);
      
    // Add to connection's group list
    if not FConnectionGroups.TryGetValue(ConnectionId, ConnectionGroupsList) then
    begin
      ConnectionGroupsList := TList<string>.Create;
      FConnectionGroups.Add(ConnectionId, ConnectionGroupsList);
    end;
    if not ConnectionGroupsList.Contains(GroupName) then
      ConnectionGroupsList.Add(GroupName);
  finally
    FLock.Leave;
  end;
end;

procedure TGroupManager.RemoveFromGroupAsync(const ConnectionId, GroupName: string);
var
  GroupConnections, ConnectionGroupsList: TList<string>;
begin
  FLock.Enter;
  try
    // Remove from group's connection list
    if FGroups.TryGetValue(GroupName, GroupConnections) then
    begin
      GroupConnections.Remove(ConnectionId);
      if GroupConnections.Count = 0 then
      begin
        FGroups.Remove(GroupName);
        GroupConnections.Free;
      end;
    end;
    
    // Remove from connection's group list
    if FConnectionGroups.TryGetValue(ConnectionId, ConnectionGroupsList) then
      ConnectionGroupsList.Remove(GroupName);
  finally
    FLock.Leave;
  end;
end;

procedure TGroupManager.RemoveFromAllGroupsAsync(const ConnectionId: string);
var
  ConnectionGroupsList, GroupConnections: TList<string>;
  GroupName: string;
  GroupsToRemove: TArray<string>;
begin
  FLock.Enter;
  try
    if FConnectionGroups.TryGetValue(ConnectionId, ConnectionGroupsList) then
    begin
      // Copy to array to avoid modification during iteration
      GroupsToRemove := ConnectionGroupsList.ToArray;
      
      for GroupName in GroupsToRemove do
      begin
        if FGroups.TryGetValue(GroupName, GroupConnections) then
        begin
          GroupConnections.Remove(ConnectionId);
          if GroupConnections.Count = 0 then
          begin
            FGroups.Remove(GroupName);
            GroupConnections.Free;
          end;
        end;
      end;
      
      FConnectionGroups.Remove(ConnectionId);
      ConnectionGroupsList.Free;
    end;
  finally
    FLock.Leave;
  end;
end;

function TGroupManager.GetGroupsForConnection(const ConnectionId: string): TArray<string>;
var
  ConnectionGroupsList: TList<string>;
begin
  FLock.Enter;
  try
    if FConnectionGroups.TryGetValue(ConnectionId, ConnectionGroupsList) then
      Result := ConnectionGroupsList.ToArray
    else
      Result := nil;
  finally
    FLock.Leave;
  end;
end;

function TGroupManager.IsInGroup(const ConnectionId, GroupName: string): Boolean;
var
  GroupConnections: TList<string>;
begin
  FLock.Enter;
  try
    if FGroups.TryGetValue(GroupName, GroupConnections) then
      Result := GroupConnections.Contains(ConnectionId)
    else
      Result := False;
  finally
    FLock.Leave;
  end;
end;

function TGroupManager.GetConnectionsInGroup(const GroupName: string): TArray<string>;
var
  GroupConnections: TList<string>;
begin
  FLock.Enter;
  try
    if FGroups.TryGetValue(GroupName, GroupConnections) then
      Result := GroupConnections.ToArray
    else
      Result := nil;
  finally
    FLock.Leave;
  end;
end;

end.
