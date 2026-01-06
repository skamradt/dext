program TestDextHubs;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  // Dext.Hubs units
  Dext.Web.Hubs.Interfaces,
  Dext.Web.Hubs.Types,
  Dext.Web.Hubs.Hub,
  Dext.Web.Hubs.Connections,
  Dext.Web.Hubs.Clients,
  Dext.Web.Hubs.Context,
  Dext.Web.Hubs.Protocol.Json;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure Check(Condition: Boolean; const TestName: string);
begin
  if Condition then
  begin
    Inc(TestsPassed);
    WriteLn('[PASS] ', TestName);
  end
  else
  begin
    Inc(TestsFailed);
    WriteLn('[FAIL] ', TestName);
  end;
end;

procedure TestTHubMessage;
var
  Msg: THubMessage;
begin
  WriteLn;
  WriteLn('=== THubMessage Tests ===');
  
  // Test Invocation
  Msg := THubMessage.Invocation('SendMessage', [TValue.From('Hello'), TValue.From(42)]);
  Check(Msg.MessageType = hmtInvocation, 'Invocation MessageType');
  Check(Msg.Target = 'SendMessage', 'Invocation Target');
  Check(Length(Msg.Arguments) = 2, 'Invocation Arguments count');
  
  // Test Completion
  Msg := THubMessage.Completion('inv123', TValue.From(True));
  Check(Msg.MessageType = hmtCompletion, 'Completion MessageType');
  Check(Msg.InvocationId = 'inv123', 'Completion InvocationId');
  
  // Test Ping
  Msg := THubMessage.Ping;
  Check(Msg.MessageType = hmtPing, 'Ping MessageType');
  
  // Test Close
  Msg := THubMessage.Close('Server shutdown');
  Check(Msg.MessageType = hmtClose, 'Close MessageType');
  Check(Msg.Error = 'Server shutdown', 'Close Error');
end;

procedure TestTJsonHubProtocol;
var
  Protocol: TJsonHubProtocol;
  Msg, Parsed: THubMessage;
  Json: string;
begin
  WriteLn;
  WriteLn('=== TJsonHubProtocol Tests ===');
  
  Protocol := TJsonHubProtocol.Create;
  try
    // Test protocol properties
    Check(Protocol.Name = 'json', 'Protocol Name');
    Check(Protocol.Version = 1, 'Protocol Version');
    Check(Protocol.TransferFormat = 'Text', 'Protocol TransferFormat');
    
    // Test Invocation serialization
    Msg := THubMessage.Invocation('ReceiveMessage', [TValue.From('Test')]);
    Json := Protocol.Serialize(Msg);
    Check(Pos('"type":1', Json) > 0, 'Serialize Invocation type');
    Check(Pos('"target":"ReceiveMessage"', Json) > 0, 'Serialize Invocation target');
    Check(Pos('"arguments":', Json) > 0, 'Serialize Invocation arguments');
    
    // Test IsCompleteMessage
    Check(Protocol.IsCompleteMessage(Json), 'IsCompleteMessage with RS');
    Check(not Protocol.IsCompleteMessage('{"type":1}'), 'IsCompleteMessage without RS');
    
    // Test Deserialization
    Parsed := Protocol.Deserialize(Json);
    Check(Parsed.MessageType = hmtInvocation, 'Deserialize MessageType');
    Check(Parsed.Target = 'ReceiveMessage', 'Deserialize Target');
    
    // Test Ping serialization
    Json := TJsonHubProtocol.SerializePing;
    Check(Pos('"type":6', Json) > 0, 'SerializePing');
    
    // Test Close serialization
    Json := TJsonHubProtocol.SerializeClose('Error message');
    Check(Pos('"type":7', Json) > 0, 'SerializeClose type');
    Check(Pos('"error":"Error message"', Json) > 0, 'SerializeClose error');
  finally
    Protocol.Free;
  end;
end;

procedure TestTConnectionManager;
var
  ConnectionManager: TConnectionManager;
  Connection1, Connection2: THubConnection;
  Retrieved: IHubConnection;
  AllConns: TArray<IHubConnection>;
begin
  WriteLn;
  WriteLn('=== TConnectionManager Tests ===');
  
  ConnectionManager := TConnectionManager.Create;
  try
    // Test Add
    Connection1 := THubConnection.Create('conn1', ttServerSentEvents);
    Connection2 := THubConnection.Create('conn2', ttServerSentEvents);
    
    ConnectionManager.Add(Connection1);
    ConnectionManager.Add(Connection2);
    
    Check(ConnectionManager.Count = 2, 'Add connections count');
    Check(ConnectionManager.Contains('conn1'), 'Contains conn1');
    Check(ConnectionManager.Contains('conn2'), 'Contains conn2');
    Check(not ConnectionManager.Contains('conn3'), 'Not contains conn3');
    
    // Test TryGet
    Check(ConnectionManager.TryGet('conn1', Retrieved), 'TryGet existing');
    Check(Retrieved.ConnectionId = 'conn1', 'TryGet correct connection');
    Check(not ConnectionManager.TryGet('notexist', Retrieved), 'TryGet non-existing');
    
    // Test Get
    Retrieved := ConnectionManager.Get('conn2');
    Check(Retrieved.ConnectionId = 'conn2', 'Get connection');
    
    // Test GetAll
    AllConns := ConnectionManager.GetAll;
    Check(Length(AllConns) = 2, 'GetAll count');
    
    // Test Remove
    ConnectionManager.Remove('conn1');
    Check(ConnectionManager.Count = 1, 'Remove count');
    Check(not ConnectionManager.Contains('conn1'), 'Removed connection gone');
    
  finally
    ConnectionManager.Free;
  end;
end;

procedure TestTGroupManager;
var
  GroupManager: TGroupManager;
  Groups: TArray<string>;
  Connections: TArray<string>;
begin
  WriteLn;
  WriteLn('=== TGroupManager Tests ===');
  
  GroupManager := TGroupManager.Create;
  try
    // Test AddToGroup
    GroupManager.AddToGroupAsync('conn1', 'group1');
    GroupManager.AddToGroupAsync('conn1', 'group2');
    GroupManager.AddToGroupAsync('conn2', 'group1');
    
    Check(GroupManager.IsInGroup('conn1', 'group1'), 'IsInGroup conn1 in group1');
    Check(GroupManager.IsInGroup('conn1', 'group2'), 'IsInGroup conn1 in group2');
    Check(GroupManager.IsInGroup('conn2', 'group1'), 'IsInGroup conn2 in group1');
    Check(not GroupManager.IsInGroup('conn2', 'group2'), 'IsInGroup conn2 not in group2');
    
    // Test GetGroupsForConnection
    Groups := GroupManager.GetGroupsForConnection('conn1');
    Check(Length(Groups) = 2, 'GetGroupsForConnection count');
    
    // Test GetConnectionsInGroup
    Connections := GroupManager.GetConnectionsInGroup('group1');
    Check(Length(Connections) = 2, 'GetConnectionsInGroup count');
    
    // Test RemoveFromGroup
    GroupManager.RemoveFromGroupAsync('conn1', 'group1');
    Check(not GroupManager.IsInGroup('conn1', 'group1'), 'RemoveFromGroup');
    Check(GroupManager.IsInGroup('conn1', 'group2'), 'RemoveFromGroup keeps other groups');
    
    // Test RemoveFromAllGroups
    GroupManager.RemoveFromAllGroupsAsync('conn1');
    Groups := GroupManager.GetGroupsForConnection('conn1');
    Check(Length(Groups) = 0, 'RemoveFromAllGroups');
    
  finally
    GroupManager.Free;
  end;
end;

procedure TestTNegotiateResponse;
var
  Response: TNegotiateResponse;
  Json: string;
begin
  WriteLn;
  WriteLn('=== TNegotiateResponse Tests ===');
  
  Response := TNegotiateResponse.Create('test-connection-id');
  
  Check(Response.ConnectionId = 'test-connection-id', 'ConnectionId');
  Check(Response.NegotiateVersion = 1, 'NegotiateVersion');
  Check(Length(Response.AvailableTransports) = 2, 'AvailableTransports count');
  
  Json := Response.ToJson;
  Check(Pos('"connectionId":"test-connection-id"', Json) > 0, 'ToJson connectionId');
  Check(Pos('"negotiateVersion":1', Json) > 0, 'ToJson negotiateVersion');
  Check(Pos('"availableTransports":', Json) > 0, 'ToJson availableTransports');
end;

procedure TestTHubContext;
var
  ConnectionManager: TConnectionManager;
  GroupManager: TGroupManager;
  HubContext: THubContext;
  Clients: IHubClients;
begin
  WriteLn;
  WriteLn('=== THubContext Tests ===');
  
  GroupManager := TGroupManager.Create;
  ConnectionManager := TConnectionManager.Create;
  ConnectionManager.SetGroupManager(GroupManager);
  
  HubContext := THubContext.Create(ConnectionManager, GroupManager);
  try
    Clients := HubContext.Clients;
    Check(Clients <> nil, 'GetClients returns non-nil');
    Check(HubContext.Groups <> nil, 'GetGroups returns non-nil');
  finally
    HubContext.Free;
  end;
end;

procedure TestTHubCallerContext;
var
  Context: THubCallerContext;
begin
  WriteLn;
  WriteLn('=== THubCallerContext Tests ===');
  
  Context := THubCallerContext.Create('my-connection', ttServerSentEvents);
  try
    Check(Context.ConnectionId = 'my-connection', 'ConnectionId');
    Check(Context.TransportType = ttServerSentEvents, 'TransportType');
    Check(Context.Items <> nil, 'Items not nil');
    Check(Context.UserIdentifier = '', 'UserIdentifier empty without user');
    
    // Test Items
    Context.Items.Add('key1', TValue.From('value1'));
    Check(Context.Items['key1'].AsString = 'value1', 'Items access');
  finally
    Context.Free;
  end;
end;

type
  TTestHub = class(THub)
  public
    procedure TestMethod(const Message: string);
  end;

procedure TTestHub.TestMethod(const Message: string);
begin
  // Test method - would normally send to clients
  Clients.All.SendAsync('Echo', [TValue.From(Message)]);
end;

procedure TestTHub;
var
  Hub: TTestHub;
  ConnectionManager: TConnectionManager;
  GroupManager: TGroupManager;
  CallerContext: IHubCallerContext;
  HubClients: IHubClients;
begin
  WriteLn;
  WriteLn('=== THub Tests ===');
  
  GroupManager := TGroupManager.Create;
  ConnectionManager := TConnectionManager.Create;
  ConnectionManager.SetGroupManager(GroupManager);
  
  Hub := TTestHub.Create;
  try
    CallerContext := THubCallerContext.Create('test-conn', ttServerSentEvents);
    HubClients := THubClients.Create(ConnectionManager, 'test-conn');
    
    Hub.SetContext(CallerContext, HubClients, GroupManager);
    
    // Just verify it doesn't crash - actual sending needs connections
    try
      Hub.TestMethod('Hello World');
      Check(True, 'Hub method invocation');
    except
      Check(False, 'Hub method invocation');
    end;
    
    // Test lifecycle methods
    try
      Hub.OnConnectedAsync;
      Hub.OnDisconnectedAsync(nil);
      Check(True, 'Hub lifecycle methods');
    except
      Check(False, 'Hub lifecycle methods');
    end;
  finally
    Hub.Free;
  end;
end;

procedure TestValueSerialization;
var
  Value: TValue;
  JsonValue: TJSONValue;
begin
  WriteLn;
  WriteLn('=== Value Serialization Tests ===');
  
  // Integer
  Value := TValue.From(42);
  JsonValue := TJsonHubProtocol.ValueToJson(Value);
  try
    Check(JsonValue is TJSONNumber, 'Integer to JSON');
    Check(TJSONNumber(JsonValue).AsInt = 42, 'Integer value');
  finally
    JsonValue.Free;
  end;
  
  // String
  Value := TValue.From('Hello');
  JsonValue := TJsonHubProtocol.ValueToJson(Value);
  try
    Check(JsonValue is TJSONString, 'String to JSON');
    Check(TJSONString(JsonValue).Value = 'Hello', 'String value');
  finally
    JsonValue.Free;
  end;
  
  // Boolean
  Value := TValue.From(True);
  JsonValue := TJsonHubProtocol.ValueToJson(Value);
  try
    Check(JsonValue is TJSONBool, 'Boolean to JSON');
    Check(TJSONBool(JsonValue).AsBoolean = True, 'Boolean value');
  finally
    JsonValue.Free;
  end;
  
  // Float
  Value := TValue.From(3.14);
  JsonValue := TJsonHubProtocol.ValueToJson(Value);
  try
    Check(JsonValue is TJSONNumber, 'Float to JSON');
  finally
    JsonValue.Free;
  end;
end;

begin
  try
    WriteLn('=========================================');
    WriteLn('   Dext.Hubs Unit Tests');
    WriteLn('=========================================');
    
    TestTHubMessage;
    TestTJsonHubProtocol;
    TestTConnectionManager;
    TestTGroupManager;
    TestTNegotiateResponse;
    TestTHubContext;
    TestTHubCallerContext;
    TestTHub;
    TestValueSerialization;
    
    WriteLn;
    WriteLn('=========================================');
    WriteLn(Format('Results: %d passed, %d failed', [TestsPassed, TestsFailed]));
    WriteLn('=========================================');
    
    if TestsFailed > 0 then
      ExitCode := 1
    else
      ExitCode := 0;
      
  except
    on E: Exception do
    begin
      WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  {$IFDEF DEBUG}
  WriteLn;
  Write('Press Enter to exit...');
  ReadLn;
  {$ENDIF}
end.
