{***************************************************************************}
{           Dext Framework                                                  }
{           Copyright (C) 2025 Cesar Romero & Dext Contributors             }
{***************************************************************************}
unit Dext.Telemetry.Context;

interface

uses
  System.SysUtils,
  System.Classes,
  Dext.Types.UUID,
  Dext.Logging;

type
  PScopeNode = ^TScopeNode;
  TScopeNode = record
    Parent: PScopeNode;
    TraceId: TUUID;
    SpanId: TUUID;
    State: TObject; // Or structured data
    Message: string;
    // ... other context data
  end;

  /// <summary>
  ///   Manages the ambient trace context for the current thread.
  /// </summary>
  TraceContext = record
  private
    class threadvar FCurrent: PScopeNode;
  public
    /// <summary>
    ///   Gets the current active scope node.
    /// </summary>
    class function Current: PScopeNode; static;
    
    /// <summary>
    ///   Gets the current TraceId (from current scope or generates new if root).
    ///   Note: If no scope is active, implementation might decide to return TUUID.Empty 
    ///   or generate a "root" TraceId for the implicit global scope.
    /// </summary>
    class function CurrentTraceId: TUUID; static;
    
    /// <summary>
    ///   Gets the current SpanId.
    /// </summary>
    class function CurrentSpanId: TUUID; static;

    /// <summary>
    ///   Pushes a new scope onto the stack.
    /// </summary>
    class function Push(const Message: string; const TraceId, SpanId: TUUID): PScopeNode; overload; static;
    
    /// <summary>
    ///   Pops the current scope.
    /// </summary>
    class procedure Pop(Node: PScopeNode); static;
  end;

  /// <summary>
  ///   RAII wrapper for a scope.
  /// </summary>
  TScopeGuard = class(TInterfacedObject, IDisposable)
  private
    FNode: PScopeNode;
  public
    constructor Create(Node: PScopeNode);
    destructor Destroy; override;
    procedure Dispose;
  end;

implementation

{ TraceContext }

class function TraceContext.Current: PScopeNode;
begin
  Result := FCurrent;
end;

class function TraceContext.CurrentTraceId: TUUID;
begin
  if FCurrent <> nil then
    Result := FCurrent.TraceId
  else
    Result := TUUID.Empty; // Or should we generate one? For now, Empty means "no context".
end;

class function TraceContext.CurrentSpanId: TUUID;
begin
  if FCurrent <> nil then
    Result := FCurrent.SpanId
  else
    Result := TUUID.Empty;
end;

class function TraceContext.Push(const Message: string; const TraceId, SpanId: TUUID): PScopeNode;
begin
  New(Result);
  Result.Parent := FCurrent;
  
  // Inherit TraceId if not provided (and parent exists)
  if TraceId.IsEmpty then
  begin
    if FCurrent <> nil then
      Result.TraceId := FCurrent.TraceId
    else
      Result.TraceId := TUUID.NewV4; // Start new Trace
  end
  else
    Result.TraceId := TraceId;
    
  // Use provided SpanId or generate new
  if SpanId.IsEmpty then
    Result.SpanId := TUUID.NewV4
  else
    Result.SpanId := SpanId;
    
  Result.Message := Message;
  Result.State := nil;
  
  FCurrent := Result;
end;

class procedure TraceContext.Pop(Node: PScopeNode);
begin
  if FCurrent = Node then
  begin
    FCurrent := Node.Parent;
    Dispose(Node);
  end
  // Else: Stack corruption or misuse (disposing out of order). 
  // In robust systems we might define behavior (e.g., search stack). 
  // For high-perf, we assume LIFO discipline via IDisposable.
end;

{ TScopeGuard }

constructor TScopeGuard.Create(Node: PScopeNode);
begin
  inherited Create;
  FNode := Node;
end;

destructor TScopeGuard.Destroy;
begin
  // Ensure Dispose called? 
  // In ARC/Interface world, Destroy is called when RefCount=0. 
  // Dispose logic should be here or in Dispose method if explicit.
  // IDisposable.Dispose is for explicit cleanup, but we use Interfaces for automatic scope?
  // C# uses `using` which calls Dispose. Delphi uses `BeginScope(): IDisposable`.
  // When wrapper goes out of scope, _Release calls Destroy.
  // So we should put logic in Destroy or have Dispose call Destroy? 
  // Wait, `Dispose` is a method of `IDisposable`.
  // If user does not call Dispose, we still want it to pop when interface dies.
  // So logic should be in Destroy. `Dispose` can just set FNode to nil to avoid double free if meaningful.
  if FNode <> nil then
    TraceContext.Pop(FNode);
    
  inherited;
end;

procedure TScopeGuard.Dispose;
begin
  // Explicit dispose
  if FNode <> nil then
  begin
    TraceContext.Pop(FNode);
    FNode := nil;
  end;
end;

end.
