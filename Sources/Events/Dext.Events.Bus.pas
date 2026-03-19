{***************************************************************************}
{                                                                           }
{           Dext Framework                                                  }
{                                                                           }
{           Copyright (C) 2025 Cesar Romero & Dext Contributors             }
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
{  Created: 2026-03-19                                                      }
{                                                                           }
{  Performance & safety design:                                              }
{                                                                           }
{  [1] REGISTRY (TEventHandlerRegistry)                                     }
{      - TMultiReadExclusiveWriteSynchronizer: write-once at startup,       }
{        read-many at runtime — zero write contention after app start.      }
{      - FBehaviors: TList<T> (concrete Dext type) — direct TRawList        }
{        access; for-in uses the record-based TListEnumerator<T>, avoiding  }
{        ARC overhead per iteration (vs. iterating through IList<T>).       }
{      - FHandlers / FEventBehaviors: IDictionary<Pointer, IList<T>> —     }
{        ARC-managed; no manual Free in destructor.                         }
{                                                                           }
{  [2] DISPATCH SNAPSHOT CACHE (TEventBus.FSnapshotCache)                  }
{      - TMultiReadExclusiveWriteSynchronizer guards the cache.             }
{        Hot-path (post-warm-up) takes a read lock — multiple threads read  }
{        concurrently with no contention. Cold-path (first Publish per       }
{        event type) takes a write lock once.                               }
{      - EventTypeName (ShortString→UnicodeString conversion) is cached in  }
{        the snapshot — zero string allocation on the hot-path.             }
{                                                                           }
{  [3] SINGLETON vs SCOPED BUS (FCreateScope flag)                         }
{      - AddEventBus (Singleton): FCreateScope=True. Publish creates a new  }
{        child scope per call — handlers are isolated from each other.      }
{      - AddScopedEventBus (Scoped): FCreateScope=False. Publish uses the   }
{        injected scoped provider directly — handlers share the request     }
{        unit-of-work (DbContext, Identity, etc.).                          }
{      - PublishBackground ALWAYS creates a fresh scope from FServiceProvider}
{        before TTask.Run. The IServiceScope is captured in the closure,    }
{        keeping it alive via ARC for the lifetime of the background task   }
{        regardless of whether the original request scope has ended.        }
{                                                                           }
{  [4] EXCEPTION AGGREGATION                                                }
{      - Each handler invocation is individually wrapped in try/except.     }
{        All handlers always run — a failing handler does not abort the     }
{        remaining ones.                                                     }
{      - Errors are collected lazily (IList<string> allocated only on       }
{        first failure — zero allocation on the happy path).                }
{      - After all handlers: if any failed, raise EEventDispatchAggregate.  }
{                                                                           }
{  [5] PER-EVENT BEHAVIORS                                                  }
{      - AcquireSnapshot merges global + event-specific behavior factories  }
{        into one TArray at cold-path time. Hot-path sees a single flat     }
{        array with zero branching.                                         }
{                                                                           }
{***************************************************************************}
unit Dext.Events.Bus;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Threading,
  System.SyncObjs,
  System.TypInfo,
  System.Rtti,
  Dext.Collections,
  Dext.Collections.Dict,
  Dext.DI.Interfaces,
  Dext.Events.Interfaces;

type
  THandlerFactoryList = IList<TFunc<IServiceProvider, TObject>>;

  /// <summary>
  ///   Frozen dispatch snapshot for one event type.
  ///   BehaviorFactories holds the pre-merged global + per-event behaviors.
  ///   Both arrays are immutable once built and safe to read concurrently.
  /// </summary>
  TDispatchSnapshot = record
    HandlerFactories: TArray<TFunc<IServiceProvider, TObject>>;
    BehaviorFactories: TArray<TFunc<IServiceProvider, TObject>>;
    EventTypeName: string;
  end;

  /// <summary>
  ///   Thread-safe registry. Write-once at startup, read-many during dispatch.
  ///   FBehaviors uses the concrete TList<T> type for record-based
  ///   enumeration (TListEnumerator<T> — no ARC per element access).
  /// </summary>
  TEventHandlerRegistry = class(TInterfacedObject, IEventHandlerRegistry)
  private
    FHandlers: IDictionary<Pointer, THandlerFactoryList>;
    FEventBehaviors: IDictionary<Pointer, THandlerFactoryList>;
    // Concrete TList<T>: direct TRawList backend, record enumerator on for-in.
    FBehaviors: TList<TFunc<IServiceProvider, TObject>>;
    FLock: TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterHandler(AEventType: PTypeInfo;
      const AFactory: TFunc<IServiceProvider, TObject>);
    procedure RegisterBehavior(
      const AFactory: TFunc<IServiceProvider, TObject>);
    procedure RegisterEventBehavior(AEventType: PTypeInfo;
      const AFactory: TFunc<IServiceProvider, TObject>);
    function GetHandlerFactories(AEventType: PTypeInfo):
      TArray<TFunc<IServiceProvider, TObject>>;
    function GetBehaviorFactories:
      TArray<TFunc<IServiceProvider, TObject>>;
    function GetEventBehaviorFactories(AEventType: PTypeInfo):
      TArray<TFunc<IServiceProvider, TObject>>;
  end;

  /// <summary>
  ///   Lightweight wrapper that binds a specific event type T to an IEventBus
  ///   instance. Created per-injection (transient); delegates synchronously to
  ///   the underlying bus. Thread-safe: all thread-safety is provided by TEventBus.
  ///   Register via Services.AddEventPublisher<TOrderCreatedEvent>.
  /// </summary>
  TEventPublisher<T> = class(TInterfacedObject, IEventPublisher<T>)
  private
    FBus: IEventBus;
  public
    constructor Create(const ABus: IEventBus);
    function Publish(const AEvent: T): TPublishResult;
    procedure PublishBackground(const AEvent: T);
  end;

  /// <summary>
  ///   High-performance event bus. See unit header for full design notes.
  /// </summary>
  TEventBus = class(TInterfacedObject, IEventBus)
    // Interface method resolution: maps IEventBus.Dispatch/DispatchBackground to
    // private implementation methods, avoiding any conflict with the inherited
    // TObject.Dispatch(var Message) virtual method.
    function IEventBus.Dispatch           = BusDispatch;
    procedure IEventBus.DispatchBackground = BusDispatchBackground;
  private
    FServiceProvider: IServiceProvider;
    FRegistry: IEventHandlerRegistry;
    FSnapshotCache: IDictionary<Pointer, TDispatchSnapshot>;
    FSnapshotLock: TMultiReadExclusiveWriteSynchronizer; // guards snapshot cache
    FCreateScope: Boolean;

    function AcquireSnapshot(AEventType: PTypeInfo): TDispatchSnapshot;
    function DoDispatch(AEventType: PTypeInfo; const AEvent: TValue;
      const ASnapshot: TDispatchSnapshot;
      const AScopedProvider: IServiceProvider): TPublishResult;
    // IEventBus implementation (bound via resolution clause above)
    function BusDispatch(AEventType: PTypeInfo;
      const AEvent: TValue): TPublishResult;
    procedure BusDispatchBackground(AEventType: PTypeInfo; const AEvent: TValue);
  public
    constructor Create(const AServiceProvider: IServiceProvider;
      const ARegistry: IEventHandlerRegistry;
      const ACreateScope: Boolean = True);
    destructor Destroy; override;

    // Typed convenience — not on the interface (E2535).
    // For interface-based callers use TEventBusExtensions.Publish<T>.
    function Publish<T>(const AEvent: T): TPublishResult;
    procedure PublishBackground<T>(const AEvent: T);
  end;

implementation

// Standalone pipeline builder — each call creates a new stack frame so the
// anonymous function captures its OWN copies of ABehavior and ANext.
// Inline vars inside a for-loop share one stack slot in some Delphi versions,
// causing all closures to alias the final iteration value (→ infinite recursion).

function WrapBehavior(const ABehavior: IEventBehavior;
  AEventType: PTypeInfo; const AEvent: TValue;
  const ANext: TEventNextDelegate): TEventNextDelegate;
begin
  Result :=
    procedure
    begin
      ABehavior.Intercept(AEventType, AEvent, ANext);
    end;
end;

{ TEventPublisher<T> }

constructor TEventPublisher<T>.Create(const ABus: IEventBus);
begin
  inherited Create;
  FBus := ABus;
end;

function TEventPublisher<T>.Publish(const AEvent: T): TPublishResult;
var
  V: TValue;
begin
  TValue.Make(@AEvent, TypeInfo(T), V);
  Result := FBus.Dispatch(TypeInfo(T), V);
end;

procedure TEventPublisher<T>.PublishBackground(const AEvent: T);
var
  V: TValue;
begin
  TValue.Make(@AEvent, TypeInfo(T), V);
  FBus.DispatchBackground(TypeInfo(T), V);
end;

{ TEventHandlerRegistry }

constructor TEventHandlerRegistry.Create;
begin
  inherited Create;
  FHandlers      := TCollections.CreateDictionary<Pointer, THandlerFactoryList>;
  FEventBehaviors := TCollections.CreateDictionary<Pointer, THandlerFactoryList>;
  FBehaviors     := TList<TFunc<IServiceProvider, TObject>>.Create(False);
  FLock          := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TEventHandlerRegistry.Destroy;
begin
  FLock.Free;
  FBehaviors.Free; // concrete TList — explicit free
  FEventBehaviors := nil;
  FHandlers := nil;
  inherited;
end;

procedure TEventHandlerRegistry.RegisterHandler(AEventType: PTypeInfo;
  const AFactory: TFunc<IServiceProvider, TObject>);
var
  List: THandlerFactoryList;
begin
  FLock.BeginWrite;
  try
    if not FHandlers.TryGetValue(AEventType, List) then
    begin
      List := TCollections.CreateList<TFunc<IServiceProvider, TObject>>;
      FHandlers.Add(AEventType, List);
    end;
    List.Add(AFactory);
  finally
    FLock.EndWrite;
  end;
end;

procedure TEventHandlerRegistry.RegisterBehavior(
  const AFactory: TFunc<IServiceProvider, TObject>);
begin
  FLock.BeginWrite;
  try
    FBehaviors.Add(AFactory);
  finally
    FLock.EndWrite;
  end;
end;

procedure TEventHandlerRegistry.RegisterEventBehavior(AEventType: PTypeInfo;
  const AFactory: TFunc<IServiceProvider, TObject>);
var
  List: THandlerFactoryList;
begin
  FLock.BeginWrite;
  try
    if not FEventBehaviors.TryGetValue(AEventType, List) then
    begin
      List := TCollections.CreateList<TFunc<IServiceProvider, TObject>>;
      FEventBehaviors.Add(AEventType, List);
    end;
    List.Add(AFactory);
  finally
    FLock.EndWrite;
  end;
end;

function TEventHandlerRegistry.GetHandlerFactories(
  AEventType: PTypeInfo): TArray<TFunc<IServiceProvider, TObject>>;
var
  List: THandlerFactoryList;
begin
  FLock.BeginRead;
  try
    if FHandlers.TryGetValue(AEventType, List) then
      Result := List.ToArray
    else
      Result := [];
  finally
    FLock.EndRead;
  end;
end;

function TEventHandlerRegistry.GetBehaviorFactories:
  TArray<TFunc<IServiceProvider, TObject>>;
var
  I: Integer;
  F: TFunc<IServiceProvider, TObject>; // record enumerator via concrete TList<T>
begin
  FLock.BeginRead;
  try
    SetLength(Result, FBehaviors.Count);
    I := 0;
    for F in FBehaviors do // uses TListEnumerator<T> — zero ARC overhead
    begin
      Result[I] := F;
      Inc(I);
    end;
  finally
    FLock.EndRead;
  end;
end;

function TEventHandlerRegistry.GetEventBehaviorFactories(
  AEventType: PTypeInfo): TArray<TFunc<IServiceProvider, TObject>>;
var
  List: THandlerFactoryList;
begin
  FLock.BeginRead;
  try
    if FEventBehaviors.TryGetValue(AEventType, List) then
      Result := List.ToArray
    else
      Result := [];
  finally
    FLock.EndRead;
  end;
end;

{ TEventBus }

constructor TEventBus.Create(const AServiceProvider: IServiceProvider;
  const ARegistry: IEventHandlerRegistry; const ACreateScope: Boolean);
begin
  inherited Create;
  FServiceProvider := AServiceProvider;
  FRegistry        := ARegistry;
  FCreateScope     := ACreateScope;
  FSnapshotCache   := TCollections.CreateDictionary<Pointer, TDispatchSnapshot>;
  FSnapshotLock    := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TEventBus.Destroy;
begin
  FSnapshotLock.Free;
  FSnapshotCache   := nil;
  FRegistry        := nil;
  FServiceProvider := nil;
  inherited;
end;

function TEventBus.AcquireSnapshot(AEventType: PTypeInfo): TDispatchSnapshot;
var
  GlobalBeh, EventBeh: TArray<TFunc<IServiceProvider, TObject>>;
  GlobalLen: Integer;
  I: Integer;
begin
  // Hot-path: read lock — multiple threads can read concurrently post-warm-up.
  FSnapshotLock.BeginRead;
  try
    if FSnapshotCache.TryGetValue(AEventType, Result) then
      Exit;
  finally
    FSnapshotLock.EndRead;
  end;

  // Cold-path: first Publish for this event type — build and store snapshot.
  FSnapshotLock.BeginWrite;
  try
    if not FSnapshotCache.TryGetValue(AEventType, Result) then
    begin
      Result.EventTypeName    := string(AEventType.Name);
      Result.HandlerFactories := FRegistry.GetHandlerFactories(AEventType);

      GlobalBeh := FRegistry.GetBehaviorFactories;
      EventBeh  := FRegistry.GetEventBehaviorFactories(AEventType);
      GlobalLen := Length(GlobalBeh);

      SetLength(Result.BehaviorFactories, GlobalLen + Length(EventBeh));
      for I := 0 to GlobalLen - 1 do
        Result.BehaviorFactories[I] := GlobalBeh[I];
      for I := 0 to High(EventBeh) do
        Result.BehaviorFactories[GlobalLen + I] := EventBeh[I];

      FSnapshotCache.AddOrSetValue(AEventType, Result);
    end;
  finally
    FSnapshotLock.EndWrite;
  end;
end;

function TEventBus.DoDispatch(AEventType: PTypeInfo; const AEvent: TValue;
  const ASnapshot: TDispatchSnapshot;
  const AScopedProvider: IServiceProvider): TPublishResult;
var
  Behaviors: TArray<IEventBehavior>;
  Pipeline: TEventNextDelegate;
  RttiCtx: TRttiContext;
  I, J: Integer;
  Errors: IList<string>;
begin
  Result.EventTypeName   := ASnapshot.EventTypeName;
  Result.HandlersInvoked := 0;
  Result.HandlersFailed  := 0;

  SetLength(Behaviors, Length(ASnapshot.BehaviorFactories));
  for I := 0 to High(ASnapshot.BehaviorFactories) do
  begin
    // Supports() is required here: 'as IEventBehavior' on a TObject-typed
    // variable is rejected by Delphi 11/12 (E2015) because TObject does not
    // statically declare IEventBehavior. Supports() queries at runtime via GUID.
    var BehaviorObj: TObject := ASnapshot.BehaviorFactories[I](AScopedProvider);
    if not Supports(BehaviorObj, IEventBehavior, Behaviors[I]) then
      raise EEventBusException.CreateFmt(
        'Behavior "%s" does not implement IEventBehavior', [BehaviorObj.ClassName]);
  end;

  for I := 0 to High(ASnapshot.HandlerFactories) do
  begin
    Inc(Result.HandlersInvoked);
    try
      // CRITICAL: inline-var creates a new stack slot per iteration so that
      // the closures below capture different variables for each handler.
      var HandlerObj: TObject := ASnapshot.HandlerFactories[I](AScopedProvider);
      var Method: TRttiMethod := RttiCtx.GetType(HandlerObj.ClassType)
                                         .GetMethod('Handle');

      // Delphi generic instantiations (IEventHandler<T>) can produce a
      // synthetic PTypeInfo for the Handle parameter that differs from the
      // canonical TypeInfo(T) passed through the TValue.  TRttiMethod.Invoke
      // uses pointer equality for const/var/out record parameters, so we must
      // re-wrap the raw data with the TypeInfo RTTI actually expects.
      // Safety: only re-wrap when the type NAMES match (same type, different
      // pointers).  A name mismatch means a wrong handler — let Invoke fail
      // with a clear error rather than silently reinterpreting record data.
      var LocalEvent: TValue := AEvent;
      var Params := Method.GetParameters;
      if (Length(Params) = 1) and (Params[0].ParamType <> nil)
         and (LocalEvent.TypeInfo <> Params[0].ParamType.Handle) then
      begin
        if string(LocalEvent.TypeInfo.Name) <> Params[0].ParamType.Name then
          raise EEventBusException.CreateFmt(
            'Handler "%s" expects "%s" but received "%s"',
            [HandlerObj.ClassName, Params[0].ParamType.Name,
             string(LocalEvent.TypeInfo.Name)]);
        TValue.Make(LocalEvent.GetReferenceToRawData,
                    Params[0].ParamType.Handle, LocalEvent);
      end;

      if Length(Behaviors) = 0 then
      begin
        // Fast-path: direct RTTI call — no closure allocation.
        Method.Invoke(HandlerObj, [LocalEvent]);
      end
      else
      begin
        // Pipeline path: build behavior chain inside-out.
        // Each local captures its own copy of HandlerObj / LocalEvent per iteration.
        var LocalHandlerObj: TObject := HandlerObj;
        var LocalMethod: TRttiMethod := Method;
        Pipeline :=
          procedure
          begin
            LocalMethod.Invoke(LocalHandlerObj, [LocalEvent]);
          end;

        // Wrap in behaviors — reverse order so first-registered runs outermost.
        for J := High(Behaviors) downto 0 do
          Pipeline := WrapBehavior(Behaviors[J], AEventType, LocalEvent, Pipeline);

        Pipeline();
      end;

    except
      on E: Exception do
      begin
        Inc(Result.HandlersFailed);
        if not Assigned(Errors) then
          Errors := TCollections.CreateList<string>;
        Errors.Add(Format('[%s] %s', [E.ClassName, E.Message]));
      end;
    end;
  end;

  if Assigned(Errors) and (Errors.Count > 0) then
    raise EEventDispatchAggregate.Create(
      Format('%d of %d handler(s) failed for event "%s"',
        [Result.HandlersFailed, Result.HandlersInvoked, Result.EventTypeName]),
      Errors.ToArray);
end;

function TEventBus.BusDispatch(AEventType: PTypeInfo;
  const AEvent: TValue): TPublishResult;
var
  Snapshot: TDispatchSnapshot;
  Scope: IServiceScope;
  ScopedProvider: IServiceProvider;
begin
  Snapshot := AcquireSnapshot(AEventType);

  if Length(Snapshot.HandlerFactories) = 0 then
  begin
    Result.EventTypeName   := Snapshot.EventTypeName;
    Result.HandlersInvoked := 0;
    Result.HandlersFailed  := 0;
    Exit;
  end;

  if FCreateScope then
  begin
    Scope          := FServiceProvider.CreateScope;
    ScopedProvider := Scope.ServiceProvider;
  end
  else
    ScopedProvider := FServiceProvider;

  Result := DoDispatch(AEventType, AEvent, Snapshot, ScopedProvider);
end;

procedure TEventBus.BusDispatchBackground(AEventType: PTypeInfo;
  const AEvent: TValue);
var
  EventCopy: TValue;
  BackgroundScope: IServiceScope;
  Snapshot: TDispatchSnapshot;
begin
  Snapshot := AcquireSnapshot(AEventType);

  if Length(Snapshot.HandlerFactories) = 0 then
    Exit;

  EventCopy       := AEvent;
  BackgroundScope := FServiceProvider.CreateScope;

  TTask.Run(
    procedure
    begin
      DoDispatch(AEventType, EventCopy, Snapshot, BackgroundScope.ServiceProvider);
    end
  );
end;

function TEventBus.Publish<T>(const AEvent: T): TPublishResult;
var
  V: TValue;
begin
  // Box to TValue and delegate — keeps the generic method body trivial,
  // avoiding a Delphi 11/12 compiler bug (E2018) with generic methods
  // returning record types on classes with interface method resolution clauses.
  TValue.Make(@AEvent, TypeInfo(T), V);
  Result := BusDispatch(TypeInfo(T), V);
end;

procedure TEventBus.PublishBackground<T>(const AEvent: T);
var
  V: TValue;
begin
  TValue.Make(@AEvent, TypeInfo(T), V);
  BusDispatchBackground(TypeInfo(T), V);
end;

end.
