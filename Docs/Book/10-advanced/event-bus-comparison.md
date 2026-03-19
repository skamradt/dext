# Event Bus Comparison: Dext vs Delphi Event Bus vs NX Horizon

Three Delphi event bus implementations with different design goals:

| | **Dext Event Bus** | **Delphi Event Bus (DEB)** | **NX Horizon** |
|---|---|---|---|
| Author | Cesar Romero | Daniele Spinetti | Aldin Alic (dalijap) |
| License | Apache 2.0 | Apache 2.0 | MIT |
| Source | `Sources/Events/` (this repo) | [github/spinettaro/delphi-event-bus](https://github.com/spinettaro/delphi-event-bus) | [github/dalijap/nx-horizon](https://github.com/dalijap/nx-horizon) |
| Min Delphi | 10.3 Rio | 2010 | XE4 |
| Code size | ~900 lines (7 units) | ~1 790 lines (4 files) | ~715 lines (1 file) |

---

## Registration Style

### Dext Event Bus

Explicit, type-safe, DI-driven. Registered once at startup; handlers are resolved from the DI container on every `Publish`.

```pascal
Services
  .AddEventBus
  .AddEventHandler<TOrderPlacedEvent, TOrderEmailHandler>
  .AddEventHandler<TOrderPlacedEvent, TOrderAuditHandler>
  .AddEventBehavior<TEventExceptionBehavior>
  .AddEventBehavior<TEventLoggingBehavior>;
```

### Delphi Event Bus (DEB)

Attribute-based. Any class method decorated with `[Subscribe]` becomes a handler. The object must register itself at runtime.

```pascal
// Subscribe attribute on a method
[Subscribe]
procedure OnOrderPlaced(AEvent: IOrderPlacedEvent);

// Register the subscriber at runtime
GlobalEventBus.RegisterSubscriberForEvents(Self);
// Unregister on destroy:
GlobalEventBus.UnregisterForEvents(Self);
```

### NX Horizon

Programmatic, lambda-friendly. Returns an `INxEventSubscription` handle that controls the subscription lifetime.

```pascal
FSubscription := NxHorizon.Instance.Subscribe<TOrderPlacedEvent>(
  Async,
  procedure(const AEvent: TOrderPlacedEvent)
  begin
    // handle event
  end);

// Unsubscribe:
FSubscription := nil; // ARC releases it
```

---

## Event Types

| | Dext | DEB | NX Horizon |
|---|---|---|---|
| Records | ✅ (recommended) | ❌ interfaces only | ✅ any type |
| Classes | ✅ | ✅ (must be interface) | ✅ |
| Interfaces | ✅ | ✅ only | ✅ |
| Primitives | ✅ | ❌ | ✅ (`Post<Integer>(42)`) |
| Named channels | ❌ | ✅ `[Channel('NAME')]` | ❌ (wrap with a record) |

Dext and NX Horizon both recommend **plain records** as events — zero heap allocation, full value semantics. DEB requires events to be **interfaces**, which introduces ARC overhead and circular-reference risk.

---

## DI Integration

| | Dext | DEB | NX Horizon |
|---|---|---|---|
| DI-aware handler resolution | ✅ full | ❌ | ❌ |
| Handler receives injected services | ✅ constructor injection | ❌ manual | ❌ manual |
| Scoped lifetime (per HTTP request) | ✅ `AddScopedEventBus` | ❌ | ❌ |
| Shares DbContext / identity with caller | ✅ (scoped mode) | ❌ | ❌ |

Dext is the only implementation with first-class DI integration. Handlers are ordinary classes whose dependencies are injected by the container — no static references, no manual wiring.

```pascal
// Dext: handler gets DbContext from the same HTTP request scope
TOrderEmailHandler = class(TInterfacedObject, IEventHandler<TOrderPlacedEvent>)
  constructor Create(
    const AMailer: IMailService;       // injected
    const ADbContext: TOrderDbContext; // same instance as the controller
    const ALogger: ILogger);          // same request context
```

---

## Thread Safety & Delivery Modes

### Dext

All internal state is guarded by `TMultiReadExclusiveWriteSynchronizer`. The snapshot cache is read-locked per dispatch, write-locked only on first warm-up. Background publishing creates a fresh DI scope before queuing the task.

```pascal
TEventBusExtensions.Publish<TOrderPlacedEvent>(FBus, Event);           // synchronous
TEventBusExtensions.PublishBackground<TOrderPlacedEvent>(FBus, Event); // fire-and-forget
// Or preferred: inject IEventPublisher<T> for typed calls
FPublisher.Publish(Event);
```

No thread-marshaling to the main thread — that is the application's responsibility. Keeps the bus decoupled from VCL/FMX.

### DEB

Four delivery modes via the `[Subscribe]` attribute:

| Mode | Description |
|------|-------------|
| `Posting` | Same thread as the caller |
| `Main` | Marshaled to the main (UI) thread |
| `Async` | Background thread |
| `Background` | Background unless already off main thread |

Global lock (`TCriticalSection`) serializes all registrations and posts — a bottleneck under high concurrency.

```pascal
[Subscribe(TThreadMode.Main)]
procedure OnOrderPlaced(AEvent: IOrderPlacedEvent);
```

### NX Horizon

Four delivery modes passed per subscription:

| Mode | Description |
|------|-------------|
| `Sync` | Same thread |
| `Async` | `TTask.Run` |
| `MainSync` | `TThread.Synchronize` |
| `MainAsync` | `TThread.Queue` |

Uses countdown events (`TCountdownEvent`) for clean shutdown. No global lock — individual subscription lists are independently protected, giving better concurrent throughput than DEB.

---

## Memory Management

| | Dext | DEB | NX Horizon |
|---|---|---|---|
| Handler lifetime | DI container (transient/scoped) | Caller owns subscriber | `INxEventSubscription` handle |
| Memory leak risk | None — DI manages lifecycle | ⚠️ if `UnregisterForEvents` not called | Low — ARC on subscription handle |
| Circular reference risk | None | ⚠️ interface events can create cycles | None |
| Safe in 24/7 processes | ✅ | ⚠️ | ✅ |

DEB's interface-based events combined with manual `RegisterSubscriberForEvents` / `UnregisterForEvents` pairing is a known source of memory leaks in long-running ERP systems if a module is unloaded without unregistering.

---

## Error Handling

| | Dext | DEB | NX Horizon |
|---|---|---|---|
| Handler exception wrapping | ✅ `TEventExceptionBehavior` | Silent (swallowed in async) | Silent |
| All handlers run even if one fails | ✅ | ❌ stops on first failure | Depends on delivery mode |
| Aggregate error report | ✅ `EEventDispatchAggregate` | ❌ | ❌ |
| Structured logging on failure | ✅ `TEventLoggingBehavior` | ❌ | ❌ |

Dext is explicit about failure: `TEventExceptionBehavior` ensures every handler runs, then raises an aggregate exception. `TEventLoggingBehavior` routes errors to `ILogger` before re-raising. Both are opt-in pipeline behaviors.

---

## Pipeline / Middleware

Only **Dext** has a first-class behavior pipeline:

```
Publish<T>(Event)
  └─ TEventExceptionBehavior (outer — catches & re-raises with context)
       └─ TEventLoggingBehavior (ILogger: entry/exit/timing/errors)
            └─ TOrderValidationBehavior (per-event: short-circuit if invalid)
                 └─ handler.Handle(Event)
```

| Feature | Dext | DEB | NX Horizon |
|---------|------|-----|-----------|
| Global behaviors (all events) | ✅ | ❌ | ❌ |
| Per-event behaviors | ✅ | ❌ | ❌ |
| Built-in logging behavior | ✅ `TEventLoggingBehavior` | ❌ | ❌ |
| Built-in exception behavior | ✅ `TEventExceptionBehavior` | ❌ | ❌ |
| Custom behaviors | ✅ implement `IEventBehavior` | ❌ | ❌ |

---

## Typed Publisher

Dext provides `IEventPublisher<T>` — a narrow facade for components that only emit one event type. Easier to mock in unit tests.

```pascal
// Only declares what this class can emit — no IEventBus exposure
TOrderService = class(TInterfacedObject, IOrderService)
  constructor Create(const APublisher: IEventPublisher<TOrderPlacedEvent>);
```

DEB and NX Horizon have no equivalent; callers always hold a reference to the full bus.

---

## Testing Support

### Dext

`TEventBusTracker` (`Dext.Events.Testing`) is a drop-in `IEventBus` replacement. No server needed, no threads — tracks published events in-memory.

```pascal
TEventBusTracker.Register(Services, Tracker);
// ...
Tracker.HasPublished<TOrderPlacedEvent>.Should.BeTrue;
Tracker.LastPublished<TOrderPlacedEvent>.CustomerId.Should.Equal(42);
```

### DEB

No dedicated testing utilities. Tests typically use a real `GlobalEventBus` instance, which carries global state between test cases.

### NX Horizon

No dedicated testing utilities. Lightweight design allows creating a separate `TNxHorizon` instance per test, which is a reasonable workaround.

---

## Application Lifecycle Events

| | Dext | DEB | NX Horizon |
|---|---|---|---|
| Bridges `IHostApplicationLifetime` | ✅ `AddEventBusLifecycle` | ❌ | ❌ |
| Application started/stopping/stopped events | ✅ | ❌ | ❌ |

---

## Dispatch Result

Only **Dext** returns structured dispatch statistics from `Publish`:

```pascal
var R := FPublisher.Publish(Event);
// R.HandlersInvoked, R.HandlersFailed, R.HandlersSucceeded, R.EventTypeName
```

---

## Code Complexity & Maintainability

| | Dext | DEB | NX Horizon |
|---|---|---|---|
| Lines of code | ~900 (7 units) | ~1 790 (4 files) | ~715 (1 file) |
| External dependencies | Dext.Core (DI, collections, logging) | None | None |
| Global singleton | ❌ (DI-provided) | ✅ `GlobalEventBus` | ✅ `NxHorizon.Instance` |
| Testable without framework | ✅ | ❌ (global state) | ✅ (own instance) |

NX Horizon's single-file design is the easiest to read and audit in isolation. Dext is larger but leverages the framework's existing DI, collections, and logging — no duplicated infrastructure.

---

## When to Use Which

| Scenario | Recommendation |
|----------|---------------|
| Web API with DI container (controller → handler shares DbContext) | **Dext** `AddScopedEventBus` |
| Background service / CLI app | **Dext** `AddEventBus` |
| VCL/FMX app that needs UI-thread marshaling | **DEB** (built-in `TThreadMode.Main`) |
| Existing codebase, no DI, minimal setup | **NX Horizon** |
| Long-running ERP / 24/7 process, memory safety critical | **Dext** or **NX Horizon** (avoid DEB) |
| Need named string channels | **DEB** |
| Need behavior pipeline (retry, audit, validation) | **Dext** |
| Publish to typed subscriber only (`IEventPublisher<T>`) | **Dext** |
| Simplest possible integration, single file | **NX Horizon** |

---

## Summary

```
NX Horizon  — smallest, safest, no DI, great for standalone use
DEB         — attribute-based elegance, VCL/FMX thread marshaling, memory leak risk
Dext        — DI-first, pipeline behaviors, scoped web integration, full testing support
```

Dext is the natural choice inside the Dext Framework. NX Horizon is a strong alternative for projects without a DI container. DEB is best for VCL/FMX applications where automatic UI-thread delivery (`TThreadMode.Main`) eliminates boilerplate.

---

*See also: [Event Bus](event-bus.md) — full Dext Event Bus documentation.*
