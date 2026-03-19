# Event Bus

The Dext Event Bus is a high-performance, DI-integrated in-process publish/subscribe system. It decouples the code that triggers business events from the code that reacts to them.

## Concepts

| Term | Description |
|------|-------------|
| **Event** | A plain record or class describing something that happened (`TOrderPlacedEvent`) |
| **Handler** | A class that reacts to one event type (`IEventHandler<T>`) |
| **Behavior** | Cross-cutting wrapper around handlers (logging, retry, timing) |
| **Bus** | The `IEventBus` service that dispatches events to handlers |

## Quick Start

### 1. Define an event

```pascal
// Events are plain records — zero allocation, stack-friendly
type
  TOrderPlacedEvent = record
    OrderId: Integer;
    CustomerId: Integer;
    TotalAmount: Currency;
  end;
```

### 2. Implement a handler

```pascal
uses Dext.Events;

type
  TOrderEmailHandler = class(TInterfacedObject,
    IEventHandler<TOrderPlacedEvent>)
  private
    FMailer: IMailService; // injected by DI
  public
    constructor Create(const AMailer: IMailService);
    procedure Handle(const AEvent: TOrderPlacedEvent);
  end;

procedure TOrderEmailHandler.Handle(const AEvent: TOrderPlacedEvent);
begin
  FMailer.Send(AEvent.CustomerId, 'Order #' + AEvent.OrderId.ToString + ' confirmed');
end;
```

### 3. Register at startup

```pascal
Services
  .AddEventBus
  .AddEventHandler<TOrderPlacedEvent, TOrderEmailHandler>
  .AddEventPublisher<TOrderPlacedEvent>        // typed publisher (preferred)
  .AddEventBehavior<TEventExceptionBehavior>   // structured error handling
  .AddEventBehavior<TEventLoggingBehavior>;    // structured ILogger output
```

### 4. Publish

**Preferred: inject `IEventPublisher<T>`** — typed, ISP-compliant, easy to mock:

```pascal
type
  TOrderService = class(TInterfacedObject, IOrderService)
  private
    FPublisher: IEventPublisher<TOrderPlacedEvent>;
  public
    constructor Create(const APublisher: IEventPublisher<TOrderPlacedEvent>);
    procedure PlaceOrder(const ARequest: TPlaceOrderRequest);
  end;

procedure TOrderService.PlaceOrder(const ARequest: TPlaceOrderRequest);
var
  Event: TOrderPlacedEvent;
begin
  Event.OrderId := NewOrderId;
  Event.CustomerId := ARequest.CustomerId;
  Event.TotalAmount := ARequest.Total;
  FPublisher.Publish(Event);   // typed — no generic parameter needed
end;
```

**Alternative: inject `IEventBus`** when a service publishes multiple event types:

```pascal
TEventBusExtensions.Publish<TOrderPlacedEvent>(FBus, Event);
```

---

## Multiple Handlers

Register as many handlers as needed for the same event — they all run in registration order:

```pascal
Services
  .AddEventBus
  .AddEventHandler<TOrderPlacedEvent, TOrderEmailHandler>     // sends email
  .AddEventHandler<TOrderPlacedEvent, TOrderAuditHandler>     // writes audit log
  .AddEventHandler<TOrderPlacedEvent, TInventoryHandler>;     // deducts stock
```

> [!IMPORTANT]
> A failing handler does **not** stop the others. All handlers always run.
> If any fail, `EEventDispatchAggregate` is raised after all handlers complete.
> Its `Errors` array contains one entry per failed handler.

---

## Bus Lifetime

### Singleton bus (default)

Each `Publish` call creates a fresh DI scope. Handlers are isolated from each other and from the caller. Best for background services and CLI apps.

```pascal
Services.AddEventBus;  // singleton IEventBus, new scope per Publish
```

### Scoped bus (web API)

`Publish` reuses the current DI scope (the HTTP request scope). Handlers share the same `DbContext`, identity, and unit-of-work as the controller that published the event. Best for web API controllers.

```pascal
Services.AddScopedEventBus;  // IEventBus created per HTTP request
```

---

## Publish Result

`Publish<T>` returns a `TPublishResult` with dispatch statistics:

```pascal
var Result := FPublisher.Publish(Event);

WriteLn(Result.EventTypeName);      // 'TOrderPlacedEvent'
WriteLn(Result.HandlersInvoked);    // total handlers that ran
WriteLn(Result.HandlersFailed);     // handlers that raised exceptions
WriteLn(Result.HandlersSucceeded);  // = Invoked - Failed
```

---

## Background Publishing

Fire-and-forget. Returns immediately; handlers run on a thread pool thread.

```pascal
FPublisher.PublishBackground(Event);
```

A fresh DI scope is created before the task is queued, so handlers can safely
access services regardless of the caller's scope lifetime.

> [!NOTE]
> Register `TEventExceptionBehavior` to capture background errors — they are not
> propagated to the caller.

---

## Typed Publisher

Inject `IEventPublisher<T>` instead of `IEventBus` in components that only ever
publish one event type. Narrower interface — easier to read, easier to mock.

```pascal
// Registration
Services
  .AddEventBus
  .AddEventHandler<TOrderPlacedEvent, TOrderEmailHandler>
  .AddEventPublisher<TOrderPlacedEvent>;  // <-- register typed publisher

// Injection — declares exactly what this class can emit
type
  TOrderService = class(TInterfacedObject, IOrderService)
  private
    FPublisher: IEventPublisher<TOrderPlacedEvent>;
  public
    constructor Create(const APublisher: IEventPublisher<TOrderPlacedEvent>);
    procedure PlaceOrder(const ARequest: TPlaceOrderRequest);
  end;

procedure TOrderService.PlaceOrder(const ARequest: TPlaceOrderRequest);
begin
  FPublisher.Publish(Event);            // typed — no generic parameter needed
  // or:
  FPublisher.PublishBackground(Event);
end;
```

---

## Behaviors (Pipeline)

Behaviors wrap every handler invocation. They form an ordered pipeline where
first-registered runs outermost (like middleware).

### Built-in behaviors

| Class | Effect | When to use |
|-------|--------|-------------|
| `TEventExceptionBehavior` | Catches handler exceptions, wraps as `EEventDispatchException` | Always recommended |
| `TEventLoggingBehavior` | Routes dispatch timing and errors to Dext's `ILogger` | Production |
| `TEventTimingBehavior` | Writes handler duration to `OutputDebugString` | Dev/debug only |

### TEventLoggingBehavior

The recommended production behavior. Resolves `ILoggerFactory` from DI (no extra registration needed) and writes structured entries under the category `'Dext.EventBus'`:

| Situation | Level | Message |
|-----------|-------|---------|
| Before handler | `Debug` | `Handling TMyEvent` |
| After success | `Debug` | `Handled TMyEvent in 3ms` |
| Handler raised | `Error` | `TMyEvent handler raised after 2ms: <message>` |

On failure the exception is re-raised so the pipeline (and `TEventExceptionBehavior` if present) continues to handle it.

```pascal
Services
  .AddEventBus
  .AddEventBehavior<TEventExceptionBehavior>  // outer: wraps with event context
  .AddEventBehavior<TEventLoggingBehavior>;   // inner: ILogger timing + error
```

### Global behaviors (all event types)

```pascal
Services
  .AddEventBus
  .AddEventBehavior<TEventExceptionBehavior>   // outermost
  .AddEventBehavior<TEventLoggingBehavior>;    // inner (production)
  // .AddEventBehavior<TEventTimingBehavior>;  // alternative: dev/debug only
```

### Per-event behaviors (one event type only)

Per-event behaviors run *inside* global behaviors, closer to the handler.

```pascal
Services
  .AddEventBus
  .AddEventBehavior<TEventExceptionBehavior>                          // global (outermost)
  .AddEventBehaviorFor<TOrderPlacedEvent, TOrderValidationBehavior>;  // per-event (inner)
```

### Writing a custom behavior

Implement `IEventBehavior.Intercept` and call `ANext()` to continue the pipeline.
The example below is a per-event retry behavior:

```pascal
type
  TRetryBehavior = class(TInterfacedObject, IEventBehavior)
  private
    const MaxAttempts = 3;
  public
    procedure Intercept(AEventType: PTypeInfo; const AEvent: TValue;
      const ANext: TEventNextDelegate);
  end;

procedure TRetryBehavior.Intercept(AEventType: PTypeInfo;
  const AEvent: TValue; const ANext: TEventNextDelegate);
var
  Attempt: Integer;
begin
  for Attempt := 1 to MaxAttempts do
  begin
    try
      ANext();   // <-- MUST call ANext to continue the pipeline
      Exit;      // success
    except
      on E: Exception do
        if Attempt = MaxAttempts then raise;
    end;
    Sleep(100 * Attempt); // back-off
  end;
end;
```

Register per-event so it only wraps `TOrderPlacedEvent` handlers:

```pascal
Services
  .AddEventBehavior<TEventExceptionBehavior>                      // global
  .AddEventBehavior<TEventLoggingBehavior>                        // global
  .AddEventBehaviorFor<TOrderPlacedEvent, TRetryBehavior>;        // per-event
```

> [!IMPORTANT]
> Always call `ANext()` unless you intentionally want to short-circuit the pipeline.

---

## Application Lifecycle Events

Bridge `IHostApplicationLifetime` signals to the event bus with a single call:

```pascal
Services
  .AddEventBus
  .AddEventHandler<TApplicationStartedEvent,  TMyStartupHandler>
  .AddEventHandler<TApplicationStoppingEvent, TMyShutdownHandler>
  .AddEventBusLifecycle;   // registers the lifecycle bridge as a hosted service
```

The three lifecycle events are plain empty records:

| Event | When |
|-------|------|
| `TApplicationStartedEvent` | After the host has fully started |
| `TApplicationStoppingEvent` | When a graceful shutdown begins |
| `TApplicationStoppedEvent` | After all hosted services have stopped |

---

## Testing

Use `TEventBusTracker` from `Dext.Events.Testing` as a drop-in `IEventBus`
replacement in unit tests.

```pascal
uses
  Dext.Testing,        // TTestFixture, TTest, Should
  Dext.Events.Testing; // TEventBusTracker

type
  TOrderServiceTests = class(TTestFixture)
  published
    procedure PlaceOrder_PublishesOrderPlacedEvent;
    procedure PlaceOrder_PopulatesEventFields;
  end;

procedure TOrderServiceTests.PlaceOrder_PublishesOrderPlacedEvent;
var
  Tracker: TEventBusTracker;
  Services: TDextServices;
  Provider: IServiceProvider;
  Service: IOrderService;
begin
  Services := TDextServices.Create;
  TEventBusTracker.Register(Services, Tracker)  // registers fake IEventBus
    .AddTransient<IOrderService, TOrderService>;

  Provider := Services.BuildServiceProvider;
  Service  := TServiceProviderExtensions.GetRequiredService<IOrderService>(Provider);

  Service.PlaceOrder(MakeRequest(42, 99.0));

  Tracker.HasPublished<TOrderPlacedEvent>.Should.BeTrue;
end;

procedure TOrderServiceTests.PlaceOrder_PopulatesEventFields;
var
  Tracker: TEventBusTracker;
  // ... same setup ...
  LastEvent: TOrderPlacedEvent;
begin
  // ...
  LastEvent := Tracker.LastPublished<TOrderPlacedEvent>;

  LastEvent.CustomerId.Should.Equal(42);
  LastEvent.TotalAmount.Should.Equal(99.0);
end;
```

### TEventBusTracker API

| Method | Description |
|--------|-------------|
| `HasPublished<T>` | Returns True if at least one T was published |
| `PublishedCount<T>` | Count of published T events |
| `LastPublished<T>` | Most recently published T (raises if none) |
| `GetPublished<T>` | All published T events as `TArray<T>` |
| `Clear` | Clears all recorded events (use between test cases) |

---

## Performance Notes

| Optimization | Detail |
|-------------|--------|
| **Snapshot cache** | Handler and behavior lists are cached per event type after the first `Publish`. Subsequent publishes have zero registry overhead. |
| **Fast path** | When no behaviors are registered, the handler is called directly — no closure allocation. |
| **Thread safety** | The snapshot cache is guarded by `TMultiReadExclusiveWriteSynchronizer`. Multiple threads can `Publish` concurrently after warm-up with no contention. |
| **Dext collections** | Internal collections use `IDictionary<K,V>` and `TList<T>` (backed by `TRawList` / `TRawDictionary`) for lower allocation pressure and cache-friendly iteration. |
| **Background dispatch** | `PublishBackground` uses `System.Threading.TTask.Run` (RTL thread pool). A fresh DI scope is captured before queuing. |

### Dext.Core dependency

`Dext.Events` requires `Dext.Core` for DI (`Dext.DI.Interfaces`), `TActivator` for handler wiring, and the high-performance collections listed above.

Dext.Core also ships higher-concurrency primitives (`Dext.Collections.Concurrent` with striped locks, `TSpinLock`), but the event bus does not use them. Its "configure once, publish from many threads" pattern is a natural fit for `TMultiReadExclusiveWriteSynchronizer`.

---

## Complete Registration Example

```pascal
procedure TStartup.ConfigureServices(const Services: TDextServices;
  const Configuration: IConfiguration);
begin
  Services
    // Choose bus lifetime:
    .AddEventBus                // singleton — new scope per Publish
    // .AddScopedEventBus       // scoped   — shares HTTP request scope

    // Handlers
    .AddEventHandler<TOrderPlacedEvent,   TOrderEmailHandler>
    .AddEventHandler<TOrderPlacedEvent,   TOrderAuditHandler>
    .AddEventHandler<TPaymentDoneEvent,   TPaymentNotifyHandler>

    // Typed publishers (optional)
    .AddEventPublisher<TOrderPlacedEvent>
    .AddEventPublisher<TPaymentDoneEvent>

    // Global behaviors (outermost first)
    .AddEventBehavior<TEventExceptionBehavior>    // always recommended
    .AddEventBehavior<TEventLoggingBehavior>      // production ILogger output

    // Per-event behaviors
    .AddEventBehaviorFor<TOrderPlacedEvent, TOrderValidationBehavior>

    // Lifecycle bridge (optional)
    .AddEventBusLifecycle;
end;
```

---

[← Background Services](background-services.md) | [Next: Configuration →](configuration.md)

---

*Comparing Dext Event Bus with other Delphi implementations? See [Event Bus Comparison](event-bus-comparison.md).*
