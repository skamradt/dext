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
{***************************************************************************}
unit Dext.Events.Interfaces;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  Dext.DI.Interfaces;

type
  /// <summary>
  ///   Delegate representing the continuation in the event handling pipeline.
  ///   Call ANext() to pass execution to the next behavior (or the handler).
  ///   Omitting the call short-circuits the pipeline for the current handler.
  /// </summary>
  TEventNextDelegate = reference to procedure;

  /// <summary>
  ///   Carries the result of a Publish<T> call.
  ///   HandlersInvoked counts all handlers that were started, including those
  ///   that raised — use HandlersFailed to distinguish successes.
  /// </summary>
  TPublishResult = record
    HandlersInvoked: Integer;
    HandlersFailed: Integer;
    EventTypeName: string;
    function HandlersSucceeded: Integer; inline;
  end;

  /// <summary>
  ///   Marker base interface for all event handlers.
  ///   Internal — implement IEventHandler<T> in application code.
  /// </summary>
  IEventHandler = interface
    ['{A1E74C28-3F9D-4B5A-8C0E-2D6F1A7B3C9E}']
  end;

  /// <summary>
  ///   Type-safe event handler. Implement this interface to handle event type T.
  ///   Handlers are resolved from the DI container on every Publish call, so
  ///   constructor injection of all service lifetimes is fully supported.
  ///   Register via Services.AddEventHandler<TEvent, THandler>().
  /// </summary>
  IEventHandler<T> = interface(IEventHandler)
    ['{B2F85D39-4E0A-5C6B-9D1F-3E7A2B8C4D0F}']
    procedure Handle(const AEvent: T);
  end;

  /// <summary>
  ///   Cross-cutting behavior applied around every handler invocation.
  ///   Behaviors form an ordered pipeline — each must call ANext() to continue.
  ///   Registered globally (all events) or per event type.
  ///   Register via Services.AddEventBehavior&lt;TBehavior&gt;() (global) or
  ///   Services.AddEventBehaviorFor&lt;TEvent, TBehavior&gt;() (per-event).
  ///
  ///   Named Intercept (not Handle) to clearly distinguish middleware from
  ///   the final IEventHandler&lt;T&gt;.Handle call.
  /// </summary>
  IEventBehavior = interface
    ['{C3A96E4A-5F1B-6D7C-0E2A-4F8B3C9D5E1A}']
    procedure Intercept(AEventType: PTypeInfo; const AEvent: TValue;
      const ANext: TEventNextDelegate);
  end;

  /// <summary>
  ///   Typed single-event publisher facade. Narrower dependency than IEventBus —
  ///   inject IEventPublisher<TOrderCreatedEvent> instead of IEventBus in
  ///   components that only ever publish one specific event type.
  ///
  ///   Follows the Interface Segregation Principle: the consumer declares
  ///   exactly which event it can emit, improving readability and testability
  ///   (mock a single-method interface rather than the whole bus).
  ///
  ///   Register via Services.AddEventPublisher<TOrderCreatedEvent>.
  ///   The implementation delegates to the registered IEventBus.
  /// </summary>
  IEventPublisher<T> = interface
    ['{A7B3C9D1-E5F4-4B2A-9E0D-1C8F7A6B5D3E}']
    function Publish(const AEvent: T): TPublishResult;
    procedure PublishBackground(const AEvent: T);
  end;

  /// <summary>
  ///   The central event bus.
  ///   Register as singleton via Services.AddEventBus() or as scoped
  ///   (shares the request DI scope) via Services.AddScopedEventBus().
  ///
  ///   IEventBus intentionally uses TValue-based methods — Delphi interfaces
  ///   cannot declare generic methods (E2535). For generic call-site sugar use
  ///   TEventBusExtensions.Publish&lt;T&gt; / PublishBackground&lt;T&gt;, or inject the
  ///   narrow IEventPublisher&lt;T&gt; (preferred — cleaner, ISP-compliant).
  /// </summary>
  IEventBus = interface
    ['{D4B07F5B-6A2C-7E8D-1F3B-5A9C4D0E6F2B}']

    /// <summary>
    ///   Dispatches AEvent synchronously. All handlers execute in registration
    ///   order. Returns dispatch statistics.
    ///   Raises EEventDispatchAggregate if any handler raised an exception.
    ///   Prefer TEventBusExtensions.Publish&lt;T&gt; for a typed call site.
    /// </summary>
    function Dispatch(AEventType: PTypeInfo;
      const AEvent: TValue): TPublishResult;

    /// <summary>
    ///   Dispatches AEvent on a background thread (fire-and-forget).
    ///   Returns immediately; a fresh DI scope is created for background
    ///   handlers regardless of bus lifetime.
    ///   Prefer TEventBusExtensions.PublishBackground&lt;T&gt; for a typed call site.
    /// </summary>
    procedure DispatchBackground(AEventType: PTypeInfo; const AEvent: TValue);
  end;

  /// <summary>
  ///   Internal registry: maps event TypeInfo to handler factory lists and
  ///   manages global + per-event behavior factory lists.
  ///   Populated at startup by AddEventHandler/AddEventBehavior.
  ///   Do not consume directly — use IEventBus.
  /// </summary>
  IEventHandlerRegistry = interface
    ['{E5C18A6C-7B3D-8F9E-2A4C-6B0D5E1F7A3C}']
    procedure RegisterHandler(AEventType: PTypeInfo;
      const AFactory: TFunc<IServiceProvider, TObject>);
    // Global behavior — applied to all event types
    procedure RegisterBehavior(
      const AFactory: TFunc<IServiceProvider, TObject>);
    // Per-event behavior — applied only to a specific event type
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
  ///   Typed call-site sugar over IEventBus.
  ///   Delphi E2535 prevents generic methods on interfaces, so these static
  ///   helpers box the event to TValue and delegate to IEventBus.Dispatch.
  ///   Defined here (not on the DI helper) to avoid the naming conflict with
  ///   TEventBusDIExtensions in Dext.Events.Extensions.
  ///
  ///   Usage:
  ///   <code>
  ///     // FBus: IEventBus (injected)
  ///     TEventBusExtensions.Publish&lt;TOrderPlacedEvent&gt;(FBus, Event);
  ///     // or inject the narrow IEventPublisher&lt;T&gt; (preferred — ISP):
  ///     FPublisher.Publish(Event);
  ///   </code>
  /// </summary>
  TEventBusExtensions = record
    class function Publish<T>(const ABus: IEventBus;
      const AEvent: T): TPublishResult; static; inline;
    class procedure PublishBackground<T>(const ABus: IEventBus;
      const AEvent: T); static; inline;
  end;

  EEventBusException = class(Exception);

  /// <summary>
  ///   Raised by TEventExceptionBehavior when a single handler fails.
  ///   Carries the event type name for diagnosing dispatch failures.
  /// </summary>
  EEventDispatchException = class(EEventBusException)
  public
    EventTypeName: string;
  end;

  /// <summary>
  ///   Raised by IEventBus.Publish&lt;T&gt; when one or more handlers raise an
  ///   exception. All handlers are always invoked before this is raised.
  ///   Errors contains one entry per failed handler (ClassName + Message).
  /// </summary>
  EEventDispatchAggregate = class(EEventBusException)
  public
    Errors: TArray<string>;
    constructor Create(const AMessage: string; const AErrors: TArray<string>);
  end;

implementation

{ TPublishResult }

function TPublishResult.HandlersSucceeded: Integer;
begin
  Result := HandlersInvoked - HandlersFailed;
end;

{ EEventDispatchAggregate }

constructor EEventDispatchAggregate.Create(const AMessage: string;
  const AErrors: TArray<string>);
begin
  inherited Create(AMessage);
  Errors := AErrors;
end;

{ TEventBusExtensions }

class function TEventBusExtensions.Publish<T>(const ABus: IEventBus;
  const AEvent: T): TPublishResult;
var
  V: TValue;
begin
  TValue.Make(@AEvent, TypeInfo(T), V);
  Result := ABus.Dispatch(TypeInfo(T), V);
end;

class procedure TEventBusExtensions.PublishBackground<T>(const ABus: IEventBus;
  const AEvent: T);
var
  V: TValue;
begin
  TValue.Make(@AEvent, TypeInfo(T), V);
  ABus.DispatchBackground(TypeInfo(T), V);
end;

end.
