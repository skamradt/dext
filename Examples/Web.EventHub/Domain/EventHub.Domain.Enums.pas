unit EventHub.Domain.Enums;

{***************************************************************************}
{                                                                           }
{           Web.EventHub - Domain Enums                                     }
{                                                                           }
{           Enumeration types for the Event Hub system                      }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Core.SmartTypes;

type
  /// <summary>
  ///   Status of an event in its lifecycle
  /// </summary>
  TEventStatus = (
    esDraft,        // Event created but not yet visible to the public
    esPublished,    // Event is published and accepting registrations
    esCanceled,     // Event was canceled by the organizer
    esFinished      // Event has concluded
  );
  TEventStatusType = Prop<TEventStatus>;

  /// <summary>
  ///   Status of a registration (attendee -> event)
  /// </summary>
  TRegistrationStatus = (
    rsConfirmed,    // Attendee has a confirmed spot
    rsWaitList,     // Attendee is on the waiting list
    rsCanceled      // Registration was canceled
  );
  TRegistrationStatusType = Prop<TRegistrationStatus>;

implementation

end.
