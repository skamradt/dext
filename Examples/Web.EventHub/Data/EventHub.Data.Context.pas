unit EventHub.Data.Context;

{***************************************************************************}
{                                                                           }
{           Web.EventHub - Database Context                                 }
{                                                                           }
{           DbContext with entity sets for ORM operations                   }
{                                                                           }
{***************************************************************************}

interface

uses
  // 1. Dext Specialized Units
  Dext.Entity.Core,
  // 2. Dext Facades
  Dext.Entity,
  // 3. Project Units
  EventHub.Domain.Entities;

type
  /// <summary>
  ///   Database context for the Event Hub system.
  ///   Uses properties to expose IDbSet<T> (recommended pattern).
  /// </summary>
  TEventHubDbContext = class(TDbContext)
  private
    function GetVenues: IDbSet<TVenue>;
    function GetEvents: IDbSet<TEvent>;
    function GetSpeakers: IDbSet<TSpeaker>;
    function GetAttendees: IDbSet<TAttendee>;
    function GetRegistrations: IDbSet<TRegistration>;
  public
    /// <summary>Physical venues where events take place</summary>
    property Venues: IDbSet<TVenue> read GetVenues;

    /// <summary>Events (conferences, meetups, workshops)</summary>
    property Events: IDbSet<TEvent> read GetEvents;

    /// <summary>Speakers presenting at events</summary>
    property Speakers: IDbSet<TSpeaker> read GetSpeakers;

    /// <summary>Registered attendees</summary>
    property Attendees: IDbSet<TAttendee> read GetAttendees;

    /// <summary>Event registrations (attendee -> event)</summary>
    property Registrations: IDbSet<TRegistration> read GetRegistrations;
  end;

implementation

{ TEventHubDbContext }

function TEventHubDbContext.GetVenues: IDbSet<TVenue>;
begin
  Result := Entities<TVenue>;
end;

function TEventHubDbContext.GetEvents: IDbSet<TEvent>;
begin
  Result := Entities<TEvent>;
end;

function TEventHubDbContext.GetSpeakers: IDbSet<TSpeaker>;
begin
  Result := Entities<TSpeaker>;
end;

function TEventHubDbContext.GetAttendees: IDbSet<TAttendee>;
begin
  Result := Entities<TAttendee>;
end;

function TEventHubDbContext.GetRegistrations: IDbSet<TRegistration>;
begin
  Result := Entities<TRegistration>;
end;

end.
