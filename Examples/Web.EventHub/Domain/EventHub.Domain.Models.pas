unit EventHub.Domain.Models;

{***************************************************************************}
{                                                                           }
{           Web.EventHub - Domain Models (DTOs)                             }
{                                                                           }
{           Request/Response records for API communication                  }
{                                                                           }
{***************************************************************************}

interface

uses
  // 1. Delphi Units
  System.SysUtils,
  // 2. Dext Facades
  Dext.Web,
  // 3. Project Units
  EventHub.Domain.Enums;

type
  { ======================================================================== }
  { Auth DTOs                                                                }
  { ======================================================================== }

  TLoginRequest = record
    Username: string;
    Password: string;
  end;

  TLoginResponse = record
    Token: string;
    ExpiresIn: Integer;
  end;

  { ======================================================================== }
  { Venue DTOs                                                               }
  { ======================================================================== }

  TCreateVenueRequest = record
    Name: string;
    Address: string;
    City: string;
    Capacity: Integer;
  end;

  { ======================================================================== }
  { Event DTOs                                                               }
  { ======================================================================== }

  TCreateEventRequest = record
    VenueId: Integer;
    Title: string;
    Description: string;
    StartDate: TDateTime;
    EndDate: TDateTime;
    MaxCapacity: Integer;
  end;

  TUpdateEventRequest = record
    [FromRoute('id')]
    EventId: Integer;
    Title: string;
    Description: string;
    StartDate: TDateTime;
    EndDate: TDateTime;
    MaxCapacity: Integer;
  end;

  TPublishEventRequest = record
    [FromRoute('id')]
    EventId: Integer;
  end;

  TCancelEventRequest = record
    [FromRoute('id')]
    EventId: Integer;
  end;

  TEventResponse = record
    Id: Integer;
    VenueId: Integer;
    VenueName: string;
    Title: string;
    Description: string;
    StartDate: TDateTime;
    EndDate: TDateTime;
    MaxCapacity: Integer;
    ConfirmedCount: Integer;
    AvailableSlots: Integer;
    Status: TEventStatus;
    SpeakerCount: Integer;
  end;

  { ======================================================================== }
  { Speaker DTOs                                                             }
  { ======================================================================== }

  TAddSpeakerRequest = record
    [FromRoute('eventId')]
    EventId: Integer;
    // Body fields
    Name: string;
    Bio: string;
    Email: string;
  end;

  TSpeakerResponse = record
    Id: Integer;
    EventId: Integer;
    Name: string;
    Bio: string;
    Email: string;
  end;

  { ======================================================================== }
  { Attendee DTOs                                                            }
  { ======================================================================== }

  TRegisterAttendeeRequest = record
    Name: string;
    Email: string;
    Phone: string;
  end;

  TAttendeeResponse = record
    Id: Integer;
    Name: string;
    Email: string;
    Phone: string;
  end;

  { ======================================================================== }
  { Registration DTOs                                                        }
  { ======================================================================== }

  TCreateRegistrationRequest = record
    EventId: Integer;
    AttendeeId: Integer;
  end;

  TCancelRegistrationRequest = record
    [FromRoute('id')]
    RegistrationId: Integer;
  end;

  TRegistrationResponse = record
    Id: Integer;
    EventId: Integer;
    EventTitle: string;
    AttendeeId: Integer;
    AttendeeName: string;
    Status: TRegistrationStatus;
    RegisteredAt: TDateTime;
  end;

  { ======================================================================== }
  { Metrics DTOs                                                             }
  { ======================================================================== }

  TDashboardMetrics = record
    TotalEvents: Integer;
    PublishedEvents: Integer;
    TotalAttendees: Integer;
    TotalRegistrations: Integer;
    ConfirmedRegistrations: Integer;
    WaitListRegistrations: Integer;
  end;

implementation

end.
