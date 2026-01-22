unit Dext.Hosting.CLI.Hubs.Dashboard;

interface

uses
  System.SysUtils,
  Dext.Web.Hubs;

type
  TDashboardHub = class(THub)
  public
    // Client invokes this to say hello (optional check)
    procedure Ping;
  end;

implementation

{ TDashboardHub }

procedure TDashboardHub.Ping;
begin
  // Just keeps connection alive or verifies connectivity
end;

end.
