unit Settings.Service;

interface

uses
  System.SysUtils;

type
  ISettingsService = interface
    ['{C1D2E3F4-A5B6-47C8-9D0E-1F2A3B4C5D6E}']
    procedure UpdateProfile(const Name, Email: string);
    function UpdatePassword(const Current, New, Confirm: string; out Error: string): Boolean;
  end;

  TSettingsService = class(TInterfacedObject, ISettingsService)
  public
    procedure UpdateProfile(const Name, Email: string);
    function UpdatePassword(const Current, New, Confirm: string; out Error: string): Boolean;
  end;

implementation

{ TSettingsService }

procedure TSettingsService.UpdateProfile(const Name, Email: string);
begin
  // Simulate DB update
  // In a real app, inject TUser or similar context/service here
end;

function TSettingsService.UpdatePassword(const Current, New, Confirm: string; out Error: string): Boolean;
begin
  if New <> Confirm then
  begin
    Error := 'New passwords do not match';
    Exit(False);
  end;
  
  if Length(New) < 6 then
  begin
    Error := 'Password must be at least 6 characters';
    Exit(False);
  end;

  Result := True;
end;

end.
