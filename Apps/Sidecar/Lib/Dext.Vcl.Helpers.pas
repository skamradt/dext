unit Dext.Vcl.Helpers;

interface

uses
  System.Classes,
  Vcl.Menus,
  Vcl.ActnList;

type
  TMenuHelper = class helper for TMenu
  public
    function Add(Action: TBasicAction): TMenuItem; overload;
    function Add(const Caption: string; NotifyMethod: TNotifyEvent): TMenuItem; overload;
    function Add(const Caption: string; NotifyMethod: TNotifyEvent; ImageIndex: Integer): TMenuItem; overload;

    function Add(Parent: TMenuItem; Action: TBasicAction): TMenuItem; overload;
    function Add(Parent: TMenuItem; const Caption: string; NotifyMethod: TNotifyEvent): TMenuItem; overload;
    function Add(Parent: TMenuItem; const Caption: string; NotifyMethod: TNotifyEvent; ImageIndex: Integer): TMenuItem; overload;

    procedure AddSeparator;
  end;

implementation

{ TMenuHelper }

function TMenuHelper.Add(const Caption: string; NotifyMethod: TNotifyEvent): TMenuItem;
begin
  Result := Add(Caption, NotifyMethod, -1);
end;

function TMenuHelper.Add(const Caption: string; NotifyMethod: TNotifyEvent; ImageIndex: Integer): TMenuItem;
begin
  Result := Add(Items, Caption, NotifyMethod, ImageIndex);
end;

function TMenuHelper.Add(Action: TBasicAction): TMenuItem;
begin
  Result := Add(Items, Action);
end;

function TMenuHelper.Add(Parent: TMenuItem; const Caption: string; NotifyMethod: TNotifyEvent): TMenuItem;
begin
  Result := Add(Parent, Caption, NotifyMethod, -1);
end;

procedure TMenuHelper.AddSeparator;
begin
  Add('-', TNotifyEvent(nil));
end;

function TMenuHelper.Add(Parent: TMenuItem; const Caption: string; NotifyMethod: TNotifyEvent; ImageIndex: Integer): TMenuItem;
begin
  Assert(Parent <> nil);
  Result := TMenuItem.Create(Self);
  Result.Caption := Caption;
  Result.OnClick := NotifyMethod;
  Result.ImageIndex := ImageIndex;
  Parent.Add(Result);
end;

function TMenuHelper.Add(Parent: TMenuItem; Action: TBasicAction): TMenuItem;
begin
  Result := Add(Parent, '', TNotifyEvent(nil));
  Result.Action := Action;
end;

end.
