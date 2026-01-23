unit Dext.Vcl.TrayIcon;

interface

uses
  System.Classes,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Menus,
  Winapi.Messages,
  Winapi.Windows,
  Dext.Vcl.FormDecorator;

type
  TTrayIconEx = class(Vcl.ExtCtrls.TTrayIcon)
  private
    FCloseToTray: Boolean;
    FPopupMenuDefault: TPopupMenu;
    FForm: TForm;
    FMinimized: Boolean;
    FMinimizeToTray: Boolean;
    FHideMainFormMenuItem: TMenuItem;
    FRestoreMainFormMenuItem: TMenuItem;
    FMenuItemClose: TMenuItem;
    FStartMinimized: Boolean;
    FOnCloseClick: TNotifyEvent;

    procedure ActionCloseClick(Sender: TObject);
    procedure ActionDoubleClick(Sender: TObject);
    procedure CreatePopupMenu;
  protected
    FWindowProcProxy: IWindowProcProxy;
    FShowMainForm: Boolean;
    FRestoreFormState: TWindowState;

    procedure SetCloseToTray(const Value: Boolean);
    procedure SetMinimizeToTray(const Value: Boolean);
    procedure TrayWindowProc(var Message: TMessage; var Handled: Boolean);
    procedure ShowApplicationTaskbarIcon;
  public
    constructor Create(Form: TForm; DoubleClickEvent: TNotifyEvent = nil); reintroduce;
    function AddMenuItem(const Caption: string; const MenuClick: TNotifyEvent; const Hint: string = ''): TMenuItem;
    procedure ToggleMinimizeRestore;

    property CloseToTray: Boolean read FCloseToTray write SetCloseToTray;
    property DefaultPopupMenu: TPopupMenu read FPopupMenuDefault write FPopupMenuDefault;
    property MenuItemClose: TMenuItem read FMenuItemClose;
    property Minimized: Boolean read FMinimized;
    property MinimizeToTray: Boolean read FMinimizeToTray write SetMinimizeToTray;
    property StartMinimized: Boolean read FStartMinimized write FStartMinimized;
    property OnCloseClick: TNotifyEvent read FOnCloseClick write FOnCloseClick;
  end;

const
  TRAY_ICON_NAME_SUFIX = '_ApplicationTrayIcon';

resourcestring
  STrayCloseApplicationCaption = 'Sair';
  STrayHideMainFormCaption     = 'Ocultar';
  STrayShowMainFormCaption     = 'Restaurar';

implementation

uses
  Dext.Vcl.Helpers; // Replaces Foundation.Vcl

{ TTrayIconEx }

constructor TTrayIconEx.Create(Form: TForm; DoubleClickEvent: TNotifyEvent = nil);
begin
  inherited Create(Form);
  FForm := Form;

  if FForm <> nil then
  begin
    FRestoreFormState := FForm.WindowState;
    FWindowProcProxy  := TWindowProcProxy.Create(FForm, TrayWindowProc);
    Self.Name         := FForm.Name + TRAY_ICON_NAME_SUFIX;
  end;

  Visible := True;
  FMinimized := True;
  FStartMinimized := True;

  if Assigned(DoubleClickEvent) then
    OnDblClick := DoubleClickEvent
  else
    OnDblClick := ActionDoubleClick;

  Icon.Assign(Application.Icon);
  BalloonHint := Application.Title;
  Hint := Application.Title;
  ShowBalloonHint;
  MinimizeToTray := True;
  CloseToTray := True;
  CreatePopupMenu;

  if FStartMinimized then
  begin
    FShowMainForm := False;
    Application.ShowMainForm := False;
  end
  else
    FShowMainForm := True;
end;

procedure TTrayIconEx.ActionCloseClick(Sender: TObject);
begin
  if Assigned(FOnCloseClick) then
  begin
    FOnCloseClick(Sender);
  end;

  Visible := False;
  FWindowProcProxy := nil;

  if (Application <> nil) and (Application.MainForm <> nil) then
  begin
    Application.MainForm.Close;
  end;

  Application.Terminate;
end;

procedure TTrayIconEx.ActionDoubleClick(Sender: TObject);
begin
  ToggleMinimizeRestore;
end;

procedure TTrayIconEx.SetCloseToTray(const Value: Boolean);
begin
  FCloseToTray := Value;
end;

procedure TTrayIconEx.SetMinimizeToTray(const Value: Boolean);
begin
  FMinimizeToTray := Value;
end;

function TTrayIconEx.AddMenuItem(const Caption: string; const MenuClick: TNotifyEvent; const Hint: string = ''): TMenuItem;
begin
  Result := FPopupMenuDefault.Add(Caption, MenuClick);
  Result.Hint := Hint;
end;

procedure TTrayIconEx.CreatePopupMenu;
begin
  FPopupMenuDefault := TPopupMenu.Create(Self);

  if FForm <> nil then
  begin
    FHideMainFormMenuItem := AddMenuItem(STrayHideMainFormCaption, ActionDoubleClick,
      STrayHideMainFormCaption + ' ' + Application.Title);

    FHideMainFormMenuItem.Enabled := False;

    FRestoreMainFormMenuItem := AddMenuItem(STrayShowMainFormCaption, ActionDoubleClick,
      STrayShowMainFormCaption + ' ' + Application.Title);

    AddMenuItem('-', nil);
  end;

  FMenuItemClose := AddMenuItem(STrayCloseApplicationCaption, ActionCloseClick);
  PopupMenu := FPopupMenuDefault;
end;

procedure TTrayIconEx.ShowApplicationTaskbarIcon;
begin
  if not IsWindowVisible(Application.Handle) then
  begin
    ShowWindow(Application.Handle, SW_SHOW);
  end;
end;

procedure TTrayIconEx.ToggleMinimizeRestore;
begin
  if FForm = nil then
  begin
    Exit;
  end;

  if FMinimized then
  begin
    ShowApplicationTaskbarIcon;
    Application.Restore;
    Application.BringToFront;
    if Application.MainForm <> nil then
    begin
      if not FShowMainForm then
      begin
        FShowMainForm := True;
        Application.MainForm.Show;
      end;

      if FRestoreFormState = wsMaximized then
        ShowWindow(Application.MainForm.Handle, SW_SHOWMAXIMIZED)
      else
        ShowWindow(Application.MainForm.Handle, SW_RESTORE);
    end;

    FHideMainFormMenuItem   .Enabled := True;
    FRestoreMainFormMenuItem.Enabled := False;
    Application.ShowMainForm         := True;
  end
  else
  begin
    Application.Minimize;
    ShowWindow(Application.Handle, SW_HIDE);

    if Application.MainForm <> nil then
    begin
      FRestoreFormState := Application.MainForm.WindowState;
      Application.MainForm.WindowState := wsMinimized;
      ShowWindow(Application.MainForm.Handle, SW_HIDE);
    end;

    FHideMainFormMenuItem.Enabled := False;
    FRestoreMainFormMenuItem.Enabled := True;
    Application.ShowMainForm := False;
  end;
  FMinimized := not FMinimized;
end;

procedure TTrayIconEx.TrayWindowProc(var Message: TMessage; var Handled: Boolean);

  procedure MinimizeToTrayOnClose;
  begin
    if CloseToTray then
    begin
      FMinimized := False;
      ToggleMinimizeRestore;
      Message.Msg    := WM_SYSCOMMAND;
      Message.wParam := SC_MINIMIZE;
    end;
  end;

var
  Shutdown: Boolean;
begin
  Shutdown := False;
  case Message.Msg of
    WM_SYSCOMMAND:
      begin
        if (Message.lParam <> WM_RBUTTONDOWN) then
        begin
          case (Message.wParam and $FFF0) of
            SC_MINIMIZE:
              if MinimizeToTray then
              begin
                FMinimized := False;
                ToggleMinimizeRestore;
              end;

            SC_CLOSE:
              MinimizeToTrayOnClose;
          end;
        end;
      end;

    WM_CLOSE:
      MinimizeToTrayOnClose;

    WM_QUERYENDSESSION:
      Shutdown := True;

    WM_ENDSESSION:
      /// TWMEndSession.EndSession
      Shutdown := LongBool(Message.LParam);
  end;

  if Shutdown then
  begin
    ActionCloseClick(FMenuItemClose);
  end;
end;

end.
