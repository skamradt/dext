unit Main.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, 
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Dext.Vcl.TrayIcon,
  Winapi.ShellAPI,
  Dext.Services.FileWatcher,
  Dext.Hosting.CLI.Config,
  Dext.Sidecar.Server;

type
  TMainForm = class(TForm)
    MainPanel: TPanel;
    TitleLabel: TLabel;
    StatusLabel: TLabel;
    UrlLabel: TLabel;
    HintLabel: TLabel;
    MinimizeButton: TButton;
    StartMinimizedCheckBox: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure UrlLabelClick(Sender: TObject);
    procedure MinimizeButtonClick(Sender: TObject);
    procedure StartMinimizedCheckBoxClick(Sender: TObject);
  private
    FTrayIcon: TTrayIconEx;
    FWatcher: TDextFileWatcher;
    FConfig: TDextGlobalConfig;
    FServer: TSidecarServer;
    FForceClose: Boolean;
    procedure OnTrayDashboardClick(Sender: TObject);
    procedure OnTrayRestoreClick(Sender: TObject);
    procedure OnTrayDblClick(Sender: TObject);
    procedure OnTrayCloseClick(Sender: TObject);
    procedure OnFileChange(Sender: TObject; const Action: string; const FileName: string);
    procedure MinimizeToTray;
    procedure RestoreFromTray;
    procedure LoadConfig;
    procedure SaveConfig;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FForceClose := False;
  
  // Load configuration
  FConfig := TDextGlobalConfig.Create;
  LoadConfig;
  
  // Update checkbox state
  StartMinimizedCheckBox.Checked := FConfig.StartMinimized;

  // Initialize and start embedded server
  FServer := TSidecarServer.Create(3030);
  try
    FServer.Start;
  except
    on E: Exception do
      OutputDebugString(PChar('DextSidecar: Server Start Error: ' + E.Message));
  end;
  
  // Update labels
  UrlLabel.Caption := FServer.GetUrl;
  if FServer.Running then
    StatusLabel.Caption := 'Server running at:'
  else
    StatusLabel.Caption := 'Server failed to start';

  // Initialize Tray Icon
  // Initialize Tray Icon
  FTrayIcon := TTrayIconEx.Create(Self, OnTrayDblClick);
  FTrayIcon.Hint := 'Dext Sidecar';
  FTrayIcon.BalloonHint := 'Dext Sidecar Running';
  FTrayIcon.BalloonFlags := Integer(bfInfo);
  FTrayIcon.StartMinimized := FConfig.StartMinimized;
  
  // Assign rights to icon BEFORE showing it
  if (Application.Icon <> nil) and (Application.Icon.Handle <> 0) then
    FTrayIcon.Icon.Assign(Application.Icon)
  else
    FTrayIcon.Icon := Application.Icon; // Fallback
  
  // Add Menu Items
  FTrayIcon.AddMenuItem('Open Dashboard', OnTrayDashboardClick);
  FTrayIcon.AddMenuItem('Restore Window', OnTrayRestoreClick);
  // Note: 'Sair' (Exit) menu item is automatically added by TTrayIconEx
  
  // Set the close click handler for proper shutdown
  FTrayIcon.OnCloseClick := OnTrayCloseClick;
  
  // Start File Watcher
  FWatcher := TDextFileWatcher.Create(GetCurrentDir);
  FWatcher.OnChange := OnFileChange;
  FWatcher.Start;

  // Show Tray Icon finally
  FTrayIcon.Visible := True;
  
  // Only show balloon if starting minimized to notify user
  if FConfig.StartMinimized then
    FTrayIcon.ShowBalloonHint;
  Application.ProcessMessages;

  // Check if should start minimized
  if FConfig.StartMinimized then
  begin
    Application.ShowMainForm := False;
    Self.Visible := False;
    // Open browser automatically when starting minimized
    ShellExecute(0, 'open', PChar(FServer.GetUrl), nil, nil, SW_SHOWNORMAL);
  end
  else
  begin
    Application.ShowMainForm := True;
    Self.Visible := True;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Stop server
  if Assigned(FServer) then
  begin
    FServer.Stop;
    FreeAndNil(FServer);
  end;
  
  if Assigned(FWatcher) then
  begin
    FWatcher.Stop;
    FreeAndNil(FWatcher);
  end;

  if Assigned(FTrayIcon) then
    FreeAndNil(FTrayIcon);
    
  if Assigned(FConfig) then
    FreeAndNil(FConfig);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // If force close flag is set, allow closing (server already stopped)
  if FForceClose then
  begin
    CanClose := True;
    Exit;
  end;
  
  // Only minimize to tray if checkbox is checked
  if StartMinimizedCheckBox.Checked then
  begin
    CanClose := False;
    MinimizeToTray;
  end
  else
  begin
    // Stop the server before closing
    if Assigned(FServer) then
      FServer.Stop;

    CanClose := True;
  end;
end;

procedure TMainForm.LoadConfig;
begin
  try
    FConfig.Load;
  except
  end;
end;

procedure TMainForm.SaveConfig;
begin
  try
    FConfig.Save;
  except
  end;
end;

procedure TMainForm.UrlLabelClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(FServer.GetUrl), nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.MinimizeButtonClick(Sender: TObject);
begin
  MinimizeToTray;
end;

procedure TMainForm.StartMinimizedCheckBoxClick(Sender: TObject);
begin
  FConfig.StartMinimized := StartMinimizedCheckBox.Checked;
  SaveConfig;
end;

procedure TMainForm.MinimizeToTray;
begin
  Self.Hide;
  FTrayIcon.BalloonHint := 'Dext Sidecar is still running';
  FTrayIcon.ShowBalloonHint;
end;

procedure TMainForm.RestoreFromTray;
begin
  Self.Show;
  Self.WindowState := wsNormal;
  Application.BringToFront;
  Self.BringToFront;
end;

procedure TMainForm.OnTrayDashboardClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(FServer.GetUrl), nil, nil, SW_SHOWNORMAL);
end;

procedure TMainForm.OnTrayRestoreClick(Sender: TObject);
begin
  RestoreFromTray;
end;

procedure TMainForm.OnTrayDblClick(Sender: TObject);
begin
  RestoreFromTray;
end;

procedure TMainForm.OnTrayCloseClick(Sender: TObject);
begin
  // Set force close flag to bypass minimize-to-tray behavior
  FForceClose := True;
  
  // Stop the server before closing
  if Assigned(FServer) then
  begin
    FServer.Stop;
  end;
end;

procedure TMainForm.OnFileChange(Sender: TObject; const Action: string; const FileName: string);
begin
  OutputDebugString(PChar('DextSidecar: File ' + Action + ': ' + FileName));
  if Assigned(FTrayIcon) then
  begin
    FTrayIcon.BalloonHint := 'File Changed: ' + ExtractFileName(FileName);
    FTrayIcon.ShowBalloonHint;
  end;
end;

end.
