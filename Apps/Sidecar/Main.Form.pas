unit Main.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ExtCtrls,
  Dext.Vcl.TrayIcon,
  Winapi.ShellAPI,
  Dext.Services.FileWatcher;

type
  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTrayIcon: TTrayIconEx;
    FWatcher: TDextFileWatcher;
    procedure OnTrayDashboardClick(Sender: TObject);
    procedure OnTrayDblClick(Sender: TObject);
    procedure OnFileChange(Sender: TObject; const Action: string; const FileName: string);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Hide the main form
  Application.ShowMainForm := False;
  Self.Visible := False;

  // Initialize Tray Icon
  FTrayIcon := TTrayIconEx.Create(Self, OnTrayDblClick);
  FTrayIcon.Hint := 'Dext Sidecar';
  FTrayIcon.BalloonHint := 'Dext Sidecar Running';
  
  // Add Menu Items
  FTrayIcon.AddMenuItem('Open Dashboard', OnTrayDashboardClick);
  FTrayIcon.AddMenuItem('-', nil);
  
  // Start File Watcher
  FWatcher := TDextFileWatcher.Create(GetCurrentDir);
  FWatcher.OnChange := OnFileChange;
  FWatcher.Start;

  // Fix Icon: Load system icon if Application icon is missing (e.g. no .res file)
  if FTrayIcon.Icon.Handle = 0 then
    FTrayIcon.Icon.Handle := LoadIcon(0, IDI_APPLICATION);
    
  // Force refresh
  FTrayIcon.Visible := False;
  FTrayIcon.Visible := True;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FWatcher) then
  begin
    FWatcher.Stop;
    FWatcher.Free;
  end;
  FTrayIcon.Free;
end;

procedure TMainForm.OnTrayDashboardClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://localhost:3030', nil, nil, SW_SHOWNORMAL);
end;



procedure TMainForm.OnTrayDblClick(Sender: TObject);
begin
  // Default behavior or open dashboard?
  OnTrayDashboardClick(Sender);
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
