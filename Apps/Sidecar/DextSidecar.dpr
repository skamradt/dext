program DextSidecar;

uses
  Dext.MM,
  Vcl.Forms,
  Main.Form in 'Main.Form.pas' {MainForm},
  Dext.Hosting.CLI.Config,
  Dext.Vcl.TrayIcon in 'Lib\Dext.Vcl.TrayIcon.pas',
  Dext.Vcl.FormDecorator in 'Lib\Dext.Vcl.FormDecorator.pas',
  Dext.Vcl.Helpers in 'Lib\Dext.Vcl.Helpers.pas',
  Dext.Classes in 'Lib\Dext.Classes.pas',
  Dext.Services.FileWatcher in 'Dext.Services.FileWatcher.pas',
  Dext.Sidecar.Server in 'Dext.Sidecar.Server.pas',
  Vcl.Themes,
  Vcl.Styles;

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ShowMainForm := False;
  Application.Title := 'Dext Sidecar';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
