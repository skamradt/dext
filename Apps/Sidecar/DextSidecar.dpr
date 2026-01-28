program DextSidecar;

uses
  Dext.MM,
  Vcl.Forms,
  Dext.Hosting.CLI.Config,
  Main.Form in 'Main.Form.pas' {MainForm},
  Dext.Classes in 'Lib\Dext.Classes.pas',
  Dext.Dashboard.Routes in '..\..\Sources\Dashboard\Dext.Dashboard.Routes.pas',
  Dext.Services.FileWatcher in 'Dext.Services.FileWatcher.pas',
  Dext.Sidecar.Server in 'Dext.Sidecar.Server.pas',
  Dext.Vcl.FormDecorator in 'Lib\Dext.Vcl.FormDecorator.pas',
  Dext.Vcl.Helpers in 'Lib\Dext.Vcl.Helpers.pas',
  Dext.Vcl.TrayIcon in 'Lib\Dext.Vcl.TrayIcon.pas',
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
