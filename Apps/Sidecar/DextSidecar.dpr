program DextSidecar;

uses
  Vcl.Forms,
  Main.Form in 'Main.Form.pas' {MainForm},
  Dext.Vcl.TrayIcon in 'Lib\Dext.Vcl.TrayIcon.pas',
  Dext.Vcl.FormDecorator in 'Lib\Dext.Vcl.FormDecorator.pas',
  Dext.Vcl.Helpers in 'Lib\Dext.Vcl.Helpers.pas',
  Dext.Classes in 'Lib\Dext.Classes.pas';



begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ShowMainForm := False;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
