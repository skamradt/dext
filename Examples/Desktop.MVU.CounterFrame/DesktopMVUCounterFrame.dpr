program DesktopMVUCounterFrame;

uses
  Vcl.Forms,
  Counter.Main in 'Counter.Main.pas' {MainForm},
  Counter.Model in 'Counter.Model.pas',
  Counter.Messages in 'Counter.Messages.pas',
  Counter.Update in 'Counter.Update.pas',
  Counter.View in 'Counter.View.pas' {CounterViewFrame: TFrame},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
