program DesktopMVUCounter;

uses
  Vcl.Forms,
  Counter.Main in 'Counter.Main.pas' {MainForm},
  Counter.MVU in 'Counter.MVU.pas',
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
