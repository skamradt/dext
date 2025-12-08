program TestController;

{$APPTYPE CONSOLE}

uses
  FastMM5,
  System.SysUtils,
  ControllerExample.Controller in 'ControllerExample.Controller.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    WriteLn('Compiling...');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
