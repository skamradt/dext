program Dext.EntityDataSet.Tests;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  System.SysUtils,
  Dext.Utils,
  Dext.Testing,
  Dext.Entity.DataSet.Tests in 'Dext.Entity.DataSet.Tests.pas';

begin
  SetConsoleCharSet;
  try
    WriteLn;
    WriteLn('=== Dext - EntityDataSet Unit Tests ===');
    WriteLn;

    TTest.SetExitCode(
      TTest.Configure
        .Verbose
        .RegisterFixtures([
          TEntityDataSetTests,
          TProductDataSetTests,
          TMasterDetailDataSetTests,
          TEntityDataSetCRUDTests
        ]).Run
    );
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  ConsolePause;
end.
