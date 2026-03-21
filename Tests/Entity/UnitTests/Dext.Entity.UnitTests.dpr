program Dext.Entity.UnitTests;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  System.SysUtils,
  Dext.Testing.Runner,
  Dext.Testing.Attributes,
  Dext.Testing.Fluent,
  Dext.Utils,
  // Unit tests
  Dext.Entity.SmartTypes.Tests in 'Dext.Entity.SmartTypes.Tests.pas',
  Dext.Entity.FluentQuery.Tests in 'Dext.Entity.FluentQuery.Tests.pas',
  Dext.Entity.DataSet.Tests in 'Dext.Entity.DataSet.Tests.pas',
  Dext.Entity.Async.Tests in 'Dext.Entity.Async.Tests.pas',
  Dext.Entity.SqlGenerator.Tests in 'Dext.Entity.SqlGenerator.Tests.pas',
  Dext.Entity.FluentMapping.Tests in 'Dext.Entity.FluentMapping.Tests.pas';

begin
  SetConsoleCharSet();
  try
    WriteLn;
    WriteLn('🧪 Dext Entity Unit Tests');
    WriteLn('=========================');
    WriteLn;

    var TestResult := TTest
      .Configure
      .Verbose
      .RegisterFixtures([
        TEntityDataSetCRUDTests,
        TEntityDataSetTests,
        TProductDataSetTests,
        TMasterDetailDataSetTests,
        TEntityDataSetStressTests,
        TDataSetSmartTypesTests
      ]).Run;

    TTest.SetExitCode(TestResult);
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;

  ConsolePause;
end.
