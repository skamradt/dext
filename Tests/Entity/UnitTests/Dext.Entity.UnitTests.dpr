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
  Dext.Entity.SmartTypes.Tests,
  Dext.Entity.FluentQuery.Tests,
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

    if TTest.Configure
      .Verbose
      .RegisterFixtures([
        TSmartTypesTests,
        TFluentQueryTests,
        TSqlGeneratorTests,
        TFluentMappingTests,
        TAsyncTests
      ])
      .ExportToJUnit('entity-unit-tests.xml')
      .Run then
      ExitCode := 0
    else
      ExitCode := 1;

  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  if DebugHook <> 0 then
  begin
    WriteLn;
    Write('Press Enter to exit...');
    ReadLn;
  end;
end.
