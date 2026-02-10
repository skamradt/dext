program TestORMRelationships;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  Dext.Testing.Attributes,
  Dext.Testing.Runner,
  Dext.Testing,
  TestEntityRelationships in 'TestEntityRelationships.pas',
  TestManyToManyIntegration in 'TestManyToManyIntegration.pas';

begin
  SetConsoleCharset;
  try
    WriteLn('🧪 Dext ORM Relationship Mapping Tests');
    WriteLn('=======================================');
    WriteLn;

    TTest.SetExitCode(
      TTest
        .Configure
        .Verbose
        .RegisterFixtures([TEntityRelationshipTests])
        .RegisterFixtures([TManyToManyIntegrationTests])
        .Run
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
