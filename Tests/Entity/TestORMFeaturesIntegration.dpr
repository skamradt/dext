program TestORMFeaturesIntegration;

{$APPTYPE CONSOLE}
{$TYPEINFO ON}
{$METHODINFO ON}

uses
  Dext.MM,
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  System.DateUtils,
  Data.DB,
  FireDAC.Comp.Client,
  Dext.Entity.Drivers.FireDAC.Links,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Error,
  FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Pool,
  FireDAC.Stan.Async,
  FireDAC.Phys,
  Dext,
  Dext.Collections,
  Dext.Assertions,
  Dext.Testing.Attributes,
  Dext.Testing.Runner,
  Dext.Entity.Core,
  Dext.Entity.Attributes,
  Dext.Entity.Context,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.Mapping,
  Dext.Entity.Dialects,
  Dext.Types.Lazy,
  Dext.Utils,
  TestORMFeatures in '..\Testing\TestORMFeatures.pas';

begin
  SetConsoleCharSet;
  try
    WriteLn('===========================================');
    WriteLn('  Dext ORM Features Integration Tests');
    WriteLn('===========================================');
    WriteLn;
    
    // Register test fixtures
    TTestRunner.RegisterFixture([
      TOptimisticConcurrencyTests, 
      TSoftDeleteTests, 
      TAuditFieldsTests,
      TRelationshipTests
    ]);
    
    // Enable verbose output to see failure details
    TTestRunner.SetVerbose(True);
    
    // Run all registered tests
    TTestRunner.RunAll;
    
  except
    on E: Exception do
    begin
      WriteLn('');
      WriteLn('FATAL ERROR: ', E.Message);
      ExitCode := 1;
    end;
  end;
  
  ConsolePause;
end.

