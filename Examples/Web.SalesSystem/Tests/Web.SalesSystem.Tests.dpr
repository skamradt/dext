program Web.SalesSystem.Tests;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  System.SysUtils,
  Dext.Testing,
  Sales.Tests.OrderModel in 'Sales.Tests.OrderModel.pas',
  Sales.Domain.Models in '..\Domain\Sales.Domain.Models.pas',
  Sales.Domain.Entities in '..\Domain\Sales.Domain.Entities.pas',
  Sales.Domain.Enums in '..\Domain\Sales.Domain.Enums.pas';

begin
  try
    WriteLn('🧪 Running Sales System Domain Tests...');
    
    // Configure and Run Tests
    TTest.SetExitCode(
      TTest.Configure
        .RegisterFixtures([TOrderModelTests])
        .Run
    );
      
    WriteLn('Done.');
  except
    on E: Exception do
      WriteLn('Fatal Error: ', E.Message);
  end;
end.
