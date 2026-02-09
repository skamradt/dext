program Web.SalesSystem.Tests;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  Dext.Testing,
  // Test Fixtures
  Sales.Tests.OrderModel in 'Sales.Tests.OrderModel.pas',
  Sales.Tests.Auth in 'Sales.Tests.Auth.pas',
  Sales.Tests.Serialization in 'Sales.Tests.Serialization.pas',
  // Domain Units (needed by tests)
  Sales.Domain.Models in '..\Domain\Sales.Domain.Models.pas',
  Sales.Domain.Entities in '..\Domain\Sales.Domain.Entities.pas',
  Sales.Domain.Enums in '..\Domain\Sales.Domain.Enums.pas',
  // Server Units (for Auth tests)
  Sales.Auth in '..\Server\Sales.Auth.pas';

begin
  SetConsoleCharSet;
  try
    WriteLn('🧪 Running Sales System Unit Tests...');
    WriteLn('');
    
    // Configure and Run Tests
    TTest.SetExitCode(
      TTest
        .Configure
        .Verbose
        .RegisterFixtures([
          TOrderModelTests,     // Domain Model tests
          TAuthServiceTests,    // Authentication logic tests
          TSerializationTests   // JSON serialization tests
        ])
        .Run
    );
      
    WriteLn('Done.');
  except
    on E: Exception do
      WriteLn('Fatal Error: ', E.Message);
  end;
  ConsolePause;
end.

