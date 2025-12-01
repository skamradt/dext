program EntityDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  EntityDemo.Tests.AdvancedQuery in 'EntityDemo.Tests.AdvancedQuery.pas',
  EntityDemo.Tests.Base in 'EntityDemo.Tests.Base.pas',
  EntityDemo.Tests.Bulk in 'EntityDemo.Tests.Bulk.pas',
  EntityDemo.Tests.CompositeKeys in 'EntityDemo.Tests.CompositeKeys.pas',
  EntityDemo.Tests.Concurrency in 'EntityDemo.Tests.Concurrency.pas',
  EntityDemo.Tests.CRUD in 'EntityDemo.Tests.CRUD.pas',
  EntityDemo.Tests.ExplicitLoading in 'EntityDemo.Tests.ExplicitLoading.pas',
  EntityDemo.Tests.FluentAPI in 'EntityDemo.Tests.FluentAPI.pas',
  EntityDemo.Tests.LazyExecution in 'EntityDemo.Tests.LazyExecution.pas',
  EntityDemo.Tests.LazyLoading in 'EntityDemo.Tests.LazyLoading.pas',
  EntityDemo.Tests.Relationships in 'EntityDemo.Tests.Relationships.pas',
  EntityDemo.Entities in 'EntityDemo.Entities.pas';

procedure RunAllTests;
var
  Test: TBaseTest;
begin
  WriteLn('🚀 Dext Entity ORM Demo Suite');
  WriteLn('=============================');
  WriteLn('');

  // 1. CRUD Tests
  Test := TCRUDTest.Create;
  try
    Test.Run;
  finally
    Test.Free;
  end;

  // 2. Relationships Tests
  Test := TRelationshipTest.Create;
  try
    Test.Run;
  finally
    Test.Free;
  end;

  // 3. Advanced Query Tests
  Test := TAdvancedQueryTest.Create;
  try
    Test.Run;
  finally
    Test.Free;
  end;

  // 4. Composite Keys Tests
  Test := TCompositeKeyTest.Create;
  try
    Test.Run;
  finally
    Test.Free;
  end;

  // 5. Explicit Loading Tests
  Test := TExplicitLoadingTest.Create;
  try
    Test.Run;
  finally
    Test.Free;
  end;

  // 6. Lazy Loading Tests
  Test := TLazyLoadingTest.Create;
  try
    Test.Run;
  finally
    Test.Free;
  end;

  // 7. Fluent API Tests
  Test := TFluentAPITest.Create;
  try
    Test.Run;
  finally
    Test.Free;
  end;

  // 8. Lazy Execution Tests
  Test := TLazyExecutionTest.Create;
  try
    Test.Run;
  finally
    Test.Free;
  end;

  // 9. Bulk Operations Tests
  Test := TBulkTest.Create;
  try
    Test.Run;
  finally
    Test.Free;
  end;

  // 10. Concurrency Tests
  Test := TConcurrencyTest.Create;
  try
    Test.Run;
  finally
    Test.Free;
  end;
  
  WriteLn('');
  WriteLn('✨ All tests completed.');
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    RunAllTests;
  except
    on E: Exception do
      Writeln('❌ Critical Error: ', E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
