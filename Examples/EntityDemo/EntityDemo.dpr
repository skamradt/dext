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

procedure RunTest(const TestClass: TBaseTestClass);
var
  Test: TBaseTest;
begin
  WriteLn('Running Test: ', TestClass.ClassName);
  Test := TestClass.Create;
  try
    Test.Run;
  finally
    Test.Free;
  end;
end;

procedure RunAllTests;
begin
  WriteLn('🚀 Dext Entity ORM Demo Suite');
  WriteLn('=============================');
  WriteLn('');

  // 1. CRUD Tests
  RunTest(TCRUDTest);
  // 2. Relationships Tests
  RunTest(TRelationshipTest);
  // 3. Advanced Query Tests
  RunTest(TAdvancedQueryTest);
  // 4. Composite Keys Tests
  RunTest(TCompositeKeyTest);
  // 5. Explicit Loading Tests
  RunTest(TExplicitLoadingTest);
  // 6. Lazy Loading Tests
  RunTest(TLazyLoadingTest);
  // 7. Fluent API Tests
  RunTest(TFluentAPITest);
  // 8. Lazy Execution Tests
  RunTest(TLazyExecutionTest);
  // 9. Bulk Operations Tests
  RunTest(TBulkTest);
  // 10. Concurrency Tests
  RunTest(TConcurrencyTest);
  
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
