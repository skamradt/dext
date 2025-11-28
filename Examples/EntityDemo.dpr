program EntityDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  EntityDemo.Tests.CRUD,
  EntityDemo.Tests.Relationships,
  EntityDemo.Tests.CompositeKeys,
  EntityDemo.Tests.Bulk,
  EntityDemo.Tests.Concurrency,
  EntityDemo.Tests.FluentAPI;

procedure RunAllTests;
var
  CRUDTest: TCRUDTest;
  RelTest: TRelationshipTest;
  CompKeyTest: TCompositeKeyTest;
  BulkTest: TBulkTest;
  ConcTest: TConcurrencyTest;
  FluentTest: TFluentAPITest;
begin
  WriteLn('🚀 Dext Entity ORM Demo Suite');
  WriteLn('=============================');
  WriteLn('');

  CRUDTest := TCRUDTest.Create;
  try
    CRUDTest.Run;
  finally
    CRUDTest.Free;
  end;

  RelTest := TRelationshipTest.Create;
  try
    RelTest.Run;
  finally
    RelTest.Free;
  end;

  CompKeyTest := TCompositeKeyTest.Create;
  try
    CompKeyTest.Run;
  finally
    CompKeyTest.Free;
  end;

  BulkTest := TBulkTest.Create;
  try
    BulkTest.Run;
  finally
    BulkTest.Free;
  end;

  ConcTest := TConcurrencyTest.Create;
  try
    ConcTest.Run;
  finally
    ConcTest.Free;
  end;

  FluentTest := TFluentAPITest.Create;
  try
    FluentTest.Run;
  finally
    FluentTest.Free;
  end;
  
  WriteLn('✨ All tests completed.');
end;

begin
  try
    RunAllTests;
  except
    on E: Exception do
      Writeln('❌ Critical Error: ', E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
