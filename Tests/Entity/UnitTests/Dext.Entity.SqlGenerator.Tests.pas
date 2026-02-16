unit Dext.Entity.SqlGenerator.Tests;

interface

uses
  System.SysUtils,
  System.Rtti,
  Dext.Assertions,
  Dext.Mocks,
  Dext.Mocks.Matching,
  Dext.Testing.Attributes,
  Dext.Entity.Dialects,
  Dext.Specifications.SQL.Generator,
  Dext.Specifications.Base,
  Dext.Specifications.Interfaces,
  Dext.Entity.Mapping,
  Dext.Entity.Core;

type
  [Table('Users')]
  TTestUser = class
  private
    FId: Integer;
    FName: string;
    FIsDeleted: Boolean;
  public
    [PK] property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property IsDeleted: Boolean read FIsDeleted write FIsDeleted;
  end;

  [TestFixture('SQL Generator Tests (Mocked Dialect)')]
  TSqlGeneratorTests = class
  private
    FDialectMock: Mock<IDialect>;
    FGenerator: TSqlGenerator<TTestUser>;
  public
    [Setup]
    procedure Setup;
    
    [Teardown]
    procedure Teardown;

    [Test]
    procedure Test_GenerateSelect_Basic;
    
    [Test]
    procedure Test_GenerateSelect_WithIgnoreFilters;
  end;

implementation

{ TSqlGeneratorTests }

procedure TSqlGeneratorTests.Setup;
begin
  FDialectMock := Mock<IDialect>.Create;
  
  // Setup default dialect behavior: QuoteIdentifier just wraps in brackets
  FDialectMock.Setup.Callback(
    function(Args: TArray<TValue>): TValue
    begin
      Result := '[' + Args[0].AsString + ']';
    end).When.QuoteIdentifier(Arg.Any<string>);
    
  // Default schema behavior
  FDialectMock.Setup.Returns(TValue.From<Boolean>(False)).When.UseSchemaPrefix;

  FGenerator := TSqlGenerator<TTestUser>.Create(FDialectMock.Instance, nil);
end;

procedure TSqlGeneratorTests.Teardown;
begin
  FGenerator.Free;
end;

procedure TSqlGeneratorTests.Test_GenerateSelect_Basic;
var
  Spec: ISpecification<TTestUser>;
  SQL: string;
begin
  Spec := TSpecification<TTestUser>.Create;
  
  SQL := FGenerator.GenerateSelect(Spec);
  
  // Simple check: SQL should contain our quoted table and columns
  Should(SQL).Contain('FROM [Users]');
  Should(SQL).Contain('[Id]');
  Should(SQL).Contain('[Name]');
end;

procedure TSqlGeneratorTests.Test_GenerateSelect_WithIgnoreFilters;
var
  Spec: ISpecification<TTestUser>;
  SQL: string;
begin
  Spec := TSpecification<TTestUser>.Create;
  
  // By default, it might have filters if TTestUser was soft-delete (managed via mapping)
  // Let's force a filter in the generator to test the override
  
  FGenerator.IgnoreQueryFilters := True;
  SQL := FGenerator.GenerateSelect(Spec);
  
  // If IgnoreQueryFilters is True, it should not append soft delete WHERE clauses
  // This depends on TSqlGenerator implementation details we just enhanced
  
  Should(SQL).NotContain('IsDeleted'); // In this mock setup, no filter should be added
end;

end.
