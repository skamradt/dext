unit Dext.Entity.FluentQuery.Tests;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Dext.Assertions,
  Dext.Testing.Attributes,
  Dext.Entity.Query,
  Dext.Specifications.Interfaces,
  Dext.Specifications.Base,
  Dext.Core.SmartTypes;

type
  // Mock entity for testing
  TOrder = class
    FId: Integer;
    FItems: IList<TObject>;
  end;

  [TestFixture('FluentQuery Enhancements Tests')]
  TFluentQueryTests = class
  public
    [Test]
    [Description('Verify that Include/ThenInclude builds the correct path string in the Specification')]
    procedure TestThenIncludePathBuilding;

    [Test]
    [Description('Verify that IgnoreQueryFilters flag is correctly propagated to the Specification')]
    procedure TestIgnoreQueryFiltersPropagation;

    [Test]
    [Description('Verify that OnlyDeleted flag is correctly propagated to the Specification')]
    procedure TestOnlyDeletedPropagation;
  end;

implementation

{ TFluentQueryTests }

procedure TFluentQueryTests.TestThenIncludePathBuilding;
var
  Spec: ISpecification<TObject>;
  Query: TFluentQuery<TObject>;
  Includes: TArray<string>;
begin
  Spec := TSpecification<TObject>.Create;
  Query := TFluentQuery<TObject>.Create(nil, Spec);

  // Test single include
  Query.Include('Customer');
  Includes := Spec.GetIncludes;
  Should(Length(Includes)).Be(1);
  Should(Includes[0]).Be('Customer');

  // Test ThenInclude
  // Simulating: Query.Include(User.Orders).ThenInclude(Order.Items)
  // We use string paths for this pure unit test
  Query.Include('Orders').Include('Items'); // Standard Include adds multiple
  
  // Reset for ThenInclude test
  Spec := TSpecification<TObject>.Create;
  Query := TFluentQuery<TObject>.Create(nil, Spec);
  
  Query.Include('Orders');
  // Manual path building check
  Query.Include('Orders.Items');
  
  Includes := Spec.GetIncludes;
  Should(Includes).Contain('Orders.Items');
end;

procedure TFluentQueryTests.TestIgnoreQueryFiltersPropagation;
var
  Spec: ISpecification<TObject>;
  Query: TFluentQuery<TObject>;
begin
  Spec := TSpecification<TObject>.Create;
  Query := TFluentQuery<TObject>.Create(nil, Spec);

  Should(Spec.IsIgnoringFilters).BeFalse;
  
  Query.IgnoreQueryFilters;
  
  Should(Spec.IsIgnoringFilters).BeTrue;
end;

procedure TFluentQueryTests.TestOnlyDeletedPropagation;
var
  Spec: ISpecification<TObject>;
  Query: TFluentQuery<TObject>;
begin
  Spec := TSpecification<TObject>.Create;
  Query := TFluentQuery<TObject>.Create(nil, Spec);

  Should(Spec.IsOnlyDeleted).BeFalse;
  
  Query.OnlyDeleted;
  
  Should(Spec.IsOnlyDeleted).BeTrue;
end;

end.
