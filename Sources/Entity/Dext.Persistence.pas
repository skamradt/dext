unit Dext.Persistence;

interface

uses
  // Re-export commonly used units for convenience
  Dext.Entity,
  Dext.Entity.Attributes,
  Dext.Entity.Core,
  Dext.Specifications.Base,
  Dext.Specifications.Criteria,
  Dext.Specifications.Fluent,
  Dext.Specifications.Interfaces,
  Dext.Specifications.Types;

type
  // Alias common types to avoid lengthy uses clauses
  TDbContext = Dext.Entity.TDbContext;
  
  // TProp for fluent queries
  TProp = Dext.Specifications.Types.TProp;
  
  // Fluent Specification builder
  Specification = Dext.Specifications.Fluent.Specification;
  
  // Attributes
  TableAttribute = Dext.Entity.Attributes.TableAttribute;
  ColumnAttribute = Dext.Entity.Attributes.ColumnAttribute;
  PKAttribute = Dext.Entity.Attributes.PKAttribute;
  AutoIncAttribute = Dext.Entity.Attributes.AutoIncAttribute;
  ForeignKeyAttribute = Dext.Entity.Attributes.ForeignKeyAttribute;
  VersionAttribute = Dext.Entity.Attributes.VersionAttribute;
  NotMappedAttribute = Dext.Entity.Attributes.NotMappedAttribute;

  // Enums
  TCascadeAction = Dext.Entity.Attributes.TCascadeAction;

implementation

end.
