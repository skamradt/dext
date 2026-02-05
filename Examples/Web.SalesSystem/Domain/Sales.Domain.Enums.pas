unit Sales.Domain.Enums;

interface

uses
  Dext.Core.SmartTypes;

type
  { Status Enums }
  // Using PascalCase for enum members to map nicely to JSON strings later
  TCustomerStatus = (Active, Inactive, Blocked);
  TOrderStatus = (Draft, Submitted, Approved, Shipped, Cancelled);

  { Smart Property Types for Enums }
  // This allows us to use TCustomer.Props.Status in queries
  TCustomerStatusType = Prop<TCustomerStatus>;
  TOrderStatusType = Prop<TOrderStatus>;

implementation

end.
