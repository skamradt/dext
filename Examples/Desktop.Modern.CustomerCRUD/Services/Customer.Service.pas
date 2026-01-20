{***************************************************************************}
{                                                                           }
{           Dext Framework - Example                                        }
{                                                                           }
{           Customer Service - Business logic interface                     }
{                                                                           }
{***************************************************************************}
unit Customer.Service;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Dext,              // Core facade: DI, Logging, Validation
  Dext.Collections,
  Dext.Entity,       // ORM facade: DbContext, DbSet
  Dext.Core.SmartTypes,
  Customer.Entity,
  Customer.Context;

type
  /// <summary>
  /// Service interface for Customer operations
  /// </summary>
  ICustomerService = interface
    ['{837D03EF-1965-4EFD-9879-076F254EB53E}']
    function GetAll: IList<TCustomer>;
    function GetById(Id: Integer): TCustomer;
    function Save(Customer: TCustomer): TCustomer;
    procedure Delete(Id: Integer);
    function Search(const Term: string): IList<TCustomer>;
    function Count: Integer;
  end;
  
  /// <summary>
  /// Modernized customer service using Dext ORM
  /// </summary>
  TCustomerService = class(TInterfacedObject, ICustomerService)
  private
    FContext: TCustomerContext;
    FLogger: ILogger;
  public
    [ServiceConstructor]
    constructor Create(const AContext: TCustomerContext; const ALogger: ILogger);
    
    function GetAll: IList<TCustomer>;
    function GetById(Id: Integer): TCustomer;
    function Save(Customer: TCustomer): TCustomer;
    procedure Delete(Id: Integer);
    function Search(const Term: string): IList<TCustomer>;
    function Count: Integer;
  end;

implementation

{ TCustomerService }

constructor TCustomerService.Create(const AContext: TCustomerContext; const ALogger: ILogger);
begin
  inherited Create;
  FContext := AContext;
  FLogger := ALogger;
  
  // Ensure database schema is created on first access
  FContext.EnsureCreated;
end;

function TCustomerService.GetAll: IList<TCustomer>;
begin
  Result := FContext.Customers.ToList;
end;

function TCustomerService.GetById(Id: Integer): TCustomer;
begin
  Result := FContext.Customers.Find(Id);
end;

function TCustomerService.Save(Customer: TCustomer): TCustomer;
var
  ValidationResult: TValidationResult;
begin
  // Leverage Dext Validation via non-generic class method
  ValidationResult := TValidator.Validate(Customer);
  try
    if not ValidationResult.IsValid then
      raise Exception.Create('Validation failed: ' + ValidationResult.Errors[0].ErrorMessage);

    if Customer.Id = 0 then
      FContext.Customers.Add(Customer)
    else
      FContext.Customers.Update(Customer);

    // Timestamps and versioning are now handled automatically by the ORM
    FContext.SaveChanges;
    
    FLogger.Info('Customer saved: %s', [Customer.Name.Value]);
    Result := Customer;
  finally
    ValidationResult.Free;
  end;
end;

procedure TCustomerService.Delete(Id: Integer);
var
  Customer: TCustomer;
begin
  Customer := GetById(Id);
  if Customer <> nil then
  begin
    FContext.Customers.Remove(Customer);
    FContext.SaveChanges;
    FLogger.Warn('Customer removed: %d', [Id]);
  end;
end;

function TCustomerService.Search(const Term: string): IList<TCustomer>;
begin
  // Using Smart Properties for type-safe query
  Result := FContext.Customers.Where(
    function(C: TCustomer): BooleanExpression
    begin
      Result := (C.Name.Contains(Term)) or (C.Email.Contains(Term));
    end
  ).ToList;
end;

function TCustomerService.Count: Integer;
begin
  Result := FContext.Customers.Count(nil); // Count without predicate
end;

end.
