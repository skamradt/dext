unit Sales.Domain.Models;

interface

uses
  System.SysUtils,
  Dext.Collections,
  Sales.Domain.Entities,
  Sales.Domain.Enums;

type
  EDomainError = class(Exception);

  { -------------------------------------------------------------------------- }
  { TOrderModel (Behavior)                                                     }
  { -------------------------------------------------------------------------- }
  TOrderModel = class
  private
    FEntity: TOrder;
  public
    constructor Create(Entity: TOrder);

    // Business Actions - The ONLY way to mutate state significantly
    procedure AddItem(Product: TProduct; Quantity: Integer);
    procedure Submit;
    procedure Approve;
    procedure Cancel;
    
    // Read-only access to state
    property Entity: TOrder read FEntity;
  end;

implementation

{ TOrderModel }

constructor TOrderModel.Create(Entity: TOrder);
begin
  if Entity = nil then
    raise EDomainError.Create('Order Entity cannot be nil');
  FEntity := Entity;
end;

procedure TOrderModel.AddItem(Product: TProduct; Quantity: Integer);
var
  Item: TOrderItem;
begin
  // 1. Invariants
  if FEntity.Status.Value <> TOrderStatus.Draft then
    raise EDomainError.Create('Items can only be added to Draft orders.');

  if Quantity <= 0 then
    raise EDomainError.Create('Quantity must be positive.');

  if Product.StockQuantity.Value < Quantity then
    raise EDomainError.Create(Format('Insufficient stock for product %s. Available: %d', 
      [Product.Name.Value, Product.StockQuantity.Value]));

  // 2. Logic
  Item := TOrderItem.Create;
  Item.ProductId := Product.Id;
  Item.Quantity := Quantity;
  Item.UnitPrice := Product.Price;
  Item.Total := Item.UnitPrice * Item.Quantity;
  
  // Link to Product (for display/logic immediately) but FK is ProductId
  Item.Product := Product;

  FEntity.Items.Add(Item);

  // 3. Aggregate Update
  FEntity.Total := FEntity.Total + Item.Total;
end;

procedure TOrderModel.Submit;
begin
  if FEntity.Status.Value <> TOrderStatus.Draft then
    raise EDomainError.Create('Only Draft orders can be submitted.');

  if FEntity.Items.Count = 0 then
    raise EDomainError.Create('Cannot submit an empty order.');

  if FEntity.Total.Value < 10.00 then
    raise EDomainError.Create('Minimum order total is 10.00.');

  FEntity.Status := TOrderStatus.Submitted;
  FEntity.CreatedAt := Now;
end;

procedure TOrderModel.Approve;
begin
  // Only submitted orders can be approved
  if FEntity.Status.Value <> TOrderStatus.Submitted then
    raise EDomainError.Create('Order must be Submitted before approval.');

  // Here we could add more checks, e.g. Customer Credit Limit
  
  FEntity.Status := TOrderStatus.Approved;
end;

procedure TOrderModel.Cancel;
begin
  if FEntity.Status.Value = TOrderStatus.Shipped then
    raise EDomainError.Create('Cannot cancel a shipped order.');

  FEntity.Status := TOrderStatus.Cancelled;
end;

end.
