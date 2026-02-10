unit Sales.Tests.OrderModel;

interface

uses
  Dext.Testing,
  Sales.Domain.Models,
  Sales.Domain.Entities,
  Sales.Domain.Enums;

type
  [TestFixture]
  TOrderModelTests = class
  private
    FOrder: TOrder;
    FModel: TOrderModel;
    FProduct: TProduct;
  public
    [Setup]
    procedure Setup;
    
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Should_Initialize_Draft_Order;

    [Test]
    procedure Should_Add_Items_And_Update_Total;

    [Test]
    procedure Should_Validate_Stock_When_Adding_Item;

    [Test]
    procedure Should_Validate_Minimum_Total_On_Submit;

    [Test]
    procedure Should_Submit_Valid_Order;
  end;

implementation

uses
  System.SysUtils;

{ TOrderModelTests }

procedure TOrderModelTests.Setup;
begin
  // Arrange
  FOrder := TOrder.Create;
  FOrder.Status := TOrderStatus.Draft;
  FOrder.Total := 0;

  FModel := TOrderModel.Create(FOrder);

  FProduct := TProduct.Create;
  FProduct.Id := 1;
  FProduct.Name := 'Test Product';
  FProduct.Price := 100.00;
  FProduct.StockQuantity := 50; 
end;

procedure TOrderModelTests.TearDown;
begin
  FModel.Free; // Does NOT free FOrder? Actually TOrderModel is aggregation, usually wrapper.
               // In Models unit: FEntity := Entity. 
               // Owner of Entity is usually the DataContext or the caller.
               // Here we own them.
  FOrder.Free; 
  FProduct.Free;
end;

procedure TOrderModelTests.Should_Initialize_Draft_Order;
begin
  Should(FModel.Entity.Status.Value).Be(TOrderStatus.Draft);
  Should.List<TOrderItem>(FModel.Entity.Items).BeEmpty;
  Should(FModel.Entity.Total.Value).Be(0);
end;

procedure TOrderModelTests.Should_Add_Items_And_Update_Total;
begin
  // Act
  FModel.AddItem(FProduct, 2);

  // Assert
  Should.List<TOrderItem>(FOrder.Items).HaveCount(1);
  Should(FOrder.Total.Value).Be(200.00); // 2 * 100
  
  var Item := FOrder.Items[0];
  Should(Item.Quantity.Value).Be(2);
  Should(Item.Total.Value).Be(200.00);
end;

procedure TOrderModelTests.Should_Validate_Stock_When_Adding_Item;
begin
  // Arrange
  FProduct.StockQuantity := 5;

  // Act & Assert
  Assert.WillRaise(
    procedure 
    begin
        FModel.AddItem(FProduct, 10); // Requesting more than stock
    end, 
    EDomainError, 
    'Insufficient stock'
  );
end;

procedure TOrderModelTests.Should_Validate_Minimum_Total_On_Submit;
begin
  // Arrange -> Total 0
  
  // Act & Assert
  Assert.WillRaise(
    procedure
    begin
      FModel.Submit; // Min is 10.00 in Model logic
    end,

    EDomainError, 
    'empty' // "Cannot submit an empty order" check comes first?
  );

  // Add small item (Price 5.00)
  FProduct.Price := 5.00;
  FModel.AddItem(FProduct, 1); // Total 5.00

  Assert.WillRaise(
    procedure 
    begin
      FModel.Submit;
    end,
    EDomainError,
    'Minimum order total'
  );
end;

procedure TOrderModelTests.Should_Submit_Valid_Order;
begin
  // Arrange
  FModel.AddItem(FProduct, 1); // Total 100.00 > 10.00

  // Act
  FModel.Submit;

  // Assert
  Should(FOrder.Status.Value).Be(TOrderStatus.Submitted);
  Should(FOrder.CreatedAt.Value).NotBe(0); // Should have timestamp
end;

end.
