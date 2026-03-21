unit Dext.Entity.DataSet.Tests;

interface

uses
  System.SysUtils,
  System.Classes,
  Data.DB,
  Dext.Assertions,
  Dext.Testing.Attributes,
  Dext.Entity.DataSet,
  Dext.Core.SmartTypes,
  Dext.Types.Nullable,
  Dext.Types.Lazy,
  Dext.Entity.Attributes,
  Dext.Collections,
  Dext.Entity;

type
  // =========================================================================
  //  Simple entity for basic tests
  // =========================================================================
  [Table('users')]
  TUserTest = class
  private
    FId: Integer;
    FName: string;
    FScore: Double;
    FActive: Boolean;
  public
    [PrimaryKey, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Score: Double read FScore write FScore;
    property Active: Boolean read FActive write FActive;
  end;

  // =========================================================================
  //  Complex entity for variety of types and Blobs
  // =========================================================================
  [Table('products')]
  TProductTest = class
  private
    FId: Integer;
    FName: string;
    FDescription: string;
    FPrice: Currency;
    FWeight: Double;
    FStockQty: Int64;
    FActive: Boolean;
    FCreatedAt: TDateTime;
    FPhoto: TBytes;
  public
    [PrimaryKey, AutoInc]
    property Id: Integer read FId write FId;

    [Required, MaxLength(100)]
    property Name: string read FName write FName;

    [MaxLength(500)]
    property Description: string read FDescription write FDescription;

    [Required]
    property Price: Currency read FPrice write FPrice;

    property Weight: Double read FWeight write FWeight;
    property StockQty: Int64 read FStockQty write FStockQty;
    property Active: Boolean read FActive write FActive;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property Photo: TBytes read FPhoto write FPhoto;
  end;

  // =========================================================================
  //  Detail entity for master-detail tests
  // =========================================================================
  [Table('order_items')]
  TOrderItemTest = class
  private
    FId: Integer;
    FOrderId: Integer;
    FProductName: string;
    FQuantity: Integer;
    FUnitPrice: Currency;
  public
    [PrimaryKey, AutoInc]
    property Id: Integer read FId write FId;

    [Required]
    property OrderId: Integer read FOrderId write FOrderId;

    [Required, MaxLength(100)]
    property ProductName: string read FProductName write FProductName;

    [Required]
    property Quantity: Integer read FQuantity write FQuantity;

    [Required]
    property UnitPrice: Currency read FUnitPrice write FUnitPrice;
  end;

  // =========================================================================
  //  SmartTypes Entity
  // =========================================================================
  TSmartProduct = class
  private
    FID: IntType;
    FName: StringType;
    FPrice: Prop<Double>;
    FQuantity: Nullable<Integer>;
    FActive: BoolType;
    FDescription: Lazy<string>;
  public
    property ID: IntType read FID write FID;
    property Name: StringType read FName write FName;
    property Price: Prop<Double> read FPrice write FPrice;
    property Quantity: Nullable<Integer> read FQuantity write FQuantity;
    property Active: BoolType read FActive write FActive;
    property Description: Lazy<string> read FDescription write FDescription;
  end;

  // =========================================================================
  //  Fixtures
  // =========================================================================

  [TestFixture('TEntityDataSet Basic Loading and Navigation')]
  TEntityDataSetTests = class
  private
    FDataSet: TEntityDataSet;
    FUsers: TArray<TObject>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_LoadArray_Count;
    [Test]
    procedure Test_FieldValues;
    [Test]
    procedure Test_Navigation_Next_Prior;
    [Test]
    procedure Test_Filter_Expression;
    [Test]
    procedure Test_Locate_Success;
  end;

  [TestFixture('TEntityDataSet Complex Types and Blobs')]
  TProductDataSetTests = class
  private
    FDataSet: TEntityDataSet;
    FProducts: TArray<TObject>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_FieldCount;
    [Test]
    procedure Test_Field_MaxLength;
    [Test]
    procedure Test_Field_Required;
    [Test]
    procedure Test_Field_DataTypes;
    [Test]
    procedure Test_Blob_FieldType;
    [Test]
    procedure Test_Blob_IsNull_When_Empty;
    [Test]
    procedure Test_Blob_Read_Stream;
    [Test]
    procedure Test_Blob_Write_Stream;
    [Test]
    procedure Test_Memo_Read_Stream;
    [Test]
    procedure Test_Memo_Write_Stream;
    [Test]
    procedure Test_Sort_By_Name;
    [Test]
    procedure Test_Sort_By_Price;
    [Test]
    procedure Test_Filter_By_Price;
  end;

  [TestFixture('TEntityDataSet Master-Detail')]
  TMasterDetailDataSetTests = class
  private
    FMasterDS: TEntityDataSet;
    FDetailDS: TEntityDataSet;
    FOrders: TArray<TObject>;
    FItems: TArray<TObject>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_Detail_Count;
    [Test]
    procedure Test_Detail_FilterByMaster;
    [Test]
    procedure Test_Detail_FieldValues;
  end;

  [TestFixture('TEntityDataSet CRUD Operations')]
  TEntityDataSetCRUDTests = class
  private
    FDataSet: TEntityDataSet;
    FSourceList: IList<TObject>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_Append_And_Post;
    [Test]
    procedure Test_Edit_Existing;
    [Test]
    procedure Test_Delete_And_Sync;
    [Test]
    procedure Test_Insert_Between_Records;
    [Test]
    procedure Test_Refresh_Syncs_External_Changes;
  end;

  [TestFixture('TEntityDataSet Stress and Filtering')]
  TEntityDataSetStressTests = class
  private
    FDataSet: TEntityDataSet;
    FUsers: IList<TObject>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_Stress_1k_Records_Filter;
    [Test]
    procedure Test_Edit_Into_Filter_Hides_Record;
  end;

  [TestFixture('TEntityDataSet SmartTypes Support')]
  TDataSetSmartTypesTests = class
  public
    [Test]
    procedure Test_Read_SmartTypes;
    [Test]
    procedure Test_Read_Nullable_Empty;
    [Test]
    procedure Test_Read_Lazy_Value;
  end;

implementation

{ TEntityDataSetTests }

procedure TEntityDataSetTests.Setup;
begin
  SetLength(FUsers, 2);
  var U1 := TUserTest.Create; U1.Id := 1; U1.Name := 'Cesar'; U1.Score := 100; U1.Active := True;
  var U2 := TUserTest.Create; U2.Id := 2; U2.Name := 'Romero'; U2.Score := 200; U2.Active := False;
  FUsers[0] := U1; FUsers[1] := U2;

  FDataSet := TEntityDataSet.Create(nil);
  FDataSet.Load(FUsers, TUserTest);
end;

procedure TEntityDataSetTests.TearDown;
begin
  FDataSet.Free;
  for var U in FUsers do U.Free;
end;

procedure TEntityDataSetTests.Test_LoadArray_Count;
begin
  Should(FDataSet.RecordCount).Be(2);
end;

procedure TEntityDataSetTests.Test_FieldValues;
begin
  Should(FDataSet.FieldByName('Name').AsString).Be('Cesar');
  Should(FDataSet.FieldByName('Score').AsFloat).Be(100.0);
end;

procedure TEntityDataSetTests.Test_Navigation_Next_Prior;
begin
  FDataSet.Next;
  Should(FDataSet.FieldByName('Name').AsString).Be('Romero');
  FDataSet.Prior;
  Should(FDataSet.FieldByName('Name').AsString).Be('Cesar');
end;

procedure TEntityDataSetTests.Test_Filter_Expression;
begin
  FDataSet.Filter := 'Score > 150';
  FDataSet.Filtered := True;
  Should(FDataSet.RecordCount).Be(1);
  Should(FDataSet.FieldByName('Name').AsString).Be('Romero');
end;

procedure TEntityDataSetTests.Test_Locate_Success;
begin
  Should(FDataSet.Locate('Name', 'Romero', [])).BeTrue;
  Should(FDataSet.FieldByName('Id').AsInteger).Be(2);
end;

{ TProductDataSetTests }

procedure TProductDataSetTests.Setup;
begin
  SetLength(FProducts, 3);
  var P1 := TProductTest.Create;
  P1.Id := 1; P1.Name := 'Widget Alpha'; P1.Price := 29.90; P1.Active := True;
  P1.Description := 'Premium widget for daily use';
  P1.Photo := [$FF, $D8, $FF, $E0];
  FProducts[0] := P1;

  var P2 := TProductTest.Create;
  P2.Id := 2; P2.Name := 'Gadget Beta'; P2.Price := 149.99; P2.Active := True;
  FProducts[1] := P2;

  var P3 := TProductTest.Create;
  P3.Id := 3; P3.Name := 'Tool Gamma'; P3.Price := 9.50; P3.Active := False;
  FProducts[2] := P3;

  FDataSet := TEntityDataSet.Create(nil);
  FDataSet.Load(FProducts, TProductTest);
end;

procedure TProductDataSetTests.TearDown;
begin
  FDataSet.Free;
  for var P in FProducts do P.Free;
end;

procedure TProductDataSetTests.Test_FieldCount;
begin
  Should(FDataSet.FieldCount).Be(9);
end;

procedure TProductDataSetTests.Test_Field_MaxLength;
begin
  Should(FDataSet.FieldByName('Name').Size).Be(100);
  Should(FDataSet.FieldByName('Description').Size).Be(500);
end;

procedure TProductDataSetTests.Test_Field_Required;
begin
  Should(FDataSet.FieldByName('Name').Required).BeTrue;
  Should(FDataSet.FieldByName('Price').Required).BeTrue;
  Should(FDataSet.FieldByName('Description').Required).BeFalse;
end;

procedure TProductDataSetTests.Test_Field_DataTypes;
begin
  Should(FDataSet.FieldByName('Price').DataType).Be(ftCurrency);
  Should(FDataSet.FieldByName('StockQty').DataType).Be(ftLargeint);
  Should(FDataSet.FieldByName('Photo').DataType).Be(ftBlob);
end;

procedure TProductDataSetTests.Test_Blob_FieldType;
begin
  Should(FDataSet.FieldByName('Photo') is TBlobField).BeTrue;
end;

procedure TProductDataSetTests.Test_Blob_IsNull_When_Empty;
begin
  FDataSet.Next; // P2 (nil photo)
  Should(FDataSet.FieldByName('Photo').IsNull).BeTrue;
end;

procedure TProductDataSetTests.Test_Blob_Read_Stream;
var Stream: TStream;
begin
  FDataSet.First;
  Stream := FDataSet.CreateBlobStream(FDataSet.FieldByName('Photo'), bmRead);
  try
    Should(Stream.Size).Be(4);
  finally
    Stream.Free;
  end;
end;

procedure TProductDataSetTests.Test_Blob_Write_Stream;
var Stream: TStream;
begin
  FDataSet.First;
  FDataSet.Edit;
  Stream := FDataSet.CreateBlobStream(FDataSet.FieldByName('Photo'), bmWrite);
  try
    var NewData: TBytes := [$01, $02, $03];
    Stream.Write(NewData[0], 3);
  finally
    Stream.Free;
  end;
  FDataSet.Post;
  Should(Length(TProductTest(FProducts[0]).Photo)).Be(3);
end;

procedure TProductDataSetTests.Test_Memo_Read_Stream;
var
  Stream: TStream;
  Reader: TStreamReader;
begin
  FDataSet.First;
  Stream := FDataSet.CreateBlobStream(FDataSet.FieldByName('Description'), bmRead);
  try
    Reader := TStreamReader.Create(Stream, TEncoding.Unicode);
    try
      Should(Reader.ReadToEnd).Be('Premium widget for daily use');
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TProductDataSetTests.Test_Memo_Write_Stream;
var
  Stream: TStream;
  Writer: TStreamWriter;
begin
  FDataSet.First;
  FDataSet.Edit;
  Stream := FDataSet.CreateBlobStream(FDataSet.FieldByName('Description'), bmWrite);
  try
    Writer := TStreamWriter.Create(Stream, TEncoding.Unicode);
    try
      Writer.Write('Novo Texto');
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
  FDataSet.Post;
  Should(TProductTest(FProducts[0]).Description).Be('Novo Texto');
end;

procedure TProductDataSetTests.Test_Sort_By_Name;
begin
  FDataSet.IndexFieldNames := 'Name';
  FDataSet.First;
  Should(FDataSet.FieldByName('Name').AsString).Be('Gadget Beta');
end;

procedure TProductDataSetTests.Test_Sort_By_Price;
begin
  FDataSet.IndexFieldNames := 'Price';
  FDataSet.First;
  Should(FDataSet.FieldByName('Name').AsString).Be('Tool Gamma');
end;

procedure TProductDataSetTests.Test_Filter_By_Price;
begin
  FDataSet.Filter := 'Price > 20';
  FDataSet.Filtered := True;
  Should(FDataSet.RecordCount).Be(2);
end;

{ TMasterDetailDataSetTests }

procedure TMasterDetailDataSetTests.Setup;
begin
  FMasterDS := TEntityDataSet.Create(nil);
  FDetailDS := TEntityDataSet.Create(nil);

  SetLength(FOrders, 2);
  var O1 := TUserTest.Create; O1.Id := 100; O1.Name := 'O1'; FOrders[0] := O1;
  var O2 := TUserTest.Create; O2.Id := 200; O2.Name := 'O2'; FOrders[1] := O2;

  SetLength(FItems, 3);
  var I1 := TOrderItemTest.Create; I1.Id := 1; I1.OrderId := 100; FItems[0] := I1;
  var I2 := TOrderItemTest.Create; I2.Id := 2; I2.OrderId := 100; FItems[1] := I2;
  var I3 := TOrderItemTest.Create; I3.Id := 3; I3.OrderId := 200; FItems[2] := I3;

  FMasterDS.Load(FOrders, TUserTest);
  FDetailDS.Load(FItems, TOrderItemTest);
end;

procedure TMasterDetailDataSetTests.TearDown;
begin
  FDetailDS.Free; FMasterDS.Free;
  for var O in FOrders do O.Free;
  for var I in FItems do I.Free;
end;

procedure TMasterDetailDataSetTests.Test_Detail_Count;
begin
  Should(FDetailDS.RecordCount).Be(3);
end;

procedure TMasterDetailDataSetTests.Test_Detail_FilterByMaster;
begin
  FDetailDS.Filter := 'OrderId = 100';
  FDetailDS.Filtered := True;
  Should(FDetailDS.RecordCount).Be(2);
end;

procedure TMasterDetailDataSetTests.Test_Detail_FieldValues;
begin
  Should(FDetailDS.FieldByName('OrderId').Required).BeTrue;
end;

{ TEntityDataSetCRUDTests }

procedure TEntityDataSetCRUDTests.Setup;
begin
  FDataSet := TEntityDataSet.Create(nil);
  FSourceList := TCollections.CreateList<TObject>(True);
  var U1 := TUserTest.Create; U1.Id := 1; U1.Name := 'Cesar'; FSourceList.Add(U1);
  FDataSet.Load(FSourceList, TUserTest, False);
end;

procedure TEntityDataSetCRUDTests.TearDown;
begin
  FDataSet.Free;
end;

procedure TEntityDataSetCRUDTests.Test_Append_And_Post;
begin
  FDataSet.Append;
  FDataSet.FieldByName('Id').AsInteger := 2;
  FDataSet.FieldByName('Name').AsString := 'New';
  FDataSet.Post;
  Should(FSourceList.Count).Be(2);
end;

procedure TEntityDataSetCRUDTests.Test_Edit_Existing;
begin
  FDataSet.First;
  FDataSet.Edit;
  FDataSet.FieldByName('Name').AsString := 'Editado';
  FDataSet.Post;
  Should(TUserTest(FSourceList[0]).Name).Be('Editado');
end;

procedure TEntityDataSetCRUDTests.Test_Delete_And_Sync;
begin
  FDataSet.First;
  FDataSet.Delete;
  Should(FSourceList.Count).Be(0);
end;

procedure TEntityDataSetCRUDTests.Test_Insert_Between_Records;
begin
  var U2 := TUserTest.Create; U2.Id := 3; U2.Name := 'Treis'; FSourceList.Add(U2);
  FDataSet.Refresh;
  FDataSet.Last;
  FDataSet.Insert;
  FDataSet.FieldByName('Id').AsInteger := 2;
  FDataSet.FieldByName('Name').AsString := 'Dois';
  FDataSet.Post;
  FDataSet.First; FDataSet.Next;
  Should(FDataSet.FieldByName('Id').AsInteger).Be(2);
end;

procedure TEntityDataSetCRUDTests.Test_Refresh_Syncs_External_Changes;
begin
  var U2 := TUserTest.Create;
  U2.Id := 2;
  U2.Name := 'Externo';
  FSourceList.Add(U2);
  FDataSet.Refresh;
  Should(FDataSet.RecordCount).Be(2);
  FDataSet.Last;
  Should(FDataSet.FieldByName('Name').AsString).Be('Externo');
end;

{ TEntityDataSetStressTests }

procedure TEntityDataSetStressTests.Setup;
begin
  FUsers := TCollections.CreateList<TObject>(True);
  for var I := 1 to 1000 do
  begin
    var U := TUserTest.Create;
    U.Id := I; U.Name := 'U' + I.ToString; U.Score := I * 2; U.Active := (I mod 2 = 0);
    FUsers.Add(U);
  end;
  FDataSet := TEntityDataSet.Create(nil);
  FDataSet.Load(FUsers, TUserTest, False);
end;

procedure TEntityDataSetStressTests.TearDown;
begin
  FDataSet.Free;
end;

procedure TEntityDataSetStressTests.Test_Stress_1k_Records_Filter;
begin
  FDataSet.Filter := 'Score > 1000';
  FDataSet.Filtered := True;
  Should(FDataSet.RecordCount).Be(500);
end;

procedure TEntityDataSetStressTests.Test_Edit_Into_Filter_Hides_Record;
begin
  FDataSet.Filter := 'Active = True';
  FDataSet.Filtered := True;
  FDataSet.First; // ID 2
  FDataSet.Edit;
  FDataSet.FieldByName('Active').AsBoolean := False;
  FDataSet.Post;
  Should(FDataSet.RecordCount).Be(499);
end;

{ TDataSetSmartTypesTests }

procedure TDataSetSmartTypesTests.Test_Read_SmartTypes;
var
  DataSet: TEntityDataSet;
  Product: TSmartProduct;
begin
  Product := TSmartProduct.Create;
  try
    Product.ID := 1; Product.Name := 'Smart Watch'; Product.Price := 299.90; Product.Quantity := 10; Product.Active := True;
    DataSet := TEntityDataSet.Create(nil);
    DataSet.Load(TArray<TObject>.Create(Product), TSmartProduct);
    try
      DataSet.Open;
      Should(DataSet.FieldByName('ID').AsInteger).Be(1);
      Should(DataSet.FieldByName('Name').AsString).Be('Smart Watch');
    finally
      DataSet.Free;
    end;
  finally
    Product.Free;
  end;
end;

procedure TDataSetSmartTypesTests.Test_Read_Nullable_Empty;
var
  DataSet: TEntityDataSet;
  Product: TSmartProduct;
begin
  Product := TSmartProduct.Create;
  try
    Product.Quantity.Clear;
    DataSet := TEntityDataSet.Create(nil);
    try
      DataSet.Load(TArray<TObject>.Create(Product), TSmartProduct);
      DataSet.Open;
      Should(DataSet.FieldByName('Quantity').IsNull).BeTrue;
    finally
      DataSet.Free;
    end;
  finally
    Product.Free;
  end;
end;

procedure TDataSetSmartTypesTests.Test_Read_Lazy_Value;
var
  DataSet: TEntityDataSet;
  Product: TSmartProduct;
begin
  Product := TSmartProduct.Create;
  try
    Product.Description := 'Lazy';
    DataSet := TEntityDataSet.Create(nil);
    try
      DataSet.Load(TArray<TObject>.Create(Product), TSmartProduct);
      DataSet.Open;
      Should(DataSet.FieldByName('Description').AsString).Be('Lazy');
    finally
      DataSet.Free;
    end;
  finally
    Product.Free;
  end;
end;

end.
