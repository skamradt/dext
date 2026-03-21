unit Dext.Entity.DataSet.Tests;

interface

uses
  System.SysUtils,
  System.Classes,
  Dext.Entity.Attributes,
  Dext.Entity.DataSet,
  Dext.Testing,
  Dext.Collections,
  Data.DB;

type
  // =========================================================================
  //  Entidade simples para testes básicos (existente)
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
  //  Entidade complexa com mais tipos, atributos e blob
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
  //  Entidade detalhe (Order Item) para teste mestre-detalhe
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
  //  Fixture de testes básicos (existente)
  // =========================================================================
  [TestFixture]
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
    [Test]
    procedure Test_LoadFromUtf8Json;
  end;

  // =========================================================================
  //  Fixture de testes com entidade complexa
  // =========================================================================
  [TestFixture]
  TProductDataSetTests = class
  private
    FDataSet: TEntityDataSet;
    FProducts: TArray<TObject>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Testes de Metadata / FieldDefs
    [Test]
    procedure Test_FieldCount;
    [Test]
    procedure Test_Field_MaxLength;
    [Test]
    procedure Test_Field_Required;
    [Test]
    procedure Test_Field_ReadOnly_AutoInc;
    [Test]
    procedure Test_Field_DataTypes;

    // Testes de Dados com tipos variados
    [Test]
    procedure Test_Currency_Value;
    [Test]
    procedure Test_Int64_Value;
    [Test]
    procedure Test_DateTime_Value;
    [Test]
    procedure Test_Boolean_Value;

    // Testes de Blob
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

    // Testes de Sorting
    [Test]
    procedure Test_Sort_By_Name;
    [Test]
    procedure Test_Sort_By_Price;

    // Teste de filtro com Currency
    [Test]
    procedure Test_Filter_By_Price;
  end;

  // =========================================================================
  //  Fixture de testes Mestre-Detalhe
  // =========================================================================
  [TestFixture]
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
    [Test]
    procedure Test_Detail_Required_Fields;
  end;

  // =========================================================================
  //  Fixture de testes CRUD e Edição
  // =========================================================================
  [TestFixture]
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
    procedure Test_Locate_After_CRUD;
    [Test]
    procedure Test_Filter_After_Append;
    [Test]
    procedure Test_Insert_Between_Records;
    [Test]
    procedure Test_Insert_With_Sort;
  end;

  // =========================================================================
  //  Fixture de testes de Stress e Transição de Estado
  // =========================================================================
  [TestFixture]
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
    [Test]
    procedure Test_MultiFieldSort_Composite;
    [Test]
    procedure Test_Edit_Record_While_Filtered_Maintains_Position;
  end;

implementation

uses
  Dext.Core.Span;

// ===========================================================================
//  TEntityDataSetTests (testes básicos existentes)
// ===========================================================================

procedure TEntityDataSetTests.Setup;
begin
  FDataSet := TEntityDataSet.Create(nil);
  
  SetLength(FUsers, 2);
  
  var U1 := TUserTest.Create;
  U1.Id := 1;
  U1.Name := 'Cesar';
  U1.Score := 100.5;
  U1.Active := True;
  FUsers[0] := U1;

  var U2 := TUserTest.Create;
  U2.Id := 2;
  U2.Name := 'Dext';
  U2.Score := 99.9;
  U2.Active := False;
  FUsers[1] := U2;
end;

procedure TEntityDataSetTests.TearDown;
begin
  FDataSet.Free;
  for var I := 0 to High(FUsers) do
    FUsers[I].Free;
end;

procedure TEntityDataSetTests.Test_LoadArray_Count;
begin
  FDataSet.Load(FUsers, TUserTest);
  Should(FDataSet.RecordCount).Be(2).Because('Deve carregar 2 registros');
end;

procedure TEntityDataSetTests.Test_FieldValues;
begin
  FDataSet.Load(FUsers, TUserTest);
  
  Should(FDataSet.FieldByName('Name').AsString).Be('Cesar').Because('Nome do primeiro registro deve ser Cesar');
  Should(FDataSet.FieldByName('Score').AsFloat).Be(100.5).Because('Score do primeiro registro incorreto');
  Should(FDataSet.FieldByName('Active').AsBoolean).BeTrue.Because('Active deve ser true');
end;

procedure TEntityDataSetTests.Test_Navigation_Next_Prior;
begin
  FDataSet.Load(FUsers, TUserTest);
  FDataSet.Next;
  
  Should(FDataSet.FieldByName('Name').AsString).Be('Dext').Because('Nome do segundo registro deve ser Dext');
  
  FDataSet.Prior;
  Should(FDataSet.FieldByName('Name').AsString).Be('Cesar').Because('Deve voltar para o primeiro registro');
end;

procedure TEntityDataSetTests.Test_Filter_Expression;
begin
  FDataSet.Load(FUsers, TUserTest);
  FDataSet.Filter := 'Score > 100';
  FDataSet.Filtered := True;
  
  Should(FDataSet.RecordCount).Be(1).Because('Deve ter 1 registro após o filtro');
  Should(FDataSet.FieldByName('Name').AsString).Be('Cesar').Because('Nome filtrado incorreto');
end;

procedure TEntityDataSetTests.Test_Locate_Success;
begin
  FDataSet.Load(FUsers, TUserTest);
  
  var Found := FDataSet.Locate('Name', 'Dext', []);
  Should(Found).BeTrue.Because('Locate deve retornar True');
  Should(FDataSet.FieldByName('Name').AsString).Be('Dext').Because('Cursor deve estar em Dext');
end;

procedure TEntityDataSetTests.Test_LoadFromUtf8Json;
var
  Json: string;
  Bytes: TBytes;
begin
  // Para que o FEntityMap seja registrado (No dataset ele usa o InternalPreOpen)
  FDataSet.Load(FUsers, TUserTest); 
  
  Json := '[{"Id": 10, "Name": "Cesar2", "Score": 500.0, "Active": true},' +
          ' {"Id": 20, "Name": "Dext2", "Score": 600.0, "Active": false}]';
  
  Bytes := TEncoding.UTF8.GetBytes(Json);
  var Span := TByteSpan.Create(@Bytes[0], Length(Bytes));
  
  FDataSet.LoadFromUtf8Json(Span, TUserTest);
  try
    Should(FDataSet.RecordCount).Be(2).Because('Deve carregar 2 registros via JSON');
    Should(FDataSet.FieldByName('Name').AsString).Be('Cesar2').Because('Nome do primeiro JSON incorreto');
  finally
    for var J := 0 to FDataSet.Items.Count - 1 do
      FDataSet.Items[J].Free;
  end;
end;

// ===========================================================================
//  TProductDataSetTests (entidade complexa com atributos)
// ===========================================================================

procedure TProductDataSetTests.Setup;
begin
  FDataSet := TEntityDataSet.Create(nil);

  SetLength(FProducts, 3);

  var P1 := TProductTest.Create;
  P1.Id := 1;
  P1.Name := 'Widget Alpha';
  P1.Description := 'Premium widget for daily use';
  P1.Price := 29.90;
  P1.Weight := 0.5;
  P1.StockQty := 1000;
  P1.Active := True;
  P1.CreatedAt := EncodeDate(2026, 1, 15) + EncodeTime(10, 30, 0, 0);
  P1.Photo := TBytes.Create($FF, $D8, $FF, $E0); // JPEG header bytes
  FProducts[0] := P1;

  var P2 := TProductTest.Create;
  P2.Id := 2;
  P2.Name := 'Gadget Beta';
  P2.Description := 'Advanced gadget';
  P2.Price := 149.99;
  P2.Weight := 1.2;
  P2.StockQty := 500;
  P2.Active := True;
  P2.CreatedAt := EncodeDate(2026, 2, 20) + EncodeTime(14, 0, 0, 0);
  P2.Photo := nil;
  FProducts[1] := P2;

  var P3 := TProductTest.Create;
  P3.Id := 3;
  P3.Name := 'Tool Gamma';
  P3.Description := '';
  P3.Price := 9.50;
  P3.Weight := 0.1;
  P3.StockQty := 9999999999;
  P3.Active := False;
  P3.CreatedAt := EncodeDate(2026, 3, 1);
  P3.Photo := nil;
  FProducts[2] := P3;

  FDataSet.Load(FProducts, TProductTest);
end;

procedure TProductDataSetTests.TearDown;
begin
  FDataSet.Free;
  for var I := 0 to High(FProducts) do
    FProducts[I].Free;
end;

procedure TProductDataSetTests.Test_FieldCount;
begin
  // TProductTest tem 9 propriedades: Id, Name, Description, Price, Weight, StockQty, Active, CreatedAt, Photo
  Should(FDataSet.FieldCount).Be(9).Because('TProductTest deve gerar 9 fields');
end;

procedure TProductDataSetTests.Test_Field_MaxLength;
begin
  var NameField := FDataSet.FieldByName('Name');
  var DescField := FDataSet.FieldByName('Description');

  // Name tem [MaxLength(100)]
  Should(NameField.Size).Be(100).Because('Name deve ter MaxLength=100');
  // Description tem [MaxLength(500)]
  Should(DescField.Size).Be(500).Because('Description deve ter MaxLength=500');
end;

procedure TProductDataSetTests.Test_Field_Required;
begin
  var NameField := FDataSet.FieldByName('Name');
  var PriceField := FDataSet.FieldByName('Price');
  var DescField := FDataSet.FieldByName('Description');
  var WeightField := FDataSet.FieldByName('Weight');

  // Name tem [Required]
  Should(NameField.Required).BeTrue.Because('Name deve ser Required');
  // Price tem [Required]
  Should(PriceField.Required).BeTrue.Because('Price deve ser Required');
  // Description NÃO tem [Required]
  Should(DescField.Required).BeFalse.Because('Description não deve ser Required');
  // Weight NÃO tem [Required]
  Should(WeightField.Required).BeFalse.Because('Weight não deve ser Required');
end;

procedure TProductDataSetTests.Test_Field_ReadOnly_AutoInc;
begin
  var IdField := FDataSet.FieldByName('Id');
  var NameField := FDataSet.FieldByName('Name');

  // Id tem [AutoInc] -> deve ser ReadOnly
  Should(IdField.ReadOnly).BeTrue.Because('Id (AutoInc) deve ser ReadOnly');
  // Id com AutoInc NÃO deve ser Required (AutoInc é preenchido pelo banco)
  Should(IdField.Required).BeFalse.Because('Id (AutoInc) não deve ser Required');
  // Name NÃO tem AutoInc -> não deve ser ReadOnly
  Should(NameField.ReadOnly).BeFalse.Because('Name não deve ser ReadOnly');
end;

procedure TProductDataSetTests.Test_Field_DataTypes;
begin
  Should(Ord(FDataSet.FieldByName('Id').DataType)).Be(Ord(ftInteger)).Because('Id deve ser ftInteger');
  Should(Ord(FDataSet.FieldByName('Name').DataType)).Be(Ord(ftWideString)).Because('Name deve ser ftWideString');
  Should(Ord(FDataSet.FieldByName('Price').DataType)).Be(Ord(ftCurrency)).Because('Price deve ser ftCurrency');
  Should(Ord(FDataSet.FieldByName('StockQty').DataType)).Be(Ord(ftLargeint)).Because('StockQty deve ser ftLargeint');
  Should(Ord(FDataSet.FieldByName('Active').DataType)).Be(Ord(ftBoolean)).Because('Active deve ser ftBoolean');
  Should(Ord(FDataSet.FieldByName('CreatedAt').DataType)).Be(Ord(ftDateTime)).Because('CreatedAt deve ser ftDateTime');
  Should(Ord(FDataSet.FieldByName('Photo').DataType)).Be(Ord(ftBlob)).Because('Photo deve ser ftBlob');
end;

procedure TProductDataSetTests.Test_Currency_Value;
begin
  Should(FDataSet.FieldByName('Price').AsCurrency).Be(Currency(29.90)).Because('Price do primeiro produto');
end;

procedure TProductDataSetTests.Test_Int64_Value;
begin
  FDataSet.Last; // Terceiro produto: StockQty = 9999999999
  Should(FDataSet.FieldByName('StockQty').AsLargeInt).Be(Int64(9999999999)).Because('StockQty do Tool Gamma');
end;

procedure TProductDataSetTests.Test_DateTime_Value;
begin
  var DT := FDataSet.FieldByName('CreatedAt').AsDateTime;
  var Y, M, D: Word;
  DecodeDate(DT, Y, M, D);
  Should(Integer(Y)).Be(2026).Because('Ano do CreatedAt deve ser 2026');
  Should(Integer(M)).Be(1).Because('Mês do CreatedAt deve ser Janeiro');
  Should(Integer(D)).Be(15).Because('Dia do CreatedAt deve ser 15');
end;

procedure TProductDataSetTests.Test_Boolean_Value;
begin
  Should(FDataSet.FieldByName('Active').AsBoolean).BeTrue.Because('Primeiro produto deve estar ativo');
  FDataSet.Last;
  Should(FDataSet.FieldByName('Active').AsBoolean).BeFalse.Because('Último produto deve estar inativo');
end;

procedure TProductDataSetTests.Test_Blob_FieldType;
begin
  var PhotoField := FDataSet.FieldByName('Photo');
  Should(PhotoField is TBlobField).BeTrue.Because('Photo deve ser TBlobField');
end;

procedure TProductDataSetTests.Test_Blob_IsNull_When_Empty;
begin
  FDataSet.Next; // Segundo produto: Photo = nil
  Should(FDataSet.FieldByName('Photo').IsNull).BeTrue.Because('Photo nil deve ser IsNull');
end;

procedure TProductDataSetTests.Test_Blob_Read_Stream;
var
  Stream: TStream;
begin
  FDataSet.First; // Primeiro produto tem Photo [$FF, $D8, $FF, $E0]
  Stream := FDataSet.CreateBlobStream(FDataSet.FieldByName('Photo'), bmRead);
  try
    Should(Stream.Size).Be(4).Because('Tamanhos do stream de foto incorreto');
    var B: Byte;
    Stream.Read(B, 1);
    Should(B).Be($FF);
    Stream.Read(B, 1);
    Should(B).Be($D8);
  finally
    Stream.Free;
  end;
end;

procedure TProductDataSetTests.Test_Blob_Write_Stream;
var
  Stream: TStream;
  PhotoField: TField;
begin
  FDataSet.First;
  PhotoField := FDataSet.FieldByName('Photo');

  FDataSet.Edit;
  Stream := FDataSet.CreateBlobStream(PhotoField, bmWrite);
  try
    var NewData: TBytes := [$01, $02, $03];
    Stream.Write(NewData[0], 3);
  finally
    Stream.Free; // Salva no campo (se seguir padrão Spring4D)
  end;
  FDataSet.Post;

  // Verificar na entidade real
  var P1 := TProductTest(FProducts[0]);
  Should(Length(P1.Photo)).Be(3);
  Should(P1.Photo[0]).Be($01);
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
      var Text := Reader.ReadToEnd;
      Should(Text).Be('Premium widget for daily use');
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
      Writer.Write('Novo Texto Longo');
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
  FDataSet.Post;

  var P1 := TProductTest(FProducts[0]);
  Should(P1.Description).Be('Novo Texto Longo');
end;

procedure TProductDataSetTests.Test_Sort_By_Name;
begin
  FDataSet.IndexFieldNames := 'Name';
  FDataSet.First;
  // Ordem alfabética: Gadget Beta, Tool Gamma, Widget Alpha
  Should(FDataSet.FieldByName('Name').AsString).Be('Gadget Beta').Because('Primeiro após sort por Name');
  FDataSet.Last;
  Should(FDataSet.FieldByName('Name').AsString).Be('Widget Alpha').Because('Último após sort por Name');
end;

procedure TProductDataSetTests.Test_Sort_By_Price;
begin
  FDataSet.IndexFieldNames := 'Price';
  FDataSet.First;
  // Ordem por Price: 9.50, 29.90, 149.99
  Should(FDataSet.FieldByName('Name').AsString).Be('Tool Gamma').Because('Produto mais barato primeiro');
  FDataSet.Last;
  Should(FDataSet.FieldByName('Name').AsString).Be('Gadget Beta').Because('Produto mais caro por último');
end;

procedure TProductDataSetTests.Test_Filter_By_Price;
begin
  FDataSet.Filter := 'Price > 20';
  FDataSet.Filtered := True;
  Should(FDataSet.RecordCount).Be(2).Because('Dois produtos com Price > 20');
end;

// ===========================================================================
//  TMasterDetailDataSetTests 
// ===========================================================================

procedure TMasterDetailDataSetTests.Setup;
begin
  FMasterDS := TEntityDataSet.Create(nil);
  FDetailDS := TEntityDataSet.Create(nil);

  // Master: 2 "orders" usando TUserTest como container simples
  SetLength(FOrders, 2);
  var U1 := TUserTest.Create;
  U1.Id := 100;
  U1.Name := 'Order-100';
  U1.Score := 0;
  U1.Active := True;
  FOrders[0] := U1;

  var U2 := TUserTest.Create;
  U2.Id := 200;
  U2.Name := 'Order-200';
  U2.Score := 0;
  U2.Active := True;
  FOrders[1] := U2;

  // Detail: 4 items (2 para cada order)
  SetLength(FItems, 4);

  var It1 := TOrderItemTest.Create;
  It1.Id := 1;
  It1.OrderId := 100;
  It1.ProductName := 'Widget A';
  It1.Quantity := 3;
  It1.UnitPrice := 10.00;
  FItems[0] := It1;

  var It2 := TOrderItemTest.Create;
  It2.Id := 2;
  It2.OrderId := 100;
  It2.ProductName := 'Widget B';
  It2.Quantity := 1;
  It2.UnitPrice := 25.50;
  FItems[1] := It2;

  var It3 := TOrderItemTest.Create;
  It3.Id := 3;
  It3.OrderId := 200;
  It3.ProductName := 'Gadget X';
  It3.Quantity := 5;
  It3.UnitPrice := 99.99;
  FItems[2] := It3;

  var It4 := TOrderItemTest.Create;
  It4.Id := 4;
  It4.OrderId := 200;
  It4.ProductName := 'Gadget Y';
  It4.Quantity := 2;
  It4.UnitPrice := 150.00;
  FItems[3] := It4;

  FMasterDS.Load(FOrders, TUserTest);
  FDetailDS.Load(FItems, TOrderItemTest);
end;

procedure TMasterDetailDataSetTests.TearDown;
begin
  FDetailDS.Free;
  FMasterDS.Free;
  for var I := 0 to High(FItems) do
    FItems[I].Free;
  for var I := 0 to High(FOrders) do
    FOrders[I].Free;
end;

procedure TMasterDetailDataSetTests.Test_Detail_Count;
begin
  Should(FDetailDS.RecordCount).Be(4).Because('Deve ter 4 itens no detalhe total');
end;

procedure TMasterDetailDataSetTests.Test_Detail_FilterByMaster;
begin
  // Filtrar itens do OrderId = 100
  FDetailDS.Filter := 'OrderId = 100';
  FDetailDS.Filtered := True;
  Should(FDetailDS.RecordCount).Be(2).Because('Order 100 deve ter 2 itens');

  // Mudar para OrderId = 200
  FDetailDS.Filter := 'OrderId = 200';
  Should(FDetailDS.RecordCount).Be(2).Because('Order 200 deve ter 2 itens');
end;

procedure TMasterDetailDataSetTests.Test_Detail_FieldValues;
begin
  FDetailDS.First;
  Should(FDetailDS.FieldByName('ProductName').AsString).Be('Widget A').Because('Primeiro item deve ser Widget A');
  Should(FDetailDS.FieldByName('Quantity').AsInteger).Be(3).Because('Quantidade do primeiro item');
  Should(FDetailDS.FieldByName('UnitPrice').AsCurrency).Be(Currency(10.00)).Because('Preço unitário do primeiro item');
end;

procedure TMasterDetailDataSetTests.Test_Detail_Required_Fields;
begin
  // OrderId tem [Required]
  Should(FDetailDS.FieldByName('OrderId').Required).BeTrue.Because('OrderId deve ser Required');
  // ProductName tem [Required]
  Should(FDetailDS.FieldByName('ProductName').Required).BeTrue.Because('ProductName deve ser Required');
  // Quantity tem [Required]
  Should(FDetailDS.FieldByName('Quantity').Required).BeTrue.Because('Quantity deve ser Required');
  // UnitPrice tem [Required]
  Should(FDetailDS.FieldByName('UnitPrice').Required).BeTrue.Because('UnitPrice deve ser Required');
end;

// ===========================================================================
//  TEntityDataSetCRUDTests (Operações em Lista Real)
// ===========================================================================

procedure TEntityDataSetCRUDTests.Setup;
begin
  FDataSet := TEntityDataSet.Create(nil);
  FSourceList := TCollections.CreateList<TObject>(True); // Owning list
  
  var U1 := TUserTest.Create;
  U1.Id := 1;
  U1.Name := 'Cesar';
  FSourceList.Add(U1);
  
  // Associar ao DataSet mantendo a referência real (FOwnsItems = False aqui)
  FDataSet.Load(FSourceList, TUserTest, False);
end;

procedure TEntityDataSetCRUDTests.TearDown;
begin
  FDataSet.Free;
  FSourceList := nil;
end;

procedure TEntityDataSetCRUDTests.Test_Append_And_Post;
begin
  Should(FSourceList.Count).Be(1).Because('Lista inicial deve ter 1');
  
  FDataSet.Append;
  FDataSet.FieldByName('Id').AsInteger := 10;
  FDataSet.FieldByName('Name').AsString := 'New User';
  FDataSet.Post;
  
  Should(FDataSet.RecordCount).Be(2).Because('DataSet deve ter 2 registros agora');
  Should(FSourceList.Count).Be(2).Because('A lista real de origem DEVE ter aumentado');
  
  var NewObj := TUserTest(FSourceList[1]);
  Should(NewObj.Name).Be('New User').Because('O objeto na lista real deve estar preenchido');
end;

procedure TEntityDataSetCRUDTests.Test_Edit_Existing;
begin
  FDataSet.First;
  FDataSet.Edit;
  FDataSet.FieldByName('Name').AsString := 'Cesar Editado';
  FDataSet.Post;
  
  Should(TUserTest(FSourceList[0]).Name).Be('Cesar Editado').Because('Objeto original deve refletir edição');
end;

procedure TEntityDataSetCRUDTests.Test_Delete_And_Sync;
begin
  Should(FDataSet.RecordCount).Be(1);
  
  FDataSet.First;
  FDataSet.Delete;
  
  Should(FDataSet.RecordCount).Be(0).Because('DataSet deve estar vazio');
  Should(FSourceList.Count).Be(0).Because('Lista real deve estar vazia');
end;

procedure TEntityDataSetCRUDTests.Test_Locate_After_CRUD;
begin
  // Adiciona um via dataset
  FDataSet.Append;
  FDataSet.FieldByName('Id').AsInteger := 55;
  FDataSet.FieldByName('Name').AsString := 'Target';
  FDataSet.Post;
  
  var Found := FDataSet.Locate('Name', 'Target', []);
  Should(Found).BeTrue.Because('Locate deve achar registro recém inserido');
  Should(FDataSet.FieldByName('Id').AsInteger).Be(55);
end;

procedure TEntityDataSetCRUDTests.Test_Filter_After_Append;
begin
  FDataSet.Filter := 'Id > 5';
  FDataSet.Filtered := True;
  
  Should(FDataSet.RecordCount).Be(0).Because('Cesar (Id=1) não passa no filtro > 5');
  
  FDataSet.Append;
  FDataSet.FieldByName('Id').AsInteger := 10;
  FDataSet.FieldByName('Name').AsString := 'Passes Filter';
  FDataSet.Post;
  
  Should(FDataSet.RecordCount).Be(1).Because('Apenas o novo registro (Id=10) deve ser visível');
  Should(FDataSet.FieldByName('Name').AsString).Be('Passes Filter');
end;

procedure TEntityDataSetCRUDTests.Test_Insert_Between_Records;
begin
  // Record 1: Id 1 (Cesar) - Adicionado no Setup
  
  // Adicionar Record 3: Id 3 (Romero) no final
  FDataSet.Append;
  FDataSet.FieldByName('Id').AsInteger := 3;
  FDataSet.FieldByName('Name').AsString := 'Romero';
  FDataSet.Post;

  FDataSet.Last;
  Should(FDataSet.RecordCount).Be(2);
  Should(FDataSet.FieldByName('Id').AsInteger).Be(3);

  // Inserir entre Record 1 e Record 3
  FDataSet.Insert; // Deve inserir ANTES de Record 3
  FDataSet.FieldByName('Id').AsInteger := 2;
  FDataSet.FieldByName('Name').AsString := 'Meio';
  FDataSet.Post;

  Should(FDataSet.RecordCount).Be(3);

  // VERIFICAR ORDEM (Deve ser 1, 2, 3)
  FDataSet.First;
  Should(FDataSet.FieldByName('Id').AsInteger).Be(1).Because('Primeiro deve ser Cesar (Id=1)');
  FDataSet.Next;
  Should(FDataSet.FieldByName('Id').AsInteger).Be(2).Because('Segundo deve ser o novo (Id=2) e não o Romero');
  FDataSet.Next;
  Should(FDataSet.FieldByName('Id').AsInteger).Be(3).Because('Terceiro deve ser Romero (Id=3)');
end;

procedure TEntityDataSetCRUDTests.Test_Insert_With_Sort;
begin
  // Record 1: Id 1 (Cesar) - from Setup
  
  // Add Record 3
  FDataSet.Append;
  FDataSet.FieldByName('Id').AsInteger := 3;
  FDataSet.FieldByName('Name').AsString := 'Romero';
  FDataSet.Post;
  
  // Apply sort by Name ascending
  FDataSet.IndexFieldNames := 'Name';
  
  // After sort: Cesar(1), Romero(3)
  FDataSet.First;
  Should(FDataSet.FieldByName('Name').AsString).Be('Cesar').Because('Sort: Cesar first');
  FDataSet.Next;
  Should(FDataSet.FieldByName('Name').AsString).Be('Romero').Because('Sort: Romero second');
  
  // Insert while on Romero - new record with Name 'Meio' should appear
  // between Cesar and Romero after sort is applied
  FDataSet.Insert;
  FDataSet.FieldByName('Id').AsInteger := 2;
  FDataSet.FieldByName('Name').AsString := 'Meio';
  FDataSet.Post;
  
  Should(FDataSet.RecordCount).Be(3);
  
  // With sort by Name: Cesar, Meio, Romero
  FDataSet.First;
  Should(FDataSet.FieldByName('Name').AsString).Be('Cesar').Because('Sort after insert: Cesar first');
  FDataSet.Next;
  Should(FDataSet.FieldByName('Name').AsString).Be('Meio').Because('Sort after insert: Meio second');
  FDataSet.Next;
  Should(FDataSet.FieldByName('Name').AsString).Be('Romero').Because('Sort after insert: Romero third');
end;

{ TEntityDataSetStressTests }

procedure TEntityDataSetStressTests.Setup;
var
  i: Integer;
begin
  FUsers := TCollections.CreateList<TObject>(True);
  for i := 1 to 1000 do
  begin
    var User := TUserTest.Create;
    User.Id := i;
    User.Name := 'User ' + i.ToString.PadLeft(4, '0');
    User.Score := i * 1.5;
    User.Active := (i mod 2 = 0);
    FUsers.Add(User);
  end;

  FDataSet := TEntityDataSet.Create(nil);
  FDataSet.Load(FUsers, TUserTest, False);
  FDataSet.Open;
end;

procedure TEntityDataSetStressTests.TearDown;
begin
  FDataSet.Free;
  FUsers := nil; // ARC handled by TCollections
end;

procedure TEntityDataSetStressTests.Test_Stress_1k_Records_Filter;
begin
  FDataSet.Filter := 'Score > 1000';
  FDataSet.Filtered := True;
  
  // Total 1000 users. i*1.5 > 1000 => i > 666.6 => 1000 - 666 = 334 records
  Should(FDataSet.RecordCount).Be(334);
  
  FDataSet.First;
  Should(FDataSet.FieldByName('Id').AsInteger).Be(667);
  
  FDataSet.Last;
  Should(FDataSet.FieldByName('Id').AsInteger).Be(1000);
end;

procedure TEntityDataSetStressTests.Test_Edit_Into_Filter_Hides_Record;
begin
  FDataSet.Filter := 'Active = True';
  FDataSet.Filtered := True;
  
  Should(FDataSet.RecordCount).Be(500).Because('Half of the records are active');
  
  FDataSet.First;
  Should(FDataSet.FieldByName('Id').AsInteger).Be(2);
  
  // Edit record 2 to be inactive
  FDataSet.Edit;
  FDataSet.FieldByName('Active').AsBoolean := False;
  FDataSet.Post;
  
  Should(FDataSet.RecordCount).Be(499).Because('One active record became inactive');
  Should(FDataSet.FieldByName('Id').AsInteger).NotBe(2).Because('The inactive record should no longer be visible');
end;

procedure TEntityDataSetStressTests.Test_MultiFieldSort_Composite;
begin
  // Sort by Active (False=0, True=1) then Id DESC
  FDataSet.IndexFieldNames := 'Active;Id DESC';
  
  FDataSet.First;
  // First record should be Active=False (0) and the highest Id (999)
  Should(FDataSet.FieldByName('Active').AsBoolean).BeFalse;
  Should(FDataSet.FieldByName('Id').AsInteger).Be(999);
  
  FDataSet.Next;
  Should(FDataSet.FieldByName('Id').AsInteger).Be(997);
  
  // Go to start of Active=True block
  FDataSet.Filter := 'Active = True';
  FDataSet.Filtered := True;
  FDataSet.First;
  Should(FDataSet.FieldByName('Active').AsBoolean).BeTrue;
  Should(FDataSet.FieldByName('Id').AsInteger).Be(1000);
end;

procedure TEntityDataSetStressTests.Test_Edit_Record_While_Filtered_Maintains_Position;
begin
  FDataSet.Filter := 'Id <= 5';
  FDataSet.Filtered := True;
  
  FDataSet.First; // Id = 1 (inactive)
  FDataSet.Next;  // Id = 2 (active)
  
  Should(FDataSet.FieldByName('Id').AsInteger).Be(2);
  
  // Edit Name only (Active remains True)
  FDataSet.Edit;
  FDataSet.FieldByName('Name').AsString := 'Updated 2';
  FDataSet.Post;
  
  // Should stay on ID 2
  Should(FDataSet.FieldByName('Id').AsInteger).Be(2);
  Should(FDataSet.FieldByName('Name').AsString).Be('Updated 2');
end;

end.
