unit WebFrameworkTests.Tests.DataApi;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Net.HttpClient,
  System.Net.URLClient,
  Dext.Json,
  Dext.Json.Types,
  FireDAC.Comp.Client,
  WebFrameworkTests.Tests.Base,
  Dext.Web.DataApi,
  Dext.Entity.Attributes,
  Dext.Entity.Dialects,
  Dext.Entity.Drivers.FireDAC,
  Dext.Web.Interfaces,
  Dext,
  Dext.Entity,
  Dext.Web;

type
  [Table('TestItems')]
  TTestItem = class
  private
    FId: Integer;
    FName: string;
    FValue: Integer;
    FActive: Boolean;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Value: Integer read FValue write FValue;
    property Active: Boolean read FActive write FActive;
  end;

  TCategoryDTO = class
  private
    FName: string;
  public
    property Name: string read FName write FName;
  end;

  TItemWithCategoryDTO = class
  private
    FId: Integer;
    FName: string;
    FCategory: TCategoryDTO;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    [Nested]
    property Category: TCategoryDTO read FCategory write FCategory;
  end;

  TDataApiTest = class(TBaseTest)
  private
    FConn: IDbConnection;
    FDb: TDbContext;
    procedure SeedData;
  protected
    procedure ConfigureHost(const Builder: IWebHostBuilder); override;
    procedure Setup; override;
    procedure TearDown; override;
  public
    procedure Run; override;
  end;

implementation

{ TDataApiTest }

procedure TDataApiTest.Setup;
var
  FD: TFDConnection;
begin
  FD := TFDConnection.Create(nil);
  FD.DriverName := 'SQLite';
  FD.Params.Values['Database'] := ':memory:';
  FConn := TFireDACConnection.Create(FD);
  FDb := TDbContext.Create(FConn, TSQLiteDialect.Create);
  FDb.Entities<TTestItem>;
  FDb.EnsureCreated;
  SeedData;
  inherited;
end;

procedure TDataApiTest.TearDown;
begin
  inherited;
  FDb.Free;
end;

procedure TDataApiTest.SeedData;
begin
  for var i := 1 to 10 do
  begin
    var Item := TTestItem.Create;
    Item.Name := 'Item ' + i.ToString;
    Item.Value := i * 10;
    Item.Active := (i mod 2 = 0);
    FDb.Entities<TTestItem>.Add(Item);
  end;
  FDb.SaveChanges;
end;

procedure TDataApiTest.ConfigureHost(const Builder: IWebHostBuilder);
begin
  // Register the existing DbContext instance as a singleton so the API can resolve it
  Builder.ConfigureServices(procedure(Services: IServiceCollection)
    begin
      TDextServices.Create(Services).AddSingletonInstance<TDbContext>(FDb);
    end);

  Builder.Configure(procedure(App: IApplicationBuilder)
    var
      AB: AppBuilder;
    begin
        AB := AppBuilder.Create(App);

        // Use the new fluent syntax
        AB.MapDataApi<TTestItem>('/api/test-items');

        // Multi-Mapping test endpoint
        AB.MapDataApi<TItemWithCategoryDTO>('/api/composed-items',
          DataApiOptions.UseSql('SELECT Id, Name, "Category A" as Category_Name FROM TestItems')
        );
    end);
end;

procedure TDataApiTest.Run;
var
  Resp: System.Net.HttpClient.IHTTPResponse;
  JsonArray: IDextJsonArray;
  Item: IDextJsonObject;
  Node: IDextJsonNode;
begin
  Log('--- Starting Data API Tests ---');

  // 1. Test basic GetList
  Resp := FClient.Get(GetBaseUrl + '/api/test-items');
  AssertEqual('200', Resp.StatusCode.ToString, 'Basic GET List');
  
  Node := TDextJson.Provider.Parse(Resp.ContentAsString);
  if (Node <> nil) and (Node.GetNodeType = jntArray) then
  begin
    JsonArray := Node as IDextJsonArray;
    AssertTrue(JsonArray.GetCount = 10, 'Should return 10 items', 'Returned ' + JsonArray.GetCount.ToString + ' items');
  end
  else
    LogError('Failed to parse JSON response as array');

  // 2. Test dynamic filter: _gt (greater than)
  Resp := FClient.Get(GetBaseUrl + '/api/test-items?value_gt=50');
  JsonArray := TDextJson.Provider.Parse(Resp.ContentAsString) as IDextJsonArray;
  AssertTrue(JsonArray.GetCount = 5, 'Should return 5 items (60, 70, 80, 90, 100)', 'Returned ' + JsonArray.GetCount.ToString + ' items');

  // 3. Test dynamic filter: _eq
  Resp := FClient.Get(GetBaseUrl + '/api/test-items?name=Item 3');
  JsonArray := TDextJson.Provider.Parse(Resp.ContentAsString) as IDextJsonArray;
  AssertTrue(JsonArray.GetCount = 1, 'Should return 1 item', 'Returned ' + JsonArray.GetCount.ToString + ' items');

  // 4. Test pagination: _limit and _offset
  Resp := FClient.Get(GetBaseUrl + '/api/test-items?_limit=2&_offset=1');
  JsonArray := TDextJson.Provider.Parse(Resp.ContentAsString) as IDextJsonArray;
  AssertTrue(JsonArray.GetCount = 2, 'Should return 2 items due to limit', 'Returned ' + JsonArray.GetCount.ToString + ' items');
  AssertEqual('Item 2', JsonArray.GetObject(0).GetString('name'), 'First item should be Item 2 (offset 1)');

  // 5. Test ordering: _orderby
  Resp := FClient.Get(GetBaseUrl + '/api/test-items?_orderby=Value desc&_limit=1');
  JsonArray := TDextJson.Provider.Parse(Resp.ContentAsString) as IDextJsonArray;
  AssertEqual('100', JsonArray.GetObject(0).GetInteger('value').ToString, 'Highest value should be 100');

  // 6. Test Multi-Mapping (Joins/Nested)
  Resp := FClient.Get(GetBaseUrl + '/api/composed-items?_limit=1');
  AssertEqual('200', Resp.StatusCode.ToString, 'Composed GET List');
  JsonArray := TDextJson.Provider.Parse(Resp.ContentAsString) as IDextJsonArray;
  AssertTrue(JsonArray.GetCount = 1, 'Should return 1 item', 'Should return 1 item but got ' + JsonArray.GetCount.ToString);
  
  Item := JsonArray.GetObject(0);
  AssertTrue(Item.Contains('category'), 'Should have nested category object', 'category object missing');
  AssertEqual('Category A', Item.GetObject('category').GetString('name'), 'Nested category name should match');

  Log('--- Data API Tests Completed ---');
end;

end.
