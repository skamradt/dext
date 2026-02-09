unit Sales.Tests.Serialization;

{***************************************************************************}
{                                                                           }
{           Sales System - Serialization Unit Tests                         }
{                                                                           }
{           Tests DTO serialization/deserialization without HTTP            }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Testing;

type
  // Test DTOs - mirror the real ones for isolated testing
  TTestLoginRequest = record
    username: string;
    password: string;
  end;

  TTestLoginResponse = record
    token: string;
  end;

  TTestOrderItemDto = record
    productId: Integer;
    quantity: Integer;
  end;

  /// <summary>
  ///   Unit tests for DTO serialization.
  ///   Tests JSON conversion in isolation, without HTTP layer.
  /// </summary>
  [TestFixture]
  TSerializationTests = class
  public
    // Login DTOs
    [Test]
    procedure LoginRequest_Should_Deserialize_From_Json;

    [Test]
    procedure LoginResponse_Should_Serialize_To_Json;

    // Order DTOs
    [Test]
    procedure OrderItemDto_Should_Deserialize_From_Json;

    [Test]
    procedure OrderItemDto_Array_Should_Deserialize_From_Json;

    [Test]
    procedure OrderDto_Should_Handle_Empty_Items;

    // Edge Cases
    [Test]
    procedure Should_Handle_CamelCase_Json;
  end;

implementation

uses
  System.SysUtils,
  System.JSON,
  Dext.Json;

{ TSerializationTests }

procedure TSerializationTests.LoginRequest_Should_Deserialize_From_Json;
var
  Json: string;
  Request: TTestLoginRequest;
begin
  // Arrange
  Json := '{"username":"admin","password":"secret123"}';

  // Act
  Request := TDextJson.Deserialize<TTestLoginRequest>(Json);

  // Assert
  Should(Request.username).Be('admin');
  Should(Request.password).Be('secret123');
end;

procedure TSerializationTests.LoginResponse_Should_Serialize_To_Json;
var
  Response: TTestLoginResponse;
  Json: string;
  JsonObj: TJSONObject;
begin
  // Arrange
  Response.token := 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.test';

  // Act
  Json := TDextJson.Serialize(Response);

  // Assert
  JsonObj := TJSONObject.ParseJSONValue(Json) as TJSONObject;
  try
    Should(JsonObj).NotBeNil.Because('Should produce valid JSON');
    Should(JsonObj.GetValue<string>('token')).Be(Response.token);
  finally
    JsonObj.Free;
  end;
end;

procedure TSerializationTests.OrderItemDto_Should_Deserialize_From_Json;
var
  Json: string;
  Item: TTestOrderItemDto;
begin
  // Arrange
  Json := '{"productId":42,"quantity":5}';

  // Act
  Item := TDextJson.Deserialize<TTestOrderItemDto>(Json);

  // Assert
  Should(Item.productId).Be(42);
  Should(Item.quantity).Be(5);
end;

procedure TSerializationTests.OrderItemDto_Array_Should_Deserialize_From_Json;
var
  Json: string;
  Items: TArray<TTestOrderItemDto>;
begin
  // Arrange
  Json := '[{"productId":1,"quantity":2},{"productId":3,"quantity":4}]';

  // Act
  Items := TDextJson.Deserialize<TArray<TTestOrderItemDto>>(Json);

  // Assert
  Should(Length(Items)).Be(2);
  Should(Items[0].productId).Be(1);
  Should(Items[0].quantity).Be(2);
  Should(Items[1].productId).Be(3);
  Should(Items[1].quantity).Be(4);
end;

procedure TSerializationTests.OrderDto_Should_Handle_Empty_Items;
var
  Json: string;
  Items: TArray<TTestOrderItemDto>;
begin
  // Arrange
  Json := '[]';

  // Act
  Items := TDextJson.Deserialize<TArray<TTestOrderItemDto>>(Json);

  // Assert
  Should(Length(Items)).Be(0)
    .Because('Empty array should deserialize to empty array');
end;

procedure TSerializationTests.Should_Handle_CamelCase_Json;
var
  Json: string;
  Item: TTestOrderItemDto;
begin
  // Arrange - camelCase (JavaScript style)
  Json := '{"productId":10,"quantity":3}';

  // Act
  Item := TDextJson.Deserialize<TTestOrderItemDto>(Json);

  // Assert
  Should(Item.productId).Be(10);
  Should(Item.quantity).Be(3);
end;

end.
