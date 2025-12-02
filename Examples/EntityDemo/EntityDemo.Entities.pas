unit EntityDemo.Entities;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Dext.Persistence,
  Dext.Specifications.Base,
  Dext.Types.Nullable;

type
  TUser = class; // Forward declaration

  [Table('addresses')]
  TAddress = class
  private
    FId: Integer;
    FStreet: string;
    FCity: string;
    FUsers: Lazy<TList<TUser>>;
    function GetUsers: TList<TUser>;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Street: string read FStreet write FStreet;
    property City: string read FCity write FCity;
    
    [NotMapped]
    property Users: TList<TUser> read GetUsers;
  end;

  [Table('users')]
  TUser = class
  private
    FId: Integer;
    FName: string;
    FAge: Integer;
    FEmail: string;
    FCity: string;
    FAddressId: Nullable<Integer>;
    FAddress: Lazy<TAddress>;
    function GetAddress: TAddress;
    procedure SetAddress(const Value: TAddress);
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;

    [Column('full_name')]
    property Name: string read FName write FName;

    property Age: Integer read FAge write FAge;
    property Email: string read FEmail write FEmail;
    property City: string read FCity write FCity;
    
    [Column('address_id')]
    property AddressId: Nullable<Integer> read FAddressId write FAddressId;

    [ForeignKey('AddressId', caCascade), NotMapped]  // CASCADE on delete
    property Address: TAddress read GetAddress write SetAddress;
  end;

  [Table('order_items')]
  TOrderItem = class
  private
    FOrderId: Integer;
    FProductId: Integer;
    FQuantity: Integer;
    FPrice: Double;
  public
    [PK, Column('order_id')]
    property OrderId: Integer read FOrderId write FOrderId;

    [PK,Column('product_id')]
    property ProductId: Integer read FProductId write FProductId;

    property Quantity: Integer read FQuantity write FQuantity;
    property Price: Double read FPrice write FPrice;
  end;

  [Table('products')]
  TProduct = class
  private
    FId: Integer;
    FName: string;
    FPrice: Double;
    FVersion: Integer;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Price: Double read FPrice write FPrice;
    
    [Version]
    property Version: Integer read FVersion write FVersion;
  end;

  // 🧬 Metadata Implementation (TypeOf)
  UserEntity = class
  public
    class var Id: TPropExpression;
    class var Name: TPropExpression;
    class var Age: TPropExpression;
    class var Email: TPropExpression;
    class var City: TPropExpression;
    class var AddressId: TPropExpression;
    class var Address: TPropExpression;

    class constructor Create;
  end;

  // Specification using Metadata
  TAdultUsersSpec = class(TSpecification<TUser>)
  public
    constructor Create; override;
  end;

implementation

{ TAddress }

constructor TAddress.Create;
begin
  inherited Create;
  // FUsers is initialized as empty Lazy (default record)
  // For new objects created by user:
  FUsers := Lazy<TList<TUser>>.CreateFrom(TList<TUser>.Create);
end;

destructor TAddress.Destroy;
begin
  if FUsers.IsValueCreated then
    FUsers.Value.Free;
  inherited;
end;

function TAddress.GetUsers: TList<TUser>;
begin
  Result := FUsers.Value;
end;

{ TUser }

function TUser.GetAddress: TAddress;
begin
  Result := FAddress.Value;
end;

procedure TUser.SetAddress(const Value: TAddress);
begin
  // When setting manually, we wrap it in a Lazy that is already created
  FAddress := Lazy<TAddress>.CreateFrom(Value);
end;

{ UserEntity }

class constructor UserEntity.Create;
begin
  Id := TPropExpression.Create('Id');
  Name := TPropExpression.Create('full_name');
  Age := TPropExpression.Create('Age');
  Email := TPropExpression.Create('Email');
  City := TPropExpression.Create('City');
  AddressId := TPropExpression.Create('AddressId');
  Address := TPropExpression.Create('Address');
end;

{ TAdultUsersSpec }

constructor TAdultUsersSpec.Create;
begin
  inherited Create;
  // Now we can use the typed metadata!
  Where(UserEntity.Age >= 18);
end;

end.
