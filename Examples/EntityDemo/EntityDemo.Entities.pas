unit EntityDemo.Entities;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Dext.Persistence,
  Dext.Collections, // Add Collections
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
    FUsers: Lazy<IList<TUser>>; // Changed to IList
    function GetUsers: IList<TUser>; // Changed to IList
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class function NewInstance: TObject; override;

    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Street: string read FStreet write FStreet;
    property City: string read FCity write FCity;
    
    [NotMapped]
    property Users: IList<TUser> read GetUsers; // Changed to IList
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
    
    destructor Destroy; override;
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
  // Initialize FUsers with empty list
  // This is needed for manually created entities (not from DB)
  // For entities from DB, TLazyInjector will replace this with TLazyLoader
  FUsers := Lazy<IList<TUser>>.CreateFrom(TCollections.CreateObjectList<TUser>(False), True);
end;

destructor TAddress.Destroy;
begin
  // FUsers is managed. If it holds an interface, ARC handles it.
  // If lazy created a value, Lazy destructor might free it if not interface?
  // Lazy<T> implementation handles interface/object distinction usually?
  // Actually, Dext.Types.Lazy might need check.
  // Generally, if T is interface, Delphi manages it.
  inherited;
end;

class function TAddress.NewInstance: TObject;
begin
  Result := inherited NewInstance;
end;

function TAddress.GetUsers: IList<TUser>;
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

destructor TUser.Destroy;
begin
  inherited;
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
  Where(UserEntity.Age >= 18);
end;

end.
