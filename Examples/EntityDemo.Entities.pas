unit EntityDemo.Entities;

interface

uses
  Dext.Entity.Attributes,
  Dext.Specifications.Base,
  Dext.Specifications.Criteria,
  Dext.Specifications.Interfaces,
  Dext.Specifications.Types;

type
  [Table('addresses')]
  TAddress = class
  private
    FId: Integer;
    FStreet: string;
    FCity: string;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    property Street: string read FStreet write FStreet;
    property City: string read FCity write FCity;
  end;

  [Table('users')]
  TUser = class
  private
    FId: Integer;
    FName: string;
    FAge: Integer;
    FEmail: string;
    FAddressId: Integer;
    FAddress: TAddress;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;

    [Column('full_name')]
    property Name: string read FName write FName;

    property Age: Integer read FAge write FAge;
    property Email: string read FEmail write FEmail;
    
    [Column('address_id')]
    property AddressId: Integer read FAddressId write FAddressId;

    [ForeignKey('AddressId', caCascade)]  // CASCADE on delete
    property Address: TAddress read FAddress write FAddress;
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
    class var Id: TProp;
    class var Name: TProp;
    class var Age: TProp;
    
    class constructor Create;
  end;

  // Specification using Metadata
  TAdultUsersSpec = class(TSpecification<TUser>)
  public
    constructor Create; override;
  end;

implementation

{ UserEntity }

class constructor UserEntity.Create;
begin
  Id := TProp.Create('Id');
  Name := TProp.Create('Name'); // Should match property name, not column name for now, unless we map it
  Age := TProp.Create('Age');
end;

{ TAdultUsersSpec }

constructor TAdultUsersSpec.Create;
begin
  inherited Create;
  // Now we can use the typed metadata!
  Where(UserEntity.Age >= 18);
end;

end.
