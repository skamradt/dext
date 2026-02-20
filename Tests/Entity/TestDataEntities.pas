unit TestDataEntities;

interface

uses
  System.SysUtils, System.Classes, Dext.Entity.Attributes, Dext.Entity.TypeConverters, Dext.Types.UUID;

type
  TUserRole = (urUser, urAdmin, urSuperAdmin);

  [Table('test_guid_entities')]
  TGuidEntity = class
  private
    FId: TGUID;
    FName: string;
  public
    [PK]
    property Id: TGUID read FId write FId;
    [Column('name')]
    property Name: string read FName write FName;
  end;

  [Table('test_uuid_entities')]
  TUuidEntity = class
  private
    FId: TUUID;
    FName: string;
  public
    [PK]
    property Id: TUUID read FId write FId;
    [Column('name')]
    property Name: string read FName write FName;
  end;

  [Table('test_composite_guid_int')]
  TCompositeGuidInt = class
  private
    FGuidKey: TGUID;
    FIntKey: Integer;
    FData: string;
  public
    [PK]
    property GuidKey: TGUID read FGuidKey write FGuidKey;
    [PK]
    [Column('int_key')]
    property IntKey: Integer read FIntKey write FIntKey;
    [Column('data')]
    property Data: string read FData write FData;
  end;

  [Table('test_composite_int_datetime')]
  TCompositeIntDateTime = class
  private
    FId: Integer;
    FTimestamp: TDateTime;
    FValue: string;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    [PK]
    [Column('timestamp')]
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
    [Column('value')]
    property Value: string read FValue write FValue;
  end;

  [Table('test_enum_entities')]
  TEnumEntity = class
  private
    FId: Integer;
    FRole: TUserRole;
    FStatus: TUserRole;
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    [Column('role_int')]
    property Role: TUserRole read FRole write FRole;
    [Column('role_name'), EnumAsString]
    property Status: TUserRole read FStatus write FStatus;
  end;

  {$M+}
  TJsonMetadata = class
  private
    FName: string;
    FValue: Integer;
  published
    property Name: string read FName write FName;
    property Value: Integer read FValue write FValue;
  end;

  [Table('test_json_entities')]
  TJsonEntity = class
  private
    FId: Integer;
    FMetadata: TJsonMetadata;
  public
    constructor Create;
    destructor Destroy; override;
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    [Column('metadata'), JsonColumn]
    property Metadata: TJsonMetadata read FMetadata write FMetadata;
  end;

implementation

{ TJsonEntity }

constructor TJsonEntity.Create;
begin
  FMetadata := TJsonMetadata.Create;
end;

destructor TJsonEntity.Destroy;
begin
  FMetadata.Free;
  inherited;
end;

end.
