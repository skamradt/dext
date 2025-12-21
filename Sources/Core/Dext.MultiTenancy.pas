unit Dext.MultiTenancy;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  ITenant = interface
    ['{B9FA6A6A-7F4C-4D3E-9A5B-1C2D3E4F5A6B}']
    function GetId: string;
    function GetName: string;
    function GetConnectionString: string;
    function GetSchema: string;
    function GetProperties: TDictionary<string, string>;
    
    property Id: string read GetId;
    property Name: string read GetName;
    property ConnectionString: string read GetConnectionString;
    property Schema: string read GetSchema;
    property Properties: TDictionary<string, string> read GetProperties;
  end;

  ITenantProvider = interface
    ['{DA7B3C4E-5F6A-7B8C-9D0E-1F2A3B4C5D6E}']
    function GetTenant: ITenant;
    procedure SetTenant(const ATenant: ITenant);
    property Tenant: ITenant read GetTenant write SetTenant;
  end;

  TTenant = class(TInterfacedObject, ITenant)
  private
    FId: string;
    FName: string;
    FConnectionString: string;
    FSchema: string;
    FProperties: TDictionary<string, string>;
  public
    constructor Create(const AId, AName, AConnectionString: string; const ASchema: string = '');
    destructor Destroy; override;
    
    function GetId: string;
    function GetName: string;
    function GetConnectionString: string;
    function GetSchema: string;
    function GetProperties: TDictionary<string, string>;
  end;

  TTenantProvider = class(TInterfacedObject, ITenantProvider)
  private
    FTenant: ITenant;
  public
    function GetTenant: ITenant;
    procedure SetTenant(const ATenant: ITenant);
  end;

implementation

{ TTenant }

constructor TTenant.Create(const AId, AName, AConnectionString, ASchema: string);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FConnectionString := AConnectionString;
  FSchema := ASchema;
  FProperties := TDictionary<string, string>.Create;
end;

destructor TTenant.Destroy;
begin
  FProperties.Free;
  inherited;
end;

function TTenant.GetConnectionString: string;
begin
  Result := FConnectionString;
end;

function TTenant.GetId: string;
begin
  Result := FId;
end;

function TTenant.GetName: string;
begin
  Result := FName;
end;

function TTenant.GetSchema: string;
begin
  Result := FSchema;
end;

function TTenant.GetProperties: TDictionary<string, string>;
begin
  Result := FProperties;
end;

{ TTenantProvider }

function TTenantProvider.GetTenant: ITenant;
begin
  Result := FTenant;
end;

procedure TTenantProvider.SetTenant(const ATenant: ITenant);
begin
  FTenant := ATenant;
end;

end.
