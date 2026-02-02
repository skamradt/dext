unit Customer.ViewModel;

interface

uses
  System.SysUtils,
  System.Classes,
  Customer.Entity,
  Customer.Rules,
  Dext,
  Dext.Collections;

type
  /// <summary>
  /// Simplified ViewModel for Customer editing.
  /// Properties are now simple read/write for automatic binding.
  /// </summary>
  TCustomerViewModel = class
  private
    FCustomer: TCustomer;
    FOwnsCustomer: Boolean;
    FErrors: TStrings;

    function GetId: Integer;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetEmail: string;
    procedure SetEmail(const Value: string);
    function GetPhone: string;
    procedure SetPhone(const Value: string);
    function GetDocument: string;
    procedure SetDocument(const Value: string);
    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    function GetNotes: string;
    procedure SetNotes(const Value: string);
    function GetIsNew: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(ACustomer: TCustomer; AOwnsCustomer: Boolean = False);
    procedure Clear;
    function Validate: Boolean;
    function GetEntity: TCustomer;
    procedure ReleaseOwnership;

    // Bindable properties (simple pass-through to Entity)
    property Id: Integer read GetId;
    property Name: string read GetName write SetName;
    property Email: string read GetEmail write SetEmail;
    property Phone: string read GetPhone write SetPhone;
    property Document: string read GetDocument write SetDocument;
    property Active: Boolean read GetActive write SetActive;
    property Notes: string read GetNotes write SetNotes;

    // State properties
    property IsNew: Boolean read GetIsNew;
    property Errors: TStrings read FErrors;
  end;

implementation

{ TCustomerViewModel }

constructor TCustomerViewModel.Create;
begin
  inherited;
  FErrors := TStringList.Create;
  Clear;
end;

destructor TCustomerViewModel.Destroy;
begin
  if FOwnsCustomer and Assigned(FCustomer) then
    FCustomer.Free;
  FErrors.Free;
  inherited;
end;

procedure TCustomerViewModel.Load(ACustomer: TCustomer; AOwnsCustomer: Boolean);
begin
  if FOwnsCustomer and Assigned(FCustomer) then
    FCustomer.Free;
  FCustomer := ACustomer;
  FOwnsCustomer := AOwnsCustomer;
  FErrors.Clear;
end;

procedure TCustomerViewModel.Clear;
begin
  if FOwnsCustomer and Assigned(FCustomer) then
    FCustomer.Free;

  FCustomer := TCustomer.Create;
  FOwnsCustomer := True;
  FErrors.Clear;
end;

function TCustomerViewModel.Validate: Boolean;
var
  ErrorArray: TArray<string>;
  S: string;
begin
  FErrors.Clear;
  Result := TCustomerRules.ValidateAll(FCustomer, ErrorArray);
  for S in ErrorArray do
    FErrors.Add(S);
end;

function TCustomerViewModel.GetEntity: TCustomer;
begin
  Result := FCustomer;
end;

procedure TCustomerViewModel.ReleaseOwnership;
begin
  FOwnsCustomer := False;
end;

function TCustomerViewModel.GetId: Integer;
begin
  Result := FCustomer.Id;
end;

function TCustomerViewModel.GetName: string;
begin
  Result := FCustomer.Name;
end;

procedure TCustomerViewModel.SetName(const Value: string);
begin
  FCustomer.Name := Value;
end;

function TCustomerViewModel.GetEmail: string;
begin
  Result := FCustomer.Email;
end;

procedure TCustomerViewModel.SetEmail(const Value: string);
begin
  FCustomer.Email := Value;
end;

function TCustomerViewModel.GetPhone: string;
begin
  Result := FCustomer.Phone;
end;

procedure TCustomerViewModel.SetPhone(const Value: string);
begin
  FCustomer.Phone := Value;
end;

function TCustomerViewModel.GetDocument: string;
begin
  Result := FCustomer.Document;
end;

procedure TCustomerViewModel.SetDocument(const Value: string);
begin
  FCustomer.Document := Value;
end;

function TCustomerViewModel.GetActive: Boolean;
begin
  Result := FCustomer.Active;
end;

procedure TCustomerViewModel.SetActive(const Value: Boolean);
begin
  FCustomer.Active := Value;
end;

function TCustomerViewModel.GetNotes: string;
begin
  Result := FCustomer.Notes;
end;

procedure TCustomerViewModel.SetNotes(const Value: string);
begin
  FCustomer.Notes := Value;
end;

function TCustomerViewModel.GetIsNew: Boolean;
begin
  Result := FCustomer.Id = 0;
end;

end.

