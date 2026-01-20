{***************************************************************************}
{                                                                           }
{           Dext Framework - Example                                        }
{                                                                           }
{           Customer ViewModel - Bindable presentation model                }
{                                                                           }
{***************************************************************************}
unit Customer.ViewModel;

interface

uses
  System.SysUtils,
  System.Classes,
  Dext,
  Dext.Collections,
  Dext.Validation,
  Customer.Entity;

type
  /// <summary>
  /// Event type for property change notifications (compatible with methods)
  /// </summary>
  TPropertyChangedEvent = procedure(const PropertyName: string) of object;
  
  /// <summary>
  /// ViewModel for Customer editing with property change notification
  /// </summary>
  TCustomerViewModel = class
  private
    FCustomer: TCustomer;
    FOwnsCustomer: Boolean;
    FIsDirty: Boolean;
    FErrors: TStrings;
    FOnPropertyChanged: TPropertyChangedEvent;
    
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
    function GetIsValid: Boolean;
    
    procedure NotifyChange(const PropertyName: string);
    procedure MarkDirty;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>Load a customer into the ViewModel</summary>
    procedure Load(ACustomer: TCustomer; AOwnsCustomer: Boolean = False);

    /// <summary>Clear the ViewModel to create a new customer</summary>
    procedure Clear;
    
    /// <summary>Validate the current state</summary>
    function Validate: Boolean;
    
    /// <summary>Get the underlying customer entity</summary>
    function GetEntity: TCustomer;
    
    /// <summary>Release ownership of the entity (after it's been persisted to DbContext)</summary>
    procedure ReleaseOwnership;
    
    // Bindable properties
    property Id: Integer read GetId;
    property Name: string read GetName write SetName;
    property Email: string read GetEmail write SetEmail;
    property Phone: string read GetPhone write SetPhone;
    property Document: string read GetDocument write SetDocument;
    property Active: Boolean read GetActive write SetActive;
    property Notes: string read GetNotes write SetNotes;
    
    // State properties
    property IsNew: Boolean read GetIsNew;
    property IsValid: Boolean read GetIsValid;
    property IsDirty: Boolean read FIsDirty;
    property Errors: TStrings read FErrors;
    
    // Event for UI binding (compatible with methods)
    property OnPropertyChanged: TPropertyChangedEvent read FOnPropertyChanged write FOnPropertyChanged;
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
  FIsDirty := False;
  FErrors.Clear;
  NotifyChange('');
end;

procedure TCustomerViewModel.Clear;
begin
  if FOwnsCustomer and Assigned(FCustomer) then
    FCustomer.Free;

  FCustomer := TCustomer.Create;
  FOwnsCustomer := True;
  FIsDirty := False;
  FErrors.Clear;
  
  NotifyChange('');
end;

function TCustomerViewModel.Validate: Boolean;
var
  ValRes: TValidationResult;
  Error: TValidationError;
begin
  FErrors.Clear;
  
  // Use framework validation based on entity attributes [Required], [EmailAddress], etc.
  ValRes := TValidator.Validate(FCustomer);
  try
    for Error in ValRes.Errors do
      FErrors.Add(Error.ErrorMessage);
  finally
    ValRes.Free;
  end;
    
  Result := FErrors.Count = 0;
  NotifyChange('Errors');
  NotifyChange('IsValid');
end;

function TCustomerViewModel.GetEntity: TCustomer;
begin
  Result := FCustomer;
end;

procedure TCustomerViewModel.ReleaseOwnership;
begin
  // After entity is persisted to DbContext, ownership transfers to IdentityMap
  FOwnsCustomer := False;
end;

// Property accessors with change notification

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
  if FCustomer.Name <> Value then
  begin
    FCustomer.Name := Value;
    MarkDirty;
    NotifyChange('Name');
  end;
end;

function TCustomerViewModel.GetEmail: string;
begin
  Result := FCustomer.Email;
end;

procedure TCustomerViewModel.SetEmail(const Value: string);
begin
  if FCustomer.Email <> Value then
  begin
    FCustomer.Email := Value;
    MarkDirty;
    NotifyChange('Email');
  end;
end;

function TCustomerViewModel.GetPhone: string;
begin
  Result := FCustomer.Phone;
end;

procedure TCustomerViewModel.SetPhone(const Value: string);
begin
  if FCustomer.Phone <> Value then
  begin
    FCustomer.Phone := Value;
    MarkDirty;
    NotifyChange('Phone');
  end;
end;

function TCustomerViewModel.GetDocument: string;
begin
  Result := FCustomer.Document;
end;

procedure TCustomerViewModel.SetDocument(const Value: string);
begin
  if FCustomer.Document <> Value then
  begin
    FCustomer.Document := Value;
    MarkDirty;
    NotifyChange('Document');
  end;
end;

function TCustomerViewModel.GetActive: Boolean;
begin
  Result := FCustomer.Active;
end;

procedure TCustomerViewModel.SetActive(const Value: Boolean);
begin
  if FCustomer.Active <> Value then
  begin
    FCustomer.Active := Value;
    MarkDirty;
    NotifyChange('Active');
  end;
end;

function TCustomerViewModel.GetNotes: string;
begin
  Result := FCustomer.Notes;
end;

procedure TCustomerViewModel.SetNotes(const Value: string);
begin
  if FCustomer.Notes <> Value then
  begin
    FCustomer.Notes := Value;
    MarkDirty;
    NotifyChange('Notes');
  end;
end;

function TCustomerViewModel.GetIsNew: Boolean;
begin
  Result := FCustomer.Id = 0;
end;

function TCustomerViewModel.GetIsValid: Boolean;
begin
  Result := FErrors.Count = 0;
end;

procedure TCustomerViewModel.MarkDirty;
begin
  if not FIsDirty then
  begin
    FIsDirty := True;
    NotifyChange('IsDirty');
  end;
end;

procedure TCustomerViewModel.NotifyChange(const PropertyName: string);
begin
  if Assigned(FOnPropertyChanged) then
    FOnPropertyChanged(PropertyName);
end;

end.
