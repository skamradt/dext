unit Customer.Controller;

interface

uses
  System.SysUtils,
  System.Classes,
  Customer.Entity,
  Customer.Service,
  Customer.ViewModel,
  Dext,
  Dext.Collections;

type
{$M+}
  /// <summary>
  /// View interface for Customer management.
  /// Used by the Controller to update the UI without direct coupling.
  /// </summary>
  ICustomerView = interface
    ['{B7E206D4-A6A4-4A2D-A2A6-D2C979E9B9A6}']
    procedure RefreshList(const Customers: IList<TCustomer>);
    procedure ShowEditView(ViewModel: TCustomerViewModel);
    procedure ShowListView;
    procedure ShowMessage(const Msg: string);
  end;

  /// <summary>
  /// Interface for the Customer Controller.
  /// </summary>
  ICustomerController = interface
    ['{D7A2C59B-6A3D-4A42-B3E1-E9D2C9B9A6F7}']
    procedure LoadCustomers;
    procedure CreateNewCustomer;
    procedure EditCustomer(Customer: TCustomer);
    procedure SaveCustomer(ViewModel: TCustomerViewModel);
    procedure DeleteCustomer(Customer: TCustomer);
    procedure CancelEdit;
    
    procedure SetView(const AView: ICustomerView);
    function GetView: ICustomerView;
    property View: ICustomerView read GetView write SetView;
  end;
{$M-}

  /// <summary>
  /// Controller for Customer management.
  /// Decouples the MainForm from services and orchestration logic.
  /// </summary>
  TCustomerController = class(TInterfacedObject, ICustomerController)
  private
    FCustomerService: ICustomerService;
    FLogger: ILogger;
    FView: ICustomerView;
    procedure SetView(const AView: ICustomerView);
    function GetView: ICustomerView;
  public
    constructor Create(ACustomerService: ICustomerService; ALogger: ILogger);

    procedure LoadCustomers;
    procedure CreateNewCustomer;
    procedure EditCustomer(Customer: TCustomer);
    procedure SaveCustomer(ViewModel: TCustomerViewModel);
    procedure DeleteCustomer(Customer: TCustomer);
    procedure CancelEdit;

    property View: ICustomerView read FView write FView;
  end;

implementation

{ TCustomerController }

constructor TCustomerController.Create(ACustomerService: ICustomerService; ALogger: ILogger);
begin
  inherited Create;
  FCustomerService := ACustomerService;
  FLogger := ALogger;
end;

procedure TCustomerController.LoadCustomers;
var
  Customers: IList<TCustomer>;
begin
  FLogger.Info('Controller: Loading customers');
  try
    Customers := FCustomerService.GetAll;
    if Assigned(FView) then
      FView.RefreshList(Customers);
  except
    on E: Exception do
    begin
      FLogger.Error('Controller: Failed to load customers: %s', [E.Message]);
      if Assigned(FView) then
        FView.ShowMessage('Error loading customers: ' + E.Message);
    end;
  end;
end;

procedure TCustomerController.CreateNewCustomer;
var
  ViewModel: TCustomerViewModel;
begin
  FLogger.Info('Controller: Navigating to new customer');
  ViewModel := TCustomerViewModel.Create;
  ViewModel.Clear;
  if Assigned(FView) then
    FView.ShowEditView(ViewModel);
end;

procedure TCustomerController.EditCustomer(Customer: TCustomer);
var
  ViewModel: TCustomerViewModel;
begin
  FLogger.Info('Controller: Navigating to edit customer %d', [Customer.Id.Value]);
  ViewModel := TCustomerViewModel.Create;
  ViewModel.Load(Customer, False);
  if Assigned(FView) then
    FView.ShowEditView(ViewModel);
end;

procedure TCustomerController.SaveCustomer(ViewModel: TCustomerViewModel);
var
  Customer: TCustomer;
begin
  if not ViewModel.Validate then
  begin
    FLogger.Warn('Controller: Validation failed for customer');
    Exit;
  end;

  FLogger.Info('Controller: Saving customer');
  try
    Customer := FCustomerService.Save(ViewModel.GetEntity);
    ViewModel.ReleaseOwnership;
    FLogger.Info('Controller: Customer saved with ID %d', [Customer.Id.Value]);
    
    if Assigned(FView) then
      FView.ShowListView;
      
    LoadCustomers;
  except
    on E: Exception do
    begin
      FLogger.Error('Controller: Failed to save customer: %s', [E.Message]);
      if Assigned(FView) then
        FView.ShowMessage('Error saving customer: ' + E.Message);
    end;
  end;
end;

procedure TCustomerController.DeleteCustomer(Customer: TCustomer);
begin
  FLogger.Info('Controller: Deleting customer %d', [Customer.Id.Value]);
  try
    FCustomerService.Delete(Customer.Id);
    LoadCustomers;
  except
    on E: Exception do
    begin
      FLogger.Error('Controller: Failed to delete customer: %s', [E.Message]);
      if Assigned(FView) then
        FView.ShowMessage('Error deleting customer: ' + E.Message);
    end;
  end;
end;

procedure TCustomerController.CancelEdit;
begin
  FLogger.Info('Controller: Edit cancelled');
  if Assigned(FView) then
    FView.ShowListView;
end;

procedure TCustomerController.SetView(const AView: ICustomerView);
begin
  FView := AView;
end;

function TCustomerController.GetView: ICustomerView;
begin
  Result := FView;
end;

end.
