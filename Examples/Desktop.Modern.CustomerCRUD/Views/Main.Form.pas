{***************************************************************************}
{                                                                           }
{           Dext Framework - Example                                        }
{                                                                           }
{           Main Form - Application shell with DI integration               }
{                                                                           }
{***************************************************************************}
unit Main.Form;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Dext,            // Use facade for ILogger
  Dext.Collections,
  Customer.Entity,
  Customer.Service,
  Customer.ViewModel,
  Customer.List,
  Customer.Edit;

type
  TMainForm = class(TForm)
    MainPanel: TPanel;
    SidePanel: TPanel;
    ContentPanel: TPanel;
    LogoLabel: TLabel;
    BtnCustomers: TButton;
    BtnAbout: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnCustomersClick(Sender: TObject);
    procedure BtnAboutClick(Sender: TObject);
  private
    FCustomerService: ICustomerService;
    FLogger: ILogger;
    
    FListFrame: TCustomerListFrame;
    FEditFrame: TCustomerEditFrame;
    
    procedure ShowListView;
    procedure ShowEditView(Customer: TCustomer);
    procedure ShowNewCustomer;
    
    procedure LoadCustomers;
    procedure OnSaveCustomer(ViewModel: TCustomerViewModel);
    procedure OnCancelEdit;
    procedure OnDeleteCustomer(Customer: TCustomer);
  public
    /// <summary>Inject dependencies after construction</summary>
    procedure InjectDependencies(ACustomerService: ICustomerService; ALogger: ILogger);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Create frames
  FListFrame := TCustomerListFrame.Create(ContentPanel);
  FListFrame.Parent := ContentPanel;
  FListFrame.Align := alClient;
  FListFrame.OnNewCustomer := ShowNewCustomer;
  FListFrame.OnCustomerSelected := ShowEditView;
  FListFrame.OnDeleteCustomer := OnDeleteCustomer;
  FListFrame.OnRefresh := LoadCustomers;
  
  FEditFrame := TCustomerEditFrame.Create(ContentPanel);
  FEditFrame.Parent := ContentPanel;
  FEditFrame.Align := alClient;
  FEditFrame.Visible := False;
  FEditFrame.OnSave := OnSaveCustomer;
  FEditFrame.OnCancel := OnCancelEdit;
  
  // Default view
  ShowListView;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Release interface references before DI container is destroyed
  FCustomerService := nil;
  FLogger := nil;
  
  FEditFrame.Free;
  FListFrame.Free;
end;

procedure TMainForm.InjectDependencies(ACustomerService: ICustomerService; ALogger: ILogger);
begin
  FCustomerService := ACustomerService;
  FLogger := ALogger;
  
  FLogger.Info('MainForm: Dependencies injected');
  
  // Load initial data
  LoadCustomers;
end;

procedure TMainForm.ShowListView;
begin
  FEditFrame.Visible := False;
  FListFrame.Visible := True;
  Caption := 'Desktop Modern - Customer Management';
end;

procedure TMainForm.ShowEditView(Customer: TCustomer);
var
  ViewModel: TCustomerViewModel;
begin
  FLogger.Info('MainForm: Editing customer %d', [Customer.Id.Value]);
  
  ViewModel := TCustomerViewModel.Create;
  try
    ViewModel.Load(Customer, False);
    FEditFrame.LoadCustomer(ViewModel);
  finally
    ViewModel.Free;
  end;
  
  FListFrame.Visible := False;
  FEditFrame.Visible := True;
  Caption := 'Desktop Modern - Edit Customer';
end;

procedure TMainForm.ShowNewCustomer;
begin
  FLogger.Info('MainForm: Creating new customer');
  
  FEditFrame.NewCustomer;
  FListFrame.Visible := False;
  FEditFrame.Visible := True;
  FEditFrame.NameEdit.SetFocus;  // Focus after frame is visible
  Caption := 'Desktop Modern - New Customer';
end;

procedure TMainForm.LoadCustomers;
var
  Customers: IList<TCustomer>;
begin
  FLogger.Info('MainForm: Loading customers');
  FListFrame.UpdateStatus('Loading...');
  
  try
    Customers := FCustomerService.GetAll;
    FListFrame.LoadCustomers(Customers);
  except
    on E: Exception do
    begin
      FLogger.Error('Failed to load customers: %s', [E.Message]);
      FListFrame.UpdateStatus('Error loading customers');
      ShowMessage('Error loading customers: ' + E.Message);
    end;
  end;
end;

procedure TMainForm.OnSaveCustomer(ViewModel: TCustomerViewModel);
var
  Customer: TCustomer;
begin
  FLogger.Info('MainForm: Saving customer');
  
  try
    Customer := FCustomerService.Save(ViewModel.GetEntity);
    // After save, DbContext/IdentityMap owns the entity
    ViewModel.ReleaseOwnership;
    FLogger.Info('MainForm: Customer saved with ID %d', [Customer.Id.Value]);
    ShowListView;
    LoadCustomers;
  except
    on E: Exception do
    begin
      FLogger.Error('Failed to save customer: %s', [E.Message]);
      ShowMessage('Error saving customer: ' + E.Message);
    end;
  end;
end;

procedure TMainForm.OnCancelEdit;
begin
  FLogger.Info('MainForm: Edit cancelled');
  ShowListView;
end;

procedure TMainForm.OnDeleteCustomer(Customer: TCustomer);
begin
  if MessageDlg(Format('Delete customer "%s"?', [Customer.Name.Value]),
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FLogger.Info('MainForm: Deleting customer %d', [Customer.Id.Value]);
    try
      FCustomerService.Delete(Customer.Id);
      LoadCustomers;
    except
      on E: Exception do
      begin
        FLogger.Error('Failed to delete customer: %s', [E.Message]);
        ShowMessage('Error deleting customer: ' + E.Message);
      end;
    end;
  end;
end;

procedure TMainForm.BtnCustomersClick(Sender: TObject);
begin
  ShowListView;
end;

procedure TMainForm.BtnAboutClick(Sender: TObject);
begin
  ShowMessage('Desktop Modern Customer CRUD' + sLineBreak + 
              'Dext Framework Example' + sLineBreak + sLineBreak +
              'Demonstrates:' + sLineBreak +
              '- Dependency Injection' + sLineBreak +
              '- ViewModel with Data Binding' + sLineBreak +
              '- ORM Entity Mapping' + sLineBreak +
              '- Logging');
end;

end.
