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
  Dext,
  Dext.Collections,
  Dext.UI.Navigator.Types,
  Dext.UI.Navigator.Interfaces,
  Customer.Entity,
  Customer.Service,
  Customer.Controller,
  Customer.ViewModel,
  Customer.List,
  Customer.Edit;

const
  ROUTE_CUSTOMER_LIST = '/customers';
  ROUTE_CUSTOMER_EDIT = '/customers/edit';
  ROUTE_CUSTOMER_NEW  = '/customers/new';

type
  /// <summary>
  /// Simplified Navigator for pre-created frames (no RTTI creation)
  /// Manages navigation history and logs transitions
  /// </summary>
  TSimpleNavigator = class
  private
    FLogger: ILogger;
    FHistory: IList<string>;
    FListFrame: TCustomerListFrame;
    FEditFrame: TCustomerEditFrame;
    FOnNavigated: TProc<string>;
    
    procedure DoNavigate(const Route: string);
  public
    constructor Create(ALogger: ILogger; AListFrame: TCustomerListFrame; AEditFrame: TCustomerEditFrame);
    destructor Destroy; override;
    
    procedure Push(const Route: string);
    procedure Pop;
    function CanGoBack: Boolean;
    function CurrentRoute: string;
    function StackDepth: Integer;
    
    property OnNavigated: TProc<string> read FOnNavigated write FOnNavigated;
  end;

  TMainForm = class(TForm, ICustomerView)
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
    FController: ICustomerController;
    FNavigator: TSimpleNavigator;
    FListFrame: TCustomerListFrame;
    FEditFrame: TCustomerEditFrame;
    
    // ICustomerView Implementation
    procedure RefreshList(const Customers: IList<TCustomer>);
    procedure ShowEditView(ViewModel: TCustomerViewModel);
    procedure ShowListView;
    procedure ShowMessage(const Msg: string);
    
    // Frame Event Delegations (Bridges to Controller)
    procedure DoNewCustomer;
    procedure DoEditCustomer(Customer: TCustomer);
    procedure DoDeleteCustomer(Customer: TCustomer);
    procedure DoRefreshList;
    procedure DoSaveCustomer(ViewModel: TCustomerViewModel);
    procedure DoCancelEdit;
    
    procedure UpdateCaption(const Route: string);
  public
    procedure InjectDependencies(AController: ICustomerController);
  end;

var
  MainForm: TMainForm;

implementation

uses
  App.Startup;

{$R *.dfm}

{ TSimpleNavigator }

constructor TSimpleNavigator.Create(ALogger: ILogger; AListFrame: TCustomerListFrame; 
  AEditFrame: TCustomerEditFrame);
begin
  inherited Create;
  FLogger := ALogger;
  FListFrame := AListFrame;
  FEditFrame := AEditFrame;
  FHistory := TCollections.CreateList<string>;
end;

destructor TSimpleNavigator.Destroy;
begin
  // FHistory.Free;
  inherited;
end;

procedure TSimpleNavigator.DoNavigate(const Route: string);
begin
  // Hide all frames
  FListFrame.Visible := False;
  FEditFrame.Visible := False;
  
  // Show target frame
  if Route = ROUTE_CUSTOMER_LIST then
  begin
    FListFrame.Visible := True;
    FListFrame.BringToFront;
  end
  else if (Route = ROUTE_CUSTOMER_EDIT) or (Route = ROUTE_CUSTOMER_NEW) then
  begin
    FEditFrame.Visible := True;
    FEditFrame.BringToFront;
    FEditFrame.EdtName.SetFocus;
  end;
  
  // Fire event
  if Assigned(FOnNavigated) then
    FOnNavigated(Route);
end;

procedure TSimpleNavigator.Push(const Route: string);
var
  PrevRoute: string;
begin
  if FHistory.Count > 0 then
    PrevRoute := FHistory.Last
  else
    PrevRoute := '';
    
  FLogger.Debug('Navigator.Push: %s -> %s', [PrevRoute, Route]);
  FHistory.Add(Route);
  DoNavigate(Route);
  FLogger.Debug('Navigator Stack: %d entries', [FHistory.Count]);
end;

procedure TSimpleNavigator.Pop;
var
  PrevRoute: string;
begin
  if FHistory.Count <= 1 then
  begin
    FLogger.Warn('Navigator.Pop: Cannot pop root view');
    Exit;
  end;
  
  PrevRoute := FHistory.Last;
  FHistory.Delete(FHistory.Count - 1);
  FLogger.Debug('Navigator.Pop: %s -> %s', [PrevRoute, FHistory.Last]);
  DoNavigate(FHistory.Last);
  FLogger.Debug('Navigator Stack: %d entries', [FHistory.Count]);
end;

function TSimpleNavigator.CanGoBack: Boolean;
begin
  Result := FHistory.Count > 1;
end;

function TSimpleNavigator.CurrentRoute: string;
begin
  if FHistory.Count > 0 then
    Result := FHistory.Last
  else
    Result := '';
end;

function TSimpleNavigator.StackDepth: Integer;
begin
  Result := FHistory.Count;
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Create frames
  FListFrame := TCustomerListFrame.Create(ContentPanel);
  FListFrame.Parent := ContentPanel;
  FListFrame.Align := alClient;
  
  FEditFrame := TCustomerEditFrame.Create(ContentPanel);
  FEditFrame.Parent := ContentPanel;
  FEditFrame.Align := alClient;
  FEditFrame.Visible := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FController) then
  begin
    FController.View := nil;
    FController := nil;
  end;
  
  FNavigator.Free;
  FListFrame.Free;
  FEditFrame.Free;
end;

procedure TMainForm.UpdateCaption(const Route: string);
begin
  if Route = ROUTE_CUSTOMER_LIST then
    Caption := 'Desktop Modern - Customer List'
  else if Route = ROUTE_CUSTOMER_EDIT then
    Caption := 'Desktop Modern - Edit Customer'
  else if Route = ROUTE_CUSTOMER_NEW then
    Caption := 'Desktop Modern - New Customer'
  else
    Caption := 'Desktop Modern - ' + Route;
end;

procedure TMainForm.InjectDependencies(AController: ICustomerController);
var
  Logger: ILogger;
begin
  FController := AController;
  FController.View := Self;
  
  Logger := TAppStartup.GetLogger;

  // Create simple navigator for pre-created frames
  FNavigator := TSimpleNavigator.Create(Logger, FListFrame, FEditFrame);
  FNavigator.OnNavigated := 
    procedure(Route: string)
    begin
      UpdateCaption(Route);
    end;
  
  Logger.Info('Navigator created with 3 routes: %s, %s, %s', 
    [ROUTE_CUSTOMER_LIST, ROUTE_CUSTOMER_EDIT, ROUTE_CUSTOMER_NEW]);

  // Wire UI Events using stable method pointers
  FListFrame.OnNewCustomer := DoNewCustomer;
  FListFrame.OnCustomerSelected := DoEditCustomer;
  FListFrame.OnDeleteCustomer := DoDeleteCustomer;
  FListFrame.OnRefresh := DoRefreshList;
  
  FEditFrame.OnSave := DoSaveCustomer;
  FEditFrame.OnCancel := DoCancelEdit;

  // Initial load and navigate to list
  FController.LoadCustomers;
  FNavigator.Push(ROUTE_CUSTOMER_LIST);
end;

{ ICustomerView Implementation }

procedure TMainForm.ShowListView;
begin
  // Use Navigator to go back
  if FNavigator.CanGoBack then
    FNavigator.Pop
  else if FNavigator.CurrentRoute <> ROUTE_CUSTOMER_LIST then
    FNavigator.Push(ROUTE_CUSTOMER_LIST);
end;

procedure TMainForm.ShowEditView(ViewModel: TCustomerViewModel);
var
  IsNew: Boolean;
begin
  // Capture state before loading (Load may modify internal state)
  IsNew := ViewModel.Id = 0;
  
  // The frame's viewmodel should take ownership of the entity if it's a new one
  // to avoid dangling pointers and ensure correct cleanup.
  FEditFrame.LoadCustomer(ViewModel, IsNew);
  
  // Navigate using Navigator
  if IsNew then
    FNavigator.Push(ROUTE_CUSTOMER_NEW)
  else
  begin
    FNavigator.Push(ROUTE_CUSTOMER_EDIT);
  end;

  // Release ownership from temporary ViewModel so it doesn't free the entity
  // even if it was the original owner.
  ViewModel.ReleaseOwnership;
  ViewModel.Free;
end;

procedure TMainForm.RefreshList(const Customers: IList<TCustomer>);
begin
  FListFrame.LoadCustomers(Customers);
end;

procedure TMainForm.ShowMessage(const Msg: string);
begin
  Vcl.Dialogs.ShowMessage(Msg);
end;

{ Frame Event Delegations }

procedure TMainForm.DoNewCustomer;
begin
  FController.CreateNewCustomer;
end;

procedure TMainForm.DoEditCustomer(Customer: TCustomer);
begin
  FController.EditCustomer(Customer);
end;

procedure TMainForm.DoDeleteCustomer(Customer: TCustomer);
begin
  if MessageDlg(Format('Delete customer "%s"?', [Customer.Name.Value]),
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FController.DeleteCustomer(Customer);
  end;
end;

procedure TMainForm.DoRefreshList;
begin
  FController.LoadCustomers;
end;

procedure TMainForm.DoSaveCustomer(ViewModel: TCustomerViewModel);
begin
  FController.SaveCustomer(ViewModel);
end;

procedure TMainForm.DoCancelEdit;
begin
  FController.CancelEdit;
end;

{ UI Actions }

procedure TMainForm.BtnCustomersClick(Sender: TObject);
begin
  FController.LoadCustomers;
  // Navigate to list if not already there
  if FNavigator.CurrentRoute <> ROUTE_CUSTOMER_LIST then
  begin
    if FNavigator.CanGoBack then
      FNavigator.Pop
    else
      FNavigator.Push(ROUTE_CUSTOMER_LIST);
  end;
end;

procedure TMainForm.BtnAboutClick(Sender: TObject);
begin
  Vcl.Dialogs.ShowMessage(
    'Desktop Modern Customer CRUD' + sLineBreak + 
    'Dext Framework Example - With Navigator' + sLineBreak + sLineBreak +
    'Navigation Stack: ' + IntToStr(FNavigator.StackDepth) + sLineBreak +
    'Current Route: ' + FNavigator.CurrentRoute + sLineBreak + 
    'Can Go Back: ' + BoolToStr(FNavigator.CanGoBack, True) + sLineBreak + sLineBreak +
    'Demonstrates:' + sLineBreak +
    '- Simple Navigator (Push/Pop)' + sLineBreak +
    '- Route-based Navigation' + sLineBreak +
    '- Controller & Interface Pattern' + sLineBreak +
    '- Magic Binding (Attributes)'
  );
end;

end.
