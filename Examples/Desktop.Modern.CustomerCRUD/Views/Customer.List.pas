{***************************************************************************}
{                                                                           }
{           Dext Framework - Example                                        }
{                                                                           }
{           Customer List Frame - Grid view of customers                    }
{                                                                           }
{***************************************************************************}
unit Customer.List;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.StrUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Grids,
  Dext,
  Dext.Collections,
  Customer.Entity;

type
  TOnCustomerSelectedEvent = reference to procedure(Customer: TCustomer);
  TOnNewCustomerEvent = reference to procedure;
  TOnDeleteCustomerEvent = reference to procedure(Customer: TCustomer);

  TCustomerListFrame = class(TFrame)
    TitlePanel: TPanel;
    TitleLabel: TLabel;
    ToolbarPanel: TPanel;
    NewButton: TButton;
    EditButton: TButton;
    DeleteButton: TButton;
    RefreshButton: TButton;
    SearchEdit: TEdit;
    LblSearch: TLabel;
    GridPanel: TPanel;
    CustomerGrid: TStringGrid;
    StatusPanel: TPanel;
    StatusLabel: TLabel;
    procedure NewButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure CustomerGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
  private
    FCustomers: TObjectList<TCustomer>;
    FSelectedIndex: Integer;
    FOnCustomerSelected: TOnCustomerSelectedEvent;
    FOnNewCustomer: TOnNewCustomerEvent;
    FOnDeleteCustomer: TOnDeleteCustomerEvent;
    FOnRefresh: TProc;
    
    procedure SetupGrid;
    procedure UpdateButtonState;
    function GetSelectedCustomer: TCustomer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure LoadCustomers(const Customers: IList<TCustomer>);
    procedure UpdateStatus(const Text: string);
    
    property SelectedCustomer: TCustomer read GetSelectedCustomer;
    property OnCustomerSelected: TOnCustomerSelectedEvent read FOnCustomerSelected write FOnCustomerSelected;
    property OnNewCustomer: TOnNewCustomerEvent read FOnNewCustomer write FOnNewCustomer;
    property OnDeleteCustomer: TOnDeleteCustomerEvent read FOnDeleteCustomer write FOnDeleteCustomer;
    property OnRefresh: TProc read FOnRefresh write FOnRefresh;
  end;

implementation

{$R *.dfm}

{ TCustomerListFrame }

constructor TCustomerListFrame.Create(AOwner: TComponent);
begin
  inherited;
  FCustomers := TObjectList<TCustomer>.Create(False); // Don't own objects
  FSelectedIndex := -1;
  SetupGrid;
end;

destructor TCustomerListFrame.Destroy;
begin
  FCustomers.Free;
  inherited;
end;

procedure TCustomerListFrame.SetupGrid;
begin
  CustomerGrid.ColCount := 5;
  CustomerGrid.RowCount := 2;
  CustomerGrid.FixedRows := 1;
  CustomerGrid.FixedCols := 0;
  CustomerGrid.Options := CustomerGrid.Options + [goRowSelect];
  
  // Headers
  CustomerGrid.Cells[0, 0] := 'ID';
  CustomerGrid.Cells[1, 0] := 'Name';
  CustomerGrid.Cells[2, 0] := 'Email';
  CustomerGrid.Cells[3, 0] := 'Phone';
  CustomerGrid.Cells[4, 0] := 'Active';
  
  // Column widths
  CustomerGrid.ColWidths[0] := 50;
  CustomerGrid.ColWidths[1] := 200;
  CustomerGrid.ColWidths[2] := 200;
  CustomerGrid.ColWidths[3] := 120;
  CustomerGrid.ColWidths[4] := 60;
end;

procedure TCustomerListFrame.LoadCustomers(const Customers: IList<TCustomer>);
var
  I: Integer;
  Customer: TCustomer;
begin
  FCustomers.Clear;
  
  if (Customers = nil) or (Customers.Count = 0) then
  begin
    CustomerGrid.RowCount := 2;
    CustomerGrid.Cells[0, 1] := '';
    CustomerGrid.Cells[1, 1] := '(No customers)';
    CustomerGrid.Cells[2, 1] := '';
    CustomerGrid.Cells[3, 1] := '';
    CustomerGrid.Cells[4, 1] := '';
    FSelectedIndex := -1;
  end
  else
  begin
    CustomerGrid.RowCount := Customers.Count + 1;
    
    I := 1;
    for Customer in Customers do
    begin
      FCustomers.Add(Customer);
      
      CustomerGrid.Cells[0, I] := IntToStr(Customer.Id);
      CustomerGrid.Cells[1, I] := Customer.Name;
      CustomerGrid.Cells[2, I] := Customer.Email;
      CustomerGrid.Cells[3, I] := Customer.Phone;
      CustomerGrid.Cells[4, I] := IfThen(Customer.Active, 'Yes', 'No');
      Inc(I);
    end;
    
    FSelectedIndex := 0;
    CustomerGrid.Row := 1;
  end;
  
  if Customers <> nil then
    UpdateStatus(Format('%d customer(s)', [Customers.Count]))
  else
    UpdateStatus('0 customer(s)');
    
  UpdateButtonState;
end;

procedure TCustomerListFrame.UpdateStatus(const Text: string);
begin
  StatusLabel.Caption := Text;
end;

function TCustomerListFrame.GetSelectedCustomer: TCustomer;
begin
  if (FSelectedIndex >= 0) and (FSelectedIndex < FCustomers.Count) then
    Result := FCustomers[FSelectedIndex]
  else
    Result := nil;
end;

procedure TCustomerListFrame.UpdateButtonState;
begin
  EditButton.Enabled := Assigned(SelectedCustomer);
  DeleteButton.Enabled := Assigned(SelectedCustomer);
end;

procedure TCustomerListFrame.CustomerGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect := True;
  if ARow > 0 then
  begin
    FSelectedIndex := ARow - 1;
    UpdateButtonState;
  end;
end;

procedure TCustomerListFrame.NewButtonClick(Sender: TObject);
begin
  if Assigned(FOnNewCustomer) then
    FOnNewCustomer();
end;

procedure TCustomerListFrame.EditButtonClick(Sender: TObject);
begin
  if Assigned(SelectedCustomer) and Assigned(FOnCustomerSelected) then
    FOnCustomerSelected(SelectedCustomer);
end;

procedure TCustomerListFrame.DeleteButtonClick(Sender: TObject);
begin
  if Assigned(SelectedCustomer) and Assigned(FOnDeleteCustomer) then
    FOnDeleteCustomer(SelectedCustomer);
end;

procedure TCustomerListFrame.RefreshButtonClick(Sender: TObject);
begin
  if Assigned(FOnRefresh) then
    FOnRefresh();
end;

procedure TCustomerListFrame.SearchEditChange(Sender: TObject);
var
  SearchTerm: string;
  Row: Integer;
  Customer: TCustomer;
  MatchFound: Boolean;
begin
  SearchTerm := LowerCase(Trim(SearchEdit.Text));

  if SearchTerm = '' then
  begin
    // No filter - show all
    CustomerGrid.RowCount := FCustomers.Count + 1;
    Row := 1;
    for Customer in FCustomers do
    begin
      CustomerGrid.Cells[0, Row] := IntToStr(Customer.Id);
      CustomerGrid.Cells[1, Row] := Customer.Name;
      CustomerGrid.Cells[2, Row] := Customer.Email;
      CustomerGrid.Cells[3, Row] := Customer.Phone;
      CustomerGrid.Cells[4, Row] := IfThen(Customer.Active, 'Yes', 'No');
      Inc(Row);
    end;
    UpdateStatus(Format('%d customer(s)', [FCustomers.Count]));
  end
  else
  begin
    // Filter customers matching search term
    Row := 1;
    for Customer in FCustomers do
    begin
      MatchFound := 
        ContainsText(Customer.Name, SearchTerm) or
        ContainsText(Customer.Email, SearchTerm) or
        ContainsText(Customer.Phone, SearchTerm) or
        ContainsText(Customer.Document, SearchTerm);
        
      if MatchFound then
      begin
        if Row >= CustomerGrid.RowCount then
          CustomerGrid.RowCount := Row + 1;
          
        CustomerGrid.Cells[0, Row] := IntToStr(Customer.Id);
        CustomerGrid.Cells[1, Row] := Customer.Name;
        CustomerGrid.Cells[2, Row] := Customer.Email;
        CustomerGrid.Cells[3, Row] := Customer.Phone;
        CustomerGrid.Cells[4, Row] := IfThen(Customer.Active, 'Yes', 'No');
        Inc(Row);
      end;
    end;
    
    // Trim excess rows
    CustomerGrid.RowCount := Row;
    UpdateStatus(Format('%d of %d customer(s)', [Row - 1, FCustomers.Count]));
  end;
  
  FSelectedIndex := -1;
  UpdateButtonState;
end;

end.
