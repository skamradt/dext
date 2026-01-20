{***************************************************************************}
{                                                                           }
{           Dext Framework - Example                                        }
{                                                                           }
{           Customer Edit Frame - Form for editing customer data            }
{                                                                           }
{***************************************************************************}
unit Customer.Edit;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Customer.ViewModel;

type
  TOnSaveEvent = reference to procedure(ViewModel: TCustomerViewModel);
  TOnCancelEvent = reference to procedure;
  
  TCustomerEditFrame = class(TFrame)
    TitlePanel: TPanel;
    TitleLabel: TLabel;
    ContentPanel: TPanel;
    NameLabel: TLabel;
    NameEdit: TEdit;
    EmailLabel: TLabel;
    EmailEdit: TEdit;
    PhoneLabel: TLabel;
    PhoneEdit: TEdit;
    DocumentLabel: TLabel;
    DocumentEdit: TEdit;
    ActiveCheckBox: TCheckBox;
    NotesLabel: TLabel;
    NotesMemo: TMemo;
    ButtonPanel: TPanel;
    SaveButton: TButton;
    CancelButton: TButton;
    ErrorPanel: TPanel;
    ErrorsLabel: TLabel;
    procedure NameEditChange(Sender: TObject);
    procedure EmailEditChange(Sender: TObject);
    procedure PhoneEditChange(Sender: TObject);
    procedure DocumentEditChange(Sender: TObject);
    procedure ActiveCheckBoxClick(Sender: TObject);
    procedure NotesMemoChange(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FViewModel: TCustomerViewModel;
    FOnSave: TOnSaveEvent;
    FOnCancel: TOnCancelEvent;
    FUpdatingUI: Boolean;
    
    procedure UpdateUIFromViewModel;
    procedure UpdateErrorDisplay;
    procedure OnViewModelPropertyChanged(const PropertyName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure LoadCustomer(AViewModel: TCustomerViewModel);
    procedure NewCustomer;
    
    property OnSave: TOnSaveEvent read FOnSave write FOnSave;
    property OnCancel: TOnCancelEvent read FOnCancel write FOnCancel;
  end;

implementation

{$R *.dfm}

{ TCustomerEditFrame }

constructor TCustomerEditFrame.Create(AOwner: TComponent);
begin
  inherited;
  FViewModel := TCustomerViewModel.Create;
  FViewModel.OnPropertyChanged := OnViewModelPropertyChanged;
end;

destructor TCustomerEditFrame.Destroy;
begin
  FViewModel.Free;
  inherited;
end;

procedure TCustomerEditFrame.LoadCustomer(AViewModel: TCustomerViewModel);
begin
  // Copy data from external ViewModel
  FViewModel.Load(AViewModel.GetEntity);
  UpdateUIFromViewModel;
end;

procedure TCustomerEditFrame.NewCustomer;
begin
  FViewModel.Clear;
  UpdateUIFromViewModel;
  // Note: SetFocus should be called after frame is visible (by caller)
end;

procedure TCustomerEditFrame.UpdateUIFromViewModel;
begin
  FUpdatingUI := True;
  try
    if FViewModel.IsNew then
      TitleLabel.Caption := 'New Customer'
    else
      TitleLabel.Caption := 'Edit Customer #' + FViewModel.Id.ToString;
      
    NameEdit.Text := FViewModel.Name;
    EmailEdit.Text := FViewModel.Email;
    PhoneEdit.Text := FViewModel.Phone;
    DocumentEdit.Text := FViewModel.Document;
    ActiveCheckBox.Checked := FViewModel.Active;
    NotesMemo.Text := FViewModel.Notes;

    UpdateErrorDisplay;
  finally
    FUpdatingUI := False;
  end;
end;

procedure TCustomerEditFrame.UpdateErrorDisplay;
begin
  if FViewModel.Errors.Count > 0 then
  begin
    ErrorPanel.Visible := True;
    ErrorsLabel.Caption := FViewModel.Errors.Text;
  end
  else
    ErrorPanel.Visible := False;
    
  SaveButton.Enabled := FViewModel.IsDirty;
end;

procedure TCustomerEditFrame.OnViewModelPropertyChanged(const PropertyName: string);
begin
  if PropertyName = 'Errors' then
    UpdateErrorDisplay
  else if PropertyName = 'IsValid' then
    UpdateErrorDisplay
  else if PropertyName = 'IsDirty' then
    SaveButton.Enabled := FViewModel.IsDirty;
end;

// UI events -> ViewModel updates

procedure TCustomerEditFrame.NameEditChange(Sender: TObject);
begin
  if not FUpdatingUI then
    FViewModel.Name := NameEdit.Text;
end;

procedure TCustomerEditFrame.EmailEditChange(Sender: TObject);
begin
  if not FUpdatingUI then
    FViewModel.Email := EmailEdit.Text;
end;

procedure TCustomerEditFrame.PhoneEditChange(Sender: TObject);
begin
  if not FUpdatingUI then
    FViewModel.Phone := PhoneEdit.Text;
end;

procedure TCustomerEditFrame.DocumentEditChange(Sender: TObject);
begin
  if not FUpdatingUI then
    FViewModel.Document := DocumentEdit.Text;
end;

procedure TCustomerEditFrame.ActiveCheckBoxClick(Sender: TObject);
begin
  if not FUpdatingUI then
    FViewModel.Active := ActiveCheckBox.Checked;
end;

procedure TCustomerEditFrame.NotesMemoChange(Sender: TObject);
begin
  if not FUpdatingUI then
    FViewModel.Notes := NotesMemo.Text;
end;

// Button handlers

procedure TCustomerEditFrame.SaveButtonClick(Sender: TObject);
begin
  if FViewModel.Validate then
  begin
    if Assigned(FOnSave) then
      FOnSave(FViewModel);
  end;
end;

procedure TCustomerEditFrame.CancelButtonClick(Sender: TObject);
begin
  if Assigned(FOnCancel) then
    FOnCancel();
end;

end.
