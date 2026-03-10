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
  Dext.UI.Attributes,
  Dext.UI.Binder,
  Dext.UI.Message,
  Customer.ViewModel;

type
  // Messages for UI actions
  TSaveMsg = class(TMessage);
  TCancelMsg = class(TMessage);

  TOnSaveEvent = reference to procedure(ViewModel: TCustomerViewModel);
  TOnCancelEvent = reference to procedure;
  
  TCustomerEditFrame = class(TFrame)
    TitlePanel: TPanel;
    [BindText('Id', 'Customer #%s')]
    TitleLabel: TLabel;
    
    ContentPanel: TPanel;
    NameLabel: TLabel;
    [BindEdit('Name')]
    NameEdit: TEdit;
    
    EmailLabel: TLabel;
    [BindEdit('Email')]
    EmailEdit: TEdit;
    
    PhoneLabel: TLabel;
    [BindEdit('Phone')]
    PhoneEdit: TEdit;
    
    DocumentLabel: TLabel;
    [BindEdit('Document')]
    DocumentEdit: TEdit;
    
    [BindChecked('Active')]
    ActiveCheckBox: TCheckBox;
    
    NotesLabel: TLabel;
    [BindMemo('Notes')]
    NotesMemo: TMemo;
    
    ButtonPanel: TPanel;
    [BindEnabled('CanSave')]
    [OnClickMsg(TSaveMsg)]
    SaveButton: TButton;
    
    [OnClickMsg(TCancelMsg)]
    CancelButton: TButton;
    
    [BindVisible('Errors.Count', False)]
    ErrorPanel: TPanel;
    
    [BindText('Errors.Text')]
    ErrorsLabel: TLabel;
  private
    FViewModel: TCustomerViewModel;
    FBinder: TMVUBinder<TCustomerViewModel, TMessage>;
    FOnSave: TOnSaveEvent;
    FOnCancel: TOnCancelEvent;
    
    procedure DispatchMsg(Msg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure LoadCustomer(AViewModel: TCustomerViewModel; ATakeOwnership: Boolean = False);
    procedure NewCustomer;
    
    property OnSave: TOnSaveEvent read FOnSave write FOnSave;
    property OnCancel: TOnCancelEvent read FOnCancel write FOnCancel;
    
    // Exposed for Focus setting in MainForm
    property EdtName: TEdit read NameEdit;
  end;

implementation

{$R *.dfm}

{ TCustomerEditFrame }

constructor TCustomerEditFrame.Create(AOwner: TComponent);
begin
  inherited;
  FViewModel := TCustomerViewModel.Create;
  FBinder := TMVUBinder<TCustomerViewModel, TMessage>.Create(Self, DispatchMsg);
end;

destructor TCustomerEditFrame.Destroy;
begin
  FBinder.Free;
  FViewModel.Free;
  inherited;
end;

procedure TCustomerEditFrame.LoadCustomer(AViewModel: TCustomerViewModel; ATakeOwnership: Boolean = False);
begin
  FViewModel.Load(AViewModel.GetEntity, ATakeOwnership);
  FBinder.Render(FViewModel);
end;

procedure TCustomerEditFrame.NewCustomer;
begin
  FViewModel.Clear;
  FBinder.Render(FViewModel);
end;

procedure TCustomerEditFrame.DispatchMsg(Msg: TMessage);
begin
  if Msg is TSaveMsg then
  begin
    if FViewModel.Validate then
    begin
      if Assigned(FOnSave) then
        FOnSave(FViewModel);
    end
    else
      FBinder.Render(FViewModel); // Update error display
  end
  else if Msg is TCancelMsg then
  begin
    if Assigned(FOnCancel) then
      FOnCancel();
  end;
end;

end.
