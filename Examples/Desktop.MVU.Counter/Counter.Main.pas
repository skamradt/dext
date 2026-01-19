/// <summary>
/// Counter.Main - Main Form for MVU Counter Example
///
/// This form demonstrates how to wire up the MVU pattern in a Delphi VCL application.
/// The form itself is minimal - it just hosts the View and coordinates the MVU loop.
///
/// Key Concepts:
/// 1. The Form creates the initial Model
/// 2. The Form creates the View with a Dispatch callback
/// 3. When messages are dispatched, the Form calls Update and re-renders
/// 4. The Form orchestrates but contains NO business logic
///
/// Author: Dext Framework Team
/// </summary>
unit Counter.Main;

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
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Counter.MVU;

type
  TMainForm = class(TForm)
    MainPanel: TPanel;
    FooterPanel: TPanel;
    InfoLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FModel: TCounterModel;
    FView: TCounterView;
    
    /// <summary>
    /// DispatchMessage callback - called by View when user interacts.
    /// This is the entry point for all state changes.
    /// </summary>
    procedure DispatchMessage(const Msg: TCounterMessage);
    
    /// <summary>
    /// The core MVU loop:
    /// 1. Receive message
    /// 2. Call Update to get new Model
    /// 3. Re-render View with new Model
    /// </summary>
    procedure ProcessMessage(const Msg: TCounterMessage);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // 1. Initialize Model with default state
  FModel := TCounterModel.Init;
  
  // 2. Create View, providing it with:
  //    - A container to render into (MainPanel)
  //    - A dispatch callback for sending messages
  FView := TCounterView.Create(MainPanel, DispatchMessage);
  
  // 3. Initial render
  FView.Render(FModel);
  
  // Info label
  InfoLabel.Caption := 'MVU Pattern Demo - Model-View-Update Architecture';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FView.Free;
end;

procedure TMainForm.DispatchMessage(const Msg: TCounterMessage);
begin
  // Simply forward to ProcessMessage
  // In a real app, this could queue messages, log, etc.
  ProcessMessage(Msg);
end;

procedure TMainForm.ProcessMessage(const Msg: TCounterMessage);
begin
  // THE MVU LOOP:
  // 1. Call Update with current Model and Message
  // 2. Update returns NEW Model (immutable!)
  // 3. Store new Model
  // 4. Re-render View
  
  FModel := TCounterUpdate.Update(FModel, Msg);
  
  // Re-render - the View will update to reflect new state
  FView.Render(FModel);
  
  // Update window title to show state (optional debug)
  Caption := Format('MVU Counter - Count: %d', [FModel.Count]);
end;

end.
