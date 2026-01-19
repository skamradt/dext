/// <summary>
/// Counter.MVU - Pure MVU implementation for Delphi
///
/// This unit demonstrates the Model-View-Update architecture pattern
/// without any framework dependencies. It shows how to achieve:
/// - Immutable state management
/// - Unidirectional data flow
/// - Pure update functions
/// - Testable business logic
///
/// Author: Dext Framework Team
/// </summary>
unit Counter.MVU;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Graphics,
  Vcl.StdCtrls;

type
  {$REGION 'Model'}
  /// <summary>
  /// The Model represents the entire state of your feature.
  /// It should be immutable - we never modify it, we create a new one.
  /// Using a record ensures value semantics (copy on assignment).
  /// </summary>
  TCounterModel = record
    Count: Integer;
    Step: Integer;
    History: string;
    
    /// <summary>Creates a new model with default values</summary>
    class function Init: TCounterModel; static;
    
    /// <summary>Creates a copy with Count updated</summary>
    function WithCount(const NewCount: Integer): TCounterModel;
    
    /// <summary>Creates a copy with Step updated</summary>
    function WithStep(const NewStep: Integer): TCounterModel;
    
    /// <summary>Creates a copy with History appended</summary>
    function WithHistory(const Action: string): TCounterModel;
  end;
  {$ENDREGION}
  
  {$REGION 'Messages'}
  /// <summary>
  /// Messages represent everything that can happen in your application.
  /// They are the ONLY way to change state.
  /// Using an enum is the simplest approach for small examples.
  /// </summary>
  TCounterMessage = (
    IncrementMsg,
    DecrementMsg,
    IncrementByStepMsg,
    DecrementByStepMsg,
    ResetMsg,
    SetStep1Msg,
    SetStep5Msg,
    SetStep10Msg
  );
  {$ENDREGION}
  
  {$REGION 'Update'}
  /// <summary>
  /// The Update function is PURE - it only depends on its inputs
  /// and has no side effects. This makes it trivially testable.
  /// Given a Model and a Message, it returns a new Model.
  /// </summary>
  TCounterUpdate = class
  public
    /// <summary>
    /// Core update function - the heart of MVU.
    /// Takes current state and a message, returns new state.
    /// </summary>
    class function Update(const Model: TCounterModel; 
                          const Msg: TCounterMessage): TCounterModel; static;
  end;
  {$ENDREGION}
  
  {$REGION 'View'}
  /// <summary>
  /// Event type for dispatching messages from the View
  /// </summary>
  TDispatchProc = reference to procedure(const Msg: TCounterMessage);
  
  /// <summary>
  /// The View is responsible for rendering UI based on the Model.
  /// It should be a pure function of Model -> UI.
  /// All user interactions dispatch Messages through the Dispatch callback.
  /// </summary>
  TCounterView = class
  private
    FContainer: TWinControl;
    FDispatch: TDispatchProc;
    
    // Controls (created once, updated on render)
    FCountLabel: TLabel;
    FStepLabel: TLabel;
    FHistoryLabel: TLabel;
    FIncrementButton: TButton;
    FDecrementButton: TButton;
    FIncrementStepButton: TButton;
    FDecrementStepButton: TButton;
    FResetButton: TButton;
    FStepsPanel: TPanel;
    FStep1Button: TButton;
    FStep5Button: TButton;
    FStep10Button: TButton;
    FHistoryMemo: TMemo;
    
    procedure CreateControls;
    procedure WireEvents;
    
    // Event handlers (TNotifyEvent compatible)
    procedure OnIncrementClick(Sender: TObject);
    procedure OnDecrementClick(Sender: TObject);
    procedure OnIncrementStepClick(Sender: TObject);
    procedure OnDecrementStepClick(Sender: TObject);
    procedure OnResetClick(Sender: TObject);
    procedure OnStep1Click(Sender: TObject);
    procedure OnStep5Click(Sender: TObject);
    procedure OnStep10Click(Sender: TObject);
  public
    constructor Create(Container: TWinControl; Dispatch: TDispatchProc);
    
    /// <summary>
    /// Renders the UI based on the current Model.
    /// This is called every time the Model changes.
    /// </summary>
    procedure Render(const Model: TCounterModel);
  end;
  {$ENDREGION}
  
  {$REGION 'Runtime'}
  /// <summary>
  /// The MVU Runtime orchestrates the Model-View-Update loop.
  /// It holds the current state and coordinates updates.
  /// </summary>
  TMVURuntime<TModel> = class
  private
    FModel: TModel;
    FOnModelChanged: TProc<TModel>;
  public
    constructor Create(const InitialModel: TModel; OnChanged: TProc<TModel>);
    
    procedure UpdateModel(const NewModel: TModel);
    
    property Model: TModel read FModel;
  end;
  {$ENDREGION}

implementation

{$REGION 'TCounterModel Implementation'}

class function TCounterModel.Init: TCounterModel;
begin
  Result.Count := 0;
  Result.Step := 1;
  Result.History := '';
end;

function TCounterModel.WithCount(const NewCount: Integer): TCounterModel;
begin
  Result := Self;  // Copy current state
  Result.Count := NewCount;
end;

function TCounterModel.WithStep(const NewStep: Integer): TCounterModel;
begin
  Result := Self;
  Result.Step := NewStep;
end;

function TCounterModel.WithHistory(const Action: string): TCounterModel;
begin
  Result := Self;
  if Result.History <> '' then
    Result.History := Action + sLineBreak + Result.History
  else
    Result.History := Action;
end;

{$ENDREGION}

{$REGION 'TCounterUpdate Implementation'}

class function TCounterUpdate.Update(const Model: TCounterModel;
  const Msg: TCounterMessage): TCounterModel;
begin
  // Start with current model
  Result := Model;
  
  // Apply the message to produce new state
  case Msg of
    IncrementMsg:
      Result := Model
        .WithCount(Model.Count + 1)
        .WithHistory(Format('[%s] +1 → %d', [FormatDateTime('hh:nn:ss', Now), Model.Count + 1]));
        
    DecrementMsg:
      Result := Model
        .WithCount(Model.Count - 1)
        .WithHistory(Format('[%s] -1 → %d', [FormatDateTime('hh:nn:ss', Now), Model.Count - 1]));
        
    IncrementByStepMsg:
      Result := Model
        .WithCount(Model.Count + Model.Step)
        .WithHistory(Format('[%s] +%d → %d', [FormatDateTime('hh:nn:ss', Now), Model.Step, Model.Count + Model.Step]));
        
    DecrementByStepMsg:
      Result := Model
        .WithCount(Model.Count - Model.Step)
        .WithHistory(Format('[%s] -%d → %d', [FormatDateTime('hh:nn:ss', Now), Model.Step, Model.Count - Model.Step]));
        
    ResetMsg:
      Result := TCounterModel.Init
        .WithHistory(Format('[%s] RESET → 0', [FormatDateTime('hh:nn:ss', Now)]));
        
    SetStep1Msg:
      Result := Model
        .WithStep(1)
        .WithHistory(Format('[%s] Step = 1', [FormatDateTime('hh:nn:ss', Now)]));
        
    SetStep5Msg:
      Result := Model
        .WithStep(5)
        .WithHistory(Format('[%s] Step = 5', [FormatDateTime('hh:nn:ss', Now)]));
        
    SetStep10Msg:
      Result := Model
        .WithStep(10)
        .WithHistory(Format('[%s] Step = 10', [FormatDateTime('hh:nn:ss', Now)]));
  end;
end;

{$ENDREGION}

{$REGION 'TCounterView Implementation'}

constructor TCounterView.Create(Container: TWinControl; Dispatch: TDispatchProc);
begin
  inherited Create;
  FContainer := Container;
  FDispatch := Dispatch;
  CreateControls;
  WireEvents;
end;

procedure TCounterView.CreateControls;
const
  BUTTON_WIDTH = 120;
  BUTTON_HEIGHT = 35;
  MARGIN = 10;
var
  Y: Integer;
begin
  Y := MARGIN;
  
  // Title/Count display
  FCountLabel := TLabel.Create(FContainer);
  FCountLabel.Parent := FContainer;
  FCountLabel.Left := MARGIN;
  FCountLabel.Top := Y;
  FCountLabel.Width := FContainer.ClientWidth - (MARGIN * 2);
  FCountLabel.Height := 60;
  FCountLabel.Alignment := taCenter;
  FCountLabel.Font.Size := 36;
  FCountLabel.Font.Style := [fsBold];
  FCountLabel.Font.Color := $00B85C00;  // Dark blue
  FCountLabel.Caption := '0';
  Inc(Y, FCountLabel.Height + MARGIN);
  
  // Step indicator
  FStepLabel := TLabel.Create(FContainer);
  FStepLabel.Parent := FContainer;
  FStepLabel.Left := MARGIN;
  FStepLabel.Top := Y;
  FStepLabel.Width := FContainer.ClientWidth - (MARGIN * 2);
  FStepLabel.Alignment := taCenter;
  FStepLabel.Font.Size := 12;
  FStepLabel.Font.Color := clGray;
  FStepLabel.Caption := 'Step: 1';
  Inc(Y, FStepLabel.Height + MARGIN * 2);
  
  // Simple +1/-1 buttons
  FDecrementButton := TButton.Create(FContainer);
  FDecrementButton.Parent := FContainer;
  FDecrementButton.Left := MARGIN;
  FDecrementButton.Top := Y;
  FDecrementButton.Width := BUTTON_WIDTH;
  FDecrementButton.Height := BUTTON_HEIGHT;
  FDecrementButton.Caption := '- 1';
  FDecrementButton.Font.Size := 14;
  
  FIncrementButton := TButton.Create(FContainer);
  FIncrementButton.Parent := FContainer;
  FIncrementButton.Left := FContainer.ClientWidth - BUTTON_WIDTH - MARGIN;
  FIncrementButton.Top := Y;
  FIncrementButton.Width := BUTTON_WIDTH;
  FIncrementButton.Height := BUTTON_HEIGHT;
  FIncrementButton.Caption := '+ 1';
  FIncrementButton.Font.Size := 14;
  Inc(Y, BUTTON_HEIGHT + MARGIN);
  
  // Step buttons
  FDecrementStepButton := TButton.Create(FContainer);
  FDecrementStepButton.Parent := FContainer;
  FDecrementStepButton.Left := MARGIN;
  FDecrementStepButton.Top := Y;
  FDecrementStepButton.Width := BUTTON_WIDTH;
  FDecrementStepButton.Height := BUTTON_HEIGHT;
  FDecrementStepButton.Caption := '- Step';
  FDecrementStepButton.Font.Size := 12;
  
  FIncrementStepButton := TButton.Create(FContainer);
  FIncrementStepButton.Parent := FContainer;
  FIncrementStepButton.Left := FContainer.ClientWidth - BUTTON_WIDTH - MARGIN;
  FIncrementStepButton.Top := Y;
  FIncrementStepButton.Width := BUTTON_WIDTH;
  FIncrementStepButton.Height := BUTTON_HEIGHT;
  FIncrementStepButton.Caption := '+ Step';
  FIncrementStepButton.Font.Size := 12;
  Inc(Y, BUTTON_HEIGHT + MARGIN * 2);
  
  // Step selection panel
  FStepsPanel := TPanel.Create(FContainer);
  FStepsPanel.Parent := FContainer;
  FStepsPanel.Left := MARGIN;
  FStepsPanel.Top := Y;
  FStepsPanel.Width := FContainer.ClientWidth - (MARGIN * 2);
  FStepsPanel.Height := 40;
  FStepsPanel.BevelOuter := bvNone;
  FStepsPanel.Caption := '';
  
  var BtnWidth := (FStepsPanel.ClientWidth - 20) div 3;
  
  FStep1Button := TButton.Create(FStepsPanel);
  FStep1Button.Parent := FStepsPanel;
  FStep1Button.Left := 0;
  FStep1Button.Top := 0;
  FStep1Button.Width := BtnWidth;
  FStep1Button.Height := 35;
  FStep1Button.Caption := 'Step = 1';
  
  FStep5Button := TButton.Create(FStepsPanel);
  FStep5Button.Parent := FStepsPanel;
  FStep5Button.Left := BtnWidth + 10;
  FStep5Button.Top := 0;
  FStep5Button.Width := BtnWidth;
  FStep5Button.Height := 35;
  FStep5Button.Caption := 'Step = 5';
  
  FStep10Button := TButton.Create(FStepsPanel);
  FStep10Button.Parent := FStepsPanel;
  FStep10Button.Left := (BtnWidth + 10) * 2;
  FStep10Button.Top := 0;
  FStep10Button.Width := BtnWidth;
  FStep10Button.Height := 35;
  FStep10Button.Caption := 'Step = 10';
  Inc(Y, FStepsPanel.Height + MARGIN);
  
  // Reset button
  FResetButton := TButton.Create(FContainer);
  FResetButton.Parent := FContainer;
  FResetButton.Left := (FContainer.ClientWidth - 100) div 2;
  FResetButton.Top := Y;
  FResetButton.Width := 100;
  FResetButton.Height := 30;
  FResetButton.Caption := 'Reset';
  Inc(Y, FResetButton.Height + MARGIN * 2);
  
  // History label
  FHistoryLabel := TLabel.Create(FContainer);
  FHistoryLabel.Parent := FContainer;
  FHistoryLabel.Left := MARGIN;
  FHistoryLabel.Top := Y;
  FHistoryLabel.Caption := 'History:';
  FHistoryLabel.Font.Style := [fsBold];
  Inc(Y, FHistoryLabel.Height + 5);
  
  // History memo
  FHistoryMemo := TMemo.Create(FContainer);
  FHistoryMemo.Parent := FContainer;
  FHistoryMemo.Left := MARGIN;
  FHistoryMemo.Top := Y;
  FHistoryMemo.Width := FContainer.ClientWidth - (MARGIN * 2);
  FHistoryMemo.Height := FContainer.ClientHeight - Y - MARGIN;
  FHistoryMemo.ReadOnly := True;
  FHistoryMemo.ScrollBars := ssVertical;
  FHistoryMemo.Font.Name := 'Consolas';
  FHistoryMemo.Font.Size := 9;
  FHistoryMemo.Color := $00F5F5F5;
end;

procedure TCounterView.WireEvents;
begin
  // Each button click dispatches a message
  // This is the ONLY way the View communicates intent
  FIncrementButton.OnClick := OnIncrementClick;
  FDecrementButton.OnClick := OnDecrementClick;
  FIncrementStepButton.OnClick := OnIncrementStepClick;
  FDecrementStepButton.OnClick := OnDecrementStepClick;
  FResetButton.OnClick := OnResetClick;
  FStep1Button.OnClick := OnStep1Click;
  FStep5Button.OnClick := OnStep5Click;
  FStep10Button.OnClick := OnStep10Click;
end;

// Event Handlers - simple wrappers that dispatch messages

procedure TCounterView.OnIncrementClick(Sender: TObject);
begin
  FDispatch(IncrementMsg);
end;

procedure TCounterView.OnDecrementClick(Sender: TObject);
begin
  FDispatch(DecrementMsg);
end;

procedure TCounterView.OnIncrementStepClick(Sender: TObject);
begin
  FDispatch(IncrementByStepMsg);
end;

procedure TCounterView.OnDecrementStepClick(Sender: TObject);
begin
  FDispatch(DecrementByStepMsg);
end;

procedure TCounterView.OnResetClick(Sender: TObject);
begin
  FDispatch(ResetMsg);
end;

procedure TCounterView.OnStep1Click(Sender: TObject);
begin
  FDispatch(SetStep1Msg);
end;

procedure TCounterView.OnStep5Click(Sender: TObject);
begin
  FDispatch(SetStep5Msg);
end;

procedure TCounterView.OnStep10Click(Sender: TObject);
begin
  FDispatch(SetStep10Msg);
end;

procedure TCounterView.Render(const Model: TCounterModel);
begin
  // Update UI based on Model - pure function of state
  FCountLabel.Caption := Model.Count.ToString;
  FStepLabel.Caption := Format('Step: %d', [Model.Step]);
  
  // Update step button captions to reflect current step
  FIncrementStepButton.Caption := Format('+ %d', [Model.Step]);
  FDecrementStepButton.Caption := Format('- %d', [Model.Step]);
  
  // History
  FHistoryMemo.Text := Model.History;
  
  // Visual feedback - highlight active step button
  FStep1Button.Font.Style := [];
  FStep5Button.Font.Style := [];
  FStep10Button.Font.Style := [];
  case Model.Step of
    1: FStep1Button.Font.Style := [fsBold];
    5: FStep5Button.Font.Style := [fsBold];
    10: FStep10Button.Font.Style := [fsBold];
  end;
end;

{$ENDREGION}

{$REGION 'TMVURuntime Implementation'}

constructor TMVURuntime<TModel>.Create(const InitialModel: TModel; OnChanged: TProc<TModel>);
begin
  inherited Create;
  FModel := InitialModel;
  FOnModelChanged := OnChanged;
end;

procedure TMVURuntime<TModel>.UpdateModel(const NewModel: TModel);
begin
  FModel := NewModel;
  if Assigned(FOnModelChanged) then
    FOnModelChanged(FModel);
end;

{$ENDREGION}

end.
