{***************************************************************************}
{                                                                           }
{           Dext Framework                                                  }
{                                                                           }
{           Copyright (C) 2025 Cesar Romero & Dext Contributors             }
{                                                                           }
{           Licensed under the Apache License, Version 2.0 (the "License"); }
{           you may not use this file except in compliance with the License.}
{           You may obtain a copy of the License at                         }
{                                                                           }
{               http://www.apache.org/licenses/LICENSE-2.0                  }
{                                                                           }
{           Unless required by applicable law or agreed to in writing,      }
{           software distributed under the License is distributed on an     }
{           "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    }
{           either express or implied. See the License for the specific     }
{           language governing permissions and limitations under the        }
{           License.                                                        }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Author:  Cesar Romero                                                    }
{  Created: 2026-01-19                                                      }
{                                                                           }
{***************************************************************************}

/// <summary>
/// Dext.UI.Binder - RTTI-based binding engine for MVU pattern
///
/// Automatically discovers binding attributes on Frame controls and:
/// - Connects Model properties to control values (BindText, BindChecked, etc.)
/// - Wires control events to message dispatch (OnClickMsg, OnChangeMsg)
///
/// Usage:
///   FBinder := TMVUBinder<TMyModel, TMyMessage>.Create(MyFrame, DispatchProc);
///   FBinder.Render(Model);  // Updates all bound controls
/// </summary>
unit Dext.UI.Binder;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Rtti,
  System.TypInfo,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Dext.UI.Attributes,
  Dext.Collections,
  Dext.UI.Message;

type
  /// <summary>
  /// Message sent when the UI needs to be updated after a model change.
  /// </summary>
  TUIUpdateMsg = class(TMessage);

  /// <summary>
  /// Internal binding information
  /// </summary>
  TBindingInfo = record
    Control: TControl;
    PropertyPath: string;
    Format: string;
    BindingType: (btText, btChecked, btEnabled, btVisible, btItems, btEdit, btMemo);
    Invert: Boolean;
  end;
  
  /// <summary>
  /// Internal event wiring information
  /// </summary>
  TEventWiring = record
    Control: TControl;
    EventType: (etClick, etChange, etFocus);
    MessageClass: TClass;
  end;
  
  /// <summary>
  /// RTTI-based binder that connects UI controls to the MVU Model.
  /// Discovers binding attributes on Frame fields and automates updates.
  /// </summary>
  TMVUBinder<TModel; TMsg: TMessage> = class
  public
    type PModel = ^TModel;
  private
    FFrame: TComponent;
    FDispatch: TProc<TMsg>;
    FBindings: IList<TBindingInfo>;
    FWirings: IList<TEventWiring>;
    FContext: TRttiContext;
    FModel: Pointer; // Pointer to the current model instance
    
    procedure DiscoverBindings;
    procedure DiscoverEventsWiring;
    procedure ApplyBinding(const Binding: TBindingInfo; const Model: TModel);
    procedure UpdateModelFromControl(const Binding: TBindingInfo; const Control: TControl);
    function GetPropertyValue(const Model: TModel; const PropertyPath: string): TValue;
    procedure SetPropertyValue(var Model: TModel; const PropertyPath: string; const Value: TValue);
    
    // Event handlers
    procedure HandleClick(Sender: TObject);
    procedure HandleChange(Sender: TObject);
  public
    constructor Create(AFrame: TComponent; ADispatch: TProc<TMsg>);
    destructor Destroy; override;
    
    /// <summary>
    /// Updates all bound controls from the current Model state.
    /// Call this after every Update.
    /// </summary>
    procedure Render(var Model: TModel);
  end;

implementation

uses
  System.Variants;

{ TMVUBinder<TModel, TMsg> }

constructor TMVUBinder<TModel, TMsg>.Create(AFrame: TComponent; ADispatch: TProc<TMsg>);
begin
  inherited Create;
  FFrame := AFrame;
  FDispatch := ADispatch;
  FBindings := TCollections.CreateList<TBindingInfo>;
  FWirings := TCollections.CreateList<TEventWiring>;
  FContext := TRttiContext.Create;
  
  DiscoverBindings;
  DiscoverEventsWiring;
end;

destructor TMVUBinder<TModel, TMsg>.Destroy;
begin
  // FWirings and FBindings are ARC
  FContext.Free;
  inherited;
end;

procedure TMVUBinder<TModel, TMsg>.DiscoverBindings;
var
  RttiType: TRttiType;
  Field: TRttiField;
  Attr: TCustomAttribute;
  Binding: TBindingInfo;
  Control: TControl;
begin
  RttiType := FContext.GetType(FFrame.ClassType);
  if RttiType = nil then Exit;
  
  for Field in RttiType.GetFields do
  begin
    // Check if field is a TControl
    if not Field.FieldType.IsInstance then Continue;
    if not Field.FieldType.AsInstance.MetaclassType.InheritsFrom(TControl) then Continue;
    
    Control := Field.GetValue(FFrame).AsObject as TControl;
    if Control = nil then Continue;
    
    // Check for binding attributes
    for Attr in Field.GetAttributes do
    begin
      if Attr is BindTextAttribute then
      begin
        Binding.Control := Control;
        Binding.PropertyPath := BindTextAttribute(Attr).PropertyPath;
        Binding.Format := BindTextAttribute(Attr).Format;
        Binding.BindingType := btText;
        Binding.Invert := False;
        FBindings.Add(Binding);
      end
      else if Attr is BindCheckedAttribute then
      begin
        Binding.Control := Control;
        Binding.PropertyPath := BindCheckedAttribute(Attr).PropertyPath;
        Binding.Format := '';
        Binding.BindingType := btChecked;
        Binding.Invert := False;
        FBindings.Add(Binding);
      end
      else if Attr is BindEnabledAttribute then
      begin
        Binding.Control := Control;
        Binding.PropertyPath := BindEnabledAttribute(Attr).PropertyPath;
        Binding.Format := '';
        Binding.BindingType := btEnabled;
        Binding.Invert := BindEnabledAttribute(Attr).Invert;
        FBindings.Add(Binding);
      end
      else if Attr is BindVisibleAttribute then
      begin
        Binding.Control := Control;
        Binding.PropertyPath := BindVisibleAttribute(Attr).PropertyPath;
        Binding.Format := '';
        Binding.BindingType := btVisible;
        Binding.Invert := BindVisibleAttribute(Attr).Invert;
        FBindings.Add(Binding);
      end
      else if Attr is BindEditAttribute then
      begin
        Binding.Control := Control;
        Binding.PropertyPath := BindEditAttribute(Attr).PropertyPath;
        Binding.Format := '';
        Binding.BindingType := btEdit;
        Binding.Invert := False;
        FBindings.Add(Binding);
      end
      else if Attr is BindMemoAttribute then
      begin
        Binding.Control := Control;
        Binding.PropertyPath := BindMemoAttribute(Attr).PropertyPath;
        Binding.Format := '';
        Binding.BindingType := btMemo;
        Binding.Invert := False;
        FBindings.Add(Binding);
      end;
    end;
  end;
end;

procedure TMVUBinder<TModel, TMsg>.DiscoverEventsWiring;
var
  RttiType: TRttiType;
  Field: TRttiField;
  Attr: TCustomAttribute;
  Wiring: TEventWiring;
  Control: TControl;
begin
  RttiType := FContext.GetType(FFrame.ClassType);
  if RttiType = nil then Exit;
  
  for Field in RttiType.GetFields do
  begin
    if not Field.FieldType.IsInstance then Continue;
    if not Field.FieldType.AsInstance.MetaclassType.InheritsFrom(TControl) then Continue;
    
    Control := Field.GetValue(FFrame).AsObject as TControl;
    if Control = nil then Continue;
    
    for Attr in Field.GetAttributes do
    begin
      if Attr is OnClickMsgAttribute then
      begin
        Wiring.Control := Control;
        Wiring.EventType := etClick;
        Wiring.MessageClass := OnClickMsgAttribute(Attr).MessageClass;
        FWirings.Add(Wiring);
        
        // Wire the event
        if Control is TButton then
          TButton(Control).OnClick := HandleClick
        else if Control is TPanel then
          TPanel(Control).OnClick := HandleClick
        else if Control is TLabel then
          TLabel(Control).OnClick := HandleClick;
          
        // Store message class reference in control's Tag
        Control.Tag := NativeInt(Wiring.MessageClass);
      end;
      
      // Automatic two-way binding wiring
      if (Attr is BindEditAttribute) or (Attr is BindMemoAttribute) or 
         (Attr is BindCheckedAttribute) or (Attr is BindTextAttribute) then
      begin
        if Control is TEdit then
          TEdit(Control).OnChange := HandleChange
        else if Control is TMemo then
          TMemo(Control).OnChange := HandleChange
        else if Control is TCheckBox then
          TCheckBox(Control).OnClick := HandleChange;
      end;
    end;
  end;
end;

procedure TMVUBinder<TModel, TMsg>.HandleClick(Sender: TObject);
var
  MessageClass: TClass;
  Msg: TMsg;
begin
  if not (Sender is TControl) then Exit;
  
  MessageClass := TClass(TControl(Sender).Tag);
  if MessageClass = nil then Exit;
  
  // Create message instance
  if MessageClass.InheritsFrom(TMessage) then
  begin
    Msg := TMessageClass(MessageClass).Create as TMsg;
    try
      FDispatch(Msg);
    finally
      Msg.Free;
    end;
  end;
end;

procedure TMVUBinder<TModel, TMsg>.HandleChange(Sender: TObject);
var
  Binding: TBindingInfo;
  Control: TControl;
  UpdateMsg: TMsg;
begin
  if not (Sender is TControl) then Exit;
  Control := TControl(Sender);
  
  // Find binding for this control
  for Binding in FBindings do
  begin
    if Binding.Control = Control then
    begin
      UpdateModelFromControl(Binding, Control);
      
      // Refresh all bindings to reflect potential state changes in the model (like CanSave)
      if FModel <> nil then
      begin
        Render(PModel(FModel)^);
        
        if TUIUpdateMsg.InheritsFrom(TClass(TMsg)) then
        begin
          UpdateMsg := TMessageClass(TUIUpdateMsg).Create as TMsg;
          try
            FDispatch(UpdateMsg);
          finally
            UpdateMsg.Free;
          end;
        end;
      end;
      
      Break;
    end;
  end;
end;

procedure TMVUBinder<TModel, TMsg>.Render(var Model: TModel);
var
  Binding: TBindingInfo;
begin
  FModel := @Model;
  for Binding in FBindings do
    ApplyBinding(Binding, Model);
end;

procedure TMVUBinder<TModel, TMsg>.UpdateModelFromControl(const Binding: TBindingInfo; const Control: TControl);
var
  NewValue: TValue;
begin
  if FModel = nil then Exit;
  
  case Binding.BindingType of
    btEdit, btText:
      if Control is TEdit then
        NewValue := TEdit(Control).Text
      else if Control is TLabel then
        NewValue := TLabel(Control).Caption;
        
    btMemo:
      if Control is TMemo then
        NewValue := TMemo(Control).Text;
        
    btChecked:
      if Control is TCheckBox then
        NewValue := TCheckBox(Control).Checked;
  else
    Exit;
  end;
  
  if FModel <> nil then
    SetPropertyValue(PModel(FModel)^, Binding.PropertyPath, NewValue);
end;

procedure TMVUBinder<TModel, TMsg>.ApplyBinding(const Binding: TBindingInfo; const Model: TModel);
var
  Value: TValue;
  StrValue: string;
  BoolValue: Boolean;
begin
  Value := GetPropertyValue(Model, Binding.PropertyPath);
  
  case Binding.BindingType of
    btText:
      begin
        if Binding.Format <> '' then
          StrValue := Format(Binding.Format, [Value.AsVariant])
        else if Value.Kind = tkInteger then
          StrValue := Value.AsInteger.ToString
        else if Value.Kind = tkInt64 then
          StrValue := Value.AsInt64.ToString
        else if Value.Kind = tkFloat then
          StrValue := FloatToStr(Value.AsExtended)
        else
          StrValue := Value.ToString;
          
        if Binding.Control is TLabel then
          TLabel(Binding.Control).Caption := StrValue
        else if Binding.Control is TEdit then
          TEdit(Binding.Control).Text := StrValue
        else if Binding.Control is TMemo then
          TMemo(Binding.Control).Text := StrValue;
      end;
      
    btChecked:
      begin
        BoolValue := Value.AsBoolean;
        if Binding.Control is TCheckBox then
          TCheckBox(Binding.Control).Checked := BoolValue;
      end;
      
    btEdit:
      begin
        StrValue := Value.ToString;
        if Binding.Control is TEdit then
        begin
          if TEdit(Binding.Control).Text <> StrValue then
            TEdit(Binding.Control).Text := StrValue;
        end;
      end;
      
    btMemo:
      begin
        StrValue := Value.ToString;
        if Binding.Control is TMemo then
        begin
          if TMemo(Binding.Control).Text <> StrValue then
            TMemo(Binding.Control).Text := StrValue;
        end;
      end;
      
    btEnabled:
      begin
        if Value.Kind in [tkInteger, tkInt64, tkFloat] then
          BoolValue := Value.AsVariant <> 0
        else if Value.Kind = tkVariant then
          BoolValue := not VarIsNull(Value.AsVariant) and (Value.AsVariant <> 0)
        else
          BoolValue := Value.AsBoolean;

        if Binding.Invert then BoolValue := not BoolValue;
        Binding.Control.Enabled := BoolValue;
      end;
      
    btVisible:
      begin
        if Value.Kind in [tkInteger, tkInt64, tkFloat] then
          BoolValue := Value.AsVariant <> 0
        else if Value.Kind = tkVariant then
          BoolValue := not VarIsNull(Value.AsVariant) and (Value.AsVariant <> 0)
        else
          BoolValue := Value.AsBoolean;

        if Binding.Invert then BoolValue := not BoolValue;
        Binding.Control.Visible := BoolValue;
      end;
  end;
end;

function TMVUBinder<TModel, TMsg>.GetPropertyValue(const Model: TModel; const PropertyPath: string): TValue;
var
  RttiType: TRttiType;
  Field: TRttiField;
  Prop: TRttiProperty;
  Instance: TObject;
  Parts: TArray<string>;
  CurrentValue: TValue;
  I: Integer;
begin
  Result := TValue.Empty;
  Parts := PropertyPath.Split(['.']);
  
  // Start with the model instance
  CurrentValue := TValue.From<TModel>(Model);
  
  for I := 0 to High(Parts) do
  begin
    if CurrentValue.IsEmpty then Exit(TValue.Empty);
    
    RttiType := FContext.GetType(CurrentValue.TypeInfo);
    if RttiType = nil then Exit(TValue.Empty);

    Instance := nil;
    if RttiType.IsInstance then
    begin
      Instance := CurrentValue.AsObject;
      if Instance <> nil then
        RttiType := FContext.GetType(Instance.ClassType);
    end;

    // Try field
    Field := RttiType.GetField(Parts[I]);
    if Field <> nil then
    begin
      if Instance <> nil then
        CurrentValue := Field.GetValue(Instance)
      else
        CurrentValue := Field.GetValue(CurrentValue.GetReferenceToRawData);
    end
    else
    begin
      // Try property
      Prop := RttiType.GetProperty(Parts[I]);
      if Prop <> nil then
      begin
        if Instance <> nil then
          CurrentValue := Prop.GetValue(Instance)
        else
          CurrentValue := Prop.GetValue(CurrentValue.GetReferenceToRawData);
      end
      else
        Exit(TValue.Empty); // Path part not found
    end;
  end;
  
  Result := CurrentValue;
end;

procedure TMVUBinder<TModel, TMsg>.SetPropertyValue(var Model: TModel; const PropertyPath: string; const Value: TValue);
var
  RttiType: TRttiType;
  Field: TRttiField;
  Prop: TRttiProperty;
  Instance: TObject;
  Parts: TArray<string>;
  CurrentValue: TValue;
  I: Integer;
begin
  Parts := PropertyPath.Split(['.']);
  
  if Length(Parts) = 1 then
  begin
    // Fast path for top-level properties
    CurrentValue := TValue.From<TModel>(Model);
    RttiType := FContext.GetType(CurrentValue.TypeInfo);
    if RttiType = nil then Exit;

    Instance := nil;
    if RttiType.IsInstance then
      Instance := CurrentValue.AsObject;

    Field := RttiType.GetField(Parts[0]);
    if Field <> nil then
    begin
      if Instance <> nil then Field.SetValue(Instance, Value)
      else Field.SetValue(CurrentValue.GetReferenceToRawData, Value);
      Exit;
    end;

    Prop := RttiType.GetProperty(Parts[0]);
    if (Prop <> nil) and Prop.IsWritable then
    begin
      if Instance <> nil then Prop.SetValue(Instance, Value)
      else Prop.SetValue(CurrentValue.GetReferenceToRawData, Value);
    end;
    Exit;
  end;

  // Nested path: Resolve all but the last part
  CurrentValue := TValue.From<TModel>(Model);
  for I := 0 to High(Parts) - 1 do
  begin
    RttiType := FContext.GetType(CurrentValue.TypeInfo);
    if RttiType = nil then Exit;
    
    Instance := nil;
    if RttiType.IsInstance then
      Instance := CurrentValue.AsObject;

    Prop := RttiType.GetProperty(Parts[I]);
    if Prop <> nil then
      CurrentValue := Prop.GetValue(Instance)
    else
    begin
      Field := RttiType.GetField(Parts[I]);
      if Field <> nil then
        CurrentValue := Field.GetValue(Instance)
      else
        Exit; // Path segment not found
    end;
  end;

  // Final part
  if CurrentValue.IsEmpty then Exit;
  RttiType := FContext.GetType(CurrentValue.TypeInfo);
  Instance := nil;
  if RttiType.IsInstance then
    Instance := CurrentValue.AsObject;

  Prop := RttiType.GetProperty(Parts[High(Parts)]);
  if (Prop <> nil) and Prop.IsWritable then
  begin
    if Instance <> nil then Prop.SetValue(Instance, Value)
    else Prop.SetValue(CurrentValue.GetReferenceToRawData, Value);
  end
  else
  begin
    Field := RttiType.GetField(Parts[High(Parts)]);
    if Field <> nil then
    begin
      if Instance <> nil then Field.SetValue(Instance, Value)
      else Field.SetValue(CurrentValue.GetReferenceToRawData, Value);
    end;
  end;
end;

end.
