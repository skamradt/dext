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
{  Created: 2025-12-08                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.Validation;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  System.RegularExpressions;

type
  /// <summary>
  ///   Validation result for a single field or the entire model.
  /// </summary>
  TValidationError = record
    FieldName: string;
    ErrorMessage: string;
  end;

  TValidationResult = class
  private
    FErrors: TList<TValidationError>;
    function GetIsValid: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure AddError(const AFieldName, AMessage: string);
    function GetErrors: TArray<TValidationError>;
    
    property IsValid: Boolean read GetIsValid;
    property Errors: TArray<TValidationError> read GetErrors;
  end;

  /// <summary>
  ///   Base class for validation attributes.
  /// </summary>
  ValidationAttribute = class abstract(TCustomAttribute)
  public
    function IsValid(const AValue: TValue): Boolean; virtual; abstract;
    function GetErrorMessage(const AFieldName: string): string; virtual; abstract;
  end;

  /// <summary>
  ///   Specifies that a field is required (not empty/zero).
  /// </summary>
  RequiredAttribute = class(ValidationAttribute)
  public
    function IsValid(const AValue: TValue): Boolean; override;
    function GetErrorMessage(const AFieldName: string): string; override;
  end;

  /// <summary>
  ///   Specifies string length constraints.
  /// </summary>
  StringLengthAttribute = class(ValidationAttribute)
  private
    FMinLength: Integer;
    FMaxLength: Integer;
  public
    constructor Create(AMinLength, AMaxLength: Integer);
    function IsValid(const AValue: TValue): Boolean; override;
    function GetErrorMessage(const AFieldName: string): string; override;
  end;

  /// <summary>
  ///   Validates that a string is a valid email address.
  /// </summary>
  EmailAddressAttribute = class(ValidationAttribute)
  public
    function IsValid(const AValue: TValue): Boolean; override;
    function GetErrorMessage(const AFieldName: string): string; override;
  end;

  /// <summary>
  ///   Specifies numeric range constraints.
  /// </summary>
  RangeAttribute = class(ValidationAttribute)
  private
    FMin: Double;
    FMax: Double;
  public
    constructor Create(AMin, AMax: Double); overload;
    constructor Create(AMin, AMax: Integer); overload;
    function IsValid(const AValue: TValue): Boolean; override;
    function GetErrorMessage(const AFieldName: string): string; override;
  end;

  /// <summary>
  ///   Validates a record using RTTI and validation attributes.
  /// </summary>
  IValidator<T> = interface
    ['{E8F9A2B3-4C5D-6E7F-8A9B-0C1D2E3F4A5B}']
    function Validate(const AValue: T): TValidationResult;
  end;

  TValidator<T> = class(TInterfacedObject, IValidator<T>)
  public
    function Validate(const AValue: T): TValidationResult;
  end;

  /// <summary>
  ///   Non-generic validator helper.
  /// </summary>
  TValidator = class
  private
     class function GetFieldValue(const AValue: TValue): TValue;
  public
    class function Validate(const AValue: TValue): TValidationResult;
  end;

implementation

{ TValidator }

class function TValidator.GetFieldValue(const AValue: TValue): TValue;
var
  Ctx: TRttiContext;
  RType: TRttiType;
  Field: TRttiField;
begin
  Result := AValue;
  if AValue.Kind = tkRecord then
  begin
    Ctx := TRttiContext.Create;
    try
      RType := Ctx.GetType(AValue.TypeInfo);
      if (RType <> nil) and (RType is TRttiRecordType) then
      begin
        Field := TRttiRecordType(RType).GetField('FValue');
        if Field <> nil then
          Result := Field.GetValue(AValue.GetReferenceToRawData);
      end;
    finally
      Ctx.Free;
    end;
  end;
end;

{ TValidationResult }

constructor TValidationResult.Create;
begin
  inherited Create;
  FErrors := TList<TValidationError>.Create;
end;

destructor TValidationResult.Destroy;
begin
  FErrors.Free;
  inherited;
end;

procedure TValidationResult.AddError(const AFieldName, AMessage: string);
var
  Error: TValidationError;
begin
  Error.FieldName := AFieldName;
  Error.ErrorMessage := AMessage;
  FErrors.Add(Error);
end;

function TValidationResult.GetErrors: TArray<TValidationError>;
begin
  Result := FErrors.ToArray;
end;

function TValidationResult.GetIsValid: Boolean;
begin
  Result := FErrors.Count = 0;
end;

{ RequiredAttribute }

function RequiredAttribute.IsValid(const AValue: TValue): Boolean;
var
  Val: TValue;
begin
  Val := TValidator.GetFieldValue(AValue);
  if Val.IsEmpty then
    Exit(False);

  case Val.Kind of
    tkString, tkLString, tkWString, tkUString:
      Result := Val.AsString.Trim <> '';
    tkInteger, tkInt64:
      Result := True; // Integers are always "present"
    tkFloat:
      Result := True;
    else
      Result := not AValue.IsEmpty;
  end;
end;

function RequiredAttribute.GetErrorMessage(const AFieldName: string): string;
begin
  Result := Format('The field "%s" is required.', [AFieldName]);
end;

{ StringLengthAttribute }

constructor StringLengthAttribute.Create(AMinLength, AMaxLength: Integer);
begin
  inherited Create;
  FMinLength := AMinLength;
  FMaxLength := AMaxLength;
end;

function StringLengthAttribute.IsValid(const AValue: TValue): Boolean;
var
  Len: Integer;
  Val: TValue;
begin
  Val := TValidator.GetFieldValue(AValue);
  if not (Val.Kind in [tkString, tkLString, tkWString, tkUString]) then
    Exit(True); // Not a string, skip validation

  Len := Val.AsString.Length;
  Result := (Len >= FMinLength) and (Len <= FMaxLength);
end;

function StringLengthAttribute.GetErrorMessage(const AFieldName: string): string;
begin
  Result := Format('The field "%s" must be between %d and %d characters.', 
    [AFieldName, FMinLength, FMaxLength]);
end;

{ EmailAddressAttribute }

function EmailAddressAttribute.IsValid(const AValue: TValue): Boolean;
var
  Email: string;
  Regex: TRegEx;
  Val: TValue;
begin
  Val := TValidator.GetFieldValue(AValue);
  if not (Val.Kind in [tkString, tkLString, tkWString, tkUString]) then
    Exit(True);

  Email := Val.AsString.Trim;
  if Email = '' then
    Exit(True); // Empty is valid (use Required for mandatory)

  // Simple email regex
  Regex := TRegEx.Create('^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$');
  Result := Regex.IsMatch(Email);
end;

function EmailAddressAttribute.GetErrorMessage(const AFieldName: string): string;
begin
  Result := Format('The field "%s" must be a valid email address.', [AFieldName]);
end;

{ RangeAttribute }

constructor RangeAttribute.Create(AMin, AMax: Double);
begin
  inherited Create;
  FMin := AMin;
  FMax := AMax;
end;

constructor RangeAttribute.Create(AMin, AMax: Integer);
begin
  Create(Double(AMin), Double(AMax));
end;

function RangeAttribute.IsValid(const AValue: TValue): Boolean;
var
  NumValue: Double;
  Val: TValue;
begin
  Val := TValidator.GetFieldValue(AValue);
  case Val.Kind of
    tkInteger:
      NumValue := Val.AsInteger;
    tkInt64:
      NumValue := Val.AsInt64;
    tkFloat:
      NumValue := Val.AsExtended;
    else
      Exit(True); // Not a number, skip
  end;

  Result := (NumValue >= FMin) and (NumValue <= FMax);
end;

function RangeAttribute.GetErrorMessage(const AFieldName: string): string;
begin
  Result := Format('The field "%s" must be between %.0f and %.0f.', 
    [AFieldName, FMin, FMax]);
end;

{ TValidator<T> }

function TValidator<T>.Validate(const AValue: T): TValidationResult;
begin
  Result := TValidator.Validate(TValue.From<T>(AValue));
end;

{ TValidator (Non-generic) }

class function TValidator.Validate(const AValue: TValue): TValidationResult;
var
  Context: TRttiContext;
  RttiType: TRttiType;
  Field: TRttiField;
  Prop: TRttiProperty;
  Attr: TCustomAttribute;
  FieldValue: TValue;
  Instance: Pointer;
  ValidationAttr: ValidationAttribute;
begin
  Result := TValidationResult.Create;
  
  if AValue.IsEmpty then
    Exit;

  Context := TRttiContext.Create;
  try
    RttiType := Context.GetType(AValue.TypeInfo);
    if RttiType = nil then Exit;

    if RttiType.IsInstance then
      Instance := AValue.AsObject
    else
      Instance := AValue.GetReferenceToRawData;

    // Validate Fields
    for Field in RttiType.GetFields do
    begin
      FieldValue := Field.GetValue(Instance);
      
      for Attr in Field.GetAttributes do
      begin
        if Attr is ValidationAttribute then
        begin
          ValidationAttr := ValidationAttribute(Attr);
          if not ValidationAttr.IsValid(FieldValue) then
          begin
            Result.AddError(Field.Name, ValidationAttr.GetErrorMessage(Field.Name));
          end;
        end;
      end;
    end;

    // Validate Properties
    for Prop in RttiType.GetProperties do
    begin
      FieldValue := Prop.GetValue(Instance);
      
      for Attr in Prop.GetAttributes do
      begin
        if Attr is ValidationAttribute then
        begin
          ValidationAttr := ValidationAttribute(Attr);
          if not ValidationAttr.IsValid(FieldValue) then
          begin
            Result.AddError(Prop.Name, ValidationAttr.GetErrorMessage(Prop.Name));
          end;
        end;
      end;
    end;
  finally
    Context.Free;
  end;
end;

end.

