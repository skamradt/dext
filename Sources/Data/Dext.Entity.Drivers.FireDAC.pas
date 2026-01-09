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
unit Dext.Entity.Drivers.FireDAC;

interface

{$I Dext.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  System.DateUtils,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  FireDAC.DApt,
  FireDAC.DApt.Intf,
  FireDAC.DatS,
  FireDAC.Phys.Intf,
  {$IFDEF DEXT_ENABLE_DB_SQLITE}
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLiteWrapper,
  FireDAC.Phys.SQLiteCli,
  {$ENDIF}
  FireDAC.Stan.Async,
  FireDAC.Stan.Def,
  FireDAC.Stan.Error,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.TypeConverters,
  Dext.Entity.Dialects,
  Dext.Types.Nullable,
  Dext.Types.UUID;

type
  TFireDACConnection = class;

  TFireDACTransaction = class(TInterfacedObject, IDbTransaction)
  private
    FTransaction: TFDTransaction;
    FOwnsTransaction: Boolean;
  public
    constructor Create(AConnection: TFDConnection);
    destructor Destroy; override;
    procedure Commit;
    procedure Rollback;
  end;

  TFireDACReader = class(TInterfacedObject, IDbReader)
  private
    FQuery: TFDQuery;
    FOwnsQuery: Boolean;
    FIsFirstMove: Boolean;
  public
    constructor Create(AQuery: TFDQuery; AOwnsQuery: Boolean);
    destructor Destroy; override;
    
    function Next: Boolean;
    function GetValue(const AColumnName: string): TValue; overload;
    function GetValue(AColumnIndex: Integer): TValue; overload;
    function GetColumnCount: Integer;
    function GetColumnName(AIndex: Integer): string;
    procedure Close;
  end;

  TFireDACCommand = class(TInterfacedObject, IDbCommand)
  private
    FQuery: TFDQuery;
    FConnection: TFDConnection;
    FDialect: TDatabaseDialect;
    FOnLog: TProc<string>;
    procedure SetParamValue(Param: TFDParam; const AValue: TValue);
    function GetDialect: TDatabaseDialect;
  public
    constructor Create(AConnection: TFDConnection; ADialect: TDatabaseDialect);
    destructor Destroy; override;
    
    procedure SetSQL(const ASQL: string);
    procedure AddParam(const AName: string; const AValue: TValue);
    procedure ClearParams;
    
    procedure Execute;
    function ExecuteQuery: IDbReader;
    function ExecuteNonQuery: Integer;
    function ExecuteScalar: TValue;
    
    procedure SetArraySize(const ASize: Integer);
    procedure SetParamArray(const AName: string; const AValues: TArray<TValue>);
    procedure ExecuteBatch(const ATimes: Integer; const AOffset: Integer = 0);
  end;

  TFireDACConnection = class(TInterfacedObject, IDbConnection)
  private
    FConnection: TFDConnection;
    FOwnsConnection: Boolean;
    FOnLog: TProc<string>;
    FDialect: TDatabaseDialect;
    procedure DetectDialect;
  public
    constructor Create(AConnection: TFDConnection; AOwnsConnection: Boolean = True);
    destructor Destroy; override;
    
    procedure Connect;
    procedure Disconnect;
    function IsConnected: Boolean;
    
    function BeginTransaction: IDbTransaction;
    function CreateCommand(const ASQL: string): IDbCommand;
    function GetLastInsertId: Variant;
    function TableExists(const ATableName: string): Boolean;

    function GetConnectionString: string;
    procedure SetConnectionString(const AValue: string);
    property ConnectionString: string read GetConnectionString write SetConnectionString;
    
    function GetDialect: TDatabaseDialect;
    property Dialect: TDatabaseDialect read GetDialect;
    
    procedure SetOnLog(AValue: TProc<string>);
    function GetOnLog: TProc<string>;
    property OnLog: TProc<string> read GetOnLog write SetOnLog;
    
    property Connection: TFDConnection read FConnection;
  end;

implementation

uses
  Dext.Utils;

{ TFireDACTransaction }

constructor TFireDACTransaction.Create(AConnection: TFDConnection);
begin
  inherited Create;
  FTransaction := TFDTransaction.Create(nil);
  FTransaction.Connection := AConnection;
  FTransaction.StartTransaction;
  FOwnsTransaction := True;
end;

destructor TFireDACTransaction.Destroy;
begin
  if FOwnsTransaction and (FTransaction <> nil) then
  begin
    if FTransaction.Active then
      FTransaction.Rollback;
    FTransaction.Free;
  end;
  inherited;
end;

procedure TFireDACTransaction.Commit;
begin
  FTransaction.Commit;
end;

procedure TFireDACTransaction.Rollback;
begin
  FTransaction.Rollback;
end;

{ TFireDACReader }

constructor TFireDACReader.Create(AQuery: TFDQuery; AOwnsQuery: Boolean);
begin
  inherited Create;
  FQuery := AQuery;
  FOwnsQuery := AOwnsQuery;
  FIsFirstMove := True;
end;

destructor TFireDACReader.Destroy;
begin
  if FOwnsQuery then
    FQuery.Free;
  inherited;
end;

procedure TFireDACReader.Close;
begin
  FQuery.Close;
end;

function TFireDACReader.GetColumnCount: Integer;
begin
  Result := FQuery.FieldCount;
end;

function TFireDACReader.GetColumnName(AIndex: Integer): string;
begin
  Result := FQuery.Fields[AIndex].FieldName;
end;

function TFireDACReader.GetValue(AColumnIndex: Integer): TValue;
var
  Field: TField;
begin
  Field := FQuery.Fields[AColumnIndex];
  case Field.DataType of
    ftDate: Result := TValue.From<TDate>(DateOf(Field.AsDateTime));
    ftTime: Result := TValue.From<TTime>(TimeOf(Field.AsDateTime));
    ftDateTime, ftTimeStamp: Result := TValue.From<TDateTime>(Field.AsDateTime);
    ftBlob, ftOraBlob, ftGraphic: Result := TValue.From<TBytes>(Field.AsBytes);
    ftString, ftWideString, ftMemo, ftWideMemo: Result := TValue.From<string>(Field.AsString);
    ftGuid:
      // Return TGUID directly - TypeConverters handle byte-order conversion
      Result := TValue.From<TGUID>(TGuidField(Field).AsGuid);
    else
      Result := TValue.FromVariant(Field.Value);
  end;
end;

function TFireDACReader.GetValue(const AColumnName: string): TValue;
var
  Field: TField;
begin
  Field := FQuery.FieldByName(AColumnName);
  case Field.DataType of
    ftDate: Result := TValue.From<TDate>(DateOf(Field.AsDateTime));
    ftTime: Result := TValue.From<TTime>(TimeOf(Field.AsDateTime));
    ftDateTime, ftTimeStamp: Result := TValue.From<TDateTime>(Field.AsDateTime);
    ftBlob, ftOraBlob, ftGraphic: Result := TValue.From<TBytes>(Field.AsBytes);
    ftString, ftWideString, ftMemo, ftWideMemo: Result := TValue.From<string>(Field.AsString);
    ftGuid:
      // Return TGUID directly - TypeConverters handle byte-order conversion
      Result := TValue.From<TGUID>(TGuidField(Field).AsGuid);
    else
      Result := TValue.FromVariant(Field.Value);
  end;
end;

function TFireDACReader.Next: Boolean;
begin
  if not FQuery.Active then
    Exit(False);
    
  if FIsFirstMove then
  begin
    FIsFirstMove := False;
    // TDataSet is already at First after Open.
    // If it's empty, Eof is true.
    Result := not FQuery.Eof;
  end
  else
  begin
    FQuery.Next;
    Result := not FQuery.Eof;
  end;
end;

{ TFireDACCommand }

constructor TFireDACCommand.Create(AConnection: TFDConnection; ADialect: TDatabaseDialect);
begin
  inherited Create;
  FConnection := AConnection;
  FDialect := ADialect;
  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;
end;

destructor TFireDACCommand.Destroy;
begin
  FQuery.Free;
  inherited;
end;

procedure TFireDACCommand.AddParam(const AName: string; const AValue: TValue);
var
  Param: TFDParam;
begin
  try
    Param := FQuery.ParamByName(AName);
    SetParamValue(Param, AValue);
  except
    on E: Exception do
    begin
      SafeWriteLn(Format('CRITICAL ERROR in AddParam(%s): %s', [AName, E.Message]));
      raise;
    end;
  end;
end;

function TFireDACCommand.GetDialect: TDatabaseDialect;
begin
  Result := FDialect;
end;

procedure TFireDACCommand.SetParamValue(Param: TFDParam; const AValue: TValue);
var
  Converter: ITypeConverter;
  ConvertedValue: TValue;
begin


  if AValue.IsEmpty then
  begin
    Param.Clear;
    // Set correct DataType for empty values - byte arrays need ftBlob!
    if AValue.TypeInfo <> nil then
    begin
      var TypeName := string(AValue.TypeInfo.Name);
      if (TypeName = 'TBytes') or (TypeName = 'TArray<System.Byte>') or (TypeName = 'TArray<Byte>') then
        Param.DataType := ftBlob
      else if Param.DataType = ftUnknown then
        Param.DataType := ftString;
    end
    else if Param.DataType = ftUnknown then
      Param.DataType := ftString;
    Exit;
  end;
  
  // Try to find a type converter
  Converter := TTypeConverterRegistry.Instance.GetConverter(AValue.TypeInfo);

  
  if Converter <> nil then
  begin
    // Convert value using converter
    ConvertedValue := Converter.ToDatabase(AValue, GetDialect);

    
    // Fix for PostgreSQL UUID "operator does not exist" error
    // Explicitly handle TGUID, TUUID and String-formatted GUIDs
    if (AValue.TypeInfo = TypeInfo(TGUID)) or (AValue.TypeInfo = TypeInfo(TUUID)) then
    begin
      Param.DataType := ftGuid;
      Param.AsString := ConvertedValue.AsString;
    end
    // aggressive check for GUID strings to handle cases where TValue lost strict type info
    else if (ConvertedValue.Kind in [tkString, tkUString, tkWString]) and
            ((Length(ConvertedValue.AsString) = 36) or (Length(ConvertedValue.AsString) = 38)) and
            (ConvertedValue.AsString.IndexOf('-') > 0) then // Simple heuristic check
    begin
       // Only force ftGuid if it looks like a GUID. Postgres requires this for = operator.
       // Valid GUID ex: 550e8400-e29b-41d4-a716-446655440000 (36) or {550e8400...} (38)
       Param.DataType := ftGuid;
       Param.AsString := ConvertedValue.AsString;
    end
    else
    // Set param value (converted value is typically a string or integer)
    case ConvertedValue.Kind of
        tkInteger, tkInt64:
        begin
          Param.DataType := ftInteger;
          Param.AsLargeInt := ConvertedValue.AsInt64;
        end;
        tkFloat:
        begin
          if ConvertedValue.TypeInfo = TypeInfo(TDateTime) then
          begin
            Param.DataType := ftDateTime;
            Param.AsDateTime := ConvertedValue.AsType<TDateTime>;
          end
          else if ConvertedValue.TypeInfo = TypeInfo(TDate) then
          begin
            Param.DataType := ftDate;
            Param.AsDate := ConvertedValue.AsType<TDate>;
          end
          else if ConvertedValue.TypeInfo = TypeInfo(TTime) then
          begin
            Param.DataType := ftTime;
            Param.AsTime := ConvertedValue.AsType<TTime>;
          end
          else
          begin
            Param.DataType := ftFloat;
            Param.AsFloat := ConvertedValue.AsExtended;
          end;
        end;
        tkString, tkUString, tkWString, tkChar, tkWChar:
        begin
          // Use ftWideMemo for large strings to avoid FireDAC size limits
          if Length(ConvertedValue.AsString) > 4000 then
          begin
            Param.DataType := ftWideMemo;
            Param.Size := Length(ConvertedValue.AsString);
            Param.AsWideMemo := ConvertedValue.AsString;
          end
          else
            Param.AsWideString := ConvertedValue.AsString;
        end;
        tkDynArray:
        begin
          // Check for byte arrays by name (TBytes / TArray<Byte> / TArray<System.Byte>)
          // TypeInfo(TBytes) may not match TArray<System.Byte> pointer
          var IsByteArray := False;
          if ConvertedValue.TypeInfo <> nil then
          begin
            var TypeName := string(ConvertedValue.TypeInfo.Name);
            IsByteArray := (TypeName = 'TBytes') or 
                           (TypeName = 'TArray<System.Byte>') or 
                           (TypeName = 'TArray<Byte>');

          end;
          
          if IsByteArray then
          begin
             // Explicitly set ftBlob. FireDAC needs this for Postgres bytea.
             Param.DataType := ftBlob;
             var Bytes := ConvertedValue.AsType<TBytes>;
             var RawStr: RawByteString;
             SetLength(RawStr, Length(Bytes));
             if Length(Bytes) > 0 then
               Move(Bytes[0], RawStr[1], Length(Bytes));
             Param.AsBlob := RawStr;

          end
          else
          begin
            Param.Value := ConvertedValue.AsVariant;

          end;
        end;
        else
          Param.Value := ConvertedValue.AsVariant;
    end;  // end case ConvertedValue.Kind
  end  // end if Converter <> nil
  else
  begin
    case AValue.Kind of
      tkInteger, tkInt64: 
      begin
        Param.DataType := ftInteger;
        Param.AsInteger := AValue.AsInteger;
      end;
      tkFloat:
      begin
        if AValue.TypeInfo = TypeInfo(TDateTime) then
        begin
          Param.DataType := ftDateTime;
          Param.AsDateTime := AValue.AsType<TDateTime>;
        end
        else if AValue.TypeInfo = TypeInfo(TDate) then
        begin
          Param.DataType := ftDate;
          Param.AsDate := AValue.AsType<TDate>;
        end
        else if AValue.TypeInfo = TypeInfo(TTime) then
        begin
          Param.DataType := ftTime;
          Param.AsTime := AValue.AsType<TTime>;
        end
        else
        begin
          Param.DataType := ftFloat;
          Param.AsFloat := AValue.AsExtended;
        end;
      end;
      tkString, tkUString, tkWString, tkChar, tkWChar:
      begin
        // Check if this string is a GUID and force ftGuid for PostgreSQL compatibility
        if (AValue.Kind in [tkString, tkUString, tkWString]) and
           ((Length(AValue.AsString) = 36) or (Length(AValue.AsString) = 38)) and
           (AValue.AsString.IndexOf('-') > 0) then
        begin
          Param.DataType := ftGuid;
          Param.AsString := AValue.AsString;
        end
        // Use ftWideMemo for large strings to avoid FireDAC size limits (default is 32767 for ftString)
        else if Length(AValue.AsString) > 4000 then
        begin
          Param.DataType := ftWideMemo;
          Param.Size := Length(AValue.AsString);
          Param.AsWideMemo := AValue.AsString;
        end
        else
          Param.AsWideString := AValue.AsString;
      end;
      tkDynArray:
      begin
        // Check for byte arrays by name (TBytes / TArray<Byte> / TArray<System.Byte>)
        var IsByteArray := False;
        if AValue.TypeInfo <> nil then
        begin
          var TypeName := string(AValue.TypeInfo.Name);
          IsByteArray := (TypeName = 'TBytes') or 
                         (TypeName = 'TArray<System.Byte>') or 
                         (TypeName = 'TArray<Byte>');

        end;
        
        if IsByteArray then
        begin
           // Set ftBlob explicitly for PostgreSQL bytea compatibility
           Param.DataType := ftBlob;
           var Bytes := AValue.AsType<TBytes>;
           var RawStr: RawByteString;
           SetLength(RawStr, Length(Bytes));
           if Length(Bytes) > 0 then
             Move(Bytes[0], RawStr[1], Length(Bytes));
           Param.AsBlob := RawStr;

        end
        else
        begin
          Param.Value := AValue.AsVariant;

        end;
      end;
      tkEnumeration:
      begin
        if AValue.TypeInfo = TypeInfo(Boolean) then
        begin
          Param.DataType := ftBoolean;
          Param.AsBoolean := AValue.AsBoolean;
        end
        else
        begin
          Param.DataType := ftInteger;
          Param.AsInteger := AValue.AsOrdinal;
        end;
      end;
      tkRecord:
      begin
        if IsNullable(AValue.TypeInfo) then
        begin
           var Helper := TNullableHelper.Create(AValue.TypeInfo);
           if Helper.HasValue(AValue.GetReferenceToRawData) then
           begin
             var InnerVal := Helper.GetValue(AValue.GetReferenceToRawData);
             SetParamValue(Param, InnerVal);
           end
           else
           begin
             Param.Clear;
             // Try to set type from underlying type
             var Underlying := GetUnderlyingType(AValue.TypeInfo);
             if Underlying <> nil then
             begin
               case Underlying.Kind of
                 tkInteger, tkInt64: Param.DataType := ftInteger;
                 tkFloat: Param.DataType := ftFloat;
                 tkString, tkUString, tkWString: Param.DataType := ftString;
                 tkEnumeration: 
                   if Underlying = TypeInfo(Boolean) then 
                     Param.DataType := ftBoolean
                   else 
                     Param.DataType := ftInteger;
               end;
             end;
           end;
        end
        else
           Param.Value := AValue.AsVariant;
      end;  // end case AValue.Kind
    else
      Param.Value := AValue.AsVariant;
    end;
  end;  // end else (no converter)
end;

procedure TFireDACCommand.ClearParams;
begin
  FQuery.Params.Clear;
end;

procedure TFireDACCommand.Execute;
begin
  ExecuteNonQuery;
end;

function TFireDACCommand.ExecuteNonQuery: Integer;
begin
  if Assigned(FOnLog) then FOnLog(FQuery.SQL.Text);
  FQuery.ExecSQL;
  Result := FQuery.RowsAffected;
end;

function TFireDACCommand.ExecuteQuery: IDbReader;
var
  Q: TFDQuery;
  i: Integer;
  Src, Dest: TFDParam;
begin
  if Assigned(FOnLog) then FOnLog(FQuery.SQL.Text);
  // Create a new Query for the Reader to allow independent iteration
  Q := TFDQuery.Create(nil);
  try
    Q.Connection := FConnection;
    Q.SQL.Text := FQuery.SQL.Text;
    
    // Copy params
    for i := 0 to FQuery.Params.Count - 1 do
    begin
      Src := FQuery.Params[i];
      Dest := Q.Params.FindParam(Src.Name);
      if Dest <> nil then
      begin
        Dest.DataType := Src.DataType;
        Dest.Value := Src.Value;
      end;
    end;
    
    Q.Open;
    Result := TFireDACReader.Create(Q, True); // Reader now owns this new query
  except
    Q.Free;
    raise;
  end;
end;

function TFireDACCommand.ExecuteScalar: TValue;
begin
  if Assigned(FOnLog) then FOnLog(FQuery.SQL.Text);
  FQuery.Open;
  try
    if not FQuery.Eof then
      Result := TValue.FromVariant(FQuery.Fields[0].Value)
    else
      Result := TValue.Empty;
  finally
    FQuery.Close;
  end;
end;

procedure TFireDACCommand.SetSQL(const ASQL: string);
begin
  FQuery.SQL.Text := ASQL;
end;

procedure TFireDACCommand.SetArraySize(const ASize: Integer);
begin
  FQuery.Params.ArraySize := ASize;
end;

procedure TFireDACCommand.SetParamArray(const AName: string; const AValues: TArray<TValue>);
var
  Param: TFDParam;
  i: Integer;
begin
  Param := FQuery.ParamByName(AName);
  for i := 0 to High(AValues) do
  begin
    // Reuse logic similar to SetParamValue but for array index
    var Val := AValues[i];
    
    if Val.IsEmpty then
    begin
      Param.Clear(i);
      if Param.DataType = ftUnknown then Param.DataType := ftString;
    end
    else
    begin
      case Val.Kind of
        tkInteger: 
        begin
          Param.DataType := ftInteger;
          Param.AsIntegers[i] := Val.AsInteger;
        end;
        tkInt64:
        begin
          Param.DataType := ftLargeInt;
          Param.AsLargeInts[i] := Val.AsInt64;
        end;
        tkFloat:
        begin
          if Val.TypeInfo = TypeInfo(TDateTime) then
          begin
            Param.DataType := ftDateTime;
            Param.AsDateTimes[i] := Val.AsType<TDateTime>;
          end
          else if Val.TypeInfo = TypeInfo(TDate) then
          begin
            Param.DataType := ftDate;
            Param.AsDates[i] := Val.AsType<TDate>;
          end
          else if Val.TypeInfo = TypeInfo(TTime) then
          begin
            Param.DataType := ftTime;
            Param.AsTimes[i] := Val.AsType<TTime>;
          end
          else
          begin
            Param.DataType := ftFloat;
            Param.AsFloats[i] := Val.AsExtended;
          end;
        end;
        tkString, tkUString, tkWString, tkChar, tkWChar:
        begin
          if Length(Val.AsString) > 4000 then
          begin
            if Param.DataType <> ftWideMemo then Param.DataType := ftWideMemo;
            if Param.Size < Length(Val.AsString) then Param.Size := Length(Val.AsString);
            Param.AsWideStrings[i] := Val.AsString; // Arrays use AsWideStrings, no AsWideMemos array prop usually
          end
          else
            Param.AsWideStrings[i] := Val.AsString;
        end;
        tkDynArray:
        begin
          if Val.TypeInfo = TypeInfo(TBytes) then
          begin
            var Bytes := Val.AsType<TBytes>;
            var RawStr: RawByteString;
            SetLength(RawStr, Length(Bytes));
            if Length(Bytes) > 0 then
              Move(Bytes[0], RawStr[1], Length(Bytes));
            Param.AsBlobs[i] := RawStr;
          end
          else
            Param.Values[i] := Val.AsVariant;
        end;
        tkEnumeration:
        begin
          if Val.TypeInfo = TypeInfo(Boolean) then
          begin
            Param.DataType := ftBoolean;
            Param.AsBooleans[i] := Val.AsBoolean;
          end
          else
          begin
            Param.DataType := ftInteger;
            Param.AsIntegers[i] := Val.AsOrdinal;
          end;
        end;
        tkRecord:
        begin
          if IsNullable(Val.TypeInfo) then
          begin
             var Helper := TNullableHelper.Create(Val.TypeInfo);
             if Helper.HasValue(Val.GetReferenceToRawData) then
             begin
               var InnerVal := Helper.GetValue(Val.GetReferenceToRawData);
               // Recursive call for inner value? No, just handle it here or duplicate logic.
               // Duplicating logic for simplicity to avoid recursion with index passing
               case InnerVal.Kind of
                 tkInteger:
                   begin
                     Param.DataType := ftInteger;
                     Param.AsIntegers[i] := InnerVal.AsInteger;
                   end;
                 tkInt64:
                   begin
                     Param.DataType := ftLargeInt;
                     Param.AsLargeInts[i] := InnerVal.AsInt64;
                   end;
                 tkFloat:
                  begin
                    if InnerVal.TypeInfo = TypeInfo(TDateTime) then
                    begin
                      Param.DataType := ftDateTime;
                      Param.AsDateTimes[i] := InnerVal.AsType<TDateTime>;
                    end
                    else if InnerVal.TypeInfo = TypeInfo(TDate) then
                    begin
                      Param.DataType := ftDate;
                      Param.AsDates[i] := InnerVal.AsType<TDate>;
                    end
                    else if InnerVal.TypeInfo = TypeInfo(TTime) then
                    begin
                      Param.DataType := ftTime;
                      Param.AsTimes[i] := InnerVal.AsType<TTime>;
                    end
                    else
                    begin
                      Param.DataType := ftFloat;
                      Param.AsFloats[i] := InnerVal.AsExtended;
                    end;
                  end;
                  tkString, tkUString, tkWString:
                  begin
                    // Check for large strings in array param
                    if Length(InnerVal.AsString) > 4000 then
                    begin
                       // Array DML with mixed sizes is tricky. FireDAC usually expects consistent types.
                       // For safety, if we detect large strings, we might want to force the whole column to be WideMemo?
                       // But SetParamArray sets values for one param name (an array of values).
                       // We can set DataType on the Param once.
                       if Param.DataType <> ftWideMemo then Param.DataType := ftWideMemo;
                       if Param.Size < Length(InnerVal.AsString) then Param.Size := Length(InnerVal.AsString);
                    end;
                    Param.AsWideStrings[i] := InnerVal.AsString;
                  end;
                  tkDynArray:
                  begin
                    if InnerVal.TypeInfo = TypeInfo(TBytes) then
                    begin
                      var Bytes := InnerVal.AsType<TBytes>;
                      var RawStr: RawByteString;
                      SetLength(RawStr, Length(Bytes));
                      if Length(Bytes) > 0 then
                        Move(Bytes[0], RawStr[1], Length(Bytes));
                      Param.AsBlobs[i] := RawStr;
                    end
                    else
                      Param.Values[i] := InnerVal.AsVariant;
                  end;
                  tkEnumeration:
                   if InnerVal.TypeInfo = TypeInfo(Boolean) then
                   begin
                     Param.DataType := ftBoolean;
                     Param.AsBooleans[i] := InnerVal.AsBoolean;
                   end
                   else
                   begin
                     Param.DataType := ftInteger;
                     Param.AsIntegers[i] := InnerVal.AsOrdinal;
                   end;
               end;
             end
             else
             begin
               Param.Clear(i);
             end;
          end
          else
             Param.Values[i] := Val.AsVariant;
        end;
      else
        Param.Values[i] := Val.AsVariant;
      end;
    end;
  end;
end;

procedure TFireDACCommand.ExecuteBatch(const ATimes: Integer; const AOffset: Integer);
begin
  FQuery.Execute(ATimes, AOffset);
end;

{ TFireDACConnection }

constructor TFireDACConnection.Create(AConnection: TFDConnection; AOwnsConnection: Boolean);
begin
  inherited Create;
  FConnection := AConnection;
  FOwnsConnection := AOwnsConnection;
end;

destructor TFireDACConnection.Destroy;
begin
  if FOwnsConnection then
    FConnection.Free;
  inherited;
end;

function TFireDACConnection.BeginTransaction: IDbTransaction;
begin
  Result := TFireDACTransaction.Create(FConnection);
end;

procedure TFireDACConnection.Connect;
begin
  FConnection.Connected := True;
end;

function TFireDACConnection.CreateCommand(const ASQL: string): IDbCommand;
var
  LCmd: TFireDACCommand;
begin
  LCmd := TFireDACCommand.Create(FConnection, GetDialect);
  LCmd.FOnLog := FOnLog;
  if ASQL <> '' then
    LCmd.SetSQL(ASQL);
  Result := LCmd;
end;

function TFireDACConnection.GetLastInsertId: Variant;
begin
  Result := FConnection.GetLastAutoGenValue('');
end;

procedure TFireDACConnection.Disconnect;
begin
  FConnection.Connected := False;
end;

function TFireDACConnection.IsConnected: Boolean;
begin
  Result := FConnection.Connected;
end;

procedure TFireDACConnection.SetOnLog(AValue: TProc<string>);
begin
  FOnLog := AValue;
end;

function TFireDACConnection.GetOnLog: TProc<string>;
begin
  Result := FOnLog;
end;

function TFireDACConnection.TableExists(const ATableName: string): Boolean;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    try
      // Get list of tables
      // Note: We use empty catalog/schema to search in current context
      FConnection.GetTableNames('', '', '', List, [osMy], [tkTable], True);
      
      // Check for existence
      // 1. Exact match
      if List.IndexOf(ATableName) >= 0 then
        Exit(True);
        
      // 2. Quoted match (if ATableName is not quoted but DB returns quoted)
      if List.IndexOf('"' + ATableName + '"') >= 0 then
        Exit(True);
        
      // 3. Unquoted match (if ATableName is quoted but DB returns unquoted)
      if List.IndexOf(ATableName.Replace('"', '')) >= 0 then
        Exit(True);
        
      // 4. Case insensitive match (fallback)
      for var Table in List do
      begin
        if SameText(Table, ATableName) or 
           SameText(Table, '"' + ATableName + '"') or
           SameText(Table, ATableName.Replace('"', '')) then
          Exit(True);
      end;
      
      Result := False;
    except
      Result := False; // If metadata query fails, assume false or handle error?
    end;
  finally
    List.Free;
  end;
end;

function TFireDACConnection.GetConnectionString: string;
begin
  Result := FConnection.ConnectionString;
end;

procedure TFireDACConnection.SetConnectionString(const AValue: string);
begin
  if FConnection.Connected then
    FConnection.Connected := False;
  FConnection.ConnectionString := AValue;
end;

procedure TFireDACConnection.DetectDialect;
var
  DriverID: string;
begin
  if FDialect <> ddUnknown then Exit;

  DriverID := FConnection.DriverName.ToLower;
  
  FDialect := TDialectFactory.DetectDialect(DriverID);
end;

function TFireDACConnection.GetDialect: TDatabaseDialect;
begin
  if FDialect = ddUnknown then
    DetectDialect;
  Result := FDialect;
end;

end.

