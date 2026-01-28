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
{  Created: 2025-12-19                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.Json.Utf8.Serializer;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  Dext.Core.Span,
  Dext.Json.Utf8,
  Dext.Core.DateUtils,
  Dext.Types.UUID;

type
  EUtf8SerializationException = class(Exception);

  TUtf8JsonSerializer = record
  private
    class procedure DeserializeRecord(var AReader: TUtf8JsonReader; AType: PTypeInfo; AInstance: Pointer); static;
    class procedure DeserializeField(var AReader: TUtf8JsonReader; Field: TRttiField; Instance: Pointer); static;
  public
    class function Deserialize<T>(const AData: TByteSpan): T; static;
  end;

implementation

{ TUtf8JsonSerializer }

class function TUtf8JsonSerializer.Deserialize<T>(const AData: TByteSpan): T;
var
  Reader: TUtf8JsonReader;
  TypeInfo: PTypeInfo;
begin
  Reader := TUtf8JsonReader.Create(AData);
  TypeInfo := System.TypeInfo(T);

  // Initial Read to get to the start
  if not Reader.Read then
    Exit(Default(T)); // Or error?

  if TypeInfo.Kind = tkRecord then
  begin
    DeserializeRecord(Reader, TypeInfo, @Result);
  end
  else
    raise EUtf8SerializationException.Create('Only Records are supported for zero-allocation deserialization currently.');
end;

class procedure TUtf8JsonSerializer.DeserializeRecord(var AReader: TUtf8JsonReader; AType: PTypeInfo; AInstance: Pointer);
var
  Ctx: TRttiContext;
  RType: TRttiType;
  Field: TRttiField;
  PropName: string;
begin
  if AReader.TokenType <> TJsonTokenType.StartObject then
   // It might be that we haven't consumed start object yet if recursion?
   // Or the caller consumed it.
   // Let's assume Reader is AT StartObject.
   if AReader.TokenType <> TJsonTokenType.StartObject then
     raise EUtf8SerializationException.Create('Expected StartObject');

  Ctx := TRttiContext.Create;
  try
    RType := Ctx.GetType(AType);
    
    while AReader.Read do
    begin
      if AReader.TokenType = TJsonTokenType.EndObject then
        Break;

      if AReader.TokenType = TJsonTokenType.PropertyName then
      begin
        // We have the property name in AReader.ValueSpan (raw bytes)
        // Optimization: Match bytes directly against field names?
        // For now, simple approach: Convert to string (allocation!) to look up field.
        // TODO: Optimize this with a Byte-Map or cached field names as bytes.
        
        PropName := AReader.GetString; // Allocating string for search
        Field := RType.GetField(PropName);
        
        // Advance to Value
        if not AReader.Read then
          raise EUtf8SerializationException.Create('Unexpected end of JSON while reading value');
            
        if Assigned(Field) then
        begin
          DeserializeField(AReader, Field, AInstance);
        end
        else
        begin
          // Unknown field, skip value
          AReader.Skip;
        end;
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

class procedure TUtf8JsonSerializer.DeserializeField(var AReader: TUtf8JsonReader; Field: TRttiField; Instance: Pointer);
begin
  case Field.FieldType.TypeKind of
    tkInteger:
      Field.SetValue(Instance, TValue.From<Integer>(AReader.GetInt32));
      
    tkInt64:
      Field.SetValue(Instance, TValue.From<Int64>(AReader.GetInt64));
      
    tkFloat:
      if (Field.FieldType.Handle = TypeInfo(TDateTime)) or 
         (Field.FieldType.Handle = TypeInfo(TDate)) or 
         (Field.FieldType.Handle = TypeInfo(TTime)) then
      begin
         var DateStr := AReader.GetString;
         var Dt: TDateTime;
         if TryParseCommonDate(DateStr, Dt) then
           Field.SetValue(Instance, TValue.From<TDateTime>(Dt))
         else
           Field.SetValue(Instance, TValue.From<TDateTime>(0));
      end
      else
        Field.SetValue(Instance, TValue.From<Double>(AReader.GetDouble));
      
    tkString, tkLString, tkWString, tkUString:
      Field.SetValue(Instance, TValue.From<string>(AReader.GetString));
      
    tkEnumeration:
      if Field.FieldType.Handle = TypeInfo(Boolean) then
        Field.SetValue(Instance, TValue.From<Boolean>(AReader.GetBoolean))
      else
        // TODO: Enum support
        AReader.Skip;
        
    tkRecord:
      begin
        // Special handling for TGUID and TUUID
        if Field.FieldType.Handle = TypeInfo(TGUID) then
        begin
          var GuidStr := AReader.GetString;
          var G: TGUID;
          
          if GuidStr.Trim = '' then
            G := TGUID.Empty
          else if GuidStr.StartsWith('{') and GuidStr.EndsWith('}') then
            G := StringToGUID(GuidStr)
          else if GuidStr.Length = 36 then
            G := StringToGUID('{' + GuidStr + '}')
          else
            G := StringToGUID(GuidStr);
            
          Field.SetValue(Instance, TValue.From<TGUID>(G));
        end
        else if Field.FieldType.Handle = TypeInfo(TUUID) then
        begin
          var GuidStr := AReader.GetString;
          var U: TUUID;
          
          if GuidStr.Trim = '' then
            U := TUUID.Null
          else
            U := TUUID.FromString(GuidStr);  // Handles all formats (with/without braces, hyphens)
            
          var Val: TValue;
          TValue.Make(@U, TypeInfo(TUUID), Val);
          Field.SetValue(Instance, Val);
        end
        else
        begin
          // Other nested records - skip for now
          // TODO: Support nested record deserialization
          AReader.Skip;
        end;
      end;
      
    else
      AReader.Skip;
  end;
end;

end.
