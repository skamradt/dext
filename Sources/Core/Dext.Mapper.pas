{***************************************************************************}
{                                                                           }
{           Dext Framework - AutoMapper                                     }
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
{  Created: 2025-12-24                                                      }
{                                                                           }
{  Description:                                                             }
{    AutoMapper for DTO <-> Entity mapping using RTTI.                     }
{    Supports custom member mappings, ignoring properties, and collections.}
{                                                                           }
{***************************************************************************}
unit Dext.Mapper;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections;

type
  /// <summary>
  ///   Function type for custom member mapping.
  /// </summary>
  TMemberMapFunc<TSource, TDest> = reference to function(const Source: TSource): TValue;

  /// <summary>
  ///   Configuration for mapping a specific member.
  /// </summary>
  TMemberMapping = class
  private
    FDestMemberName: string;
    FMapFunc: TFunc<TObject, TValue>;
    FIgnore: Boolean;
  public
    property DestMemberName: string read FDestMemberName write FDestMemberName;
    property MapFunc: TFunc<TObject, TValue> read FMapFunc write FMapFunc;
    property Ignore: Boolean read FIgnore write FIgnore;
  end;

  /// <summary>
  ///   Configuration for mapping between two types.
  /// </summary>
  TTypeMapConfig<TSource: class; TDest: class> = class
  private
    FMemberMappings: TObjectDictionary<string, TMemberMapping>;
  public
    constructor Create;
    destructor Destroy; override;
    
    /// <summary>
    ///   Configure custom mapping for a destination member.
    /// </summary>
    function ForMember(const DestMemberName: string; 
      MapFunc: TMemberMapFunc<TSource, TDest>): TTypeMapConfig<TSource, TDest>;
    
    /// <summary>
    ///   Ignore a destination member during mapping.
    /// </summary>
    function Ignore(const DestMemberName: string): TTypeMapConfig<TSource, TDest>;
    
    property MemberMappings: TObjectDictionary<string, TMemberMapping> read FMemberMappings;
  end;

  /// <summary>
  ///   Global mapper registry and mapping executor.
  /// </summary>
  TMapper = class
  private
    class var FConfigurations: TDictionary<string, TObject>;
    class function GetConfigKey(SourceType, DestType: PTypeInfo): string;
    class constructor Create;
    class destructor Destroy;
  public
    /// <summary>
    ///   Create a mapping configuration between two types.
    /// </summary>
    class function CreateMap<TSource: class; TDest: class>: TTypeMapConfig<TSource, TDest>;
    
    /// <summary>
    ///   Map a source object to a new destination object.
    /// </summary>
    class function Map<TSource: class; TDest: class>(const Source: TSource): TDest; overload;

    /// <summary>
    ///   Map a source object to an existing destination object.
    /// </summary>
    class procedure Map<TSource: class; TDest: class>(const Source: TSource; var Dest: TDest); overload;
    
    /// <summary>
    ///   Map a list of source objects to a list of destination objects.
    /// </summary>
    class function MapList<TSource: class; TDest: class>(const SourceList: TEnumerable<TSource>): TList<TDest>;

    /// <summary>
    ///   Copies values from Source to Destination properties/fields only if the source value is not default.
    ///   Source (S) can be a record or class. Destination (D) must be a class.
    /// </summary>
    class procedure Patch<S: record; D: class>(const ASource: S; var ADestination: D);
  end;

implementation

uses
  Dext.Core.Activator;

{ TMemberMapping }

{ TTypeMapConfig<TSource, TDest> }

constructor TTypeMapConfig<TSource, TDest>.Create;
begin
  FMemberMappings := TObjectDictionary<string, TMemberMapping>.Create([doOwnsValues]);
end;

destructor TTypeMapConfig<TSource, TDest>.Destroy;
begin
  FMemberMappings.Free;
  inherited;
end;

function TTypeMapConfig<TSource, TDest>.ForMember(const DestMemberName: string;
  MapFunc: TMemberMapFunc<TSource, TDest>): TTypeMapConfig<TSource, TDest>;
var
  Mapping: TMemberMapping;
begin
  Mapping := TMemberMapping.Create;
  Mapping.DestMemberName := DestMemberName;
  Mapping.MapFunc := function(Obj: TObject): TValue
    begin
      if Obj is TSource then
        Result := MapFunc(Obj as TSource)
      else
        raise Exception.Create('Invalid source type for mapping');
    end;
  FMemberMappings.AddOrSetValue(DestMemberName, Mapping);
  Result := Self;
end;

function TTypeMapConfig<TSource, TDest>.Ignore(const DestMemberName: string): TTypeMapConfig<TSource, TDest>;
var
  Mapping: TMemberMapping;
begin
  Mapping := TMemberMapping.Create;
  Mapping.DestMemberName := DestMemberName;
  Mapping.Ignore := True;
  FMemberMappings.AddOrSetValue(DestMemberName, Mapping);
  Result := Self;
end;

{ TMapper }

class constructor TMapper.Create;
begin
  FConfigurations := TDictionary<string, TObject>.Create;
end;

class destructor TMapper.Destroy;
var
  Config: TObject;
begin
  for Config in FConfigurations.Values do
    Config.Free;
  FConfigurations.Free;
end;

class function TMapper.GetConfigKey(SourceType, DestType: PTypeInfo): string;
begin
  Result := string(SourceType.Name) + '->' + string(DestType.Name);
end;

class function TMapper.CreateMap<TSource, TDest>: TTypeMapConfig<TSource, TDest>;
var
  Key: string;
  Config: TTypeMapConfig<TSource, TDest>;
begin
  Key := GetConfigKey(TypeInfo(TSource), TypeInfo(TDest));
  
  if FConfigurations.ContainsKey(Key) then
    Result := TTypeMapConfig<TSource, TDest>(FConfigurations[Key])
  else
  begin
    Config := TTypeMapConfig<TSource, TDest>.Create;
    FConfigurations.Add(Key, Config);
    Result := Config;
  end;
end;

class function TMapper.Map<TSource, TDest>(const Source: TSource): TDest;
var
  Dest: TDest;
begin
  Dest := TActivator.CreateInstance<TDest>;
  Map<TSource, TDest>(Source, Dest);
  Result := Dest;
end;

class procedure TMapper.Map<TSource, TDest>(const Source: TSource; var Dest: TDest);
var
  Ctx: TRttiContext;
  SourceType, DestType: TRttiType;
  SourceProp, DestProp: TRttiProperty;
  ConfigKey: string;
  Config: TTypeMapConfig<TSource, TDest>;
  Mapping: TMemberMapping;
  Value: TValue;
  HasConfig: Boolean;
begin
  Config := nil;
  if Source = nil then
    Exit;
    
  Ctx := TRttiContext.Create;
  try
    SourceType := Ctx.GetType(TypeInfo(TSource));
    DestType := Ctx.GetType(TypeInfo(TDest));
    
    // Check if there's a custom configuration
    ConfigKey := GetConfigKey(TypeInfo(TSource), TypeInfo(TDest));
    var ConfigObj: TObject;
    HasConfig := FConfigurations.TryGetValue(ConfigKey, ConfigObj);
    if HasConfig then
      Config := ConfigObj as TTypeMapConfig<TSource, TDest>;
    
    // Map each destination property
    for DestProp in DestType.GetProperties do
    begin
      if not DestProp.IsWritable then
        Continue;
        
      // Check if property is ignored
      if HasConfig and (Config <> nil) and Config.MemberMappings.TryGetValue(DestProp.Name, Mapping) then
      begin
        if Mapping.Ignore then
          Continue;
          
        // Use custom mapping function
        if Assigned(Mapping.MapFunc) then
        begin
          Value := Mapping.MapFunc(Source);
          DestProp.SetValue(Pointer(Dest), Value);
          Continue;
        end;
      end;
      
      // Default: map by property name
      SourceProp := SourceType.GetProperty(DestProp.Name);
      if (SourceProp <> nil) and SourceProp.IsReadable then
      begin
        Value := SourceProp.GetValue(Pointer(Source));
        
        // Handle type conversion if needed
        if Value.TypeInfo <> DestProp.PropertyType.Handle then
        begin
          // Try implicit conversion
          try
            Value := Value.Cast(DestProp.PropertyType.Handle);
          except
            // Skip if conversion fails
            Continue;
          end;
        end;
        
        DestProp.SetValue(Pointer(Dest), Value);
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

class function TMapper.MapList<TSource, TDest>(const SourceList: TEnumerable<TSource>): TList<TDest>;
var
  Item: TSource;
begin
  Result := TList<TDest>.Create;
  try
    for Item in SourceList do
      Result.Add(Map<TSource, TDest>(Item));
  except
    Result.Free;
    raise;
  end;
end;

class procedure TMapper.Patch<S, D>(const ASource: S; var ADestination: D);
var
  Context: TRttiContext;
  SrcType, DstType: TRttiType;
  SrcProp, DstProp: TRttiProperty;
  SrcField, DstField: TRttiField;
  SrcValue: TValue;
  IsDefault: Boolean;
begin
  if not Assigned(ADestination) then
    Exit;

  Context := TRttiContext.Create;
  try
    SrcType := Context.GetType(TypeInfo(S));
    DstType := Context.GetType(ADestination.ClassType);

    // Iterate through Destination Properties
    for DstProp in DstType.GetProperties do
    begin
      if not DstProp.IsWritable then
        Continue;

      // Find match in Source Properties
      SrcProp := SrcType.GetProperty(DstProp.Name);
      if Assigned(SrcProp) then
      begin
        SrcValue := SrcProp.GetValue(@ASource);
      end
      else
      begin
        // If not a property, try Source Fields
        SrcField := SrcType.GetField(DstProp.Name);
        if Assigned(SrcField) then
          SrcValue := SrcField.GetValue(@ASource)
        else
          Continue;
      end;

      // Check for default values (0, '', 0.0, etc.)
      IsDefault := False;
      case SrcValue.Kind of
        tkInteger, tkInt64: IsDefault := SrcValue.AsOrdinal = 0;
        tkFloat: IsDefault := SrcValue.AsExtended = 0;
        tkUString, tkString, tkWString, tkLString: IsDefault := SrcValue.AsString = '';
        tkEnumeration:
          if SrcValue.TypeInfo = TypeInfo(Boolean) then
            IsDefault := False // Boolean defaults are usually meaningful (False), so we don't treat them as "empty" for patch unless specific logic required
          else
            IsDefault := SrcValue.AsOrdinal = 0;
      end;

      if not IsDefault then
        DstProp.SetValue(Pointer(ADestination), SrcValue);
    end;

    // Also try to match Destination Fields if they are not exposed via properties (less common for entities)
    for DstField in DstType.GetFields do
    begin
      // Skip if already handled by property (many properties wrap fields with same name starting with F)
      var PropName := DstField.Name;
      if PropName.StartsWith('F', True) then
        PropName := PropName.Substring(1);

      if DstType.GetProperty(PropName) <> nil then
        Continue;

      SrcProp := SrcType.GetProperty(PropName);
      if Assigned(SrcProp) then
        SrcValue := SrcProp.GetValue(@ASource)
      else
      begin
        SrcField := SrcType.GetField(PropName);
        if Assigned(SrcField) then
          SrcValue := SrcField.GetValue(@ASource)
        else
          Continue;
      end;

      // Repeat default check
      IsDefault := False;
      case SrcValue.Kind of
        tkInteger, tkInt64: IsDefault := SrcValue.AsOrdinal = 0;
        tkFloat: IsDefault := SrcValue.AsExtended = 0;
        tkUString, tkString, tkWString, tkLString: IsDefault := SrcValue.AsString = '';
      end;

      if not IsDefault then
        DstField.SetValue(Pointer(ADestination), SrcValue);
    end;

  finally
    Context.Free;
  end;
end;

end.
