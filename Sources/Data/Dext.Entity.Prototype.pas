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
{  Created: 2025-12-27                                                      }
{                                                                           }
{  Prototype - Creates "ghost entities" for query building.                 }
{                                                                           }
{  Prototypes are cached per type for performance (avoids RTTI overhead).   }
{  Since prototypes are readonly (only metadata), caching is safe.          }
{                                                                           }
{  Usage:                                                                   }
{    var u := Prototype.Entity<TSmartPerson>;                               }
{    List := SetPerson.Where(u.Age = 30).ToList;                            }
{                                                                           }
{***************************************************************************}
unit Dext.Entity.Prototype;

interface

uses
  System.Character,
  System.Generics.Collections,
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  Dext.Core.SmartTypes;

type
  /// <summary>
  ///   Factory class for creating and caching prototype entities.
  ///   Prototypes are cached per type for performance.
  /// </summary>
  Prototype = class
  private class var
    FCache: TDictionary<PTypeInfo, TObject>;
    class constructor Create;
    class destructor Destroy;
    class function CreatePrototype(ATypeInfo: PTypeInfo): TObject; static;
  public
    /// <summary>
    ///   Returns a cached prototype entity for query building.
    ///   Creates and caches the prototype on first call for each type.
    /// </summary>
    class function Entity<T>: T; static;
    
    /// <summary>
    ///   Clears the prototype cache. Useful for testing or hot-reload scenarios.
    /// </summary>
    class procedure ClearCache; static;
  end;

  // Backward compatibility alias
  Build = Prototype;

implementation

uses
  Dext.Entity.Core,
  Dext.Entity.Mapping;

{ Prototype }

class constructor Prototype.Create;
begin
  FCache := TDictionary<PTypeInfo, TObject>.Create;
end;

class destructor Prototype.Destroy;
var
  Obj: TObject;
begin
  if FCache <> nil then
  begin
    for Obj in FCache.Values do
      Obj.Free;
    FCache.Free;
  end;
end;

class procedure Prototype.ClearCache;
var
  Obj: TObject;
begin
  for Obj in FCache.Values do
    Obj.Free;
  FCache.Clear;
end;

class function Prototype.CreatePrototype(ATypeInfo: PTypeInfo): TObject;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  PropInfo: IPropInfo;
  MetaVal: TValue;
  InstancePtr: Pointer;
  PropertyName, ColumnName: string;
  EntityMap: TEntityMap;
  PropMap: TPropertyMap;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(ATypeInfo);
    if (Typ = nil) or (Typ.TypeKind <> tkClass) then
      raise Exception.Create('Prototype.Entity<T> only supports class types.');

    // Create Instance - Prefer default constructor if available
    Result := Typ.AsInstance.MetaclassType.Create;
    InstancePtr := Result;

    EntityMap := TModelBuilder.Instance.GetMap(ATypeInfo);
    if EntityMap <> nil then
    begin
      for PropMap in EntityMap.Properties.Values do
      begin
        // If we have a cached offset, we can set it directly without RTTI for fields
        if PropMap.FieldOffset <> -1 then
        begin
          ColumnName := PropMap.ColumnName;
          if ColumnName = '' then ColumnName := PropMap.PropertyName;

          PropInfo := TPropInfo.Create(ColumnName);
          TValue.Make(@PropInfo, TypeInfo(IPropInfo), MetaVal);

          // Direct memory copy (Fast!)
          MetaVal.ExtractRawData(Pointer(NativeInt(InstancePtr) + PropMap.FieldOffset));
        end;
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

class function Prototype.Entity<T>: T;
var
  TypeInfoPtr: PTypeInfo;
  Obj: TObject;
begin
  TypeInfoPtr := TypeInfo(T);
  
  if PTypeInfo(TypeInfoPtr).Kind <> tkClass then
    raise Exception.Create('Prototype.Entity<T> only supports class types.');
  
  if FCache.TryGetValue(TypeInfoPtr, Obj) then
  begin
    // Fast path: direct cast
    Result := T(Pointer(@Obj)^); 
    Exit;
  end;
  
  Obj := CreatePrototype(TypeInfoPtr);
  FCache.Add(TypeInfoPtr, Obj);
  Result := T(Pointer(@Obj)^);
end;

end.
