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
    FStack: TList<PTypeInfo>;
    class constructor Create;
    class destructor Destroy;
    class function CreatePrototype(ATypeInfo: PTypeInfo): TObject; static;
    class function Entity(ATypeInfo: PTypeInfo): TObject; overload; static;
  public
    /// <summary>
    ///   Returns a cached prototype entity for query building.
    ///   Creates and caches the prototype on first call for each type.
    /// </summary>
    class function Entity<T>: T; overload; static;
    
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
  FStack := TList<PTypeInfo>.Create;
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
  FStack.Free;
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
  InstancePtr: Pointer;
  ColumnName: string;
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
        // 1. Inject IPropInfo (Metadata for SQL generation)
        if PropMap.FieldOffset <> -1 then
        begin
          ColumnName := PropMap.ColumnName;
          if ColumnName = '' then ColumnName := PropMap.PropertyName;

          PropInfo := TPropInfo.Create(PropMap.ColumnName, PropMap.PropertyName);
          IPropInfo(PPointer(NativeInt(InstancePtr) + PropMap.FieldOffset)^) := PropInfo;
        end;

        // 2. Inject Sub-Prototypes (Recursive Drill-down Support)
        if (PropMap.FieldValueOffset <> -1) and (PropMap.PropertyType <> nil) and 
           (PropMap.PropertyType.Kind = tkClass) then
        begin
           // We use the non-generic Entity call which handles the cache and recursion
           PPointer(NativeInt(InstancePtr) + PropMap.FieldValueOffset)^ := Entity(PropMap.PropertyType);
        end;
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

class function Prototype.Entity<T>: T;
var
  Obj: TObject;
begin
  Obj := Entity(TypeInfo(T));
  Result := T(Pointer(@Obj)^);
end;

class function Prototype.Entity(ATypeInfo: PTypeInfo): TObject;
begin
  if FCache.TryGetValue(ATypeInfo, Result) then
    Exit;

  // Recursion Guard
  if FStack.Contains(ATypeInfo) then
    Exit(nil);

  FStack.Add(ATypeInfo);
  try
    Result := CreatePrototype(ATypeInfo);
    FCache.Add(ATypeInfo, Result);
  finally
    FStack.Remove(ATypeInfo);
  end;
end;

end.
