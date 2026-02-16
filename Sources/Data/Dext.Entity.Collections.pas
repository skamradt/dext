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
unit Dext.Entity.Collections;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  Dext.Collections,
  Dext.Entity.Core;

type
  /// <summary>
  ///   Specialized list that tracks additions and removals to automatically 
  ///   synchronize database relationships (One-to-Many and Many-to-Many).
  /// </summary>
  TTrackingList<T: class> = class(TSmartList<T>)
  private
    FContext: IDbContext;
    FOwner: TObject;
    FPropertyName: string;
    FSuppressTracking: Boolean;
    
    procedure SetInverseProperty(const AItem: T);
    procedure ClearInverseProperty(const AItem: T);
  protected
    procedure Notify(Sender: TObject; const Item: T; Action: TCollectionNotification); override;
  public
    constructor Create(const AContext: IDbContext; const AOwner: TObject; const APropertyName: string);
    
    // Allows loading data without triggering tracking events
    procedure Load(const AItems: IEnumerable<T>);
  end;

  TTrackingListFactory = class
  public
    class function CreateList(AItemType: PTypeInfo; const AContext: IDbContext; const AOwner: TObject; const APropertyName: string): TObject; static;
  end;

implementation

uses
  Dext.Entity.Mapping,
  Dext.Entity.Context; // To access TDbContext internals if needed, or stick to IDbContext

{ TTrackingList<T> }

constructor TTrackingList<T>.Create(const AContext: IDbContext; const AOwner: TObject; const APropertyName: string);
begin
  // Tracking lists don't own objects; the DbContext/IdentityMap does.
  inherited Create(False); 
  FContext := AContext;
  FOwner := AOwner;
  FPropertyName := APropertyName;
  FSuppressTracking := False;
end;

procedure TTrackingList<T>.Load(const AItems: IEnumerable<T>);
begin
  FSuppressTracking := True;
  try
    Self.Clear;
    Self.AddRange(AItems);
  finally
    FSuppressTracking := False;
  end;
end;

procedure TTrackingList<T>.Notify(Sender: TObject; const Item: T; Action: TCollectionNotification);
var
  Map: TEntityMap;
  Prop: TPropertyMap;
begin
  inherited; // Handles standard list logic
  
  if FSuppressTracking or (FContext = nil) or (FOwner = nil) then Exit;

  // 1. Resolve mapping for the owner entity
  Map := TEntityMap(FContext.GetMapping(FOwner.ClassInfo));
  if (Map = nil) or (not Map.Properties.TryGetValue(FPropertyName, Prop)) then Exit;

  case Action of
    cnAdded:
    begin
       // Many-to-Many: Register the link in the context/DbSet
       if Prop.Relationship = rtManyToMany then
         FContext.DataSet(FOwner.ClassInfo).LinkManyToMany(FOwner, FPropertyName, Item)
       
       // One-to-Many: Ensure item is tracked and set the back-reference (InverseProperty)
       else if Prop.Relationship = rtOneToMany then
       begin
         FContext.DataSet(Item.ClassInfo).Add(Item);
         SetInverseProperty(Item);
       end;
    end;
    
    cnRemoved:
    begin
       // Many-to-Many: Unlink
       if Prop.Relationship = rtManyToMany then
         FContext.DataSet(FOwner.ClassInfo).UnlinkManyToMany(FOwner, FPropertyName, Item)
         
       // One-to-Many: Clear back-reference
       else if Prop.Relationship = rtOneToMany then
         ClearInverseProperty(Item);
    end;
  end;
end;

procedure TTrackingList<T>.SetInverseProperty(const AItem: T);
var
  ParentMap: TEntityMap;
  PropMap: TPropertyMap;
  Ctx: TRttiContext;
  Typ: TRttiType;
  InvProp: TRttiProperty;
begin
  ParentMap := TEntityMap(FContext.GetMapping(FOwner.ClassInfo));
  if (ParentMap = nil) or (not ParentMap.Properties.TryGetValue(FPropertyName, PropMap)) then Exit;
  if PropMap.InverseProperty = '' then Exit;

  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(AItem.ClassType);
    InvProp := Typ.GetProperty(PropMap.InverseProperty);
    if InvProp <> nil then
    begin
       // Use TReflection to handle Prop<T> or standard property
       TReflection.SetValue(AItem, InvProp, FOwner);
    end;
  finally
    Ctx.Free;
  end;
end;

procedure TTrackingList<T>.ClearInverseProperty(const AItem: T);
var
  ParentMap: TEntityMap;
  PropMap: TPropertyMap;
  Ctx: TRttiContext;
  Typ: TRttiType;
  InvProp: TRttiProperty;
begin
  ParentMap := TEntityMap(FContext.GetMapping(FOwner.ClassInfo));
  if (ParentMap = nil) or (not ParentMap.Properties.TryGetValue(FPropertyName, PropMap)) then Exit;
  if PropMap.InverseProperty = '' then Exit;

  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(AItem.ClassType);
    InvProp := Typ.GetProperty(PropMap.InverseProperty);
    if InvProp <> nil then
       TReflection.SetValue(AItem, InvProp, nil);
  finally
    Ctx.Free;
  end;
end;

{ TTrackingListFactory }

class function TTrackingListFactory.CreateList(AItemType: PTypeInfo; const AContext: IDbContext; const AOwner: TObject;
  const APropertyName: string): TObject;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  ListTypName: string;
  ListTyp: TRttiType;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(AItemType);
    // Generic name for TTrackingList<T>
    ListTypName := 'Dext.Entity.Collections.TTrackingList<' + Typ.QualifiedName + '>';
    ListTyp := Ctx.FindType(ListTypName);
    
    if ListTyp = nil then
      raise Exception.CreateFmt('Could not find RTTI for %s. Make sure the unit is used.', [ListTypName]);

    Result := TActivator.CreateInstance(ListTyp.AsInstance.MetaclassType, [TValue.From<IDbContext>(AContext), AOwner, APropertyName]);
  finally
    Ctx.Free;
  end;
end;

end.
