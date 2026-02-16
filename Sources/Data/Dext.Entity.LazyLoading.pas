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
unit Dext.Entity.LazyLoading;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  Dext.Collections,
  Dext.Entity.Core,
  Dext.Entity.Attributes,
  Dext.Entity.Mapping,
  Dext.Entity.Drivers.Interfaces,
  Dext.Types.Lazy,
  Dext.Types.Nullable,
  Dext.Specifications.Types,
  Dext.Specifications.Interfaces,
  Dext.Entity.Collections,
  Dext.Core.Activator;

type
  /// <summary>
  ///   Injects Lazy<T> logic into entities.
  /// </summary>
  TLazyInjector = class
  private
    class procedure InjectField(AContext: IDbContext; AEntity: TObject; AField: TRttiField);
  public
    class procedure Inject(AContext: IDbContext; AEntity: TObject);
  end;

  /// <summary>
  ///   Standard implementation of ILazy for Entity Framework.
  ///   Replaces the old TVirtualInterface-based approach.
  /// </summary>
  TLazyLoader = class(TInterfacedObject, ILazy)
  private
    FContextPtr: Pointer;
    FEntity: TObject;
    FPropName: string;
    FLoaded: Boolean;
    FValue: TValue;
    FIsCollection: Boolean;
    FOwnsValue: Boolean; // Added flag
    
    function GetDbContext: IDbContext;
    procedure LoadValue;
    procedure LoadManyToMany(Prop: TRttiProperty; const PropMap: TPropertyMap; const Ctx: TRttiContext);
    
    // ILazy implementation
    function GetIsValueCreated: Boolean;
    function GetValue: TValue;
  public
    constructor Create(AContext: IDbContext; AEntity: TObject; const APropName: string; AIsCollection: Boolean; const AExistingValue: TValue);
    destructor Destroy; override;
  end;

implementation

{ Helper Functions }

/// <summary>
///   Unwraps Nullable<T> values and validates if FK is valid (non-zero for integers, non-empty for strings)
/// </summary>
function TryUnwrapAndValidateFK(var AValue: TValue; AContext: TRttiContext): Boolean;
var
  Helper: TNullableHelper;
  Instance: Pointer;
begin
  Result := False;
  
  // Handle Nullable<T> unwrapping
  if IsNullable(AValue.TypeInfo) then
  begin
      Helper := TNullableHelper.Create(AValue.TypeInfo);
      Instance := AValue.GetReferenceToRawData;
      
      if not Helper.HasValue(Instance) then
      begin
        // Null value is valid (conceptually) but means "no FK to traverse"
        // Return False to skip loading
        Exit; 
      end;
        
      // Get the underlying value
      AValue := Helper.GetValue(Instance);
  end;

  if AValue.IsEmpty then Exit;

  // Validate based on type
  if AValue.Kind in [tkInteger, tkInt64] then
    Result := AValue.AsInt64 <> 0
  else if AValue.Kind in [tkString, tkUString, tkWString, tkLString] then
    Result := AValue.AsString <> ''
  else
    Result := True; // For other types like GUID, assume valid if not empty
end;


{ TLazyInjector }

class procedure TLazyInjector.Inject(AContext: IDbContext; AEntity: TObject);
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Field: TRttiField;
  Map: TEntityMap;
  PropMap: TPropertyMap;
begin
  Map := TEntityMap(AContext.GetMapping(AEntity.ClassInfo));
  if Map = nil then Exit;

  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(AEntity.ClassType);
    
    // 1. Handle Explicit Lazy<T> (Attributes or Implicit)
    for Field in Typ.GetFields do
    begin
      if Field.FieldType.Name.StartsWith('Lazy<') then
      begin
        InjectField(AContext, AEntity, Field);
      end;
    end;
    
    // 2. Handle Fluent Mapping IsLazy (For future Auto-Proxies)
    for PropMap in Map.Properties.Values do
    begin
      if PropMap.IsLazy and not PropMap.PropertyName.StartsWith('Lazy<') then
      begin
        // If it's a class but not Lazy<T>, it's a candidate for Auto-Proxying
        // For now, we only support Lazy<T> because Auto-Proxies require TClassProxy at instantiation time.
        // But we can check if it's already a Proxy.
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

class procedure TLazyInjector.InjectField(AContext: IDbContext; AEntity: TObject; AField: TRttiField);
var
  LazyRecordType: TRttiRecordType;
  InstanceField: TRttiField;
  PropName: string;
  IsCollection: Boolean;
  Loader: TLazyLoader;
  LazyIntf: ILazy;
  LazyVal: TValue;
  IntfVal: TValue;
  ExistingInstance: TValue;
  ExistingValue: TValue;
  LazyInst: ILazy;
begin
  // 1. Determine Property Name from Field Name (FAddress -> Address)
  PropName := AField.Name;
  if PropName.StartsWith('F') then
    Delete(PropName, 1, 1);
    
  // 2. Determine if Collection
  IsCollection := False;
  if AField.FieldType.Name.Contains('TList<') or 
     AField.FieldType.Name.Contains('TObjectList<') or
     AField.FieldType.Name.Contains('IList<') then
    IsCollection := True;

  // 3. Get Lazy<T> record structure
  LazyRecordType := AField.FieldType.AsRecord;
  InstanceField := LazyRecordType.GetField('FInstance');
  if InstanceField = nil then 
    Exit;
  
  // 4. Capture Existing Value (e.g. List created in constructor)
  ExistingValue := TValue.Empty;
  
  LazyVal := AField.GetValue(AEntity);
  ExistingInstance := InstanceField.GetValue(LazyVal.GetReferenceToRawData);
  
  if not ExistingInstance.IsEmpty then
  begin
    if ExistingInstance.Kind = tkInterface then
    begin
      // Extract the ILazy interface from the record's FInstance
      if ExistingInstance.TryAsType<ILazy>(LazyInst) and (LazyInst <> nil) then
      begin
         // If it is value created (like TValueLazy from constructor), get the value
         if LazyInst.IsValueCreated then
         begin
           try
             ExistingValue := LazyInst.Value;
           except
             // Ignore errors extracting value
           end;
         end;
      end;
    end;
  end;

  // 5. Create Loader passing existing value
  Loader := TLazyLoader.Create(AContext, AEntity, PropName, IsCollection, ExistingValue);
  LazyIntf := Loader;

  // 6. Assign interface to Lazy<T>.FInstance
  // Create TValue with ILazy type
  TValue.Make(@LazyIntf, TypeInfo(ILazy), IntfVal);
  
  // Set FInstance on the record - replaces existing one
  InstanceField.SetValue(LazyVal.GetReferenceToRawData, IntfVal);
  
  // Set the record back to the entity
  AField.SetValue(AEntity, LazyVal);
end;

{ TLazyLoader }

constructor TLazyLoader.Create(AContext: IDbContext; AEntity: TObject; const APropName: string; AIsCollection: Boolean; const AExistingValue: TValue);
begin
  inherited Create;
  FContextPtr := Pointer(AContext);
  FEntity := AEntity;
  FPropName := APropName;
  FIsCollection := AIsCollection;
  FLoaded := False;
  FValue := AExistingValue; // Store existing list if any
  FOwnsValue := False; // Default no ownership unless we create a non-refcounted object
end;

destructor TLazyLoader.Destroy;
begin
  if FIsCollection and FLoaded and (not FValue.IsEmpty) and FOwnsValue then
  begin
    // If it's a collection and we explicitly own it (e.g. TObjectList)
    if FValue.Kind = tkClass then
      FValue.AsObject.Free;
  end;
  inherited;
end;

function TLazyLoader.GetDbContext: IDbContext;
begin
  Result := IDbContext(FContextPtr);
end;

function TLazyLoader.GetIsValueCreated: Boolean;
begin
  Result := FLoaded;
end;

function TLazyLoader.GetValue: TValue;
begin
  if not FLoaded then
    LoadValue;

  Result := FValue;
end;

procedure TLazyLoader.LoadValue;
var
  Ctx: TRttiContext;
  Prop: TRttiProperty;
  FKName: string;
  Attr: TCustomAttribute;
  TypeName: string;
  StartPos, EndPos: Integer;
  ItemTypeName: string;
  ItemType: TRttiType;
  ChildSet: IDbSet;
  ParentName: string;
  FKPropName: string;
  P: TRttiProperty;
  PKVal: string;
  PropHelper: TPropExpression;
  Expr: IExpression;
  ResList: IList<TObject>;
  ListObj: TObject;
  AddMethod: TRttiMethod;
  Obj: TObject;
  FKProp: TRttiProperty;
  FKVal: TValue;
  TargetType: PTypeInfo;
  TargetSet: IDbSet;
  LoadedObj: TObject;
  IntVal: Integer;
  ExpectedClass: TClass;
  UseExistingInterface: Boolean;
  Map: TEntityMap;
  PropMap: TPropertyMap;
begin
  if FLoaded then Exit;
  
  Ctx := TRttiContext.Create;
  try
    try
      if FIsCollection then
    begin
        // Load Collection
        Prop := Ctx.GetType(FEntity.ClassType).GetProperty(FPropName);
        if Prop = nil then Exit;
        
        // Check if this is a Many-to-Many relationship
        Map := TEntityMap(GetDbContext.GetMapping(FEntity.ClassInfo));
        if (Map <> nil) and Map.Properties.TryGetValue(FPropName, PropMap) then
        begin
          if (PropMap.Relationship = rtManyToMany) and (PropMap.JoinTableName <> '') then
          begin
            LoadManyToMany(Prop, PropMap, Ctx);
            FLoaded := True;
            Exit;
          end;
        end;
        
        // Standard One-to-Many loading
        FKName := '';
        for Attr in Prop.GetAttributes do
          if Attr is ForeignKeyAttribute then
          begin
            FKName := ForeignKeyAttribute(Attr).ColumnName;
            Break;
          end;
        
        TypeName := Prop.PropertyType.Name;
        StartPos := Pos('<', TypeName);
        EndPos := Pos('>', TypeName);
        
        if (StartPos > 0) and (EndPos > StartPos) then
        begin
            ItemTypeName := Copy(TypeName, StartPos + 1, EndPos - StartPos - 1);
            ItemType := Ctx.FindType(ItemTypeName);
            
            if ItemType <> nil then
            begin
                ChildSet := GetDbContext.DataSet(ItemType.Handle);
                
                ParentName := FEntity.ClassName;
                if ParentName.StartsWith('T') then Delete(ParentName, 1, 1);
                
                FKPropName := ParentName + 'Id'; 
                
                P := ItemType.GetProperty(FKPropName);
                if P <> nil then
                begin
                    PKVal := GetDbContext.DataSet(FEntity.ClassInfo).GetEntityId(FEntity);
                    
                    PropHelper := TPropExpression.Create(FKPropName);
                    
                    // Try to convert PK to Integer if possible, as most FKs are ints
                    if TryStrToInt(PKVal, IntVal) then
                       Expr := PropHelper = IntVal
                    else
                       Expr := PropHelper = PKVal;
                       
                    ResList := ChildSet.ListObjects(Expr);
                    try
                      // Try to use existing list stored in FValue (passed from constructor)
                      UseExistingInterface := False;
                      ListObj := nil;
                      
                      if not FValue.IsEmpty then
                      begin
                        if FValue.Kind = tkInterface then
                        begin
                           // Existing IList<T>
                           UseExistingInterface := True;
                           // Note: We don't need to extract the interface pointer, invoke works on TValue wrapping interface
                        end
                        else if FValue.IsObject then
                        begin
                           // Existing TObjectList
                           ListObj := FValue.AsObject;
                        end;
                      end;
                      
                // Create TTrackingList via Factory
                if ListObj = nil then
                begin
                   ListObj := TTrackingListFactory.CreateList(ItemType.Handle, GetDbContext, FEntity, FPropName);
                end;

                if ListObj = nil then
                  Exit;
                      
                      // Get Add method
                      if UseExistingInterface then
                        AddMethod := Prop.PropertyType.GetMethod('Add')
                      else
                        AddMethod := Ctx.GetType(ListObj.ClassType).GetMethod('Add');
                        
                      if AddMethod = nil then
                        Exit;
                      
                      // Populate the list
                      for Obj in ResList do
                      begin
                           if Obj = nil then 
                             Continue;
                           
                           try
                            // Check if object is instance of expected type
                            if ItemType.AsInstance <> nil then
                            begin
                              ExpectedClass := ItemType.AsInstance.MetaclassType;
                              if not (Obj is ExpectedClass) then
                                Continue;
                            end;
                            
                            if UseExistingInterface then
                               AddMethod.Invoke(FValue, [Obj])
                            else
                               AddMethod.Invoke(ListObj, [Obj]);
                               
                          except
                            // Ignore errors adding individual items
                          end;
                      end;
                      
                      if not UseExistingInterface then
                        FValue := TValue.From(ListObj);
                        
                    finally
                      ResList := nil; // Auto-managed
                    end;
                end;
            end;
        end;
    end
  else
    begin
        // Load Reference
        FKPropName := FPropName + 'Id';
        FKProp := Ctx.GetType(FEntity.ClassType).GetProperty(FKPropName);
        
        if FKProp <> nil then
        begin
            FKVal := FKProp.GetValue(FEntity);
            
            // Unwrap Nullable<T> and validate FK value
            if TryUnwrapAndValidateFK(FKVal, Ctx) then
            begin
                Prop := Ctx.GetType(FEntity.ClassType).GetProperty(FPropName);
                TypeName := Prop.PropertyType.Name;
                
                // Extract inner type from Lazy<T>
                if TypeName.StartsWith('Lazy<') then
                begin
                  StartPos := Pos('<', TypeName);
                  EndPos := Pos('>', TypeName);
                  if (StartPos > 0) and (EndPos > StartPos) then
                  begin
                    ItemTypeName := Copy(TypeName, StartPos + 1, EndPos - StartPos - 1);
                    ItemType := Ctx.FindType(ItemTypeName);
                    if ItemType <> nil then
                      TargetType := ItemType.Handle
                    else
                      TargetType := nil;
                  end
                  else
                    TargetType := nil;
                end
                else
                  TargetType := Prop.PropertyType.Handle;
                
                if TargetType <> nil then
                begin
                  TargetSet := GetDbContext.DataSet(TargetType);
                  LoadedObj := TargetSet.FindObject(FKVal.AsVariant);
                  
                  if LoadedObj <> nil then
                       FValue := TValue.From(LoadedObj);
                end;
            end;
        end;
    end;
    
    FLoaded := True;
  except
    on E: Exception do
    begin
      // Suppress loading errors for now
      FLoaded := True;
    end;
  end;
finally
  Ctx.Free;
end;
end;

procedure TLazyLoader.LoadManyToMany(Prop: TRttiProperty; const PropMap: TPropertyMap; const Ctx: TRttiContext);
var
  SQL: string;
  SB: TStringBuilder;
  Cmd: IDbCommand;
  Reader: IDbReader;
  EntityId: string;
  RelatedIds: TList<TValue>;
  ItemTypeName: string;
  ItemType: TRttiType;
  TypeName: string;
  StartPos, EndPos: Integer;
  RelatedDbSet: IDbSet;
  ResList: IList<TObject>;
  IdValues: TArray<Variant>;
  Expr: IExpression;
  PropHelper: TPropExpression;
  ListObj: TObject;
  AddMethod: TRttiMethod;
  Obj: TObject;
  UseExistingInterface: Boolean;
  ListType: TRttiType;
begin
  // Get entity's primary key value
  EntityId := GetDbContext.DataSet(FEntity.ClassInfo).GetEntityId(FEntity);
  if EntityId = '' then Exit;
  
  // Build SQL to query join table
  SB := TStringBuilder.Create;
  try
    SB.Append('SELECT ');
    SB.Append(GetDbContext.Dialect.QuoteIdentifier(PropMap.RightKeyColumn));
    SB.Append(' FROM ');
    SB.Append(GetDbContext.Dialect.QuoteIdentifier(PropMap.JoinTableName));
    SB.Append(' WHERE ');
    SB.Append(GetDbContext.Dialect.QuoteIdentifier(PropMap.LeftKeyColumn));
    SB.Append(' = :p1');
    SQL := SB.ToString;
  finally
    SB.Free;
  end;
  
  // Unwrap Lazy<T> to get the actual List type name
  TypeName := Prop.PropertyType.Name;
  if TypeName.Contains('Lazy<') then
  begin
    StartPos := Pos('Lazy<', TypeName);
    if StartPos > 0 then
       TypeName := Copy(TypeName, StartPos + 5, Length(TypeName) - StartPos - 5);
  end;

  // Execute query
  RelatedIds := TList<TValue>.Create;
  try
    Cmd := GetDbContext.Connection.CreateCommand(SQL);
    Cmd.AddParam('p1', TValue.From<string>(EntityId));
    Reader := Cmd.ExecuteQuery;
    
    while Reader.Next do
      RelatedIds.Add(Reader.GetValue(0));
      
    // Get target item type from collection's generic argument
    StartPos := Pos('<', TypeName);
    EndPos := Pos('>', TypeName);
    
    if (StartPos > 0) and (EndPos > StartPos) then
    begin
        // Extract inner type name e.g. 'Unit.TItem' from 'IList<Unit.TItem>'
        ItemTypeName := Copy(TypeName, StartPos + 1, EndPos - StartPos - 1);
        ItemType := Ctx.FindType(ItemTypeName);
    end
    else
        ItemType := nil;

    if RelatedIds.Count = 0 then
    begin
      // No related items, ensure empty collection
      if FValue.IsEmpty then
      begin
        if TypeName.Contains('IList<') then
        begin
          ListObj := nil;
          if ItemType <> nil then
          begin
             // Try to find generic TSmartList<ItemType>
             var ListTypeName := 'Dext.Collections.TSmartList<' + ItemType.QualifiedName + '>';
             var SmartListType := Ctx.FindType(ListTypeName);
             if SmartListType <> nil then
               ListObj := TActivator.CreateInstance(SmartListType.AsInstance.MetaclassType, [False]);
          end;
          
          if ListObj = nil then
             ListObj := TSmartList<TObject>.Create(False);
             
          FValue := TValue.From(ListObj);
        end;
      end;
      Exit;
    end;
    
    if ItemType = nil then Exit;
    
    // Load related objects
    RelatedDbSet := GetDbContext.DataSet(ItemType.Handle);
    
    SetLength(IdValues, RelatedIds.Count);
    for var i := 0 to RelatedIds.Count - 1 do
      IdValues[i] := RelatedIds[i].AsVariant;
    
    PropHelper := TPropExpression.Create('Id');
    Expr := PropHelper.&In(IdValues);
    ResList := RelatedDbSet.ListObjects(Expr);
    
    // Populate collection
    UseExistingInterface := False;
    ListObj := nil;
    
    if not FValue.IsEmpty then
    begin
      if FValue.Kind = tkInterface then
        UseExistingInterface := True
      else if FValue.IsObject then
        ListObj := FValue.AsObject;
    end;
    
    if not UseExistingInterface and (ListObj = nil) then
    begin
      // Create TTrackingList via Factory
      ListObj := TTrackingListFactory.CreateList(ItemType.Handle, GetDbContext, FEntity, FPropName);
      
      if ListObj = nil then Exit;
      if not UseExistingInterface then
        FValue := TValue.From(ListObj);
    end;
    
    // Get Add method
    if UseExistingInterface then
      AddMethod := Prop.PropertyType.GetMethod('Add') // Careful: Prop.PropertyType is Lazy<T>, not IList<T>
      // Wait! Prop.PropertyType IS Lazy<T>.
      // We need Method on the INTERFACE type.
      // But RTTI for Interface property on Lazy Record? No.
      // We need RTTI for the List Type.
    else
      AddMethod := Ctx.GetType(ListObj.ClassType).GetMethod('Add');

    if UseExistingInterface then
    begin
       // If usage existing interface (FValue), we need 'Add' method of that interface.
       // We can get it from the TypeName we extracted?
       ListType := Ctx.FindType(TypeName);
       if ListType <> nil then
         AddMethod := ListType.GetMethod('Add');
    end; 
      
    if AddMethod = nil then Exit;
    
    // Add items to collection
    for Obj in ResList do
    begin
      if Obj = nil then Continue;
      try
        if UseExistingInterface then
          AddMethod.Invoke(FValue, [Obj])
        else
          AddMethod.Invoke(ListObj, [Obj]);
      except
        // Ignore errors adding items
      end;
    end;
    
    if not UseExistingInterface then
      FValue := TValue.From(ListObj);
  finally
    RelatedIds.Free;
  end;
end;

end.

