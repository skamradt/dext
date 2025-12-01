unit Dext.Entity.LazyLoading;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  Dext.Entity.Core,
  Dext.Entity.Attributes,
  Dext.Types.Lazy,
  Dext.Specifications.Types,
  Dext.Specifications.Interfaces,
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
  ///   Handles the invocation of ILazy<T> methods (GetValue, GetIsValueCreated).
  /// </summary>
  TLazyInvokeHandler = class(TInterfacedObject)
  private
    FContext: IDbContext;
    FEntity: TObject;
    FPropName: string;
    FLoaded: Boolean;
    FValue: TValue;
    FIsCollection: Boolean;
    
    procedure Invoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
  public
    constructor Create(AContext: IDbContext; AEntity: TObject; const APropName: string; AIsCollection: Boolean);
  end;

  // Deprecated: Old interceptor-based loader
  TLazyLoader = class
  public
    constructor Create(AContext: IDbContext; AEntity: TObject);
  end;

implementation

{ TLazyLoader }

constructor TLazyLoader.Create(AContext: IDbContext; AEntity: TObject);
begin
  // No-op
end;

{ TLazyInjector }

class procedure TLazyInjector.Inject(AContext: IDbContext; AEntity: TObject);
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Field: TRttiField;
begin
  Ctx := TRttiContext.Create;
  Typ := Ctx.GetType(AEntity.ClassType);
  
  for Field in Typ.GetFields do
  begin
    if Field.FieldType.Name.StartsWith('Lazy<') then
    begin
      InjectField(AContext, AEntity, Field);
    end;
  end;
end;

class procedure TLazyInjector.InjectField(AContext: IDbContext; AEntity: TObject; AField: TRttiField);
var
  LazyRecordType: TRttiRecordType;
  InstanceField: TRttiField;
  InterfaceType: TRttiInterfaceType;
  PropName: string;
  IsCollection: Boolean;
  Handler: IInterface; // Keep reference to avoid leak
  VI: TVirtualInterface;
  RecordPtr: Pointer;
begin
  // 1. Determine Property Name from Field Name (FAddress -> Address)
  PropName := AField.Name;
  if PropName.StartsWith('F') then
    Delete(PropName, 1, 1);
    
  // 2. Determine if Collection
  IsCollection := False;
  if AField.FieldType.Name.Contains('TList<') or AField.FieldType.Name.Contains('TObjectList<') then
    IsCollection := True;

  // 3. Create Handler and hold it in interface variable
  Handler := TLazyInvokeHandler.Create(AContext, AEntity, PropName, IsCollection);
  
  // 4. Create Virtual Interface for ILazy<T>
  LazyRecordType := AField.FieldType.AsRecord;
  InstanceField := LazyRecordType.GetField('FInstance');
  if InstanceField = nil then 
    Exit;
  
  if not (InstanceField.FieldType is TRttiInterfaceType) then
    Exit;
  
  InterfaceType := InstanceField.FieldType as TRttiInterfaceType;
  
  // Create TVirtualInterface capturing the Handler interface
  VI := TVirtualInterface.Create(InterfaceType.Handle, 
    procedure(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue)
    begin
      (Handler as TLazyInvokeHandler).Invoke(Method, Args, Result);
    end);
  
  // 5. Set the interface into the record's FInstance field
  RecordPtr := PByte(AEntity) + AField.Offset;
  
  var Intf: IInterface;
  if not Supports(VI, InterfaceType.GUID, Intf) then
    Exit;
  
  // CRITICAL FIX: Don't use TValue.Make with InterfaceType.Handle!
  // That tries to create a TValue of the INTERFACE type, which is wrong.
  // Instead, we need to set the FInstance field directly.
  // The FInstance field is at offset 0 in the Lazy<T> record.
  
  // Set the interface pointer directly
  PPointer(RecordPtr)^ := Pointer(Intf);
  
  // Increment reference count since we're storing it
  if Intf <> nil then
    Intf._AddRef;
end;

{ TLazyInvokeHandler }

constructor TLazyInvokeHandler.Create(AContext: IDbContext; AEntity: TObject; const APropName: string; AIsCollection: Boolean);
begin
  inherited Create;
  FContext := AContext;
  FEntity := AEntity;
  FPropName := APropName;
  FIsCollection := AIsCollection;
  FLoaded := False;
end;

procedure TLazyInvokeHandler.Invoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
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
  PropHelper: TProperty;
  Expr: IExpression;
  ResList: TList<TObject>;
  ListObj: TObject;
  AddMethod: TRttiMethod;
  Obj: TObject;
  FKProp: TRttiProperty;
  FKVal: TValue;
  TargetType: PTypeInfo;
  TargetSet: IDbSet;
  LoadedObj: TObject;
begin
  if Method.Name = 'GetIsValueCreated' then
  begin
    Result := FLoaded;
    Exit;
  end;
  
  if (Method.Name = 'GetValue') or (Method.Name = 'GetValueT') then
  begin
    if not FLoaded then
    begin
      try
        Ctx := TRttiContext.Create;
        
        if FIsCollection then
        begin
            // Load Collection
            Prop := Ctx.GetType(FEntity.ClassType).GetProperty(FPropName);
            FKName := '';
            if Prop <> nil then
            begin
               for Attr in Prop.GetAttributes do
                 if Attr is ForeignKeyAttribute then
                 begin
                   FKName := ForeignKeyAttribute(Attr).ColumnName;
                   Break;
                 end;
            end;
            
            TypeName := Method.ReturnType.Name;
            StartPos := Pos('<', TypeName);
            EndPos := Pos('>', TypeName);
            
            if (StartPos > 0) and (EndPos > StartPos) then
            begin
                ItemTypeName := Copy(TypeName, StartPos + 1, EndPos - StartPos - 1);
                ItemType := Ctx.FindType(ItemTypeName);
                
                if ItemType <> nil then
                begin
                    ChildSet := FContext.DataSet(ItemType.Handle);
                    
                    ParentName := FEntity.ClassName;
                    if ParentName.StartsWith('T') then Delete(ParentName, 1, 1);
                    
                    FKPropName := ParentName + 'Id'; 
                    
                    P := ItemType.GetProperty(FKPropName);
                    if P <> nil then
                    begin
                        PKVal := FContext.DataSet(FEntity.ClassInfo).GetEntityId(FEntity);
                        
                        PropHelper := TProperty.Create(FKPropName);
                        
                        // Try to convert PK to Integer if possible, as most FKs are ints
                        var IntVal: Integer;
                        if TryStrToInt(PKVal, IntVal) then
                           Expr := PropHelper = IntVal
                        else
                           Expr := PropHelper = PKVal;
                           
                        ResList := ChildSet.ListObjects(Expr);
                        try
                          ListObj := TActivator.CreateInstance(Method.ReturnType.AsInstance.MetaclassType, []);
                          if ListObj = nil then
                            Exit;
                            
                          AddMethod := Method.ReturnType.GetMethod('Add');
                          if AddMethod = nil then
                            Exit;
                          
                          for Obj in ResList do
                          begin
                               if Obj = nil then 
                                 Continue;
                               
                               try
                                // Check if object is instance of expected type
                                if ItemType.AsInstance <> nil then
                                begin
                                  var ExpectedClass := ItemType.AsInstance.MetaclassType;
                                  if not (Obj is ExpectedClass) then
                                    Continue;
                                end;
                                
                                AddMethod.Invoke(ListObj, [Obj]);
                              except
                                // Ignore errors adding individual items
                              end;
                          end;
                          
                          FValue := TValue.From(ListObj); // Correctly wrap the object
                        finally
                          ResList.Free;
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
                if not FKVal.IsEmpty and (FKVal.AsInteger > 0) then
                begin
                    Prop := Ctx.GetType(FEntity.ClassType).GetProperty(FPropName);
                    TargetType := Prop.PropertyType.Handle;
                    
                    TargetSet := FContext.DataSet(TargetType);
                    LoadedObj := TargetSet.FindObject(FKVal.AsVariant);
                    
                    FValue := TValue.From(LoadedObj);
                end;
            end;
        end;
        
        FLoaded := True;
      except
        // If loading fails, we just mark as loaded to avoid infinite loops/retries
        // and let the value be default (nil/empty)
        FLoaded := True;
      end;
    end;
    
    Result := FValue;
  end;
end;

end.
