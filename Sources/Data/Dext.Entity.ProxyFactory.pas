unit Dext.Entity.ProxyFactory;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  System.Generics.Collections,
  System.Classes,
  Dext.Core.Activator,
  Dext.Core.Reflection,
  Dext.Entity.Core,
  Dext.Entity.DbSet,
  Dext.Entity.Dialects,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Mapping,
  Dext.Entity.TypeConverters,
  Dext.Interception,
  Dext.Interception.ClassProxy,
  Dext.Interception.Proxy;

type
  /// <summary>
  ///   Interceptor to handle Lazy Loading for Auto-Proxies.
  /// </summary>
  TLazyProxyInterceptor = class(TInterfacedObject, IInterceptor)
  private
    FContext: IDbContext;
    FPropName: string;
    FLoaded: Boolean;
    FValue: TValue;
  public
    constructor Create(AContext: IDbContext; const APropName: string);
    procedure Intercept(const Invocation: IInvocation);
  end;

  /// <summary>
  ///   Factory to create proxied entities for Auto-Lazy loading.
  /// </summary>
  TEntityProxyFactory = class
  public
    class function CreateInstance<T: class>(AContext: IDbContext): T; static;
    class function NeedsProxy(AParam: PTypeInfo; AContext: IDbContext): Boolean; static;
  end;

implementation

{ TLazyProxyInterceptor }

constructor TLazyProxyInterceptor.Create(AContext: IDbContext; const APropName: string);
begin
  inherited Create;
  FContext := AContext;
  FPropName := APropName;
  FLoaded := False;
end;

procedure TLazyProxyInterceptor.Intercept(const Invocation: IInvocation);
var
  Ctx: TRttiContext;
  Prop, FKProp: TRttiProperty;
  Map: TEntityMap;
  PropMap, PMap: TPropertyMap;
  FKVal: TValue;
  TargetSet: IDbSet;
  LoadedObj: TObject;
  FKName, PKCol, PKVal: string;
  Instance: TObject;
  PropField: TRttiField;
  DBVal, ExistingVal: TValue;
  Dialect: ISQLDialect;
  SQL: string;
  Cmd: IDbCommand;
begin
  if SameText(Invocation.Method.Name, 'Get' + FPropName) or 
     SameText(Invocation.Method.Name, FPropName) then
  begin
    if not FLoaded then
    begin
      Instance := Invocation.Target.AsObject;
      if Instance = nil then Exit;

      Ctx := TRttiContext.Create;
      try
        Map := TEntityMap(FContext.GetMapping(Instance.ClassInfo));
        if (Map <> nil) and Map.Properties.TryGetValue(FPropName, PropMap) then
        begin
          var RType := Ctx.GetType(Map.EntityType);
          Prop := RType.GetProperty(FPropName);
          
          PropField := RType.GetField(PropMap.FieldName);
          if PropField = nil then PropField := RType.GetField('F' + FPropName);

          if PropMap.IsNavigation then
          begin
            FKName := PropMap.ForeignKeyColumn;
            if FKName = '' then FKName := FPropName + 'Id';
            
            FKProp := RType.GetProperty(FKName);
            if FKProp <> nil then
            begin
              FKVal := FKProp.GetValue(Instance);
              if not FKVal.IsEmpty then
              begin
                TargetSet := FContext.DataSet(Prop.PropertyType.Handle);
                LoadedObj := TargetSet.FindObject(FKVal.AsVariant);
                if LoadedObj <> nil then
                  FValue := TValue.From(LoadedObj);
              end;
            end;
          end
          else
          begin
            TargetSet := FContext.DataSet(Map.EntityType);
            if TargetSet <> nil then
            begin
              PKVal := TargetSet.GetEntityId(Instance);
              if PKVal <> '' then
              begin
                Dialect := FContext.Dialect;
                PKCol := '';
                for PMap in Map.Properties.Values do
                  if PMap.IsPK then
                  begin
                    PKCol := PMap.ColumnName;
                    if PKCol = '' then PKCol := PMap.PropertyName;
                    Break;
                  end;

                if PKCol <> '' then
                begin
                  SQL := Format('SELECT %s FROM %s WHERE %s = :p1', 
                    [Dialect.QuoteIdentifier(PropMap.ColumnName), 
                     Dialect.QuoteIdentifier(Map.TableName),
                     Dialect.QuoteIdentifier(PKCol)]);
                      
                  Cmd := FContext.Connection.CreateCommand(SQL);
                  Cmd.AddParam('p1', PKVal);
                  DBVal := Cmd.ExecuteScalar;
                  
                  ExistingVal := TValue.Empty;
                  if PropField <> nil then 
                    ExistingVal := PropField.GetValue(Instance)
                  else
                    ExistingVal := Prop.GetValue(Instance);

                  if not DBVal.IsEmpty then
                  begin
                  if ExistingVal.IsObject and (ExistingVal.AsObject is TStrings) then
                  begin
                    TStrings(ExistingVal.AsObject).Text := DBVal.ToString;
                    FValue := ExistingVal;
                  end
                  else if Prop.PropertyType.IsInstance then
                  begin
                    // Use TActivator to create a concrete instance (e.g. TStrings -> TStringList)
                    var NewObj := TActivator.CreateInstance(Prop.PropertyType.AsInstance.MetaclassType, []);
                    if NewObj is TStrings then
                      TStrings(NewObj).Text := DBVal.ToString;
                    
                    FValue := NewObj;
                    
                    if PropField <> nil then
                      PropField.SetValue(Instance, FValue)
                    else
                      TReflection.SetValue(Pointer(Instance), Prop, FValue);
                  end
                  else
                  begin
                    if PropMap.Converter <> nil then
                      FValue := PropMap.Converter.FromDatabase(DBVal, Prop.PropertyType.Handle)
                    else
                      FValue := DBVal;
                    
                    if PropField <> nil then
                      PropField.SetValue(Instance, FValue)
                    else
                      TReflection.SetValue(Pointer(Instance), Prop, FValue);
                  end;
                  end
                  else
                  begin
                    if ExistingVal.IsObject and (ExistingVal.AsObject is TStrings) then
                      FValue := ExistingVal
                    else
                      FValue := TValue.Empty;
                  end;
                end;
              end;
            end;
          end;
        end;
      finally
        Ctx.Free;
      end;
      FLoaded := True;
    end;
    Invocation.Result := FValue;
  end
  else
    Invocation.Proceed;
end;

{ TEntityProxyFactory }

class function TEntityProxyFactory.NeedsProxy(AParam: PTypeInfo; AContext: IDbContext): Boolean;
var
  Map: TEntityMap;
  Prop: TPropertyMap;
begin
  Result := False;
  Map := TEntityMap(AContext.GetMapping(AParam));
  if Map = nil then Exit;
  
  for Prop in Map.Properties.Values do
  begin
    if Prop.IsLazy then
      Exit(True);
  end;
end;

class function TEntityProxyFactory.CreateInstance<T>(AContext: IDbContext): T;
var
  Interceptors: TList<IInterceptor>;
  Map: TEntityMap;
  Prop: TPropertyMap;
  Proxy: TClassProxy;
begin
  if not NeedsProxy(TypeInfo(T), AContext) then
    Exit(T(TActivator.CreateInstance(TClass(T), [])));

  Map := TEntityMap(AContext.GetMapping(TypeInfo(T)));
  Interceptors := TList<IInterceptor>.Create;
  try
    for Prop in Map.Properties.Values do
    begin
      if Prop.IsLazy then
        Interceptors.Add(TLazyProxyInterceptor.Create(AContext, Prop.PropertyName));
    end;
    
    Proxy := TClassProxy.Create(TClass(T), Interceptors.ToArray, True);
    AContext.TrackProxy(Proxy);
    Result := T(Proxy.Instance);
  finally
    Interceptors.Free;
  end;
end;

end.
