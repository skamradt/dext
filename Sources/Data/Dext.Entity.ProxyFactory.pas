unit Dext.Entity.ProxyFactory;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  Dext.Entity.Core,
  Dext.Entity.Mapping,
  Dext.Interception,
  Dext.Interception.ClassProxy;

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
    procedure Intercept(Invocation: IInvocation);
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

procedure TLazyProxyInterceptor.Intercept(Invocation: IInvocation);
var
  Ctx: TRttiContext;
  Prop: TRttiProperty;
  Map: TEntityMap;
  PropMap: TPropertyMap;
  FKProp: TRttiProperty;
  FKVal: TValue;
  TargetSet: IDbSet;
  LoadedObj: TObject;
begin
  // We are looking for the getter (e.g., GetProfile or the property name itself if it's a method)
  if SameText(Invocation.Method.Name, 'Get' + FPropName) or 
     SameText(Invocation.Method.Name, FPropName) then
  begin
    if not FLoaded then
    begin
       Ctx := TRttiContext.Create;
       try
         Map := TEntityMap(FContext.GetMapping(Invocation.Instance.ClassInfo));
         if (Map <> nil) and Map.Properties.TryGetValue(FPropName, PropMap) then
         begin
            // 1. Determine FK Value from the instance
            // Assuming convention PropName + 'Id' or looking at PropMap.ForeignKeyColumn
            var FKName := PropMap.ForeignKeyColumn;
            if FKName = '' then FKName := FPropName + 'Id';
            
            FKProp := Ctx.GetType(Invocation.Instance.ClassType).GetProperty(FKName);
            if FKProp <> nil then
            begin
               FKVal := FKProp.GetValue(Invocation.Instance);
               if not FKVal.IsEmpty then
               begin
                  // 2. Load from DbSet
                  Prop := Ctx.GetType(Invocation.Instance.ClassType).GetProperty(FPropName);
                  TargetSet := FContext.DataSet(Prop.PropertyType.Handle);
                  LoadedObj := TargetSet.FindObject(FKVal.AsVariant);
                  if LoadedObj <> nil then
                    FValue := TValue.From(LoadedObj);
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
    Exit(T(TClass(T).Create));

  Map := TEntityMap(AContext.GetMapping(TypeInfo(T)));
  Interceptors := TList<IInterceptor>.Create;
  try
    for Prop in Map.Properties.Values do
    begin
      if Prop.IsLazy then
        Interceptors.Add(TLazyProxyInterceptor.Create(AContext, Prop.PropertyName));
    end;
    
    Proxy := TClassProxy.Create(TClass(T), Interceptors.ToArray, True);
    // Note: In a real implementation, we need to manage the Proxy lifecycle
    Result := T(Proxy.Instance);
  finally
    Interceptors.Free;
  end;
end;

end.
