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
unit Dext.Web.ControllerScanner;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  Dext.Web.Routing.Attributes,
  Dext.DI.Interfaces,
  Dext.Filters,
  Dext.Web.Interfaces,
  Dext.OpenAPI.Attributes;

type
  TControllerMethod = record
    Method: TRttiMethod;
    RouteAttribute: DextRouteAttribute;
    Path: string;
    HttpMethod: string;
  end;

  TControllerInfo = record
    RttiType: TRttiType;
    Methods: TArray<TControllerMethod>;
    ControllerAttribute: DextControllerAttribute;
  end;

  TCachedMethod = record
    TypeName: string;
    MethodName: string;
    IsClass: Boolean;
    FullPath: string;
    HttpMethod: string;
    RequiresAuth: Boolean;
  end;

  IControllerScanner = interface
    function FindControllers: TArray<TControllerInfo>;
    procedure RegisterServices(Services: IServiceCollection);
    function RegisterRoutes(AppBuilder: IApplicationBuilder): Integer;
  end;

  TControllerScanner = class(TInterfacedObject, IControllerScanner)
  private
    FCtx: TRttiContext;
    FCachedMethods: TList<TCachedMethod>;
    procedure ExecuteCachedMethod(Context: IHttpContext; const CachedMethod: TCachedMethod);
    function CreateHandler(const AMethod: TCachedMethod): TRequestDelegate;
  public
    constructor Create;
    destructor Destroy; override;
    function FindControllers: TArray<TControllerInfo>;
    procedure RegisterServices(Services: IServiceCollection);
    function RegisterRoutes(AppBuilder: IApplicationBuilder): Integer;
  end;

implementation

uses
  Dext.Auth.Attributes,
  Dext.Web.ModelBinding,
  Dext.Web.HandlerInvoker,
  Dext.Utils;

{ TControllerScanner }

constructor TControllerScanner.Create;
begin
  inherited Create;
  FCtx := TRttiContext.Create;
  FCachedMethods := TList<TCachedMethod>.Create;
end;

function TControllerScanner.FindControllers: TArray<TControllerInfo>;
var
  Types: TArray<TRttiType>;
  RttiType: TRttiType;
  ControllerInfo: TControllerInfo;
  Controllers: TList<TControllerInfo>;
  Method: TRttiMethod;
  MethodInfo: TControllerMethod;
  Attr: TCustomAttribute;
begin
  Controllers := TList<TControllerInfo>.Create;
  try
    Types := FCtx.GetTypes;

    SafeWriteLn('🔍 ' + Format('Scanning %d types...', [Length(Types)]));

    for RttiType in Types do
    begin
      // ✅ FILTRAR: Records ou Classes
      if (RttiType.TypeKind in [tkRecord, tkClass]) then
      begin
        // Verificar se tem métodos com atributos de rota
        var HasRouteMethods := False;
        var MethodsList: TList<TControllerMethod> := TList<TControllerMethod>.Create;

        try
          var Methods := RttiType.GetMethods;

          for Method in Methods do
          begin
            // ✅ APENAS MÉTODOS ESTÁTICOS (para records) ou PÚBLICOS (para classes)
            if (RttiType.TypeKind = tkRecord) and (not Method.IsStatic) then
              Continue;

            // Para classes, aceitamos métodos de instância
            if (RttiType.TypeKind = tkClass) and (Method.Visibility <> mvPublic) and (Method.Visibility <> mvPublished) then
               Continue;

            var Attributes := Method.GetAttributes;

            // ✅ PROCURAR ATRIBUTOS [DextGet], [DextPost], etc.
            for Attr in Attributes do
            begin
              if Attr is DextRouteAttribute then
              begin
                MethodInfo.Method := Method;
                MethodInfo.RouteAttribute := DextRouteAttribute(Attr);
                MethodInfo.Path := MethodInfo.RouteAttribute.Path;
                MethodInfo.HttpMethod := MethodInfo.RouteAttribute.Method;

                MethodsList.Add(MethodInfo);
                HasRouteMethods := True;
                Break;
              end;
            end;
          end;

          // ✅ SE TEM MÉTODOS DE ROTA, ADICIONAR COMO CONTROLLER
          if HasRouteMethods then
          begin
            SafeWriteLn('    🎉 ADDING CONTROLLER: ' + RttiType.Name);
            ControllerInfo.RttiType := RttiType;
            ControllerInfo.Methods := MethodsList.ToArray;

            // ✅ VERIFICAR ATRIBUTO [DextController] PARA PREFIXO
            ControllerInfo.ControllerAttribute := nil;
            var TypeAttributes := RttiType.GetAttributes;
            for Attr in TypeAttributes do
            begin
              if Attr is DextControllerAttribute then
              begin
                ControllerInfo.ControllerAttribute := DextControllerAttribute(Attr);
                Break;
              end;
            end;

            Controllers.Add(ControllerInfo);
          end;

        finally
          MethodsList.Free;
        end;
      end;
    end;

    Result := Controllers.ToArray;
    SafeWriteLn('🎯 ' + Format('Total controllers found: %d', [Length(Result)]));

  finally
    Controllers.Free;
  end;
end;

procedure TControllerScanner.RegisterServices(Services: IServiceCollection);
var
  Controllers: TArray<TControllerInfo>;
  Controller: TControllerInfo;
begin
  Controllers := FindControllers;
  SafeWriteLn('🔧 ' + Format('Registering %d controllers in DI...', [Length(Controllers)]));

  for Controller in Controllers do
  begin
    if Controller.RttiType.TypeKind = tkClass then
    begin
      // Register as Transient
      var ClassType := Controller.RttiType.AsInstance.MetaclassType;
      Services.AddTransient(TServiceType.FromClass(ClassType), ClassType);
      SafeWriteLn('  ✅ Registered service: ' + Controller.RttiType.Name);
    end;
  end;
end;

function TControllerScanner.RegisterRoutes(AppBuilder: IApplicationBuilder): Integer;
var
  Controllers: TArray<TControllerInfo>;
  Controller: TControllerInfo;
  ControllerMethod: TControllerMethod;
  FullPath: string;
begin
  Result := 0;
  Controllers := FindControllers;

  SafeWriteLn('🔍 ' + Format('Found %d controllers:', [Length(Controllers)]));

  // ✅ CACHE DE MÉTODOS PARA EVITAR PROBLEMAS DE REFERÊNCIA RTTI
  for Controller in Controllers do
  begin
    // ✅ CALCULAR PREFIXO DO CONTROLLER
    var Prefix := '';
    if Assigned(Controller.ControllerAttribute) then
      Prefix := Controller.ControllerAttribute.Prefix;

    SafeWriteLn('  📦 ' + Format('  %s (Prefix: "%s")', [Controller.RttiType.Name, Prefix]));

    for ControllerMethod in Controller.Methods do
    begin
      // ✅ CONSTRUIR PATH COMPLETO: Prefix + MethodPath
      FullPath := Prefix + ControllerMethod.Path;

      SafeWriteLn(Format('    %s %s -> %s', [ControllerMethod.HttpMethod, FullPath, ControllerMethod.Method.Name]));

      // ✅ VERIFICAR [SwaggerIgnore]
      var IsIgnored := False;
      for var Attr in ControllerMethod.Method.GetAttributes do
        if Attr is SwaggerIgnoreAttribute then
        begin
          IsIgnored := True;
          Break;
        end;

      if IsIgnored then
      begin
        SafeWriteLn('      🚫 Ignored by [SwaggerIgnore]');
        Continue;
      end;

      // ✅ CRIAR CACHE DO MÉTODO
      var CachedMethod: TCachedMethod;
      CachedMethod.TypeName := Controller.RttiType.QualifiedName;
      CachedMethod.MethodName := ControllerMethod.Method.Name;
      CachedMethod.IsClass := (Controller.RttiType.TypeKind = tkClass);
      CachedMethod.FullPath := FullPath;
      CachedMethod.HttpMethod := ControllerMethod.HttpMethod;
      
      // ✅ CHECK AUTH ATTRIBUTES (Controller or Method level)
      // RULE: [AllowAnonymous] on method OVERRIDES [Authorize] on controller
      var ControllerRequiresAuth := False;
      var MethodRequiresAuth := False;
      var MethodAllowsAnonymous := False;
      
      // Check controller level [Authorize]
      for var Attr in Controller.RttiType.GetAttributes do
        if Attr is AuthorizeAttribute then
        begin
          ControllerRequiresAuth := True;
          Break;
        end;
      
      // Check method level attributes
      for var Attr in ControllerMethod.Method.GetAttributes do
      begin
        if Attr is AuthorizeAttribute then
          MethodRequiresAuth := True;

        if Attr is AllowAnonymousAttribute then
          MethodAllowsAnonymous := True;
      end;

      // Final decision: 
      // - If method has [AllowAnonymous], it's always allowed (overrides controller [Authorize])
      // - Otherwise, auth is required if controller OR method has [Authorize]
      if MethodAllowsAnonymous then
        CachedMethod.RequiresAuth := False
      else
        CachedMethod.RequiresAuth := ControllerRequiresAuth or MethodRequiresAuth;

      // ✅ FILTERS REMOVED FROM CACHE
      // We now fetch them dynamically in ExecuteCachedMethod to avoid AVs
      
      FCachedMethods.Add(CachedMethod);

      // ✅ REGISTRAR ROTA USANDO CACHE (EVITA PROBLEMAS DE REFERÊNCIA RTTI)
      // Usar CreateHandler para garantir captura correta da variável no loop
      AppBuilder.MapEndpoint(ControllerMethod.HttpMethod, FullPath, CreateHandler(CachedMethod));

      // ✅ PROCESSAR ATRIBUTOS DE SEGURANÇA (SwaggerAuthorize)
      var SecuritySchemes := TList<string>.Create;
      try
        // 1. Atributos do Controller
        var TypeAttrs := Controller.RttiType.GetAttributes;
        for var Attr in TypeAttrs do
          if Attr is AuthorizeAttribute then
            SecuritySchemes.Add(AuthorizeAttribute(Attr).Scheme);

        // 2. Atributos do Método
        var MethodAttrs := ControllerMethod.Method.GetAttributes;
        for var Attr in MethodAttrs do
          if Attr is AuthorizeAttribute then
            SecuritySchemes.Add(AuthorizeAttribute(Attr).Scheme);

        // 3. Atualizar Metadados da Rota
        if SecuritySchemes.Count > 0 then
        begin
          var Routes := AppBuilder.GetRoutes;
          if Length(Routes) > 0 then
          begin
            var Metadata := Routes[High(Routes)];
            Metadata.Security := SecuritySchemes.ToArray;
            AppBuilder.UpdateLastRouteMetadata(Metadata);
            SafeWriteLn('      🔒 Secured with: ' + string.Join(', ', Metadata.Security));
          end;
        end;
      finally
        SecuritySchemes.Free;
      end;

      // ✅ PROCESSAR [SwaggerOperation], [SwaggerResponse], [SwaggerTag] e RequestType
      var Routes := AppBuilder.GetRoutes;
      if Length(Routes) > 0 then
      begin
        var Metadata := Routes[High(Routes)];
        var Updated := False;

        // 1. [SwaggerTag] do Controller
        for var TypeAttr in Controller.RttiType.GetAttributes do
        begin
          if TypeAttr is SwaggerTagAttribute then
          begin
            var TagAttr := SwaggerTagAttribute(TypeAttr);
            if Length(Metadata.Tags) = 0 then
            begin
              SetLength(Metadata.Tags, 1);
              Metadata.Tags[0] := TagAttr.Tag;
              Updated := True;
            end;
          end;
        end;

        // 2. Extrair RequestType dos parâmetros do método (para POST/PUT/PATCH)
        if (ControllerMethod.HttpMethod = 'POST') or 
           (ControllerMethod.HttpMethod = 'PUT') or 
           (ControllerMethod.HttpMethod = 'PATCH') then
        begin
          var Params := ControllerMethod.Method.GetParameters;
          for var Param in Params do
          begin
            var ParamType := Param.ParamType;
            if (ParamType <> nil) and (ParamType.TypeKind in [tkRecord, tkMRecord]) then
            begin
              // Ignorar IHttpContext e tipos básicos
              if not SameText(ParamType.Name, 'IHttpContext') then
              begin
                Metadata.RequestType := ParamType.Handle;
                Updated := True;
                SafeWriteLn('      📝 RequestType: ' + ParamType.Name);
                Break;
              end;
            end;
          end;
        end;

        // 3. [SwaggerOperation] do método
        for var Attr in ControllerMethod.Method.GetAttributes do
        begin
          if Attr is SwaggerOperationAttribute then
          begin
            var OpAttr := SwaggerOperationAttribute(Attr);
            if OpAttr.Summary <> '' then Metadata.Summary := OpAttr.Summary;
            if OpAttr.Description <> '' then Metadata.Description := OpAttr.Description;
            if Length(OpAttr.Tags) > 0 then Metadata.Tags := OpAttr.Tags;
            Updated := True;
          end;
        end;

        // 4. [SwaggerResponse] do método -> popular array Responses
        var ResponsesList: TArray<TOpenAPIResponseMetadata>;
        SetLength(ResponsesList, 0);
        for var Attr in ControllerMethod.Method.GetAttributes do
        begin
          if Attr is SwaggerResponseAttribute then
          begin
            var RespAttr := SwaggerResponseAttribute(Attr);
            var RespMeta: TOpenAPIResponseMetadata;
            RespMeta.StatusCode := RespAttr.StatusCode;
            RespMeta.Description := RespAttr.Description;
            RespMeta.MediaType := RespAttr.ContentType;
            RespMeta.SchemaType := nil; // Atributo não tem TypeInfo
            SetLength(ResponsesList, Length(ResponsesList) + 1);
            ResponsesList[High(ResponsesList)] := RespMeta;
            Updated := True;
          end;
        end;
        if Length(ResponsesList) > 0 then
          Metadata.Responses := ResponsesList;

        if Updated then
          AppBuilder.UpdateLastRouteMetadata(Metadata);
      end;

      Inc(Result);
    end;
  end;

  SafeWriteLn('✅ ' + Format('Registered %d auto-routes', [Result]));
  SafeWriteLn('💾 ' + Format('Cached %d methods for runtime execution', [FCachedMethods.Count]));
end;

destructor TControllerScanner.Destroy;
begin
  FCachedMethods.Free;
  inherited;
end;

function TControllerScanner.CreateHandler(const AMethod: TCachedMethod): TRequestDelegate;
begin
  Result := procedure(Context: IHttpContext)
  begin
    ExecuteCachedMethod(Context, AMethod);
  end;
end;

procedure TControllerScanner.ExecuteCachedMethod(Context: IHttpContext; const CachedMethod: TCachedMethod);
var
  Ctx: TRttiContext;
  ControllerType: TRttiType;
  Method: TRttiMethod;
  ControllerInstance: TObject;
  FilterAttr: TCustomAttribute;
  Filter: IActionFilter;
  I: Integer;
begin
  SafeWriteLn('🔄 ' + Format('Executing: %s -> %s.%s', [CachedMethod.FullPath, CachedMethod.TypeName, CachedMethod.MethodName]));

  // ✅ ENFORCE AUTHORIZATION
  if CachedMethod.RequiresAuth then
  begin
    if (Context.User = nil) or (Context.User.Identity = nil) or (not Context.User.Identity.IsAuthenticated) then
    begin
      SafeWriteLn('⛔ Authorization failed: User not authenticated');
      Context.Response.Status(401).Json('{"error": "Unauthorized"}');
      Exit;
    end;
  end;

  Ctx := TRttiContext.Create;
  // ✅ RE-OBTER O TIPO EM TEMPO DE EXECUÇÃO
  ControllerType := Ctx.FindType(CachedMethod.TypeName);
  if ControllerType = nil then
  begin
    SafeWriteLn('❌ Controller type not found: ' + CachedMethod.TypeName);
    Context.Response.Status(500).Json(Format('{"error": "Controller type not found: %s"}', [CachedMethod.TypeName]));
    Exit;
  end;

  // ✅ ENCONTRAR O MÉTODO EM TEMPO DE EXECUÇÃO
  Method := nil;
  for var M in ControllerType.GetMethods do
  begin
    if M.Name = CachedMethod.MethodName then
    begin
      Method := M;
      Break;
    end;
  end;

  if Method = nil then
  begin
    SafeWriteLn('❌ ' + Format('Method not found: %s.%s', [CachedMethod.TypeName, CachedMethod.MethodName]));
    Context.Response.Status(500).Json(Format('{"error": "Method not found: %s.%s"}', [CachedMethod.TypeName, CachedMethod.MethodName]));
    Exit;
  end;

  var FilterList := TObjectList<TCustomAttribute>.Create(False);
  try
    // Controller Level
    for FilterAttr in ControllerType.GetAttributes do
      if Supports(FilterAttr, IActionFilter) then
        FilterList.Add(FilterAttr);
      
    // Method Level
    for FilterAttr in Method.GetAttributes do
      if Supports(FilterAttr, IActionFilter) then
        FilterList.Add(FilterAttr);

    // ✅ EXECUTE ACTION FILTERS - OnActionExecuting
    var ActionDescriptor: TActionDescriptor;
    ActionDescriptor.ControllerName := CachedMethod.TypeName;
    ActionDescriptor.ActionName := CachedMethod.MethodName;
    ActionDescriptor.HttpMethod := CachedMethod.HttpMethod;
    ActionDescriptor.Route := CachedMethod.FullPath;

    // ✅ FIX: Use interface variable to prevent premature destruction (RefCount issue)
    var ExecutingContext: IActionExecutingContext := TActionExecutingContext.Create(Context, ActionDescriptor);
    try
      for FilterAttr in FilterList do
      begin
        if Supports(FilterAttr, IActionFilter, Filter) then
        begin
          Filter.OnActionExecuting(ExecutingContext);

          // Check for short-circuit
          if Assigned(ExecutingContext.Result) then
          begin
            SafeWriteLn('⚡ Filter short-circuited execution');
            ExecutingContext.Result.Execute(Context);
            Exit;
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        SafeWriteLn('❌ Error in OnActionExecuting filter: ' + E.Message);
        raise;
      end;
    end;

    // ✅ EXECUTAR O MÉTODO DO CONTROLLER
    try
      if CachedMethod.IsClass then
      begin
        // ✅ RESOLVER INSTÂNCIA VIA DI
        ControllerInstance := Context.GetServices.GetService(
          TServiceType.FromClass(ControllerType.AsInstance.MetaclassType));

        if ControllerInstance = nil then
        begin
          SafeWriteLn('❌ Controller instance not found: ' + CachedMethod.TypeName);
          Context.Response.Status(500).Json(Format('{"error": "Controller instance not found: %s"}', [CachedMethod.TypeName]));
          Exit;
        end;

        var Binder: IModelBinder := TModelBinder.Create;
        var Invoker := THandlerInvoker.Create(Context, Binder);
        try
          Invoker.InvokeAction(ControllerInstance, Method);
        finally
          Invoker.Free;
          Binder := nil;
          // Transient controllers MUST be freed by the invoker
          if ControllerInstance <> nil then
          begin
            ControllerInstance.Free;
          end;
        end;
      end
      else
      begin
        // ✅ RECORDS ESTÁTICOS
        var Binder: IModelBinder := TModelBinder.Create;
        var Invoker := THandlerInvoker.Create(Context, Binder);
        try
          Invoker.InvokeAction(nil, Method);
        finally
          Invoker.Free;
          Binder := nil;
        end;
      end;

      // ✅ EXECUTE ACTION FILTERS - OnActionExecuted
      var ExecutedContext: IActionExecutedContext := TActionExecutedContext.Create(Context, ActionDescriptor, nil, nil);
      // Execute filters in reverse order
      for I := FilterList.Count - 1 downto 0 do
      begin
        FilterAttr := FilterList[I];
        if Supports(FilterAttr, IActionFilter, Filter) then
          Filter.OnActionExecuted(ExecutedContext);
      end;

    except
      on E: Exception do
      begin
        SafeWriteLn('❌ Error executing method: ' + E.Message);
          
        // ✅ EXECUTE ACTION FILTERS - OnActionExecuted (with exception)
        var ExecutedContext: IActionExecutedContext := TActionExecutedContext.Create(Context, ActionDescriptor, nil, E);
        for I := FilterList.Count - 1 downto 0 do
        begin
          FilterAttr := FilterList[I];
          if Supports(FilterAttr, IActionFilter, Filter) then
          begin
            Filter.OnActionExecuted(ExecutedContext);
            if ExecutedContext.ExceptionHandled then
            begin
              SafeWriteLn('✅ Exception handled by filter');
              Exit; // Don't re-raise
            end;
          end;
        end;

        Context.Response.Status(500).Json(Format('{"error": "Execution failed: %s"}', [E.Message]));
      end;
    end;

  finally
    FilterList.Free;
  end;
end;

end.



