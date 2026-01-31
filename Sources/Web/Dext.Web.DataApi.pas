{***************************************************************************}
{                                                                           }
{           Dext Framework                                                  }
{                                                                           }
{           Copyright (C) 2025 Cesar Romero & Dext Contributors             }
{                                                                           }
{***************************************************************************}
unit Dext.Web.DataApi;

interface

uses
  System.Classes,
  System.Rtti,
  System.SysUtils,
  System.Character,
  System.Generics.Collections,
  Dext.DI.Interfaces,
  Dext.Entity,
  Dext.Entity.Core,
  Dext.Entity.Context,
  Dext.Json,
  Dext.Web.Interfaces,
  Dext.Web.Routing,
  Dext.Web.Pipeline,
  Dext.Entity.Mapping,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.TypeConverters,
  Dext.Core.ValueConverters,
  Dext.OpenAPI.Extensions;

type
  TApiMethod = (amGet, amGetList, amPost, amPut, amDelete);
  TApiMethods = set of TApiMethod;

  const AllApiMethods = [amGet, amGetList, amPost, amPut, amDelete];

type
  TDataApiOptions<T> = class
  private
    FAllowedMethods: TApiMethods;
    FTenantIdRequired: Boolean;
    FRequireAuthentication: Boolean;
    FRolesForRead: string;   // Comma-separated roles for GET operations
    FRolesForWrite: string;  // Comma-separated roles for POST/PUT/DELETE
    FNamingStrategy: TCaseStyle;
    // Swagger options (disabled by default)
    FEnableSwagger: Boolean;
    FSwaggerTag: string;
    FSwaggerDescription: string;
  public
    constructor Create;
    property AllowedMethods: TApiMethods read FAllowedMethods write FAllowedMethods;
    property TenantIdRequired: Boolean read FTenantIdRequired write FTenantIdRequired;
    property RequireAuthentication: Boolean read FRequireAuthentication write FRequireAuthentication;
    property RolesForRead: string read FRolesForRead write FRolesForRead;
    property RolesForWrite: string read FRolesForWrite write FRolesForWrite;
    property EnableSwagger: Boolean read FEnableSwagger write FEnableSwagger;
    property SwaggerTag: string read FSwaggerTag write FSwaggerTag;
    property SwaggerDescription: string read FSwaggerDescription write FSwaggerDescription;
      
    // Fluent configuration
    function Allow(AMethods: TApiMethods): TDataApiOptions<T>;
    function RequireTenant: TDataApiOptions<T>;
    function RequireAuth: TDataApiOptions<T>;
    function RequireRole(const ARoles: string): TDataApiOptions<T>;  // All operations
    function RequireReadRole(const ARoles: string): TDataApiOptions<T>;  // GET only
    function RequireWriteRole(const ARoles: string): TDataApiOptions<T>; // POST/PUT/DELETE
      
    // Naming Strategies
    function UseSnakeCase: TDataApiOptions<T>;
    function UseCamelCase: TDataApiOptions<T>;
    
    // Swagger documentation (opt-in)
    function UseSwagger: TDataApiOptions<T>;
    function Tag(const ATag: string): TDataApiOptions<T>;
    function Description(const ADescription: string): TDataApiOptions<T>;
  end;

  TDataApiHandler<T: class> = class
  private
    FOptions: TDataApiOptions<T>;
    FPath: string;
    FDbContext: TDbContext;  // Reference to DbContext (not owned)
    FUseExplicitContext: Boolean;
      
    function GetDbContext(const Context: IHttpContext): TDbContext;
    function EntityToJson(const Entity: T): string;
    function CheckAuthorization(const Context: IHttpContext; IsWriteOperation: Boolean): IResult;
  public
    constructor Create(const APath: string; AOptions: TDataApiOptions<T>; ADbContext: TDbContext = nil);
    destructor Destroy; override;
      
    procedure RegisterRoutes(const ABuilder: IApplicationBuilder);
      
    // Option A: Explicit DbContext parameter
    class procedure Map(const ABuilder: IApplicationBuilder; const APath: string; ADbContext: TDbContext); overload;
    class procedure Map(const ABuilder: IApplicationBuilder; const APath: string; ADbContext: TDbContext; AOptions: TDataApiOptions<T>); overload;
      
    // Option B: Resolve DbContext from DI (Context.Services)
    class procedure Map(const ABuilder: IApplicationBuilder; const APath: string); overload;
    class procedure Map(const ABuilder: IApplicationBuilder; const APath: string; AOptions: TDataApiOptions<T>); overload;
      
    // Request Handlers
    function HandleGetList(const Context: IHttpContext): IResult;
    function HandleGet(const Context: IHttpContext): IResult;
    function HandlePost(const Context: IHttpContext): IResult;
    function HandlePut(const Context: IHttpContext): IResult;
    function HandleDelete(const Context: IHttpContext): IResult;
  end;

function GetJsonVal(const AVal: TValue): string;
function EscapeJsonString(const S: string): string;


implementation

uses
  System.DateUtils,
  System.TypInfo,
  Dext.Collections,
  Dext.Json.Types,
  Dext.Core.DateUtils,
  Dext.Specifications.Types,
  Dext.Specifications.Interfaces,
  Dext.Auth.Identity,
  Dext.Web.Results;


function EscapeJsonString(const S: string): string;
var
  i: Integer;
  c: Char;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    c := S[i];
    case c of
      '"': Result := Result + '\"';
      '\': Result := Result + '\\';
      '/': Result := Result + '\/';
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
    else
      if (Ord(c) < 32) then
        Result := Result + Format('\u%.4x', [Ord(c)])
      else
        Result := Result + c;
    end;
  end;
end;

{ TDataApiOptions<T> }

constructor TDataApiOptions<T>.Create;
begin
  FAllowedMethods := AllApiMethods;
  FAllowedMethods := AllApiMethods;
  FTenantIdRequired := False;
  FNamingStrategy := TCaseStyle.CamelCase;
end;

function TDataApiOptions<T>.Allow(AMethods: TApiMethods): TDataApiOptions<T>;
begin
  FAllowedMethods := AMethods;
  Result := Self;
end;

function TDataApiOptions<T>.RequireTenant: TDataApiOptions<T>;
begin
  FTenantIdRequired := True;
  Result := Self;
end;

function TDataApiOptions<T>.RequireAuth: TDataApiOptions<T>;
begin
  FRequireAuthentication := True;
  Result := Self;
end;

function TDataApiOptions<T>.RequireRole(const ARoles: string): TDataApiOptions<T>;
begin
  FRequireAuthentication := True;
  FRolesForRead := ARoles;
  FRolesForWrite := ARoles;
  Result := Self;
end;

function TDataApiOptions<T>.RequireReadRole(const ARoles: string): TDataApiOptions<T>;
begin
  FRequireAuthentication := True;
  FRolesForRead := ARoles;
  Result := Self;
end;

function TDataApiOptions<T>.RequireWriteRole(const ARoles: string): TDataApiOptions<T>;
begin
  FRequireAuthentication := True;
  FRolesForWrite := ARoles;
  Result := Self;
end;

function TDataApiOptions<T>.UseSnakeCase: TDataApiOptions<T>;
begin
  FNamingStrategy := TCaseStyle.SnakeCase;
  Result := Self;
end;

function TDataApiOptions<T>.UseCamelCase: TDataApiOptions<T>;
begin
  FNamingStrategy := TCaseStyle.CamelCase;
  Result := Self;
end;

function TDataApiOptions<T>.UseSwagger: TDataApiOptions<T>;
begin
  FEnableSwagger := True;
  Result := Self;
end;

function TDataApiOptions<T>.Tag(const ATag: string): TDataApiOptions<T>;
begin
  FSwaggerTag := ATag;
  Result := Self;
end;

function TDataApiOptions<T>.Description(const ADescription: string): TDataApiOptions<T>;
begin
  FSwaggerDescription := ADescription;
  Result := Self;
end;

{ TDataApiHandler<T> }

constructor TDataApiHandler<T>.Create(const APath: string; AOptions: TDataApiOptions<T>; ADbContext: TDbContext);
begin
  inherited Create;
  FPath := APath;
  FOptions := AOptions;
  FDbContext := ADbContext;
  FUseExplicitContext := (ADbContext <> nil);
  if FOptions = nil then
    FOptions := TDataApiOptions<T>.Create;
end;

destructor TDataApiHandler<T>.Destroy;
begin
  FOptions.Free;
  // Note: FDbContext is NOT owned by handler - do not free
  inherited;
end;

function TDataApiHandler<T>.GetDbContext(const Context: IHttpContext): TDbContext;
var
  Obj: TObject;
begin
  if FUseExplicitContext then
    Result := FDbContext
  else
  begin
    // Resolve from DI using TServiceType
    Obj := Context.Services.GetService(TServiceType.FromClass(TDbContext));
    if Obj = nil then
      raise Exception.Create('TDbContext not registered in DI container. Use Map(App, Path, DbContext) or register TDbContext in ConfigureServices.');
    Result := TDbContext(Obj);
  end;
end;

function TDataApiHandler<T>.CheckAuthorization(const Context: IHttpContext; IsWriteOperation: Boolean): IResult;
var
  User: IClaimsPrincipal;
  RequiredRoles: string;
  RoleArray: TArray<string>;
  Role: string;
  HasRole: Boolean;
begin
  Result := nil;  // nil = authorized
  
  if not FOptions.RequireAuthentication then
    Exit;  // No auth required
  
  // Check if user is authenticated
  User := Context.User;
  if (User = nil) or not User.Identity.IsAuthenticated then
    Exit(Results.StatusCode(401, '{"error":"Authentication required"}'));
  
  // Determine which roles to check
  if IsWriteOperation then
    RequiredRoles := FOptions.RolesForWrite
  else
    RequiredRoles := FOptions.RolesForRead;
  
  if RequiredRoles = '' then
    Exit;  // Auth required but no specific roles
  
  // Check roles (comma-separated, user must have at least one)
  RoleArray := RequiredRoles.Split([',']);
  HasRole := False;
  for Role in RoleArray do
  begin
    if User.IsInRole(Role.Trim) then
    begin
      HasRole := True;
      Break;
    end;
  end;
  
  if not HasRole then
    Result := Results.StatusCode(403, Format('{"error":"Forbidden - requires one of roles: %s"}', [RequiredRoles]));
end;

function TDataApiHandler<T>.EntityToJson(const Entity: T): string;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  First: Boolean;
  PropName: string;
  Map: TEntityMap;
  PropMap: TPropertyMap;
begin
  if Entity = nil then
    Exit('null');
    
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(TypeInfo(T));
    Map := TModelBuilder.Instance.GetMap(TypeInfo(T));
    
    Result := '{';
    First := True;
    
    for Prop in Typ.GetProperties do
    begin
      if not Prop.IsReadable then
        Continue;
        
      // Check for mapping ignore
      if Map.Properties.TryGetValue(Prop.Name, PropMap) and PropMap.IsIgnored then
        Continue;
        
      if not First then
        Result := Result + ',';
      First := False;
      
      // Apply naming strategy
      PropName := TJsonUtils.ApplyCaseStyle(Prop.Name, FOptions.FNamingStrategy);
        
      Result := Result + '"' + PropName + '":' + GetJsonVal(Prop.GetValue(TObject(Entity)));
    end;
    
    Result := Result + '}';
  finally
    Ctx.Free;
  end;
end;

procedure TDataApiHandler<T>.RegisterRoutes(const ABuilder: IApplicationBuilder);
var
  CleanPath, EntityName, EntityTag: string;
  RttiCtx: TRttiContext;
  RttiType: TRttiType;
begin
  CleanPath := FPath.TrimRight(['/']);
  
  // Get entity name for Swagger tag (e.g., "TCustomer" -> "Customers")
  RttiCtx := TRttiContext.Create;
  try
    RttiType := RttiCtx.GetType(TypeInfo(T));
    if RttiType <> nil then
      EntityName := RttiType.Name
    else
      EntityName := 'Entity';
  finally
    RttiCtx.Free;
  end;
  
  // Remove 'T' prefix if present and pluralize
  if EntityName.StartsWith('T') then
    EntityTag := EntityName.Substring(1)
  else
    EntityTag := EntityName;
  if not EntityTag.EndsWith('s') then
    EntityTag := EntityTag + 's';
  
  // Use custom tag if provided
  if FOptions.SwaggerTag <> '' then
    EntityTag := FOptions.SwaggerTag;

  // GET List
  if amGetList in FOptions.AllowedMethods then
  begin
    ABuilder.MapGet(CleanPath, 
      procedure(Ctx: IHttpContext)
      begin
        var Res := HandleGetList(Ctx);
        Res.Execute(Ctx);
      end);
    
    // Add Swagger metadata
    if FOptions.EnableSwagger then
    begin
      TEndpointMetadataExtensions.WithSummary(ABuilder, 'List all ' + EntityTag);
      TEndpointMetadataExtensions.WithDescription(ABuilder, 
        'Returns a list of ' + EntityTag + '. Supports filtering by property values, ' +
        'pagination with _limit and _offset query parameters.');
      TEndpointMetadataExtensions.WithTag(ABuilder, EntityTag);
      TEndpointMetadataExtensions.WithResponse(ABuilder, 200, 'List of ' + EntityTag, TypeInfo(T));
      if FOptions.RequireAuthentication then
        TEndpointMetadataExtensions.RequireAuthorization(ABuilder, 'bearerAuth');
    end;
  end;

  // GET by ID
  if amGet in FOptions.AllowedMethods then
  begin
    ABuilder.MapGet(CleanPath + '/{id}', 
      procedure(Ctx: IHttpContext)
      begin
        var Res := HandleGet(Ctx);
        Res.Execute(Ctx);
      end);
    
    if FOptions.EnableSwagger then
    begin
      TEndpointMetadataExtensions.WithSummary(ABuilder, 'Get ' + EntityTag.TrimRight(['s']) + ' by ID');
      TEndpointMetadataExtensions.WithDescription(ABuilder, 
        'Returns a single ' + EntityTag.TrimRight(['s']) + ' by its unique identifier.');
      TEndpointMetadataExtensions.WithTag(ABuilder, EntityTag);
      TEndpointMetadataExtensions.WithResponse(ABuilder, 200, EntityTag.TrimRight(['s']) + ' found', TypeInfo(T));
      TEndpointMetadataExtensions.WithResponse(ABuilder, 404, 'Entity not found');
      if FOptions.RequireAuthentication then
        TEndpointMetadataExtensions.RequireAuthorization(ABuilder, 'bearerAuth');
    end;
  end;

  // POST
  if amPost in FOptions.AllowedMethods then
  begin
    ABuilder.MapPost(CleanPath, 
      procedure(Ctx: IHttpContext)
      begin
        var Res := HandlePost(Ctx);
        Res.Execute(Ctx);
      end);
    
    if FOptions.EnableSwagger then
    begin
      TEndpointMetadataExtensions.WithSummary(ABuilder, 'Create ' + EntityTag.TrimRight(['s']));
      TEndpointMetadataExtensions.WithDescription(ABuilder, 
        'Creates a new ' + EntityTag.TrimRight(['s']) + '. Returns the created entity with its generated ID.');
      TEndpointMetadataExtensions.WithTag(ABuilder, EntityTag);
      TEndpointMetadataExtensions.WithRequestType(ABuilder, TypeInfo(T));
      TEndpointMetadataExtensions.WithResponse(ABuilder, 201, 'Entity created', TypeInfo(T));
      TEndpointMetadataExtensions.WithResponse(ABuilder, 400, 'Invalid request body');
      if FOptions.RequireAuthentication then
        TEndpointMetadataExtensions.RequireAuthorization(ABuilder, 'bearerAuth');
    end;
  end;

  // PUT
  if amPut in FOptions.AllowedMethods then
  begin
    ABuilder.MapPut(CleanPath + '/{id}', 
      procedure(Ctx: IHttpContext)
      begin
        var Res := HandlePut(Ctx);
        Res.Execute(Ctx);
      end);
    
    if FOptions.EnableSwagger then
    begin
      TEndpointMetadataExtensions.WithSummary(ABuilder, 'Update ' + EntityTag.TrimRight(['s']));
      TEndpointMetadataExtensions.WithDescription(ABuilder, 
        'Updates an existing ' + EntityTag.TrimRight(['s']) + ' by ID.');
      TEndpointMetadataExtensions.WithTag(ABuilder, EntityTag);
      TEndpointMetadataExtensions.WithRequestType(ABuilder, TypeInfo(T));
      TEndpointMetadataExtensions.WithResponse(ABuilder, 200, 'Entity updated', TypeInfo(T));
      TEndpointMetadataExtensions.WithResponse(ABuilder, 404, 'Entity not found');
      if FOptions.RequireAuthentication then
        TEndpointMetadataExtensions.RequireAuthorization(ABuilder, 'bearerAuth');
    end;
  end;

  // DELETE
  if amDelete in FOptions.AllowedMethods then
  begin
    ABuilder.MapDelete(CleanPath + '/{id}', 
      procedure(Ctx: IHttpContext) 
      begin
        var Res := HandleDelete(Ctx);
        Res.Execute(Ctx);
      end);
    
    if FOptions.EnableSwagger then
    begin
      TEndpointMetadataExtensions.WithSummary(ABuilder, 'Delete ' + EntityTag.TrimRight(['s']));
      TEndpointMetadataExtensions.WithDescription(ABuilder, 
        'Deletes an existing ' + EntityTag.TrimRight(['s']) + ' by ID.');
      TEndpointMetadataExtensions.WithTag(ABuilder, EntityTag);
      TEndpointMetadataExtensions.WithResponse(ABuilder, 204, 'Entity deleted');
      TEndpointMetadataExtensions.WithResponse(ABuilder, 404, 'Entity not found');
      if FOptions.RequireAuthentication then
        TEndpointMetadataExtensions.RequireAuthorization(ABuilder, 'bearerAuth');
    end;
  end;
end;


function GetJsonVal(const AVal: TValue): string;
begin
   if AVal.IsEmpty then Exit('null');
   case AVal.Kind of
     tkInteger, tkInt64: Result := IntToStr(AVal.AsInt64);
     tkFloat: 
     begin
       if AVal.TypeInfo = TypeInfo(TDateTime) then
         Result := '"' + DateToISO8601(AVal.AsType<TDateTime>) + '"'
       else
         Result := FloatToStr(AVal.AsExtended); // TODO: Locale independent
     end;
     tkString, tkUString, tkWString, tkChar, tkWChar: Result := '"' + EscapeJsonString(AVal.AsString) + '"';
     tkEnumeration:
       if AVal.TypeInfo = TypeInfo(Boolean) then
         Result := BoolToStr(AVal.AsBoolean, true).ToLower
       else
         Result := IntToStr(AVal.AsOrdinal);
     else
       Result := '"' + EscapeJsonString(AVal.ToString) + '"';
   end;
end;

function TDataApiHandler<T>.HandleGetList(const Context: IHttpContext): IResult;
var
  DbCtx: TDbContext;
  DbSet: IDbSet;
  Entities: IList<TObject>;
  Entity: TObject;
  JsonResult: string;
  First: Boolean;
  Query: TStrings;
  i: Integer;
  ParamName, ParamValue: string;
  FilterExpr: IExpression;
  NewExpr: IExpression;
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  PropType: TRttiType;
  IntVal: Integer;
  BoolVal: Boolean;
  Limit, Offset: Integer;
  AuthResult: IResult;
  Map: TEntityMap;
  PropMap: TPropertyMap;
begin
  // Authorization check
  AuthResult := CheckAuthorization(Context, False);  // Read operation
  if AuthResult <> nil then
    Exit(AuthResult);
    
  try
    DbCtx := GetDbContext(Context);
    
    // Parse query parameters for filtering
    Query := Context.Request.Query;
    FilterExpr := nil;
    Limit := 0;
    Offset := 0;
    
    Ctx := TRttiContext.Create;
    try
      Typ := Ctx.GetType(TypeInfo(T));
      Map := TModelBuilder.Instance.GetMap(TypeInfo(T));
      
      for i := 0 to Query.Count - 1 do
      begin
        ParamName := Query.Names[i];
        ParamValue := Query.ValueFromIndex[i];
        
        // Skip reserved params for pagination
        if SameText(ParamName, '_limit') then
        begin
          TryStrToInt(ParamValue, Limit);
          Continue;
        end;
        if SameText(ParamName, '_offset') then
        begin
          TryStrToInt(ParamValue, Offset);
          Continue;
        end;
        if SameText(ParamName, '_orderby') then
          Continue; // TODO: implement ordering
        
        // Find matching property (case insensitive)
        Prop := nil;
        for var P in Typ.GetProperties do
          if SameText(P.Name, ParamName) then
          begin
            Prop := P;
            Break;
          end;
        
        if Prop = nil then
          Continue; // Skip unknown properties
          
        // Check if property is ignored in mapping
        if Map.Properties.TryGetValue(Prop.Name, PropMap) and PropMap.IsIgnored then
          Continue;
        
        // Build expression based on property type
        PropType := Prop.PropertyType;
        case PropType.TypeKind of
          tkInteger, tkInt64:
            if TryStrToInt(ParamValue, IntVal) then
              NewExpr := TBinaryExpression.Create(Prop.Name, boEqual, TValue.From<Integer>(IntVal))
            else
              Continue;
          tkEnumeration:
            if PropType.Handle = TypeInfo(Boolean) then
            begin
              BoolVal := SameText(ParamValue, 'true') or (ParamValue = '1');
              NewExpr := TBinaryExpression.Create(Prop.Name, boEqual, TValue.From<Boolean>(BoolVal));
            end
            else
              Continue;
          tkString, tkUString, tkWString, tkLString:
            NewExpr := TBinaryExpression.Create(Prop.Name, boEqual, TValue.From<string>(ParamValue));
        else
          Continue;
        end;
        
        // Combine with AND
        if FilterExpr = nil then
          FilterExpr := NewExpr
        else
          FilterExpr := TLogicalExpression.Create(FilterExpr, NewExpr, loAnd);
      end;
    finally
      Ctx.Free;
    end;
    
    // Get filtered entities
    DbSet := DbCtx.DataSet(TypeInfo(T));
    Entities := DbSet.ListObjects(FilterExpr);
    
    // Build JSON response with pagination
    JsonResult := '[';
    First := True;
    i := 0;
    for Entity in Entities do
    begin
      // Apply offset
      if (Offset > 0) and (i < Offset) then
      begin
        Inc(i);
        Continue;
      end;
      
      // Apply limit
      if (Limit > 0) and (i >= Offset + Limit) then
        Break;
      
      if not First then
        JsonResult := JsonResult + ',';
      First := False;
      JsonResult := JsonResult + EntityToJson(T(Entity));
      Inc(i);
    end;
    JsonResult := JsonResult + ']';
    
    Result := Results.Json(JsonResult);
  except
    on E: Exception do
      Result := Results.StatusCode(500, Format('{"error":"%s"}', [EscapeJsonString(E.Message)]));
  end;
end;

function TDataApiHandler<T>.HandleGet(const Context: IHttpContext): IResult;
var
  DbCtx: TDbContext;
  IdStr: string;
  Id: Integer;
  Entity: T;
  AuthResult: IResult;
begin
  // Authorization check
  AuthResult := CheckAuthorization(Context, False);  // Read operation
  if AuthResult <> nil then
    Exit(AuthResult);
    
  try
    // Get ID from route parameter
    if not Context.Request.RouteParams.TryGetValue('id', IdStr) then
      Exit(Results.BadRequest('{"error":"Missing id parameter"}'));
      
    if not TryStrToInt(IdStr, Id) then
      Exit(Results.BadRequest('{"error":"Invalid id format"}'));
    
    DbCtx := GetDbContext(Context);
    Entity := DbCtx.Entities<T>.Find(Id);
    
    if Entity = nil then
      Result := Results.NotFound(Format('{"error":"Entity with id %d not found"}', [Id]))
    else
      Result := Results.Json(EntityToJson(Entity));
  except
    on E: Exception do
      Result := Results.StatusCode(500, Format('{"error":"%s"}', [EscapeJsonString(E.Message)]));
  end;
end;

function TDataApiHandler<T>.HandlePost(const Context: IHttpContext): IResult;
var
  DbCtx: TDbContext;
  Entity: T;
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  IdProp: TRttiProperty;
  IdValue: Integer;
  Stream: TStream;
  JsonString: string;
  Bytes: TBytes;
  JsonNode: IDextJsonNode;
  JsonObj: IDextJsonObject;
  PropName: string;
  AuthResult: IResult;
  Map: TEntityMap;
  PropMap: TPropertyMap;
begin
  // Authorization check
  AuthResult := CheckAuthorization(Context, True);  // Write operation
  if AuthResult <> nil then
    Exit(AuthResult);
    
  try
    DbCtx := GetDbContext(Context);
    
    // Read JSON body
    Stream := Context.Request.Body;
    if (Stream = nil) or (Stream.Size = 0) then
      Exit(Results.BadRequest('{"error":"Request body is empty"}'));
    
    Stream.Position := 0;
    SetLength(Bytes, Stream.Size);
    Stream.ReadBuffer(Bytes[0], Stream.Size);
    JsonString := TEncoding.UTF8.GetString(Bytes);
    
    // Parse JSON
    JsonNode := TDextJson.Provider.Parse(JsonString);
    if (JsonNode = nil) or (JsonNode.GetNodeType <> jntObject) then
      Exit(Results.BadRequest('{"error":"Invalid JSON in request body"}'));
    
    JsonObj := JsonNode as IDextJsonObject;
    
    // Create new entity instance
    Ctx := TRttiContext.Create;
    try
      Typ := Ctx.GetType(TypeInfo(T));
      Map := TModelBuilder.Instance.GetMap(TypeInfo(T));
      Entity := Typ.GetMethod('Create').Invoke(Typ.AsInstance.MetaclassType, []).AsType<T>;
      
      // Populate entity properties from JSON
      for Prop in Typ.GetProperties do
      begin
        if not Prop.IsWritable then
          Continue;
        if SameText(Prop.Name, 'Id') then
          Continue; // Don't set ID from JSON, let DB generate it
          
        // Check for mapping ignore
        if Map.Properties.TryGetValue(Prop.Name, PropMap) and PropMap.IsIgnored then
          Continue;
        
        // Try exact match first, then strategy match
        PropName := Prop.Name;
        if not JsonObj.Contains(PropName) then
        begin
          PropName := TJsonUtils.ApplyCaseStyle(Prop.Name, FOptions.FNamingStrategy);
        end;
        
        if JsonObj.Contains(PropName) then
        begin
          try
            case Prop.PropertyType.TypeKind of
              tkInteger: Prop.SetValue(TObject(Entity), JsonObj.GetInteger(PropName));
              tkInt64: Prop.SetValue(TObject(Entity), JsonObj.GetInt64(PropName));
              tkFloat: 
                if Prop.PropertyType.Handle = TypeInfo(TDateTime) then
                  Prop.SetValue(TObject(Entity), StrToDateTimeDef(JsonObj.GetString(PropName), 0))
                else
                  Prop.SetValue(TObject(Entity), JsonObj.GetDouble(PropName));
              tkString, tkUString, tkWString, tkLString:
                Prop.SetValue(TObject(Entity), JsonObj.GetString(PropName));
              tkEnumeration:
                if Prop.PropertyType.Handle = TypeInfo(Boolean) then
                  Prop.SetValue(TObject(Entity), JsonObj.GetBoolean(PropName))
                else
                  Prop.SetValue(TObject(Entity), TValue.FromOrdinal(Prop.PropertyType.Handle, JsonObj.GetInteger(PropName)));
            end;
          except
            // Ignore conversion errors for individual properties
          end;
        end;
      end;
      
      DbCtx.Entities<T>.Add(Entity);
      DbCtx.SaveChanges;
      
      // Get ID for response
      IdProp := Typ.GetProperty('Id');
      if IdProp <> nil then
        IdValue := IdProp.GetValue(TObject(Entity)).AsInteger
      else
        IdValue := 0;
      
      Result := Results.Created(FPath + '/' + IntToStr(IdValue), EntityToJson(Entity));
    finally
      Ctx.Free;
    end;
  except
    on E: Exception do
      Result := Results.StatusCode(500, Format('{"error":"%s"}', [EscapeJsonString(E.Message)]));
  end;
end;

function TDataApiHandler<T>.HandlePut(const Context: IHttpContext): IResult;
var
  DbCtx: TDbContext;
  IdStr: string;
  Id: Integer;
  Entity: T;
  Ctx: TRttiContext;
  Typ: TRttiType;
  Prop: TRttiProperty;
  Stream: TStream;
  JsonString: string;
  Bytes: TBytes;
  JsonNode: IDextJsonNode;
  JsonObj: IDextJsonObject;
  PropName: string;
  AuthResult: IResult;
  Map: TEntityMap;
  PropMap: TPropertyMap;
begin
  // Authorization check
  AuthResult := CheckAuthorization(Context, True);  // Write operation
  if AuthResult <> nil then
    Exit(AuthResult);
    
  try
    if not Context.Request.RouteParams.TryGetValue('id', IdStr) then
      Exit(Results.BadRequest('{"error":"Missing id parameter"}'));
      
    if not TryStrToInt(IdStr, Id) then
      Exit(Results.BadRequest('{"error":"Invalid id format"}'));
    
    DbCtx := GetDbContext(Context);
    Entity := DbCtx.Entities<T>.Find(Id);
    
    if Entity = nil then
      Exit(Results.NotFound(Format('{"error":"Entity with id %d not found"}', [Id])));
    
    // Read JSON body
    Stream := Context.Request.Body;
    if (Stream = nil) or (Stream.Size = 0) then
      Exit(Results.BadRequest('{"error":"Request body is empty"}'));
    
    Stream.Position := 0;
    SetLength(Bytes, Stream.Size);
    Stream.ReadBuffer(Bytes[0], Stream.Size);
    JsonString := TEncoding.UTF8.GetString(Bytes);
    
    // Parse JSON
    JsonNode := TDextJson.Provider.Parse(JsonString);
    if (JsonNode = nil) or (JsonNode.GetNodeType <> jntObject) then
      Exit(Results.BadRequest('{"error":"Invalid JSON in request body"}'));
    
    JsonObj := JsonNode as IDextJsonObject;
    
    // Update entity properties from JSON
    Ctx := TRttiContext.Create;
    try
      Typ := Ctx.GetType(TypeInfo(T));
      Map := TModelBuilder.Instance.GetMap(TypeInfo(T));
      
      for Prop in Typ.GetProperties do
      begin
        if not Prop.IsWritable then
          Continue;
        if SameText(Prop.Name, 'Id') then
          Continue; // Don't update ID
          
        // Check for mapping ignore
        if Map.Properties.TryGetValue(Prop.Name, PropMap) and PropMap.IsIgnored then
          Continue;
        
        // Try exact match first, then strategy match
        PropName := Prop.Name;
        if not JsonObj.Contains(PropName) then
        begin
          // Try exact match first, then strategy match
          PropName := Prop.Name;
          if not JsonObj.Contains(PropName) then
          begin
            PropName := TJsonUtils.ApplyCaseStyle(Prop.Name, FOptions.FNamingStrategy);
          end;
        
          if JsonObj.Contains(PropName) then
          begin
            try
              case Prop.PropertyType.TypeKind of
                tkInteger: Prop.SetValue(TObject(Entity), JsonObj.GetInteger(PropName));
                tkInt64: Prop.SetValue(TObject(Entity), JsonObj.GetInt64(PropName));
                tkFloat:
                  if Prop.PropertyType.Handle = TypeInfo(TDateTime) then
                    Prop.SetValue(TObject(Entity), StrToDateTimeDef(JsonObj.GetString(PropName), 0))
                  else
                    Prop.SetValue(TObject(Entity), JsonObj.GetDouble(PropName));
                tkString, tkUString, tkWString, tkLString:
                  Prop.SetValue(TObject(Entity), JsonObj.GetString(PropName));
                tkEnumeration:
                  if Prop.PropertyType.Handle = TypeInfo(Boolean) then
                    Prop.SetValue(TObject(Entity), JsonObj.GetBoolean(PropName))
                  else
                    Prop.SetValue(TObject(Entity), TValue.FromOrdinal(Prop.PropertyType.Handle, JsonObj.GetInteger(PropName)));
              end;
            except
              // Ignore conversion errors
            end;
          end;
        end;
      end;
    finally
      Ctx.Free;
    end;
    
    DbCtx.Entities<T>.Update(Entity);
    DbCtx.SaveChanges;
    
    Result := Results.Json(EntityToJson(Entity));
  except
    on E: Exception do
      Result := Results.StatusCode(500, Format('{"error":"%s"}', [EscapeJsonString(E.Message)]));
  end;
end;

function TDataApiHandler<T>.HandleDelete(const Context: IHttpContext): IResult;
var
  DbCtx: TDbContext;
  IdStr: string;
  Id: Integer;
  Entity: T;
  AuthResult: IResult;
begin
  // Authorization check
  AuthResult := CheckAuthorization(Context, True);  // Write operation
  if AuthResult <> nil then
    Exit(AuthResult);
    
  try
    if not Context.Request.RouteParams.TryGetValue('id', IdStr) then
      Exit(Results.BadRequest('{"error":"Missing id parameter"}'));
      
    if not TryStrToInt(IdStr, Id) then
      Exit(Results.BadRequest('{"error":"Invalid id format"}'));
    
    DbCtx := GetDbContext(Context);
    Entity := DbCtx.Entities<T>.Find(Id);
    
    if Entity = nil then
      Exit(Results.NotFound(Format('{"error":"Entity with id %d not found"}', [Id])));
    
    DbCtx.Entities<T>.Remove(Entity);
    DbCtx.SaveChanges;
    
    Result := Results.Ok(Format('{"deleted":true,"id":%d}', [Id]));
  except
    on E: Exception do
      Result := Results.StatusCode(500, Format('{"error":"%s"}', [EscapeJsonString(E.Message)]));
  end;
end;

// Option A: Explicit DbContext parameter
class procedure TDataApiHandler<T>.Map(
  const ABuilder: IApplicationBuilder; 
  const APath: string;
  ADbContext: TDbContext);
var
  Options: TDataApiOptions<T>;
  Handler: TDataApiHandler<T>;
begin
  Options := TDataApiOptions<T>.Create;
  Handler := TDataApiHandler<T>.Create(APath, Options, ADbContext);
  Handler.RegisterRoutes(ABuilder);
  
  // Register handler for disposal when host shuts down
  ABuilder.RegisterForDisposal(Handler);
end;

class procedure TDataApiHandler<T>.Map(
  const ABuilder: IApplicationBuilder; 
  const APath: string; 
  ADbContext: TDbContext;
  AOptions: TDataApiOptions<T>);
var
  Handler: TDataApiHandler<T>;
begin
  if AOptions = nil then
    AOptions := TDataApiOptions<T>.Create;

  Handler := TDataApiHandler<T>.Create(APath, AOptions, ADbContext);
  Handler.RegisterRoutes(ABuilder);
  
  // Register handler for disposal when host shuts down
  ABuilder.RegisterForDisposal(Handler);
end;

// Option B: Resolve DbContext from DI
class procedure TDataApiHandler<T>.Map(
  const ABuilder: IApplicationBuilder; 
  const APath: string);
var
  Options: TDataApiOptions<T>;
  Handler: TDataApiHandler<T>;
begin
  Options := TDataApiOptions<T>.Create;
  Handler := TDataApiHandler<T>.Create(APath, Options, nil);
  Handler.RegisterRoutes(ABuilder);
  
  // Register handler for disposal when host shuts down
  ABuilder.RegisterForDisposal(Handler);
end;

class procedure TDataApiHandler<T>.Map(const ABuilder: IApplicationBuilder; const APath: string; AOptions: TDataApiOptions<T>);
begin
  Map(ABuilder, APath, nil, AOptions);
end;

end.
