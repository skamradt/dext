unit Dext;

interface

uses
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  System.Generics.Collections,
  Dext.Core.WebApplication,
  Dext.Http.Interfaces,
  Dext.DI.Interfaces,
  Dext.Core.Controllers,
  Dext.Core.Routing,
  Dext.Configuration.Interfaces,
  Dext.Options,
  Dext.Http.Results,
  Dext.Core.ControllerScanner,
  Dext.HealthChecks,
  Dext.Hosting.BackgroundService,
  Dext.Options.Extensions,
  Dext.Http.Core,
  Dext.Http.Cors,
  Dext.Auth.Middleware,
  Dext.Http.StaticFiles,
  Dext.Core.ModelBinding,
  Dext.Validation,
  Dext.OpenAPI.Attributes,
  Dext.Auth.JWT,
  Dext.Auth.Attributes,
  Dext.Filters,
  Dext.Filters.BuiltIn,
  Dext.Core.CancellationToken;

type
  // ===========================================================================
  // 🏷️ Aliases for Common Types (Facade)
  // ===========================================================================
  
  // Core
  IWebApplication = Dext.Http.Interfaces.IWebApplication;
  TDextApplication = Dext.Core.WebApplication.TDextApplication;
  
  // HTTP
  IHttpContext = Dext.Http.Interfaces.IHttpContext;
  IHttpRequest = Dext.Http.Interfaces.IHttpRequest;
  IHttpResponse = Dext.Http.Interfaces.IHttpResponse;
  IMiddleware = Dext.Http.Interfaces.IMiddleware;
  TRequestDelegate = Dext.Http.Interfaces.TRequestDelegate;
  TStaticHandler = Dext.Http.Interfaces.TStaticHandler;
  
  // DI
  IServiceCollection = Dext.DI.Interfaces.IServiceCollection;
  IServiceProvider = Dext.DI.Interfaces.IServiceProvider;
  TServiceType = Dext.DI.Interfaces.TServiceType;
  
  // Configuration
  IConfiguration = Dext.Configuration.Interfaces.IConfiguration;
  IConfigurationSection = Dext.Configuration.Interfaces.IConfigurationSection;
  
  // Results
  IResult = Dext.Http.Interfaces.IResult;
  Results = Dext.Http.Results.Results;

  // Middleware
  THealthCheckMiddleware = Dext.HealthChecks.THealthCheckMiddleware;
  
  // CORS
  TCorsOptions = Dext.Http.Cors.TCorsOptions;
  TCorsBuilder = Dext.Http.Cors.TCorsBuilder;
  
  // Auth
  TJwtAuthenticationOptions = Dext.Auth.Middleware.TJwtAuthenticationOptions;
  TJwtAuthenticationMiddleware = Dext.Auth.Middleware.TJwtAuthenticationMiddleware;
  
  // Static Files
  TStaticFileOptions = Dext.Http.StaticFiles.TStaticFileOptions;

  // Attributes & Routing
  DextControllerAttribute = Dext.Core.Routing.DextControllerAttribute;
  DextGetAttribute = Dext.Core.Routing.DextGetAttribute;
  DextPostAttribute = Dext.Core.Routing.DextPostAttribute;
  DextPutAttribute = Dext.Core.Routing.DextPutAttribute;
  DextDeleteAttribute = Dext.Core.Routing.DextDeleteAttribute;
  
  // Model Binding & Validation
  FromQueryAttribute = Dext.Core.ModelBinding.FromQueryAttribute;
  FromRouteAttribute = Dext.Core.ModelBinding.FromRouteAttribute;
  FromBodyAttribute = Dext.Core.ModelBinding.FromBodyAttribute;
  RequiredAttribute = Dext.Validation.RequiredAttribute;
  StringLengthAttribute = Dext.Validation.StringLengthAttribute;
  
  // OpenAPI
  SwaggerAuthorizeAttribute = Dext.OpenAPI.Attributes.SwaggerAuthorizeAttribute;
  
  // Auth
  AllowAnonymousAttribute = Dext.Auth.Attributes.AllowAnonymousAttribute;
  TJwtTokenHandler = Dext.Auth.JWT.TJwtTokenHandler;
  TClaim = Dext.Auth.JWT.TClaim;
  
  // Filters
  ActionFilterAttribute = Dext.Filters.ActionFilterAttribute;
  IActionExecutingContext = Dext.Filters.IActionExecutingContext;
  IActionExecutedContext = Dext.Filters.IActionExecutedContext;
  LogActionAttribute = Dext.Filters.BuiltIn.LogActionAttribute;
  RequireHeaderAttribute = Dext.Filters.BuiltIn.RequireHeaderAttribute;
  ResponseCacheAttribute = Dext.Filters.BuiltIn.ResponseCacheAttribute;
  ValidateModelAttribute = Dext.Filters.BuiltIn.ValidateModelAttribute;
  AddHeaderAttribute = Dext.Filters.BuiltIn.AddHeaderAttribute;

  // Health Checks & Background Services
  IHealthCheck = Dext.HealthChecks.IHealthCheck;
  THealthCheckResult = Dext.HealthChecks.THealthCheckResult;
  TBackgroundService = Dext.Hosting.BackgroundService.TBackgroundService;
  ICancellationToken = Dext.Core.CancellationToken.ICancellationToken;

  // ===========================================================================
  // 🛠️ Fluent Helpers & Wrappers
  // ===========================================================================

  /// <summary>
  ///   Helper for TDextServices to add framework features.
  /// </summary>
  TDextServicesHelper = record helper for TDextServices
  public
    function AddControllers: TDextServices;
    function AddHealthChecks: THealthCheckBuilder;
    function AddBackgroundServices: TBackgroundServiceBuilder;
    
    function Configure<T: class, constructor>(Configuration: IConfiguration): TDextServices; overload;
    function Configure<T: class, constructor>(Section: IConfigurationSection): TDextServices; overload;
  end;

  /// <summary>
  ///   Helper for TDextAppBuilder to provide factory methods and extensions.
  /// </summary>
  TDextAppBuilderHelper = record helper for TDextAppBuilder
  public
    // Factory Methods
    function CreateCorsOptions: TCorsOptions;
    function CreateJwtOptions(const Secret: string): TJwtAuthenticationOptions;
    function CreateStaticFileOptions: TStaticFileOptions;
    
    // Extensions
    function UseCors(const AOptions: TCorsOptions): TDextAppBuilder; overload;
    function UseCors(AConfigurator: TProc<TCorsBuilder>): TDextAppBuilder; overload;
    function UseJwtAuthentication(const AOptions: TJwtAuthenticationOptions): TDextAppBuilder;
    function UseStaticFiles(const AOptions: TStaticFileOptions): TDextAppBuilder; overload;
    function UseStaticFiles(const ARootPath: string): TDextAppBuilder; overload;
    
    // Core Forwarding
    function UseMiddleware(AMiddleware: TClass): TDextAppBuilder;
    function MapGet(const Path: string; Handler: TStaticHandler): TDextAppBuilder;
    function MapPost(const Path: string; Handler: TStaticHandler): TDextAppBuilder;
    function MapPut(const Path: string; Handler: TStaticHandler): TDextAppBuilder;
    function MapDelete(const Path: string; Handler: TStaticHandler): TDextAppBuilder;
    
    function Build: TRequestDelegate;
  end;

implementation

{ TDextServicesHelper }

function TDextServicesHelper.AddControllers: TDextServices;
var
  Scanner: IControllerScanner;
begin
  Scanner := TControllerScanner.Create(nil);
  Scanner.RegisterServices(Self.Unwrap);
  Result := Self;
end;

function TDextServicesHelper.AddHealthChecks: THealthCheckBuilder;
begin
  Result := THealthCheckBuilder.Create(Self.Unwrap);
end;

function TDextServicesHelper.AddBackgroundServices: TBackgroundServiceBuilder;
begin
  Result := TBackgroundServiceBuilder.Create(Self.Unwrap);
end;

function TDextServicesHelper.Configure<T>(Configuration: IConfiguration): TDextServices;
begin
  TOptionsServiceCollectionExtensions.Configure<T>(Self.Unwrap, Configuration);
  Result := Self;
end;

function TDextServicesHelper.Configure<T>(Section: IConfigurationSection): TDextServices;
begin
  TOptionsServiceCollectionExtensions.Configure<T>(Self.Unwrap, Section);
  Result := Self;
end;

{ TDextAppBuilderHelper }

function TDextAppBuilderHelper.CreateCorsOptions: TCorsOptions;
begin
  Result := TCorsOptions.Create;
end;

function TDextAppBuilderHelper.CreateJwtOptions(const Secret: string): TJwtAuthenticationOptions;
begin
  Result := TJwtAuthenticationOptions.Default(Secret);
end;

function TDextAppBuilderHelper.CreateStaticFileOptions: TStaticFileOptions;
begin
  Result := TStaticFileOptions.Create;
end;

function TDextAppBuilderHelper.UseCors(const AOptions: TCorsOptions): TDextAppBuilder;
begin
  TApplicationBuilderCorsExtensions.UseCors(Self.Unwrap, AOptions);
  Result := Self;
end;

function TDextAppBuilderHelper.UseCors(AConfigurator: TProc<TCorsBuilder>): TDextAppBuilder;
begin
  TApplicationBuilderCorsExtensions.UseCors(Self.Unwrap, AConfigurator);
  Result := Self;
end;

function TDextAppBuilderHelper.UseJwtAuthentication(const AOptions: TJwtAuthenticationOptions): TDextAppBuilder;
begin
  Self.Unwrap.UseMiddleware(TJwtAuthenticationMiddleware.Create(AOptions));
  Result := Self;
end;

function TDextAppBuilderHelper.UseStaticFiles(const AOptions: TStaticFileOptions): TDextAppBuilder;
begin
  TApplicationBuilderStaticFilesExtensions.UseStaticFiles(Self.Unwrap, AOptions);
  Result := Self;
end;

function TDextAppBuilderHelper.UseStaticFiles(const ARootPath: string): TDextAppBuilder;
begin
  TApplicationBuilderStaticFilesExtensions.UseStaticFiles(Self.Unwrap, ARootPath);
  Result := Self;
end;

function TDextAppBuilderHelper.UseMiddleware(AMiddleware: TClass): TDextAppBuilder;
begin
  Self.Unwrap.UseMiddleware(AMiddleware);
  Result := Self;
end;

function TDextAppBuilderHelper.MapGet(const Path: string; Handler: TStaticHandler): TDextAppBuilder;
begin
  Self.Unwrap.MapGet(Path, Handler);
  Result := Self;
end;

function TDextAppBuilderHelper.MapPost(const Path: string; Handler: TStaticHandler): TDextAppBuilder;
begin
  Self.Unwrap.MapPost(Path, Handler);
  Result := Self;
end;

function TDextAppBuilderHelper.MapPut(const Path: string; Handler: TStaticHandler): TDextAppBuilder;
begin
  Self.Unwrap.MapPut(Path, Handler);
  Result := Self;
end;

function TDextAppBuilderHelper.MapDelete(const Path: string; Handler: TStaticHandler): TDextAppBuilder;
begin
  Self.Unwrap.MapDelete(Path, Handler);
  Result := Self;
end;

function TDextAppBuilderHelper.Build: TRequestDelegate;
begin
  Result := Self.Unwrap.Build;
end;

end.
