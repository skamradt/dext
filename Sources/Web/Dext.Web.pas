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
{  Created: 2025-12-10                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.Web;

interface

uses
  System.SysUtils,
  Dext,
  Dext.Web.ResponseHelper,
  // {BEGIN_DEXT_USES}
  // Generated Uses
  Dext.Auth.Attributes,
  Dext.Auth.BasicAuth,
  Dext.Auth.Identity,
  Dext.Auth.JWT,
  Dext.Auth.Middleware,
  Dext.DI.Middleware,
  Dext.Filters.BuiltIn,
  Dext.Filters,
  Dext.HealthChecks,
  Dext.OpenAPI.Attributes,
  Dext.OpenAPI.Extensions,
  Dext.OpenAPI.Fluent,
  Dext.OpenAPI.Generator,
  Dext.OpenAPI.Types,
  Dext.RateLimiting.Core,
  Dext.RateLimiting.Limiters,
  Dext.RateLimiting,
  Dext.RateLimiting.Policy,
  Dext.Swagger.Middleware,
  Dext.Web.Core,
  Dext.Web.Cors,
  Dext.Web.DataApi,
  Dext.Web.Extensions,
  Dext.Web.Formatters.Interfaces,
  Dext.Web.Formatters.Json,
  Dext.Web.Formatters.Selector,
  Dext.Web.Indy,
  Dext.Web.Indy.Server,
  Dext.Web.Indy.Types,
  Dext.Web.Injection,
  Dext.Web.Interfaces,
  Dext.Web.Middleware.Extensions,
  Dext.Web.Middleware.Logging,
  Dext.Web.Middleware,
  Dext.Web.MultiTenancy,
  Dext.Web.Pipeline,
  Dext.Web.Results,
  Dext.Web.Routing,
  Dext.Web.RoutingMiddleware,
  Dext.Web.StaticFiles,
  Dext.Http.StatusCodes,
  Dext.Web.Versioning,
  Dext.WebHost,
  Dext.Caching,
  Dext.Caching.Redis,
  Dext.Web.ApplicationBuilder.Extensions,
  Dext.Web.WebApplication,
  Dext.Web.Indy.SSL.Interfaces,
  Dext.Web.Indy.SSL.OpenSSL,
  Dext.Web.Indy.SSL.Taurus,
  Dext.Web.Middleware.Compression,
  Dext.Web.Middleware.StartupLock,
  Dext.Web.Controllers,
  Dext.Web.ControllerScanner,
  Dext.Web.HandlerInvoker,
  Dext.Web.ModelBinding.Extensions,
  Dext.Web.ModelBinding,
  Dext.Web.Routing.Attributes
  // {END_DEXT_USES}
  ;

type
  // ===========================================================================
  // ??? Aliases for Common Web Types
  // ===========================================================================
  
  // {BEGIN_DEXT_ALIASES}
  // Generated Aliases

  // Dext.Auth.Attributes
  AuthorizeAttribute = Dext.Auth.Attributes.AuthorizeAttribute;
  AllowAnonymousAttribute = Dext.Auth.Attributes.AllowAnonymousAttribute;

  // Dext.Auth.BasicAuth
  TBasicAuthOptions = Dext.Auth.BasicAuth.TBasicAuthOptions;
  TBasicAuthValidateFunc = Dext.Auth.BasicAuth.TBasicAuthValidateFunc;
  TBasicAuthValidateWithRolesFunc = Dext.Auth.BasicAuth.TBasicAuthValidateWithRolesFunc;
  TBasicAuthMiddleware = Dext.Auth.BasicAuth.TBasicAuthMiddleware;
  TApplicationBuilderBasicAuthExtensions = Dext.Auth.BasicAuth.TApplicationBuilderBasicAuthExtensions;

  // Dext.Auth.Identity
  IIdentity = Dext.Auth.Identity.IIdentity;
  IClaimsPrincipal = Dext.Auth.Identity.IClaimsPrincipal;
  TClaimsIdentity = Dext.Auth.Identity.TClaimsIdentity;
  TClaimsPrincipal = Dext.Auth.Identity.TClaimsPrincipal;
  TClaimTypes = Dext.Auth.Identity.TClaimTypes;
  IClaimsBuilder = Dext.Auth.Identity.IClaimsBuilder;
  TClaimsBuilder = Dext.Auth.Identity.TClaimsBuilder;

  // Dext.Auth.JWT
  TClaim = Dext.Auth.JWT.TClaim;
  TJwtValidationResult = Dext.Auth.JWT.TJwtValidationResult;
  IJwtTokenHandler = Dext.Auth.JWT.IJwtTokenHandler;
  TJwtOptions = Dext.Auth.JWT.TJwtOptions;
  TJwtOptionsBuilder = Dext.Auth.JWT.TJwtOptionsBuilder;
  TJwtOptionsHelper = Dext.Auth.JWT.TJwtOptionsHelper;
  TJwtTokenHandler = Dext.Auth.JWT.TJwtTokenHandler;

  // Dext.Auth.Middleware
  TJwtAuthenticationMiddleware = Dext.Auth.Middleware.TJwtAuthenticationMiddleware;
  TApplicationBuilderJwtExtensions = Dext.Auth.Middleware.TApplicationBuilderJwtExtensions;

  // Dext.Caching
  ICacheStore = Dext.Caching.ICacheStore;
  TCacheEntry = Dext.Caching.TCacheEntry;
  TMemoryCacheStore = Dext.Caching.TMemoryCacheStore;
  TResponseCacheOptions = Dext.Caching.TResponseCacheOptions;
  TResponseCaptureWrapper = Dext.Caching.TResponseCaptureWrapper;
  TResponseCacheMiddleware = Dext.Caching.TResponseCacheMiddleware;
  TResponseCacheBuilder = Dext.Caching.TResponseCacheBuilder;
  TApplicationBuilderCacheExtensions = Dext.Caching.TApplicationBuilderCacheExtensions;
  TResponseCacheOptionsHelper = Dext.Caching.TResponseCacheOptionsHelper;

  // Dext.Caching.Redis
  TRedisCacheStore = Dext.Caching.Redis.TRedisCacheStore;

  // Dext.DI.Middleware
  TServiceScopeMiddleware = Dext.DI.Middleware.TServiceScopeMiddleware;
  TApplicationBuilderScopeExtensions = Dext.DI.Middleware.TApplicationBuilderScopeExtensions;

  // Dext.Filters
  TActionDescriptor = Dext.Filters.TActionDescriptor;
  IActionExecutingContext = Dext.Filters.IActionExecutingContext;
  IActionExecutedContext = Dext.Filters.IActionExecutedContext;
  IActionFilter = Dext.Filters.IActionFilter;
  ActionFilterAttribute = Dext.Filters.ActionFilterAttribute;
  TActionExecutingContext = Dext.Filters.TActionExecutingContext;
  TActionExecutedContext = Dext.Filters.TActionExecutedContext;

  // Dext.Filters.BuiltIn
  LogActionAttribute = Dext.Filters.BuiltIn.LogActionAttribute;
  RequireHeaderAttribute = Dext.Filters.BuiltIn.RequireHeaderAttribute;
  ResponseCacheAttribute = Dext.Filters.BuiltIn.ResponseCacheAttribute;
  ValidateModelAttribute = Dext.Filters.BuiltIn.ValidateModelAttribute;
  AddHeaderAttribute = Dext.Filters.BuiltIn.AddHeaderAttribute;

  // Dext.HealthChecks
  THealthStatus = Dext.HealthChecks.THealthStatus;
  THealthCheckResult = Dext.HealthChecks.THealthCheckResult;
  IHealthCheck = Dext.HealthChecks.IHealthCheck;
  IHealthCheckService = Dext.HealthChecks.IHealthCheckService;
  THealthCheckService = Dext.HealthChecks.THealthCheckService;
  THealthCheckMiddleware = Dext.HealthChecks.THealthCheckMiddleware;
  THealthCheckBuilder = Dext.HealthChecks.THealthCheckBuilder;

  // Dext.OpenAPI.Attributes
  SwaggerIgnoreAttribute = Dext.OpenAPI.Attributes.SwaggerIgnoreAttribute;
  SwaggerOperationAttribute = Dext.OpenAPI.Attributes.SwaggerOperationAttribute;
  SwaggerResponseAttribute = Dext.OpenAPI.Attributes.SwaggerResponseAttribute;
  SwaggerSchemaAttribute = Dext.OpenAPI.Attributes.SwaggerSchemaAttribute;
  SwaggerIgnorePropertyAttribute = Dext.OpenAPI.Attributes.SwaggerIgnorePropertyAttribute;
  SwaggerPropertyAttribute = Dext.OpenAPI.Attributes.SwaggerPropertyAttribute;
  SwaggerRequiredAttribute = Dext.OpenAPI.Attributes.SwaggerRequiredAttribute;
  SwaggerExampleAttribute = Dext.OpenAPI.Attributes.SwaggerExampleAttribute;
  SwaggerFormatAttribute = Dext.OpenAPI.Attributes.SwaggerFormatAttribute;
  SwaggerTagAttribute = Dext.OpenAPI.Attributes.SwaggerTagAttribute;
  TSwaggerParamLocation = Dext.OpenAPI.Attributes.TSwaggerParamLocation;
  SwaggerParamAttribute = Dext.OpenAPI.Attributes.SwaggerParamAttribute;
  SwaggerAuthorizeAttribute = Dext.OpenAPI.Attributes.SwaggerAuthorizeAttribute;

  // Dext.OpenAPI.Extensions
  TEndpointMetadataExtensions = Dext.OpenAPI.Extensions.TEndpointMetadataExtensions;

  // Dext.OpenAPI.Fluent
  TEndpointBuilder = Dext.OpenAPI.Fluent.TEndpointBuilder;
  SwaggerEndpoint = Dext.OpenAPI.Fluent.SwaggerEndpoint;

  // Dext.OpenAPI.Generator
  TOpenAPIOptions = Dext.OpenAPI.Generator.TOpenAPIOptions;
  TOpenAPIGenerator = Dext.OpenAPI.Generator.TOpenAPIGenerator;

  // Dext.OpenAPI.Types
  TOpenAPIDataType = Dext.OpenAPI.Types.TOpenAPIDataType;
  TOpenAPISchema = Dext.OpenAPI.Types.TOpenAPISchema;
  TOpenAPIParameterLocation = Dext.OpenAPI.Types.TOpenAPIParameterLocation;
  TOpenAPIParameter = Dext.OpenAPI.Types.TOpenAPIParameter;
  TOpenAPIRequestBody = Dext.OpenAPI.Types.TOpenAPIRequestBody;
  TOpenAPIResponse = Dext.OpenAPI.Types.TOpenAPIResponse;
  TOpenAPIOperation = Dext.OpenAPI.Types.TOpenAPIOperation;
  TOpenAPIPathItem = Dext.OpenAPI.Types.TOpenAPIPathItem;
  TOpenAPIServer = Dext.OpenAPI.Types.TOpenAPIServer;
  TOpenAPIContact = Dext.OpenAPI.Types.TOpenAPIContact;
  TOpenAPILicense = Dext.OpenAPI.Types.TOpenAPILicense;
  TOpenAPIInfo = Dext.OpenAPI.Types.TOpenAPIInfo;
  TSecuritySchemeType = Dext.OpenAPI.Types.TSecuritySchemeType;
  TApiKeyLocation = Dext.OpenAPI.Types.TApiKeyLocation;
  TOpenAPISecurityScheme = Dext.OpenAPI.Types.TOpenAPISecurityScheme;
  TOpenAPIDocument = Dext.OpenAPI.Types.TOpenAPIDocument;

  // Dext.RateLimiting
  TRateLimitMiddleware = Dext.RateLimiting.TRateLimitMiddleware;
  TApplicationBuilderRateLimitExtensions = Dext.RateLimiting.TApplicationBuilderRateLimitExtensions;

  // Dext.RateLimiting.Core
  TPartitionKeyResolver = Dext.RateLimiting.Core.TPartitionKeyResolver;
  TRateLimiterType = Dext.RateLimiting.Core.TRateLimiterType;
  TPartitionStrategy = Dext.RateLimiting.Core.TPartitionStrategy;
  TRateLimitResult = Dext.RateLimiting.Core.TRateLimitResult;
  IRateLimiter = Dext.RateLimiting.Core.IRateLimiter;
  TRateLimitConfig = Dext.RateLimiting.Core.TRateLimitConfig;

  // Dext.RateLimiting.Limiters
  TFixedWindowLimiter = Dext.RateLimiting.Limiters.TFixedWindowLimiter;
  TSlidingWindowLimiter = Dext.RateLimiting.Limiters.TSlidingWindowLimiter;
  TTokenBucketLimiter = Dext.RateLimiting.Limiters.TTokenBucketLimiter;
  TConcurrencyLimiter = Dext.RateLimiting.Limiters.TConcurrencyLimiter;

  // Dext.RateLimiting.Policy
  TRateLimitPolicy = Dext.RateLimiting.Policy.TRateLimitPolicy;

  // Dext.Swagger.Middleware
  TSwaggerMiddleware = Dext.Swagger.Middleware.TSwaggerMiddleware;
  TSwaggerExtensions = Dext.Swagger.Middleware.TSwaggerExtensions;

  // Dext.Web.ApplicationBuilder.Extensions
  TApplicationBuilderExtensions = Dext.Web.ApplicationBuilder.Extensions.TApplicationBuilderExtensions;
  TDextAppBuilderHelper = Dext.Web.ApplicationBuilder.Extensions.TDextAppBuilderHelper;

  // Dext.Web.Controllers
  IHttpHandler = Dext.Web.Controllers.IHttpHandler;
  TControllerClass = Dext.Web.Controllers.TControllerClass;
  TController = Dext.Web.Controllers.TController;

  // Dext.Web.ControllerScanner
  TControllerMethod = Dext.Web.ControllerScanner.TControllerMethod;
  TControllerInfo = Dext.Web.ControllerScanner.TControllerInfo;
  TCachedMethod = Dext.Web.ControllerScanner.TCachedMethod;
  IControllerScanner = Dext.Web.ControllerScanner.IControllerScanner;
  TControllerScanner = Dext.Web.ControllerScanner.TControllerScanner;

  // Dext.Web.Core
  TMiddlewareRegistration = Dext.Web.Core.TMiddlewareRegistration;
  TAnonymousMiddleware = Dext.Web.Core.TAnonymousMiddleware;
  TApplicationBuilder = Dext.Web.Core.TApplicationBuilder;
  TMiddleware = Dext.Web.Core.TMiddleware;

  // Dext.Web.Cors
  TCorsOptions = Dext.Web.Cors.TCorsOptions;
  TStringArrayHelper = Dext.Web.Cors.TStringArrayHelper;
  TCorsMiddleware = Dext.Web.Cors.TCorsMiddleware;
  TCorsBuilder = Dext.Web.Cors.TCorsBuilder;
  TApplicationBuilderCorsExtensions = Dext.Web.Cors.TApplicationBuilderCorsExtensions;
  TCorsOptionsHelper = Dext.Web.Cors.TCorsOptionsHelper;

  // Dext.Web.DataApi
  TApiMethod = Dext.Web.DataApi.TApiMethod;
  TApiMethods = Dext.Web.DataApi.TApiMethods;
  // TDataApiOptions<T> = Dext.Web.DataApi.TDataApiOptions<T>;
  // TDataApiHandler<T> = Dext.Web.DataApi.TDataApiHandler<T>;

  // Dext.Web.Extensions
  TWebDIHelpers = Dext.Web.Extensions.TWebDIHelpers;
  TWebRouteHelpers = Dext.Web.Extensions.TWebRouteHelpers;
  TDextServiceCollectionExtensions = Dext.Web.Extensions.TDextServiceCollectionExtensions;
  TOutputFormatterRegistry = Dext.Web.Extensions.TOutputFormatterRegistry;

  // Dext.Web.Formatters.Interfaces
  IOutputFormatterContext = Dext.Web.Formatters.Interfaces.IOutputFormatterContext;
  IOutputFormatter = Dext.Web.Formatters.Interfaces.IOutputFormatter;
  IOutputFormatterSelector = Dext.Web.Formatters.Interfaces.IOutputFormatterSelector;
  IOutputFormatterRegistry = Dext.Web.Formatters.Interfaces.IOutputFormatterRegistry;

  // Dext.Web.Formatters.Json
  TJsonOutputFormatter = Dext.Web.Formatters.Json.TJsonOutputFormatter;

  // Dext.Web.Formatters.Selector
  TMediaTypeHeaderValue = Dext.Web.Formatters.Selector.TMediaTypeHeaderValue;
  TDefaultOutputFormatterSelector = Dext.Web.Formatters.Selector.TDefaultOutputFormatterSelector;

  // Dext.Web.HandlerInvoker
  THandlerInvoker = Dext.Web.HandlerInvoker.THandlerInvoker;
  // THandlerProc<T> = Dext.Web.HandlerInvoker.THandlerProc<T>;
  // THandlerProc<T> = Dext.Web.HandlerInvoker.THandlerProc<T>;
  // THandlerProc<T> = Dext.Web.HandlerInvoker.THandlerProc<T>;
  // THandlerResultFunc<T> = Dext.Web.HandlerInvoker.THandlerResultFunc<T>;
  // THandlerResultFunc<T> = Dext.Web.HandlerInvoker.THandlerResultFunc<T>;
  // THandlerResultFunc<T> = Dext.Web.HandlerInvoker.THandlerResultFunc<T>;
  // THandlerResultFunc<T> = Dext.Web.HandlerInvoker.THandlerResultFunc<T>;
  // THandlerFunc<T> = Dext.Web.HandlerInvoker.THandlerFunc<T>;
  // THandlerFunc<T> = Dext.Web.HandlerInvoker.THandlerFunc<T>;
  // THandlerFunc<T> = Dext.Web.HandlerInvoker.THandlerFunc<T>;
  // THandlerFunc<T> = Dext.Web.HandlerInvoker.THandlerFunc<T>;
  // THandlerProcWithContext<T> = Dext.Web.HandlerInvoker.THandlerProcWithContext<T>;
  // THandlerProcWithContext<T> = Dext.Web.HandlerInvoker.THandlerProcWithContext<T>;
  // THandlerFuncWithContext<T> = Dext.Web.HandlerInvoker.THandlerFuncWithContext<T>;

  // Dext.Web.Indy
  TIndyHttpResponse = Dext.Web.Indy.TIndyHttpResponse;
  TIndyHttpRequest = Dext.Web.Indy.TIndyHttpRequest;
  TIndyHttpContext = Dext.Web.Indy.TIndyHttpContext;

  // Dext.Web.Indy.Server
  TIndyWebServer = Dext.Web.Indy.Server.TIndyWebServer;

  // Dext.Web.Indy.SSL.Interfaces
  IIndySSLHandler = Dext.Web.Indy.SSL.Interfaces.IIndySSLHandler;

  // Dext.Web.Indy.SSL.OpenSSL
  TIndyOpenSSLHandler = Dext.Web.Indy.SSL.OpenSSL.TIndyOpenSSLHandler;

  // Dext.Web.Indy.SSL.Taurus
  TIndyTaurusSSLHandler = Dext.Web.Indy.SSL.Taurus.TIndyTaurusSSLHandler;

  // Dext.Web.Indy.Types
  TIndyFormFile = Dext.Web.Indy.Types.TIndyFormFile;

  // Dext.Web.Injection
  THandlerInjector = Dext.Web.Injection.THandlerInjector;

  // Dext.Web.Interfaces
  IHttpContext = Dext.Web.Interfaces.IHttpContext;
  IHttpRequest = Dext.Web.Interfaces.IHttpRequest;
  IHttpResponse = Dext.Web.Interfaces.IHttpResponse;
  IApplicationBuilder = Dext.Web.Interfaces.IApplicationBuilder;
  IWebHost = Dext.Web.Interfaces.IWebHost;
  IWebHostBuilder = Dext.Web.Interfaces.IWebHostBuilder;
  TRequestDelegate = Dext.Web.Interfaces.TRequestDelegate;
  TStaticHandler = Dext.Web.Interfaces.TStaticHandler;
  TMiddlewareDelegate = Dext.Web.Interfaces.TMiddlewareDelegate;
  TOpenAPIResponseMetadata = Dext.Web.Interfaces.TOpenAPIResponseMetadata;
  TEndpointMetadata = Dext.Web.Interfaces.TEndpointMetadata;
  TCookieOptions = Dext.Web.Interfaces.TCookieOptions;
  IFormFile = Dext.Web.Interfaces.IFormFile;
  IFormFileCollection = Dext.Web.Interfaces.IFormFileCollection;
  IResult = Dext.Web.Interfaces.IResult;
  IMiddleware = Dext.Web.Interfaces.IMiddleware;
  TDextAppBuilder = Dext.Web.Interfaces.TDextAppBuilder;
  IWebApplication = Dext.Web.Interfaces.IWebApplication;
  IStartup = Dext.Web.Interfaces.IStartup;
  TDextWebHost = Dext.Web.Interfaces.TDextWebHost;
  TFormFileCollection = Dext.Web.Interfaces.TFormFileCollection;

  // Dext.Web.Middleware
  EHttpException = Dext.Web.Middleware.EHttpException;
  ENotFoundException = Dext.Web.Middleware.ENotFoundException;
  EUnauthorizedException = Dext.Web.Middleware.EUnauthorizedException;
  EForbiddenException = Dext.Web.Middleware.EForbiddenException;
  EValidationException = Dext.Web.Middleware.EValidationException;
  TExceptionHandlerOptions = Dext.Web.Middleware.TExceptionHandlerOptions;
  TProblemDetails = Dext.Web.Middleware.TProblemDetails;
  TExceptionHandlerMiddleware = Dext.Web.Middleware.TExceptionHandlerMiddleware;
  THttpLoggingOptions = Dext.Web.Middleware.THttpLoggingOptions;
  THttpLoggingMiddleware = Dext.Web.Middleware.THttpLoggingMiddleware;

  // Dext.Web.Middleware.Compression
  TCompressionMiddleware = Dext.Web.Middleware.Compression.TCompressionMiddleware;

  // Dext.Web.Middleware.Extensions
  TApplicationBuilderMiddlewareExtensions = Dext.Web.Middleware.Extensions.TApplicationBuilderMiddlewareExtensions;

  // Dext.Web.Middleware.Logging
  TRequestLoggingMiddleware = Dext.Web.Middleware.Logging.TRequestLoggingMiddleware;

  // Dext.Web.Middleware.StartupLock
  TStartupLockMiddleware = Dext.Web.Middleware.StartupLock.TStartupLockMiddleware;

  // Dext.Web.ModelBinding
  EBindingException = Dext.Web.ModelBinding.EBindingException;
  TBindingSource = Dext.Web.ModelBinding.TBindingSource;
  BindingAttribute = Dext.Web.ModelBinding.BindingAttribute;
  FromBodyAttribute = Dext.Web.ModelBinding.FromBodyAttribute;
  FromQueryAttribute = Dext.Web.ModelBinding.FromQueryAttribute;
  FromRouteAttribute = Dext.Web.ModelBinding.FromRouteAttribute;
  FromHeaderAttribute = Dext.Web.ModelBinding.FromHeaderAttribute;
  FromServicesAttribute = Dext.Web.ModelBinding.FromServicesAttribute;
  IModelBinder = Dext.Web.ModelBinding.IModelBinder;
  TModelBinder = Dext.Web.ModelBinding.TModelBinder;
  TModelBinderHelper = Dext.Web.ModelBinding.TModelBinderHelper;
  IBindingSourceProvider = Dext.Web.ModelBinding.IBindingSourceProvider;
  TBindingSourceProvider = Dext.Web.ModelBinding.TBindingSourceProvider;

  // Dext.Web.ModelBinding.Extensions
  IApplicationBuilderWithModelBinding = Dext.Web.ModelBinding.Extensions.IApplicationBuilderWithModelBinding;
  TApplicationBuilderWithModelBinding = Dext.Web.ModelBinding.Extensions.TApplicationBuilderWithModelBinding;
  TApplicationBuilderModelBindingExtensions = Dext.Web.ModelBinding.Extensions.TApplicationBuilderModelBindingExtensions;

  // Dext.Web.MultiTenancy
  ITenantResolutionStrategy = Dext.Web.MultiTenancy.ITenantResolutionStrategy;
  ITenantStore = Dext.Web.MultiTenancy.ITenantStore;
  TMultiTenancyMiddleware = Dext.Web.MultiTenancy.TMultiTenancyMiddleware;

  // Dext.Web.Pipeline
  IDextPipeline = Dext.Web.Pipeline.IDextPipeline;
  TDextPipeline = Dext.Web.Pipeline.TDextPipeline;

  // Dext.Web.Results
  TResult = Dext.Web.Results.TResult;
  TOutputFormatterContext = Dext.Web.Results.TOutputFormatterContext;
  TJsonResult = Dext.Web.Results.TJsonResult;
  TStatusCodeResult = Dext.Web.Results.TStatusCodeResult;
  TContentResult = Dext.Web.Results.TContentResult;
  TStreamResult = Dext.Web.Results.TStreamResult;
  Results = Dext.Web.Results.Results;
  // TObjectResult<T> = Dext.Web.Results.TObjectResult<T>;

  // Dext.Web.Routing
  TRoutePattern = Dext.Web.Routing.TRoutePattern;
  TRouteDefinition = Dext.Web.Routing.TRouteDefinition;
  IRouteMatcher = Dext.Web.Routing.IRouteMatcher;
  TRouteMatcher = Dext.Web.Routing.TRouteMatcher;
  ERouteException = Dext.Web.Routing.ERouteException;

  // Dext.Web.Routing.Attributes
  DextRouteAttribute = Dext.Web.Routing.Attributes.DextRouteAttribute;
  DextGetAttribute = Dext.Web.Routing.Attributes.DextGetAttribute;
  DextPostAttribute = Dext.Web.Routing.Attributes.DextPostAttribute;
  DextPutAttribute = Dext.Web.Routing.Attributes.DextPutAttribute;
  DextDeleteAttribute = Dext.Web.Routing.Attributes.DextDeleteAttribute;
  DextPatchAttribute = Dext.Web.Routing.Attributes.DextPatchAttribute;
  DextHeadAttribute = Dext.Web.Routing.Attributes.DextHeadAttribute;
  DextOptionsAttribute = Dext.Web.Routing.Attributes.DextOptionsAttribute;
  DextControllerAttribute = Dext.Web.Routing.Attributes.DextControllerAttribute;
  EDextHttpException = Dext.Web.Routing.Attributes.EDextHttpException;

  // Dext.Web.RoutingMiddleware
  TRoutingMiddleware = Dext.Web.RoutingMiddleware.TRoutingMiddleware;

  // Dext.Web.StaticFiles
  TContentTypeProvider = Dext.Web.StaticFiles.TContentTypeProvider;
  TStaticFileOptions = Dext.Web.StaticFiles.TStaticFileOptions;
  TStaticFileMiddleware = Dext.Web.StaticFiles.TStaticFileMiddleware;
  TApplicationBuilderStaticFilesExtensions = Dext.Web.StaticFiles.TApplicationBuilderStaticFilesExtensions;

  // Dext.Http.StatusCodes (moved to Core for shared use by Dext.Web and Dext.Net)
  HttpStatus = Dext.Http.StatusCodes.HttpStatus;

  // Dext.Web.Versioning
  IApiVersionReader = Dext.Web.Versioning.IApiVersionReader;
  TQueryStringApiVersionReader = Dext.Web.Versioning.TQueryStringApiVersionReader;
  THeaderApiVersionReader = Dext.Web.Versioning.THeaderApiVersionReader;
  TCompositeApiVersionReader = Dext.Web.Versioning.TCompositeApiVersionReader;

  // Dext.Web.WebApplication
  TDextApplication = Dext.Web.WebApplication.TDextApplication;

  // Dext.WebHost
  TWebHostBuilder = Dext.WebHost.TWebHostBuilder;

const
  // Dext.HealthChecks
  Healthy = Dext.HealthChecks.Healthy;
  Degraded = Dext.HealthChecks.Degraded;
  Unhealthy = Dext.HealthChecks.Unhealthy;
  // Dext.OpenAPI.Attributes
  Path = Dext.OpenAPI.Attributes.Path;
  Query = Dext.OpenAPI.Attributes.Query;
  Header = Dext.OpenAPI.Attributes.Header;
  Cookie = Dext.OpenAPI.Attributes.Cookie;
  // Dext.OpenAPI.Types
  odtString = Dext.OpenAPI.Types.odtString;
  odtNumber = Dext.OpenAPI.Types.odtNumber;
  odtInteger = Dext.OpenAPI.Types.odtInteger;
  odtBoolean = Dext.OpenAPI.Types.odtBoolean;
  odtArray = Dext.OpenAPI.Types.odtArray;
  odtObject = Dext.OpenAPI.Types.odtObject;
  oplQuery = Dext.OpenAPI.Types.oplQuery;
  oplPath = Dext.OpenAPI.Types.oplPath;
  oplHeader = Dext.OpenAPI.Types.oplHeader;
  oplCookie = Dext.OpenAPI.Types.oplCookie;
  sstApiKey = Dext.OpenAPI.Types.sstApiKey;
  sstHttp = Dext.OpenAPI.Types.sstHttp;
  sstOAuth2 = Dext.OpenAPI.Types.sstOAuth2;
  sstOpenIdConnect = Dext.OpenAPI.Types.sstOpenIdConnect;
  aklQuery = Dext.OpenAPI.Types.aklQuery;
  aklHeader = Dext.OpenAPI.Types.aklHeader;
  aklCookie = Dext.OpenAPI.Types.aklCookie;
  // Dext.RateLimiting.Core
  rltFixedWindow = Dext.RateLimiting.Core.rltFixedWindow;
  rltSlidingWindow = Dext.RateLimiting.Core.rltSlidingWindow;
  rltTokenBucket = Dext.RateLimiting.Core.rltTokenBucket;
  rltConcurrency = Dext.RateLimiting.Core.rltConcurrency;
  psIpAddress = Dext.RateLimiting.Core.psIpAddress;
  psHeader = Dext.RateLimiting.Core.psHeader;
  psRoute = Dext.RateLimiting.Core.psRoute;
  psCustom = Dext.RateLimiting.Core.psCustom;
  // Dext.Web.DataApi
  amGet = Dext.Web.DataApi.amGet;
  amGetList = Dext.Web.DataApi.amGetList;
  amPost = Dext.Web.DataApi.amPost;
  amPut = Dext.Web.DataApi.amPut;
  amDelete = Dext.Web.DataApi.amDelete;
  // Dext.Web.ModelBinding
  bsBody = Dext.Web.ModelBinding.bsBody;
  bsQuery = Dext.Web.ModelBinding.bsQuery;
  bsRoute = Dext.Web.ModelBinding.bsRoute;
  bsHeader = Dext.Web.ModelBinding.bsHeader;
  bsServices = Dext.Web.ModelBinding.bsServices;
  bsForm = Dext.Web.ModelBinding.bsForm;
  // {END_DEXT_ALIASES}

type
  /// <summary>
  ///   Helper for TDextServices to add web framework features.
  /// </summary>
  TDextHttpServicesHelper = record helper for TDextServices
  public
    /// <summary>
    ///   Scans the application for controllers (classes with [DextController]) and registers them in the DI container.
    /// </summary>
    function AddControllers: TDextServices;
    
    /// <summary>
    ///   Starts the Health Check builder chain.
    /// </summary>
    function AddHealthChecks: THealthCheckBuilder;
    
    /// <summary>
    ///   Starts the Background Service builder chain.
    /// </summary>
    function AddBackgroundServices: TBackgroundServiceBuilder;
    
    /// <summary>
    ///   Configures a settings class (IOptions&lt;T&gt;) from the root configuration.
    ///   (Re-exposed from Core for convenience)
    /// </summary>
    function Configure<T: class, constructor>(Configuration: IConfiguration): TDextServices; overload;
    
    /// <summary>
    ///   Configures a settings class (IOptions&lt;T&gt;) from a specific configuration section.
    ///   (Re-exposed from Core for convenience)
    /// </summary>
    function Configure<T: class, constructor>(Section: IConfigurationSection): TDextServices; overload;
  end;

  /// <summary>
  ///   Helper for TDextAppBuilder to provide factory methods and extensions for middleware configuration.
  /// </summary>
  TDextHttpAppBuilderHelper = record helper for TDextAppBuilder
  public
    // ?? Factory Methods
    
    /// <summary>
    ///   Creates a new instance of TCorsOptions with default settings.
    /// </summary>
    function CreateCorsOptions: TCorsOptions;
    
    /// <summary>
    ///   Creates a new instance of TJwtOptions with the specified secret key.
    /// </summary>
    function CreateJwtOptions(const Secret: string): TJwtOptions;
    
    /// <summary>
    ///   Creates a new instance of TStaticFileOptions with default settings.
    /// </summary>
    function CreateStaticFileOptions: TStaticFileOptions;
    
    // ?? Extensions
    
    /// <summary>
    ///   Adds CORS middleware to the pipeline using the provided options.
    /// </summary>
    function UseCors(const AOptions: TCorsOptions): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Adds CORS middleware to the pipeline using a configuration delegate.
    /// </summary>
    function UseCors(AConfigurator: TProc<TCorsBuilder>): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Adds JWT Authentication middleware to the pipeline using the provided options.
    /// </summary>
    function UseJwtAuthentication(const AOptions: TJwtOptions): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Adds JWT Authentication middleware to the pipeline using the provided options (Legacy overload).
    /// </summary>
    function UseJwtAuthentication(const ASecretKey: string; AConfigurator: TProc<TJwtOptionsBuilder>): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Adds Basic Authentication middleware with a simple validation function.
    /// </summary>
    function UseBasicAuthentication(const ARealm: string; AValidateFunc: TBasicAuthValidateFunc): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Adds Basic Authentication middleware with role support.
    /// </summary>
    function UseBasicAuthentication(const ARealm: string; AValidateFunc: TBasicAuthValidateWithRolesFunc): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Adds Basic Authentication middleware with custom options.
    /// </summary>
    function UseBasicAuthentication(const AOptions: TBasicAuthOptions; AValidateFunc: TBasicAuthValidateFunc): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Adds Swagger middleware to the application pipeline using the provided options.
    /// </summary>
    function UseSwagger(const AOptions: TOpenAPIOptions): TDextAppBuilder; overload;

    /// <summary>
    ///   Adds Swagger middleware to the application pipeline with default options.
    /// </summary>
    function UseSwagger: TDextAppBuilder; overload;
    
    /// <summary>
    ///   Adds Static Files middleware to the pipeline using the provided options.
    /// </summary>
    function UseStaticFiles(const AOptions: TStaticFileOptions): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Adds Static Files middleware to the pipeline serving from the specified root path.
    /// </summary>
    function UseStaticFiles(const ARootPath: string): TDextAppBuilder; overload;
    
    // ?? Core Forwarding
    
    /// <summary>
    ///   Adds a middleware class to the pipeline. The middleware must have a constructor accepting RequestDelegate (and optionally other services).
    /// </summary>
    function UseMiddleware(AMiddleware: TClass): TDextAppBuilder;
    
    /// <summary>
    ///   Maps a GET request to a static handler.
    /// </summary>
    function MapGet(const Path: string; Handler: TStaticHandler): TDextAppBuilder; overload;

    /// <summary>
    ///   Maps a POST request to a static handler.
    /// </summary>
    function MapPost(const Path: string; Handler: TStaticHandler): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Maps a PUT request to a static handler.
    /// </summary>
    function MapPut(const Path: string; Handler: TStaticHandler): TDextAppBuilder; overload;

    /// <summary>
    ///   Maps a DELETE request to a static handler.
    /// </summary>
    function MapDelete(const Path: string; Handler: TStaticHandler): TDextAppBuilder; overload;

    /// <summary>
    ///   Builds the request pipeline and returns the main RequestDelegate.
    /// </summary>
    /// <summary>
    ///   Builds the request pipeline and returns the main RequestDelegate.
    /// </summary>
    function Build: TRequestDelegate;

    // -------------------------------------------------------------------------
    // ?? Middleware
    // -------------------------------------------------------------------------
    function UseStaticFiles: TDextAppBuilder; overload;
    function UseStartupLock: TDextAppBuilder;
    function UseExceptionHandler: TDextAppBuilder; overload;
    function UseExceptionHandler(const AOptions: TExceptionHandlerOptions): TDextAppBuilder; overload;
    function UseHttpLogging: TDextAppBuilder; overload;
    function UseHttpLogging(const AOptions: THttpLoggingOptions): TDextAppBuilder; overload;

    // -------------------------------------------------------------------------
    // ?? Rate Limiting
    // -------------------------------------------------------------------------
    function UseRateLimiting(const APolicy: TRateLimitPolicy): TDextAppBuilder; overload;

    // -------------------------------------------------------------------------
    // ??? Routing - POST
    // -------------------------------------------------------------------------
    
    /// <summary>
    ///   Maps a POST request to a handler with 1 injected parameter.
    /// </summary>
    function MapPost<T>(const Path: string; Handler: THandlerProc<T>): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Maps a POST request to a handler with 2 injected parameters.
    /// </summary>
    function MapPost<T1, T2>(const Path: string; Handler: THandlerProc<T1, T2>): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Maps a POST request to a handler with 3 injected parameters.
    /// </summary>
    function MapPost<T1, T2, T3>(const Path: string; Handler: THandlerProc<T1, T2, T3>): TDextAppBuilder; overload;

    /// <summary>
    ///   Maps a POST request to a handler that returns a result.
    /// </summary>
    function MapPost<TResult>(const Path: string; Handler: THandlerResultFunc<TResult>): TDextAppBuilder; overload;
    function MapPost<T, TResult>(const Path: string; Handler: THandlerResultFunc<T, TResult>): TDextAppBuilder; overload;
    function MapPost<T1, T2, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, TResult>): TDextAppBuilder; overload;
    function MapPost<T1, T2, T3, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, T3, TResult>): TDextAppBuilder; overload;

    // -------------------------------------------------------------------------
    // ??? Routing - GET
    // -------------------------------------------------------------------------
    
    /// <summary>
    ///   Maps a GET request to a handler with 1 injected parameter.
    /// </summary>
    function MapGet<T>(const Path: string; Handler: THandlerProc<T>): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Maps a GET request to a handler with 2 injected parameters.
    /// </summary>
    function MapGet<T1, T2>(const Path: string; Handler: THandlerProc<T1, T2>): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Maps a GET request to a handler with 3 injected parameters.
    /// </summary>
    function MapGet<T1, T2, T3>(const Path: string; Handler: THandlerProc<T1, T2, T3>): TDextAppBuilder; overload;

    /// <summary>
    ///   Maps a GET request to a handler that returns a result.
    /// </summary>
    function MapGet<TResult>(const Path: string; Handler: THandlerResultFunc<TResult>): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Maps a GET request to a handler with 1 parameter that returns a result.
    /// </summary>
    function MapGet<T, TResult>(const Path: string; Handler: THandlerResultFunc<T, TResult>): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Maps a GET request to a handler with 2 parameters that returns a result.
    /// </summary>
    function MapGet<T1, T2, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, TResult>): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Maps a GET request to a handler with 3 parameters that returns a result.
    /// </summary>
    function MapGet<T1, T2, T3, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, T3, TResult>): TDextAppBuilder; overload;

    // -------------------------------------------------------------------------
    // ??? Routing - PUT
    // -------------------------------------------------------------------------
    function MapPut<T>(const Path: string; Handler: THandlerProc<T>): TDextAppBuilder; overload;
    function MapPut<T1, T2>(const Path: string; Handler: THandlerProc<T1, T2>): TDextAppBuilder; overload;
    function MapPut<T1, T2, T3>(const Path: string; Handler: THandlerProc<T1, T2, T3>): TDextAppBuilder; overload;

    function MapPut<T, TResult>(const Path: string; Handler: THandlerResultFunc<T, TResult>): TDextAppBuilder; overload;
    function MapPut<T1, T2, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, TResult>): TDextAppBuilder; overload;
    function MapPut<T1, T2, T3, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, T3, TResult>): TDextAppBuilder; overload;

    // -------------------------------------------------------------------------
    // ??? Routing - DELETE
    // -------------------------------------------------------------------------
    function MapDelete<T>(const Path: string; Handler: THandlerProc<T>): TDextAppBuilder; overload;
    function MapDelete<T1, T2>(const Path: string; Handler: THandlerProc<T1, T2>): TDextAppBuilder; overload;
    function MapDelete<T1, T2, T3>(const Path: string; Handler: THandlerProc<T1, T2, T3>): TDextAppBuilder; overload;

    function MapDelete<T, TResult>(const Path: string; Handler: THandlerResultFunc<T, TResult>): TDextAppBuilder; overload;
    function MapDelete<T1, T2, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, TResult>): TDextAppBuilder; overload;
    function MapDelete<T1, T2, T3, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, T3, TResult>): TDextAppBuilder; overload;
  end;

// ===========================================================================
// ??? Global Response Helpers
// ===========================================================================

procedure RespondJson(const AContext: IHttpContext; AStatusCode: Integer; const AJson: string); overload;
procedure RespondJson(const AContext: IHttpContext; AStatusCode: Integer; const AFormat: string; const AArgs: array of const); overload;
procedure RespondError(const AContext: IHttpContext; AStatusCode: Integer; const AMessage: string);
procedure RespondOk(const AContext: IHttpContext; const AJson: string);
procedure RespondCreated(const AContext: IHttpContext; const AJson: string);
procedure RespondNoContent(const AContext: IHttpContext);

implementation

procedure RespondJson(const AContext: IHttpContext; AStatusCode: Integer; const AJson: string);
begin
  Dext.Web.ResponseHelper.RespondJson(AContext, AStatusCode, AJson);
end;

procedure RespondJson(const AContext: IHttpContext; AStatusCode: Integer; const AFormat: string; const AArgs: array of const);
begin
  Dext.Web.ResponseHelper.RespondJson(AContext, AStatusCode, AFormat, AArgs);
end;

procedure RespondError(const AContext: IHttpContext; AStatusCode: Integer; const AMessage: string);
begin
  Dext.Web.ResponseHelper.RespondError(AContext, AStatusCode, AMessage);
end;

procedure RespondOk(const AContext: IHttpContext; const AJson: string);
begin
  Dext.Web.ResponseHelper.RespondOk(AContext, AJson);
end;

procedure RespondCreated(const AContext: IHttpContext; const AJson: string);
begin
  Dext.Web.ResponseHelper.RespondCreated(AContext, AJson);
end;

procedure RespondNoContent(const AContext: IHttpContext);
begin
  Dext.Web.ResponseHelper.RespondNoContent(AContext);
end;

{ TDextHttpServicesHelper }

function TDextHttpServicesHelper.AddControllers: TDextServices;
var
  Scanner: IControllerScanner;
begin
  Scanner := TControllerScanner.Create;
  Scanner.RegisterServices(Self.Unwrap);
  Result := Self;
end;

function TDextHttpServicesHelper.AddHealthChecks: THealthCheckBuilder;
begin
  Result := THealthCheckBuilder.Create(Self.Unwrap);
end;

function TDextHttpServicesHelper.AddBackgroundServices: TBackgroundServiceBuilder;
begin
  Result := TBackgroundServiceBuilder.Create(Self.Unwrap);
end;

function TDextHttpServicesHelper.Configure<T>(Configuration: IConfiguration): TDextServices;
begin
  TOptionsServiceCollectionExtensions.Configure<T>(Self.Unwrap, Configuration);
  Result := Self;
end;

function TDextHttpServicesHelper.Configure<T>(Section: IConfigurationSection): TDextServices;
begin
  TOptionsServiceCollectionExtensions.Configure<T>(Self.Unwrap, Section);
  Result := Self;
end;

{ TDextHttpAppBuilderHelper }

function TDextHttpAppBuilderHelper.CreateCorsOptions: TCorsOptions;
begin
  Result := TCorsOptions.Create;
end;

function TDextHttpAppBuilderHelper.CreateJwtOptions(const Secret: string): TJwtOptions;
begin
  Result := TJwtOptions.Create(Secret);
end;

function TDextHttpAppBuilderHelper.CreateStaticFileOptions: TStaticFileOptions;
begin
  Result := TStaticFileOptions.Create;
end;

function TDextHttpAppBuilderHelper.UseCors(const AOptions: TCorsOptions): TDextAppBuilder;
begin
  TApplicationBuilderCorsExtensions.UseCors(Self.Unwrap, AOptions);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseStartupLock: TDextAppBuilder;
begin
  TApplicationBuilderMiddlewareExtensions.UseStartupLock(Self.Unwrap);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseExceptionHandler: TDextAppBuilder;
begin
  TApplicationBuilderMiddlewareExtensions.UseExceptionHandler(Self.Unwrap);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseExceptionHandler(const AOptions: TExceptionHandlerOptions): TDextAppBuilder;
begin
  TApplicationBuilderMiddlewareExtensions.UseExceptionHandler(Self.Unwrap, AOptions);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseHttpLogging: TDextAppBuilder;
begin
  TApplicationBuilderMiddlewareExtensions.UseHttpLogging(Self.Unwrap);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseHttpLogging(const AOptions: THttpLoggingOptions): TDextAppBuilder;
begin
  TApplicationBuilderMiddlewareExtensions.UseHttpLogging(Self.Unwrap, AOptions);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseRateLimiting(const APolicy: TRateLimitPolicy): TDextAppBuilder;
begin
  TApplicationBuilderRateLimitExtensions.UseRateLimiting(Self.Unwrap, APolicy);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseCors(AConfigurator: TProc<TCorsBuilder>): TDextAppBuilder;
begin
  TApplicationBuilderCorsExtensions.UseCors(Self.Unwrap, AConfigurator);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseJwtAuthentication(const AOptions: TJwtOptions): TDextAppBuilder;
begin
  TApplicationBuilderJwtExtensions.UseJwtAuthentication(Self.Unwrap, AOptions);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseJwtAuthentication(const ASecretKey: string; AConfigurator: TProc<TJwtOptionsBuilder>): TDextAppBuilder;
begin
  TApplicationBuilderJwtExtensions.UseJwtAuthentication(Self.Unwrap, ASecretKey, AConfigurator);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseBasicAuthentication(const ARealm: string; AValidateFunc: TBasicAuthValidateFunc): TDextAppBuilder;
begin
  TApplicationBuilderBasicAuthExtensions.UseBasicAuthentication(Self.Unwrap, ARealm, AValidateFunc);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseBasicAuthentication(const ARealm: string; AValidateFunc: TBasicAuthValidateWithRolesFunc): TDextAppBuilder;
begin
  TApplicationBuilderBasicAuthExtensions.UseBasicAuthentication(Self.Unwrap, ARealm, AValidateFunc);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseBasicAuthentication(const AOptions: TBasicAuthOptions; AValidateFunc: TBasicAuthValidateFunc): TDextAppBuilder;
begin
  TApplicationBuilderBasicAuthExtensions.UseBasicAuthentication(Self.Unwrap, AOptions, AValidateFunc);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseSwagger(const AOptions: TOpenAPIOptions): TDextAppBuilder;
begin
  TSwaggerExtensions.UseSwagger(Self.Unwrap, AOptions);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseSwagger: TDextAppBuilder;
begin
  TSwaggerExtensions.UseSwagger(Self.Unwrap);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseStaticFiles(const AOptions: TStaticFileOptions): TDextAppBuilder;
begin
  TApplicationBuilderStaticFilesExtensions.UseStaticFiles(Self.Unwrap, AOptions);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseStaticFiles(const ARootPath: string): TDextAppBuilder;
begin
  TApplicationBuilderStaticFilesExtensions.UseStaticFiles(Self.Unwrap, ARootPath);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.UseMiddleware(AMiddleware: TClass): TDextAppBuilder;
begin
  Self.Unwrap.UseMiddleware(AMiddleware);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapGet(const Path: string; Handler: TStaticHandler): TDextAppBuilder;
begin
  Self.Unwrap.MapGet(Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPost(const Path: string; Handler: TStaticHandler): TDextAppBuilder;
begin
  Self.Unwrap.MapPost(Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPut(const Path: string; Handler: TStaticHandler): TDextAppBuilder;
begin
  Self.Unwrap.MapPut(Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapDelete(const Path: string; Handler: TStaticHandler): TDextAppBuilder;
begin
  Self.Unwrap.MapDelete(Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.Build: TRequestDelegate;
begin
  Result := Self.Unwrap.Build;
end;

{ TDextHttpAppBuilderHelper }

function TDextHttpAppBuilderHelper.UseStaticFiles: TDextAppBuilder;
begin
  TApplicationBuilderStaticFilesExtensions.UseStaticFiles(Self.Unwrap);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPost<T>(const Path: string; Handler: THandlerProc<T>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapPost<T>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPost<T1, T2>(const Path: string; Handler: THandlerProc<T1, T2>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapPost<T1, T2>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPost<T1, T2, T3>(const Path: string; Handler: THandlerProc<T1, T2, T3>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapPost<T1, T2, T3>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPost<TResult>(const Path: string; Handler: THandlerResultFunc<TResult>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapPost<TResult>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPost<T, TResult>(const Path: string; Handler: THandlerResultFunc<T, TResult>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapPost<T, TResult>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPost<T1, T2, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, TResult>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapPost<T1, T2, TResult>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPost<T1, T2, T3, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, T3, TResult>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapPost<T1, T2, T3, TResult>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapGet<T>(const Path: string; Handler: THandlerProc<T>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapGet<T>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapGet<T1, T2>(const Path: string; Handler: THandlerProc<T1, T2>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapGet<T1, T2>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapGet<T1, T2, T3>(const Path: string; Handler: THandlerProc<T1, T2, T3>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapGet<T1, T2, T3>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapGet<TResult>(const Path: string; Handler: THandlerResultFunc<TResult>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapGet<TResult>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapGet<T, TResult>(const Path: string; Handler: THandlerResultFunc<T, TResult>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapGet<T, TResult>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapGet<T1, T2, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, TResult>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapGet<T1, T2, TResult>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapGet<T1, T2, T3, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, T3, TResult>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapGet<T1, T2, T3, TResult>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPut<T>(const Path: string; Handler: THandlerProc<T>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapPut<T>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPut<T1, T2>(const Path: string; Handler: THandlerProc<T1, T2>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapPut<T1, T2>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPut<T1, T2, T3>(const Path: string; Handler: THandlerProc<T1, T2, T3>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapPut<T1, T2, T3>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPut<T, TResult>(const Path: string; Handler: THandlerResultFunc<T, TResult>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapPut<T, TResult>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPut<T1, T2, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, TResult>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapPut<T1, T2, TResult>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapPut<T1, T2, T3, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, T3, TResult>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapPut<T1, T2, T3, TResult>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapDelete<T>(const Path: string; Handler: THandlerProc<T>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapDelete<T>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapDelete<T1, T2>(const Path: string; Handler: THandlerProc<T1, T2>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapDelete<T1, T2>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapDelete<T1, T2, T3>(const Path: string; Handler: THandlerProc<T1, T2, T3>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapDelete<T1, T2, T3>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapDelete<T, TResult>(const Path: string; Handler: THandlerResultFunc<T, TResult>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapDelete<T, TResult>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapDelete<T1, T2, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, TResult>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapDelete<T1, T2, TResult>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

function TDextHttpAppBuilderHelper.MapDelete<T1, T2, T3, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, T3, TResult>): TDextAppBuilder;
begin
  TApplicationBuilderExtensions.MapDelete<T1, T2, T3, TResult>(Self.Unwrap, Path, Handler);
  Result := Self;
end;

end.

