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
  {$I Dext.Web.Uses.inc};

type
  // ===========================================================================
  // 🏷️ Aliases for Common Web Types
  // ===========================================================================
  
  {$I Dext.Web.Aliases.inc}

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
    // 🏭 Factory Methods
    
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
    
    // 🔌 Extensions
    
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
    ///   Adds Static Files middleware to the pipeline using the provided options.
    /// </summary>
    function UseStaticFiles(const AOptions: TStaticFileOptions): TDextAppBuilder; overload;
    
    /// <summary>
    ///   Adds Static Files middleware to the pipeline serving from the specified root path.
    /// </summary>
    function UseStaticFiles(const ARootPath: string): TDextAppBuilder; overload;
    
    // 🧩 Core Forwarding
    
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
    // 🧱 Middleware
    // -------------------------------------------------------------------------
    function UseStaticFiles: TDextAppBuilder; overload;
    function UseStartupLock: TDextAppBuilder;
    function UseExceptionHandler: TDextAppBuilder; overload;
    function UseExceptionHandler(const AOptions: TExceptionHandlerOptions): TDextAppBuilder; overload;
    function UseHttpLogging: TDextAppBuilder; overload;
    function UseHttpLogging(const AOptions: THttpLoggingOptions): TDextAppBuilder; overload;

    // -------------------------------------------------------------------------
    // 🛣️ Routing - POST
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
    // 🛣️ Routing - GET
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
    // 🛣️ Routing - PUT
    // -------------------------------------------------------------------------
    function MapPut<T>(const Path: string; Handler: THandlerProc<T>): TDextAppBuilder; overload;
    function MapPut<T1, T2>(const Path: string; Handler: THandlerProc<T1, T2>): TDextAppBuilder; overload;
    function MapPut<T1, T2, T3>(const Path: string; Handler: THandlerProc<T1, T2, T3>): TDextAppBuilder; overload;

    function MapPut<T, TResult>(const Path: string; Handler: THandlerResultFunc<T, TResult>): TDextAppBuilder; overload;
    function MapPut<T1, T2, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, TResult>): TDextAppBuilder; overload;
    function MapPut<T1, T2, T3, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, T3, TResult>): TDextAppBuilder; overload;

    // -------------------------------------------------------------------------
    // 🛣️ Routing - DELETE
    // -------------------------------------------------------------------------
    function MapDelete<T>(const Path: string; Handler: THandlerProc<T>): TDextAppBuilder; overload;
    function MapDelete<T1, T2>(const Path: string; Handler: THandlerProc<T1, T2>): TDextAppBuilder; overload;
    function MapDelete<T1, T2, T3>(const Path: string; Handler: THandlerProc<T1, T2, T3>): TDextAppBuilder; overload;

    function MapDelete<T, TResult>(const Path: string; Handler: THandlerResultFunc<T, TResult>): TDextAppBuilder; overload;
    function MapDelete<T1, T2, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, TResult>): TDextAppBuilder; overload;
    function MapDelete<T1, T2, T3, TResult>(const Path: string; Handler: THandlerResultFunc<T1, T2, T3, TResult>): TDextAppBuilder; overload;
  end;

implementation

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

