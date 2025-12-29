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
unit Dext.Web.Middleware.Extensions;

interface

uses
  System.Rtti,
  Dext.Web.Interfaces,
  Dext.Web.Middleware,
  Dext.Web.Middleware.StartupLock,
  Dext.Logging;

type
  TApplicationBuilderMiddlewareExtensions = class
  public
    class function UseHttpLogging(const ABuilder: IApplicationBuilder): IApplicationBuilder; overload;
    class function UseHttpLogging(const ABuilder: IApplicationBuilder; const AOptions: THttpLoggingOptions): IApplicationBuilder; overload;
    
    class function UseExceptionHandler(const ABuilder: IApplicationBuilder): IApplicationBuilder; overload;
    class function UseExceptionHandler(const ABuilder: IApplicationBuilder; const AOptions: TExceptionHandlerOptions): IApplicationBuilder; overload;
    
    class function UseStartupLock(const ABuilder: IApplicationBuilder): IApplicationBuilder;
  end;

implementation

uses
  Dext.DI.Interfaces,
  Dext.DI.Extensions,
  Dext.Logging.Console;

{ TApplicationBuilderMiddlewareExtensions }

class function TApplicationBuilderMiddlewareExtensions.UseHttpLogging(const ABuilder: IApplicationBuilder): IApplicationBuilder;
begin
  Result := UseHttpLogging(ABuilder, THttpLoggingOptions.Default);
end;

class function TApplicationBuilderMiddlewareExtensions.UseHttpLogging(const ABuilder: IApplicationBuilder; const AOptions: THttpLoggingOptions): IApplicationBuilder;
var
  Logger: ILogger;
  Middleware: IMiddleware;
  ServiceProvider: IServiceProvider;
begin
  // ✅ Try to resolve ILogger from DI, fallback to console logger
  Logger := nil;
  ServiceProvider := ABuilder.GetServiceProvider;
  if ServiceProvider <> nil then
    Logger := ServiceProvider.GetServiceAsInterface(TServiceType.FromInterface(ILogger)) as ILogger;
  
  if Logger = nil then
    Logger := TConsoleLogger.Create('HttpLogging');
  
  Middleware := THttpLoggingMiddleware.Create(AOptions, Logger);
  Result := ABuilder.UseMiddleware(Middleware);
end;

class function TApplicationBuilderMiddlewareExtensions.UseExceptionHandler(const ABuilder: IApplicationBuilder): IApplicationBuilder;
begin
  Result := UseExceptionHandler(ABuilder, TExceptionHandlerOptions.Production);
end;

class function TApplicationBuilderMiddlewareExtensions.UseExceptionHandler(const ABuilder: IApplicationBuilder; const AOptions: TExceptionHandlerOptions): IApplicationBuilder;
var
  Logger: ILogger;
  Middleware: IMiddleware;
  ServiceProvider: IServiceProvider;
begin
  // ✅ Try to resolve ILogger from DI, fallback to console logger
  Logger := nil;
  ServiceProvider := ABuilder.GetServiceProvider;
  if ServiceProvider <> nil then
    Logger := ServiceProvider.GetServiceAsInterface(TServiceType.FromInterface(ILogger)) as ILogger;
  
  if Logger = nil then
    Logger := TConsoleLogger.Create('ExceptionHandler');
  
  Middleware := TExceptionHandlerMiddleware.Create(AOptions, Logger);
  Result := ABuilder.UseMiddleware(Middleware);
end;

class function TApplicationBuilderMiddlewareExtensions.UseStartupLock(const ABuilder: IApplicationBuilder): IApplicationBuilder;
begin
  Result := ABuilder.UseMiddleware(TStartupLockMiddleware);
end;

end.

