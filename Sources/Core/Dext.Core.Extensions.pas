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
unit Dext.Core.Extensions;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  Dext.DI.Interfaces,
  Dext.HealthChecks,
  Dext.Hosting.BackgroundService;

type
  TDextServiceCollectionExtensions = class
  public
    class function AddHealthChecks(Services: IServiceCollection): THealthCheckBuilder;
    class function AddBackgroundServices(Services: IServiceCollection): TBackgroundServiceBuilder;
  end;

implementation

{ TDextServiceCollectionExtensions }

class function TDextServiceCollectionExtensions.AddHealthChecks(Services: IServiceCollection): THealthCheckBuilder;
var
  SharedChecks: TList<TClass>;
  CapturedChecks: TArray<TClass>;
  UpdateCallback: TProc;
  Factory: TFunc<IServiceProvider, TObject>;
begin
  SharedChecks := TList<TClass>.Create;
  SetLength(CapturedChecks, 0);
  
  UpdateCallback := procedure
    begin
      CapturedChecks := SharedChecks.ToArray;
    end;
  
  Factory := function(Provider: IServiceProvider): TObject
    var
      Service: THealthCheckService;
      CheckClass: TClass;
    begin
      Service := THealthCheckService.Create;
      for CheckClass in CapturedChecks do
        Service.RegisterCheck(CheckClass);
      Result := Service;
    end;
  
  Services.AddSingleton(
    TServiceType.FromClass(THealthCheckService),
    THealthCheckService,
    Factory
  );
  
  Result := THealthCheckBuilder.Create(Services, SharedChecks, UpdateCallback);
end;

class function TDextServiceCollectionExtensions.AddBackgroundServices(Services: IServiceCollection): TBackgroundServiceBuilder;
begin
  Result := TBackgroundServiceBuilder.Create(Services);
end;

end.

