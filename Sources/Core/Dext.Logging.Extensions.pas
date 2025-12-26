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
unit Dext.Logging.Extensions;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Dext.DI.Interfaces,
  Dext.Logging;

type
  ILoggingBuilder = interface
    ['{D4E5F678-9012-3456-7890-ABCDEF123456}']
    function Services: IServiceCollection;
    function AddProvider(const AProvider: ILoggerProvider): ILoggingBuilder;
    function SetMinimumLevel(ALevel: TLogLevel): ILoggingBuilder;
    function AddConsole: ILoggingBuilder;
  end;

  TServiceCollectionLoggingExtensions = class
  public
    class function AddLogging(const AServices: IServiceCollection; const AConfigure: TProc<ILoggingBuilder> = nil): IServiceCollection;
  end;

implementation

uses
  System.TypInfo,
  Dext.DI.Extensions,
  Dext.Logging.Console;

type
  TLoggingBuilder = class(TInterfacedObject, ILoggingBuilder)
  private
    FServices: IServiceCollection;
    FProviders: TList<ILoggerProvider>;
    FMinLevel: TLogLevel;
  public
    constructor Create(AServices: IServiceCollection);
    destructor Destroy; override;
    
    function Services: IServiceCollection;
    function AddProvider(const AProvider: ILoggerProvider): ILoggingBuilder;
    function SetMinimumLevel(ALevel: TLogLevel): ILoggingBuilder;
    function AddConsole: ILoggingBuilder;
    
    function ExtractProviders: TList<ILoggerProvider>;
    function GetMinLevel: TLogLevel;
  end;

{ TLoggingBuilder }

constructor TLoggingBuilder.Create(AServices: IServiceCollection);
begin
  inherited Create;
  FServices := AServices;
  FProviders := TList<ILoggerProvider>.Create;
  FMinLevel := TLogLevel.Information;
end;

destructor TLoggingBuilder.Destroy;
begin
  FProviders.Free;
  inherited;
end;

function TLoggingBuilder.Services: IServiceCollection;
begin
  Result := FServices;
end;

function TLoggingBuilder.AddProvider(const AProvider: ILoggerProvider): ILoggingBuilder;
begin
  FProviders.Add(AProvider);
  Result := Self;
end;

function TLoggingBuilder.SetMinimumLevel(ALevel: TLogLevel): ILoggingBuilder;
begin
  FMinLevel := ALevel;
  Result := Self;
end;

function TLoggingBuilder.AddConsole: ILoggingBuilder;
begin
  Result := AddProvider(TConsoleLoggerProvider.Create);
end;

function TLoggingBuilder.ExtractProviders: TList<ILoggerProvider>;
begin
  Result := FProviders;
  FProviders := TList<ILoggerProvider>.Create; 
end;

function TLoggingBuilder.GetMinLevel: TLogLevel;
begin
  Result := FMinLevel;
end;

{ TServiceCollectionLoggingExtensions }

class function TServiceCollectionLoggingExtensions.AddLogging(const AServices: IServiceCollection; const AConfigure: TProc<ILoggingBuilder>): IServiceCollection;
var
  LBuilderIntf: ILoggingBuilder;
  LBuilderObj: TLoggingBuilder;
  LProvidersList: TList<ILoggerProvider>;
  LProvidersArray: TArray<ILoggerProvider>;
  LMinLevel: TLogLevel;
begin
  LBuilderObj := TLoggingBuilder.Create(AServices);
  LBuilderIntf := LBuilderObj; // Mantém a referência viva
  
  if Assigned(AConfigure) then
    AConfigure(LBuilderIntf);
    
  LProvidersList := LBuilderObj.ExtractProviders;
  LProvidersArray := LProvidersList.ToArray;
  LProvidersList.Free;
  LMinLevel := LBuilderObj.GetMinLevel;
  
  // Create TLoggerFactory instance
  var Factory := TLoggerFactory.Create;
  Factory.SetMinimumLevel(LMinLevel);
  for var P in LProvidersArray do
    Factory.AddProvider(P);
  
  // Register using instance registration (no closure capture!)
  AServices.AddSingleton(
    TServiceType.FromInterface(ILoggerFactory),
    Factory
  );
    
  // Register generic ILogger (default) - Resolve from ILoggerFactory
  AServices.AddSingleton(TServiceType.FromInterface(ILogger), nil,
    function(Provider: IServiceProvider): TObject
    var
      FactoryObj: TObject;
      Factory: TLoggerFactory;
    begin
      // Get factory as object (will be TLoggerFactory instance)
      FactoryObj := Provider.GetService(TServiceType.FromInterface(ILoggerFactory));
      Factory := FactoryObj as TLoggerFactory;
      // Call CreateLoggerInstance which returns TAggregateLogger (TObject)
      Result := Factory.CreateLoggerInstance('App');
    end);

  Result := AServices;
end;

end.

