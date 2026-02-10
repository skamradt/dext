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
{  Created: 2026-01-03                                                      }
{                                                                           }
{  Dext.Testing.DI - Mock-aware Dependency Injection for testing.           }
{                                                                           }
{  Usage:                                                                   }
{    var Provider := TTestServiceProvider.Create;                           }
{    Provider.AddMock<IRepository>(MyMock);                                 }
{    var Service := Provider.GetService<TService>;                          }
{                                                                           }
{***************************************************************************}
unit Dext.Testing.DI;

interface

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  System.Generics.Collections,
  Dext.DI.Interfaces,
  Dext.Mocks,
  Dext.Mocks.Interceptor,
  Dext.Interception;

type
  /// <summary>
  ///   A test-focused service provider that allows replacing real services with mocks.
  ///   Useful for unit and integration testing scenarios.
  /// </summary>
  TTestServiceProvider = class(TInterfacedObject, IServiceProvider)
  private
    FServices: TDextServices;
    FProvider: IServiceProvider;
    FMocks: TDictionary<TGUID, IInterface>;
    FBuilt: Boolean;
    procedure EnsureNotBuilt;
    procedure EnsureBuilt;
  public
    constructor Create; overload;
    constructor Create(const BaseServices: TDextServices); overload;
    destructor Destroy; override;

    /// <summary>
    ///   Register a mock instance for a specific interface type.
    /// </summary>
    procedure AddMock<T: IInterface>(const MockInstance: Mock<T>); overload;
    
    /// <summary>
    ///   Register a mock instance using its instance directly.
    /// </summary>
    procedure AddMock<T: IInterface>(const Instance: T); overload;

    /// <summary>
    ///   Register a singleton service (real implementation).
    /// </summary>
    procedure AddSingleton<TInterface: IInterface; TImplementation: class>;

    /// <summary>
    ///   Register a transient service (real implementation).
    /// </summary>
    procedure AddTransient<TInterface: IInterface; TImplementation: class>;

    /// <summary>
    ///   Replace an existing service registration with a mock.
    ///   Useful when starting from a real service collection.
    /// </summary>
    procedure Replace<T: IInterface>(const MockInstance: Mock<T>);

    /// <summary>
    ///   Build the service provider. Must be called before GetService.
    /// </summary>
    procedure Build;

    /// <summary>
    ///   Get a service instance. Mocks take priority over real registrations.
    /// </summary>
    function GetMockOrService<T: IInterface>: T;
    
    // IServiceProvider implementation
    function GetService(const AServiceType: TServiceType): TObject;
    function GetServiceAsInterface(const AServiceType: TServiceType): IInterface;
    function GetRequiredService(const AServiceType: TServiceType): TObject;
    function CreateScope: IServiceScope;
  end;

implementation

{ TTestServiceProvider }

constructor TTestServiceProvider.Create;
begin
  inherited Create;
  FServices := TDextServices.New;
  FMocks := TDictionary<TGUID, IInterface>.Create;
  FBuilt := False;
end;

constructor TTestServiceProvider.Create(const BaseServices: TDextServices);
var
  BaseCollection: IServiceCollection;
begin
  inherited Create;
  // TDextServices is a record - we create a new one and copy services
  FServices := TDextServices.New;
  // Copy services from BaseServices if it has an underlying collection
  BaseCollection := BaseServices.Unwrap;
  if Assigned(BaseCollection) then
    FServices.Unwrap.AddRange(BaseCollection);
  FMocks := TDictionary<TGUID, IInterface>.Create;
  FBuilt := False;
end;

destructor TTestServiceProvider.Destroy;
begin
  FMocks.Free;
  // TDextServices is a record, no need to free - the underlying IServiceCollection
  // is managed by reference counting
  inherited;
end;


procedure TTestServiceProvider.EnsureNotBuilt;
begin
  if FBuilt then
    raise Exception.Create('Cannot modify services after Build() has been called');
end;

procedure TTestServiceProvider.EnsureBuilt;
begin
  if not FBuilt then
    Build;  // Auto-build on first GetService call
end;

procedure TTestServiceProvider.AddMock<T>(const MockInstance: Mock<T>);
var
  Guid: TGUID;
begin
  EnsureNotBuilt;
  Guid := GetTypeData(TypeInfo(T))^.Guid;
  FMocks.AddOrSetValue(Guid, MockInstance.Instance);
end;

procedure TTestServiceProvider.AddMock<T>(const Instance: T);
var
  Guid: TGUID;
begin
  EnsureNotBuilt;
  Guid := GetTypeData(TypeInfo(T))^.Guid;
  FMocks.AddOrSetValue(Guid, Instance);
end;

procedure TTestServiceProvider.AddSingleton<TInterface, TImplementation>;
begin
  EnsureNotBuilt;
  FServices.AddSingleton<TInterface, TImplementation>;
end;

procedure TTestServiceProvider.AddTransient<TInterface, TImplementation>;
begin
  EnsureNotBuilt;
  FServices.AddTransient<TInterface, TImplementation>;
end;

procedure TTestServiceProvider.Replace<T>(const MockInstance: Mock<T>);
begin
  // Replace simply adds to mocks dictionary - mocks take priority
  AddMock<T>(MockInstance);
end;

procedure TTestServiceProvider.Build;
begin
  if not FBuilt then
  begin
    FProvider := FServices.BuildServiceProvider;
    FBuilt := True;
  end;
end;

function TTestServiceProvider.GetMockOrService<T>: T;
var
  MockValue: IInterface;
  Guid: TGUID;
begin
  EnsureBuilt;
  
  Guid := GetTypeData(TypeInfo(T))^.Guid;
  
  // Mocks take priority
  if FMocks.TryGetValue(Guid, MockValue) then
    Exit(T(MockValue));
    
  // Fall back to real provider
  Result := T(FProvider.GetServiceAsInterface(TServiceType.FromInterface(Guid)));
end;

function TTestServiceProvider.GetService(const AServiceType: TServiceType): TObject;
begin
  EnsureBuilt;
  Result := FProvider.GetService(AServiceType);
end;

function TTestServiceProvider.GetServiceAsInterface(const AServiceType: TServiceType): IInterface;
var
  MockValue: IInterface;
begin
  EnsureBuilt;
  
  // Mocks take priority (for interfaces)
  if AServiceType.IsInterface and FMocks.TryGetValue(AServiceType.AsInterface, MockValue) then
    Exit(MockValue);
    
  // Fall back to real provider
  Result := FProvider.GetServiceAsInterface(AServiceType);
end;

function TTestServiceProvider.GetRequiredService(const AServiceType: TServiceType): TObject;
begin
  EnsureBuilt;
  Result := FProvider.GetRequiredService(AServiceType);
end;

function TTestServiceProvider.CreateScope: IServiceScope;
begin
  EnsureBuilt;
  Result := FProvider.CreateScope;
end;

end.
