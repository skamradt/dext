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
unit Dext.Entity.Cache;

interface

uses
  System.SysUtils,
  System.Generics.Collections;

type
  /// <summary>
  ///   Thread-safe cache for generated SQL queries.
  /// </summary>
  TSQLCache = class
  private
    class var FInstance: TSQLCache;
    class var FLock: TObject;
  private
    FCache: TDictionary<string, string>;
    FLockObj: TObject;
    FEnabled: Boolean;
  public
    class constructor Create;
    class destructor Destroy;
    class function Instance: TSQLCache;
    
    constructor Create;
    destructor Destroy; override;
    
    function TryGetSQL(const ASignature: string; out ASQL: string): Boolean;
    procedure AddSQL(const ASignature, ASQL: string);
    procedure Clear;
    
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

implementation

{ TSQLCache }

class constructor TSQLCache.Create;
begin
  FLock := TObject.Create;
end;

class destructor TSQLCache.Destroy;
begin
  FInstance.Free;
  FLock.Free;
end;

class function TSQLCache.Instance: TSQLCache;
begin
  if FInstance = nil then
  begin
    TMonitor.Enter(FLock);
    try
      if FInstance = nil then
        FInstance := TSQLCache.Create;
    finally
      TMonitor.Exit(FLock);
    end;
  end;
  Result := FInstance;
end;

constructor TSQLCache.Create;
begin
  FCache := TDictionary<string, string>.Create;
  FLockObj := TObject.Create;
  FEnabled := True;
end;

destructor TSQLCache.Destroy;
begin
  FCache.Free;
  FLockObj.Free;
  inherited;
end;

function TSQLCache.TryGetSQL(const ASignature: string; out ASQL: string): Boolean;
begin
  if not FEnabled then Exit(False);
  
  TMonitor.Enter(FLockObj);
  try
    Result := FCache.TryGetValue(ASignature, ASQL);
  finally
    TMonitor.Exit(FLockObj);
  end;
end;

procedure TSQLCache.AddSQL(const ASignature, ASQL: string);
begin
  if not FEnabled then Exit;
  
  TMonitor.Enter(FLockObj);
  try
    if not FCache.ContainsKey(ASignature) then
      FCache.Add(ASignature, ASQL);
  finally
    TMonitor.Exit(FLockObj);
  end;
end;

procedure TSQLCache.Clear;
begin
  TMonitor.Enter(FLockObj);
  try
    FCache.Clear;
  finally
    TMonitor.Exit(FLockObj);
  end;
end;

end.
