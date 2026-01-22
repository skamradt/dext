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
{  Author:  Cesar Romero & Antigravity                                      }
{  Created: 2026-01-21                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.Net.Authentication;

interface

uses
  System.SysUtils,
  System.Classes,
  System.NetEncoding;

type
  IAuthenticationProvider = interface
    ['{E1D2C3B4-A5B6-4C7D-8E9F-0A1B2C3D4E5F}']
    function GetHeaderValue: string;
  end;

  TBearerAuthProvider = class(TInterfacedObject, IAuthenticationProvider)
  private
    FToken: string;
  public
    constructor Create(const AToken: string);
    function GetHeaderValue: string;
  end;

  TBasicAuthProvider = class(TInterfacedObject, IAuthenticationProvider)
  private
    FUsername: string;
    FPassword: string;
  public
    constructor Create(const AUsername, APassword: string);
    function GetHeaderValue: string;
  end;

  TApiKeyAuthProvider = class(TInterfacedObject, IAuthenticationProvider)
  private
    FKey: string;
    FValue: string;
  public
    constructor Create(const AKey, AValue: string);
    function GetHeaderValue: string;
    property Key: string read FKey;
  end;

implementation

{ TBearerAuthProvider }

constructor TBearerAuthProvider.Create(const AToken: string);
begin
  inherited Create;
  FToken := AToken;
end;

function TBearerAuthProvider.GetHeaderValue: string;
begin
  Result := 'Bearer ' + FToken;
end;

{ TBasicAuthProvider }

constructor TBasicAuthProvider.Create(const AUsername, APassword: string);
begin
  inherited Create;
  FUsername := AUsername;
  FPassword := APassword;
end;

function TBasicAuthProvider.GetHeaderValue: string;
var
  Auth: string;
begin
  Auth := FUsername + ':' + FPassword;
  Result := 'Basic ' + TNetEncoding.Base64.Encode(Auth).Replace(#13#10, '');
end;

{ TApiKeyAuthProvider }

constructor TApiKeyAuthProvider.Create(const AKey, AValue: string);
begin
  inherited Create;
  FKey := AKey;
  FValue := AValue;
end;

function TApiKeyAuthProvider.GetHeaderValue: string;
begin
  Result := FValue;
end;

end.
