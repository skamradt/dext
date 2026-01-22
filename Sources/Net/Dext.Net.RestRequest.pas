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
unit Dext.Net.RestRequest;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.NetEncoding,
  System.SysUtils,
  Dext.Threading.Async,
  Dext.Threading.CancellationToken,
  Dext.Net.RestClient;

type
  { Internal forward declarations }
  IRestRequestData = interface;

  /// <summary>
  /// Fluent Request Builder for complex REST operations.
  /// Uses a record for fluent API and an internal ref-counted class for state memory safety.
  /// </summary>
  TRestRequest = record
  private
    FData: IRestRequestData;
    function GetData: IRestRequestData;
    function GetFullUrl: string;
  public
    constructor Create(AClient: TRestClient; AMethod: TDextHttpMethod; const AEndpoint: string);

    // Configuration
    function Header(const AName, AValue: string): TRestRequest;
    function QueryParam(const AName, AValue: string): TRestRequest;
    function Body(ABody: TStream; AOwns: Boolean = False): TRestRequest; overload;
    function Body<T: class>(const ABody: T): TRestRequest; overload;
    function JsonBody(const AJson: string): TRestRequest;
    function Cancellation(AToken: ICancellationToken): TRestRequest;

    // Execution
    function Execute: TAsyncBuilder<IRestResponse>; overload;
    function Execute<T: class>: TAsyncBuilder<T>; overload;
    function ExecuteAsString: TAsyncBuilder<string>;
  end;

  { Internal state interface }
  IRestRequestData = interface
    ['{D1E2F3A4-B5C6-4D7E-8F9A-0B1C2D3E4F5A}']
    function GetClient: TRestClient;
    function GetMethod: TDextHttpMethod;
    function GetEndpoint: string;
    function GetHeaders: TDictionary<string, string>;
    function GetQueryParams: TDictionary<string, string>;
    function GetBody: TStream;
    function GetToken: ICancellationToken;
    function GetOwnsBody: Boolean;

    procedure SetBody(ABody: TStream; AOwns: Boolean);
    procedure SetToken(AToken: ICancellationToken);
    function DetachBody: TStream;
  end;

implementation

uses
  Dext.Json;

type
  TRestRequestData = class(TInterfacedObject, IRestRequestData)
  private
    FClient: TRestClient;
    FMethod: TDextHttpMethod;
    FEndpoint: string;
    FHeaders: TDictionary<string, string>;
    FQueryParams: TDictionary<string, string>;
    FBody: TStream;
    FToken: ICancellationToken;
    FOwnsBody: Boolean;
  public
    constructor Create(AClient: TRestClient; AMethod: TDextHttpMethod; const AEndpoint: string);
    destructor Destroy; override;

    function GetClient: TRestClient;
    function GetMethod: TDextHttpMethod;
    function GetEndpoint: string;
    function GetHeaders: TDictionary<string, string>;
    function GetQueryParams: TDictionary<string, string>;
    function GetBody: TStream;
    function GetToken: ICancellationToken;
    function GetOwnsBody: Boolean;

    procedure SetBody(ABody: TStream; AOwns: Boolean);
    procedure SetToken(AToken: ICancellationToken);
    function DetachBody: TStream;
  end;

  { TRestRequestData }

constructor TRestRequestData.Create(AClient: TRestClient; AMethod: TDextHttpMethod;
  const AEndpoint: string);
begin
  inherited Create;
  FClient := AClient;
  FMethod := AMethod;
  FEndpoint := AEndpoint;
  FHeaders := TDictionary<string, string>.Create;
  FQueryParams := TDictionary<string, string>.Create;
end;

destructor TRestRequestData.Destroy;
begin
  FHeaders.Free;
  FQueryParams.Free;
  if FOwnsBody then
    FBody.Free;
  inherited;
end;

function TRestRequestData.GetBody: TStream;
begin
  Result := FBody;
end;

function TRestRequestData.GetClient: TRestClient;
begin
  Result := FClient;
end;

function TRestRequestData.GetEndpoint: string;
begin
  Result := FEndpoint;
end;

function TRestRequestData.GetHeaders: TDictionary<string, string>;
begin
  Result := FHeaders;
end;

function TRestRequestData.GetMethod: TDextHttpMethod;
begin
  Result := FMethod;
end;

function TRestRequestData.GetOwnsBody: Boolean;
begin
  Result := FOwnsBody;
end;

function TRestRequestData.GetQueryParams: TDictionary<string, string>;
begin
  Result := FQueryParams;
end;

function TRestRequestData.GetToken: ICancellationToken;
begin
  Result := FToken;
end;

procedure TRestRequestData.SetBody(ABody: TStream; AOwns: Boolean);
begin
  if FOwnsBody and Assigned(FBody) and (FBody <> ABody) then
    FBody.Free;
  FBody := ABody;
  FOwnsBody := AOwns;
end;

procedure TRestRequestData.SetToken(AToken: ICancellationToken);
begin
  FToken := AToken;
end;

function TRestRequestData.DetachBody: TStream;
begin
  Result := FBody;
  FBody := nil;
  FOwnsBody := False;
end;

{ TRestRequest }

constructor TRestRequest.Create(AClient: TRestClient; AMethod: TDextHttpMethod;
  const AEndpoint: string);
begin
  FData := TRestRequestData.Create(AClient, AMethod, AEndpoint);
end;

function TRestRequest.GetData: IRestRequestData;
begin
  if not Assigned(FData) then
    raise EDextRestException.Create('RestRequest not initialized');
  Result := FData;
end;

function TRestRequest.GetFullUrl: string;
var
  Url: string;
  First: Boolean;
  Data: IRestRequestData;
begin
  Data := GetData;
  Url := Data.GetEndpoint;
  if Data.GetQueryParams.Count > 0 then
  begin
    First := not Url.Contains('?');
    for var Pair in Data.GetQueryParams do
    begin
      if First then
        Url := Url + '?'
      else
        Url := Url + '&';
      Url := Url + TNetEncoding.Url.Encode(Pair.Key) + '=' +
        TNetEncoding.Url.Encode(Pair.Value);
      First := False;
    end;
  end;
  Result := Url;
end;

function TRestRequest.Header(const AName, AValue: string): TRestRequest;
begin
  GetData.GetHeaders.AddOrSetValue(AName, AValue);
  Result := Self;
end;

function TRestRequest.QueryParam(const AName, AValue: string): TRestRequest;
begin
  GetData.GetQueryParams.AddOrSetValue(AName, AValue);
  Result := Self;
end;

function TRestRequest.Body(ABody: TStream; AOwns: Boolean): TRestRequest;
begin
  GetData.SetBody(ABody, AOwns);
  Result := Self;
end;

function TRestRequest.Body<T>(const ABody: T): TRestRequest;
var
  Json: string;
begin
  Json := TDextJson.Serialize(ABody);
  Result := JsonBody(Json);
end;

function TRestRequest.JsonBody(const AJson: string): TRestRequest;
begin
  GetData.SetBody(TStringStream.Create(AJson, TEncoding.UTF8), True);
  GetData.GetHeaders.AddOrSetValue('Content-Type', 'application/json');
  Result := Self;
end;

function TRestRequest.Cancellation(AToken: ICancellationToken): TRestRequest;
begin
  GetData.SetToken(AToken);
  Result := Self;
end;

function TRestRequest.Execute: TAsyncBuilder<IRestResponse>;
begin
  var Data := GetData;
  var Client := Data.GetClient;
  var Body: TStream;
  var OwnsBody := Data.GetOwnsBody;

  if OwnsBody then
    Body := Data.DetachBody
  else
    Body := Data.GetBody;

  Result := Client.ExecuteAsync(Data.GetMethod, GetFullUrl, Body, OwnsBody,
    Data.GetHeaders);

  if Assigned(Data.GetToken) then
    Result := Result.WithCancellation(Data.GetToken);
end;

function TRestRequest.ExecuteAsString: TAsyncBuilder<string>;
begin
  Result := Execute.ThenBy<string>(function(LResp: IRestResponse): string
    begin
      Result := LResp.ContentString;
    end);
end;

function TRestRequest.Execute<T>: TAsyncBuilder<T>;
begin
  Result := Execute.ThenBy<T>(function(LResp: IRestResponse): T
    begin
      Result := TDextJson.Deserialize<T>(LResp.ContentString);
    end);
end;

end.

