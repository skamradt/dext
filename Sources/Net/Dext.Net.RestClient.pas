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
unit Dext.Net.RestClient;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.Net.HttpClient,
  System.Net.URLClient,
  System.SyncObjs,
  Dext.Threading.Async,
  Dext.Threading.CancellationToken,
  Dext.Net.ConnectionPool,
  Dext.Net.Authentication;

type
  /// <summary>
  ///   Supported HTTP Methods.
  /// </summary>
  TDextHttpMethod = (hmGET, hmPOST, hmPUT, hmDELETE, hmPATCH, hmHEAD, hmOPTIONS);

  /// <summary>
  ///   Content-Type for requests.
  /// </summary>
  TDextContentType = (
    ctJson, ctXml, ctFormUrlEncoded, ctMultipartFormData, ctBinary, ctText
  );

  /// <summary>
  ///   Interface for a REST Response.
  /// </summary>
  IRestResponse = interface
    ['{B1A2C3D4-E5F6-4A7B-8C9D-0E1F2A3B4C5D}']
    function GetStatusCode: Integer;
    function GetStatusText: string;
    function GetContentStream: TStream;
    function GetContentString: string;
    function GetHeader(const AName: string): string;
    
    property StatusCode: Integer read GetStatusCode;
    property StatusText: string read GetStatusText;
    property ContentStream: TStream read GetContentStream;
    property ContentString: string read GetContentString;
  end;

  /// <summary>
  ///   Interface for a Typed REST Response.
  /// </summary>
  IRestResponse<T> = interface(IRestResponse)
    ['{C1D2E3F4-A5B6-4C7D-8E9F-0A1B2C3D4E5F}']
    function GetData: T;
    property Data: T read GetData;
  end;

  { Internal Implementation Classes - Must be in interface for Generic Visibility }

  TRestResponseImpl = class(TInterfacedObject, IRestResponse)
  private
    FStatusCode: Integer;
    FStatusText: string;
    FContentStream: TMemoryStream;
  protected
    function GetStatusCode: Integer;
    function GetStatusText: string;
    function GetContentStream: TStream;
    function GetContentString: string;
    function GetHeader(const AName: string): string;
  public
    constructor Create(AStatusCode: Integer; const AStatusText: string; AStream: TStream);
    destructor Destroy; override;
  end;

  TRestResponseImpl<T> = class(TRestResponseImpl, IRestResponse<T>)
  private
    FData: T;
  protected
    function GetData: T;
  public
    constructor Create(AStatusCode: Integer; const AStatusText: string; AStream: TStream; AData: T);
    destructor Destroy; override;
  end;

  /// <summary>
  ///   Internal interface for Client Implementation.
  /// </summary>
  IRestClient = interface
    ['{A3B4C5D6-E7F8-49A0-B1C2-D3E4F5A6B7C8}']
    function BaseUrl(const AValue: string): IRestClient;
    function Timeout(AValue: Integer): IRestClient;
    function Retry(AValue: Integer): IRestClient;
    function Auth(AProvider: IAuthenticationProvider): IRestClient;
    function Header(const AName, AValue: string): IRestClient;
    function ContentType(AValue: TDextContentType): IRestClient;
    
    function ExecuteAsync(AMethod: TDextHttpMethod; const AEndpoint: string; 
      const ABody: TStream = nil; AOwnsBody: Boolean = False;
      AHeaders: TDictionary<string, string> = nil): TAsyncBuilder<IRestResponse>;
  end;

  TRestClientImpl = class(TInterfacedObject, IRestClient)
  private
    FBaseUrl: string;
    FTimeout: Integer;
    FMaxRetries: Integer;
    FHeaders: TDictionary<string, string>;
    FContentType: TDextContentType;
    FAuthProvider: IAuthenticationProvider;
    FPool: TConnectionPool;
    FLock: TCriticalSection;
    
    function GetFullUrl(const AEndpoint: string): string;
  public
    constructor Create(const ABaseUrl: string = '');

    destructor Destroy; override;

    function BaseUrl(const AValue: string): IRestClient;
    function Timeout(AValue: Integer): IRestClient;
    function Retry(AValue: Integer): IRestClient;
    function Auth(AProvider: IAuthenticationProvider): IRestClient;
    function Header(const AName, AValue: string): IRestClient;
    function ContentType(AValue: TDextContentType): IRestClient;

    function ExecuteAsync(AMethod: TDextHttpMethod; const AEndpoint: string; 
      const ABody: TStream = nil; AOwnsBody: Boolean = False;
      AHeaders: TDictionary<string, string> = nil): TAsyncBuilder<IRestResponse>;
  end;

  /// <summary>
  ///   Fluent REST Client - Record Pattern to support Generics.
  /// </summary>
  TRestClient = record
  private
    FImpl: IRestClient;
    class var FSharedPool: TConnectionPool;
    class destructor Destroy;
  public
    class function Create(const ABaseUrl: string = ''): TRestClient; static;
    
    // Fluent Configuration
    function BaseUrl(const AValue: string): TRestClient;
    function Timeout(AValue: Integer): TRestClient;
    function Retry(AValue: Integer): TRestClient;
    function BearerToken(const AToken: string): TRestClient;
    function BasicAuth(const AUsername, APassword: string): TRestClient;
    function ApiKey(const AName, AValue: string; AInHeader: Boolean = True): TRestClient;
    function Auth(AProvider: IAuthenticationProvider): TRestClient;
    function Header(const AName, AValue: string): TRestClient;
    function ContentType(AValue: TDextContentType): TRestClient;

    // HTTP Operations
    function Get(const AEndpoint: string = ''): TAsyncBuilder<IRestResponse>; overload;
    function Get<T: class>(const AEndpoint: string = ''): TAsyncBuilder<T>; overload;
    
    function Post(const AEndpoint: string = ''): TAsyncBuilder<IRestResponse>; overload;
    function Post(const AEndpoint: string; const ABody: TStream): TAsyncBuilder<IRestResponse>; overload;
    function Post<TRes: class>(const AEndpoint: string; const ABody: TRes): TAsyncBuilder<IRestResponse<TRes>>; overload;
    
    function Put(const AEndpoint: string = ''): TAsyncBuilder<IRestResponse>; overload;
    function Put(const AEndpoint: string; const ABody: TStream): TAsyncBuilder<IRestResponse>; overload;
    function Put<TRes: class>(const AEndpoint: string; const ABody: TRes): TAsyncBuilder<IRestResponse<TRes>>; overload;
    function Put<T: class>(const AEndpoint: string = ''): TAsyncBuilder<T>; overload;

    
    function Delete(const AEndpoint: string = ''): TAsyncBuilder<IRestResponse>; overload;
    function Delete<T: class>(const AEndpoint: string = ''): TAsyncBuilder<T>; overload;
    
    function ExecuteAsync(AMethod: TDextHttpMethod; const AEndpoint: string; 
      const ABody: TStream = nil; AOwnsBody: Boolean = False;
      AHeaders: TDictionary<string, string> = nil): TAsyncBuilder<IRestResponse>;

      
    property Impl: IRestClient read FImpl;
  end;

  /// <summary>
  ///   Exception for REST Client errors.
  /// </summary>
  EDextRestException = class(Exception);

function RestClient(const ABaseUrl: string = ''): TRestClient;

implementation

uses
  System.Math,
  Dext.Json;

function RestClient(const ABaseUrl: string = ''): TRestClient;
begin
  Result := TRestClient.Create(ABaseUrl);
end;

{ TRestResponseImpl }

constructor TRestResponseImpl.Create(AStatusCode: Integer; const AStatusText: string; AStream: TStream);
begin
  inherited Create;
  FStatusCode := AStatusCode;
  FStatusText := AStatusText;
  FContentStream := TMemoryStream.Create;
  if Assigned(AStream) then
  begin
    AStream.Position := 0;
    FContentStream.CopyFrom(AStream, AStream.Size);
    FContentStream.Position := 0;
  end;
end;

destructor TRestResponseImpl.Destroy;
begin
  FContentStream.Free;
  inherited;
end;

function TRestResponseImpl.GetContentStream: TStream;
begin
  Result := FContentStream;
end;

function TRestResponseImpl.GetContentString: string;
begin
  if FContentStream.Size = 0 then Exit('');
  
  FContentStream.Position := 0;
  var LBytes: TBytes;
  SetLength(LBytes, FContentStream.Size);
  FContentStream.ReadBuffer(LBytes[0], FContentStream.Size);
  Result := TEncoding.UTF8.GetString(LBytes);
end;

function TRestResponseImpl.GetHeader(const AName: string): string;
begin
  Result := '';
end;

function TRestResponseImpl.GetStatusCode: Integer;
begin
  Result := FStatusCode;
end;

function TRestResponseImpl.GetStatusText: string;
begin
  Result := FStatusText;
end;

{ TRestResponseImpl<T> }

constructor TRestResponseImpl<T>.Create(AStatusCode: Integer; const AStatusText: string; AStream: TStream; AData: T);
begin
  inherited Create(AStatusCode, AStatusText, AStream);
  FData := AData;
end;

destructor TRestResponseImpl<T>.Destroy;
begin
  if TValue.From<T>(FData).IsObject then
    TValue.From<T>(FData).AsObject.Free;
  inherited;
end;

function TRestResponseImpl<T>.GetData: T;
begin
  Result := FData;
end;

{ TRestClientImpl }

constructor TRestClientImpl.Create(const ABaseUrl: string);
begin
  inherited Create;
  FBaseUrl := ABaseUrl;
  FTimeout := 30000;
  FHeaders := TDictionary<string, string>.Create;
  FContentType := ctJson;
  FPool := TConnectionPool(TRestClient.FSharedPool);
  FLock := TCriticalSection.Create;
end;

destructor TRestClientImpl.Destroy;
begin
  FHeaders.Free;
  FLock.Free;
  inherited;
end;


function TRestClientImpl.GetFullUrl(const AEndpoint: string): string;

begin
  if FBaseUrl = '' then Exit(AEndpoint);
  
  Result := FBaseUrl;
  if (AEndpoint <> '') then
  begin
    if not Result.EndsWith('/') and not AEndpoint.StartsWith('/') then
      Result := Result + '/';
    Result := Result + AEndpoint;
  end;
end;

function TRestClientImpl.BaseUrl(const AValue: string): IRestClient;
begin
  FBaseUrl := AValue;
  Result := Self;
end;

function TRestClientImpl.Auth(AProvider: IAuthenticationProvider): IRestClient;
begin
  FAuthProvider := AProvider;
  Result := Self;
end;

function TRestClientImpl.ContentType(AValue: TDextContentType): IRestClient;
begin
  FContentType := AValue;
  Result := Self;
end;

function TRestClientImpl.Header(const AName, AValue: string): IRestClient;
begin
  FLock.Enter;
  try
    FHeaders.AddOrSetValue(AName, AValue);
  finally
    FLock.Leave;
  end;
  Result := Self;
end;

function TRestClientImpl.Retry(AValue: Integer): IRestClient;
begin
  FMaxRetries := AValue;
  Result := Self;
end;

function TRestClientImpl.Timeout(AValue: Integer): IRestClient;
begin
  FTimeout := AValue;
  Result := Self;
end;

function TRestClientImpl.ExecuteAsync(AMethod: TDextHttpMethod; const AEndpoint: string; 
  const ABody: TStream; AOwnsBody: Boolean; AHeaders: TDictionary<string, string>): TAsyncBuilder<IRestResponse>;
var
  LUrl: string;
  LRetries: Integer;
  LHeaders: TNetHeaders;
  LTimeout: Integer;
  LAuth: IAuthenticationProvider;
begin
  LUrl := GetFullUrl(AEndpoint);
  LRetries := FMaxRetries;
  LTimeout := FTimeout;
  LAuth := FAuthProvider;
  
  // Snapshot headers (Thread Safety)
  var LHeadList := TList<TNetHeader>.Create;
  try
    FLock.Enter;
    try
      for var LPair in FHeaders do
        LHeadList.Add(TNetHeader.Create(LPair.Key, LPair.Value));
    finally
      FLock.Leave;
    end;
      
    if Assigned(LAuth) then
    begin
       if LAuth is TApiKeyAuthProvider then
         LHeadList.Add(TNetHeader.Create(TApiKeyAuthProvider(LAuth).Key, LAuth.GetHeaderValue))
       else
         LHeadList.Add(TNetHeader.Create('Authorization', LAuth.GetHeaderValue));
    end;

    if Assigned(AHeaders) then
    begin
      for var LPair in AHeaders do
        LHeadList.Add(TNetHeader.Create(LPair.Key, LPair.Value));
    end;
    
    LHeaders := LHeadList.ToArray;
  finally
    LHeadList.Free;
  end;

  
  Result := TAsyncTask.Run<IRestResponse>(
    function: IRestResponse
    var
      LHttpClient: THttpClient;
      LResponse: IHTTPResponse;
      LAttempt: Integer;
      LLastError: Exception;
      LMethodStr: string;
    begin
      try
        LAttempt := 0;
        LLastError := nil;
        
        while LAttempt <= LRetries do
        begin
          LHttpClient := TConnectionPool(TRestClient.FSharedPool).Acquire;
          try
            LHttpClient.ConnectionTimeout := LTimeout;
            LHttpClient.SendTimeout := LTimeout;
            LHttpClient.ResponseTimeout := LTimeout;
            
            case AMethod of
              hmGET:    LMethodStr := 'GET';
              hmPOST:   LMethodStr := 'POST';
              hmPUT:    LMethodStr := 'PUT';
              hmDELETE: LMethodStr := 'DELETE';
              hmPATCH:  LMethodStr := 'PATCH';
              hmHEAD:   LMethodStr := 'HEAD';
              hmOPTIONS:LMethodStr := 'OPTIONS';
              else LMethodStr := '';
            end;

            try
              LResponse := LHttpClient.Execute(LMethodStr, LUrl, ABody, nil, LHeaders) as IHTTPResponse;

              Result := TRestResponseImpl.Create(LResponse.StatusCode, LResponse.StatusText, LResponse.ContentStream);
              Exit;
            except
              on E: Exception do
              begin
                LLastError := E;
                Inc(LAttempt);
                if LAttempt > LRetries then Break;
                Sleep(Trunc(Power(2, LAttempt) * 100));
              end;
            end;
          finally
            TConnectionPool(TRestClient.FSharedPool).Release(LHttpClient);
          end;
        end;
        
        if Assigned(LLastError) then raise LLastError;
      finally
        if AOwnsBody and Assigned(ABody) then
          ABody.Free;
      end;
    end
  );
end;



{ TRestClient }

class destructor TRestClient.Destroy;
begin
  FSharedPool.Free;
end;

class function TRestClient.Create(const ABaseUrl: string): TRestClient;
begin
  // Thread-safe pool initialization
  if not Assigned(FSharedPool) then
  begin
    var LNewPool := TConnectionPool.Create;
    if TInterlocked.CompareExchange(Pointer(FSharedPool), Pointer(LNewPool), nil) <> nil then
      LNewPool.Free;
  end;
  Result.FImpl := TRestClientImpl.Create(ABaseUrl);
end;

function TRestClient.BaseUrl(const AValue: string): TRestClient;
begin
  FImpl.BaseUrl(AValue);
  Result := Self;
end;

function TRestClient.BearerToken(const AToken: string): TRestClient;
begin
  FImpl.Auth(TBearerAuthProvider.Create(AToken));
  Result := Self;
end;

function TRestClient.BasicAuth(const AUsername, APassword: string): TRestClient;
begin
  FImpl.Auth(TBasicAuthProvider.Create(AUsername, APassword));
  Result := Self;
end;

function TRestClient.ApiKey(const AName, AValue: string; AInHeader: Boolean): TRestClient;
begin
  if AInHeader then
    FImpl.Auth(TApiKeyAuthProvider.Create(AName, AValue));
  Result := Self;
end;

function TRestClient.Auth(AProvider: IAuthenticationProvider): TRestClient;
begin
  FImpl.Auth(AProvider);
  Result := Self;
end;

function TRestClient.ContentType(AValue: TDextContentType): TRestClient;
begin
  FImpl.ContentType(AValue);
  Result := Self;
end;

function TRestClient.Header(const AName, AValue: string): TRestClient;
begin
  FImpl.Header(AName, AValue);
  Result := Self;
end;

function TRestClient.Retry(AValue: Integer): TRestClient;
begin
  FImpl.Retry(AValue);
  Result := Self;
end;

function TRestClient.Timeout(AValue: Integer): TRestClient;
begin
  FImpl.Timeout(AValue);
  Result := Self;
end;

function TRestClient.Get(const AEndpoint: string): TAsyncBuilder<IRestResponse>;
begin
  Result := ExecuteAsync(hmGET, AEndpoint);
end;

function TRestClient.Get<T>(const AEndpoint: string): TAsyncBuilder<T>;
var
  LBuilder: TAsyncBuilder<IRestResponse>;
begin
  LBuilder := Get(AEndpoint);
  Result := LBuilder.ThenBy<T>(
    function(LRes: IRestResponse): T
    begin
      Result := TDextJson.Deserialize<T>(LRes.ContentString);
    end);
end;

function TRestClient.Post(const AEndpoint: string): TAsyncBuilder<IRestResponse>;
begin
  Result := ExecuteAsync(hmPOST, AEndpoint);
end;

function TRestClient.Post(const AEndpoint: string; const ABody: TStream): TAsyncBuilder<IRestResponse>;
begin
  Result := ExecuteAsync(hmPOST, AEndpoint, ABody);
end;

function TRestClient.Post<TRes>(const AEndpoint: string; const ABody: TRes): TAsyncBuilder<IRestResponse<TRes>>;
var
  LStream: TStringStream;
  LBuilder: TAsyncBuilder<IRestResponse>;
begin
  LStream := TStringStream.Create(TDextJson.Serialize(ABody), TEncoding.UTF8);
  LBuilder := ExecuteAsync(hmPOST, AEndpoint, LStream, True);
  Result := LBuilder.ThenBy<IRestResponse<TRes>>(
      TFunc<IRestResponse, IRestResponse<TRes>>(
        function(Base: IRestResponse): IRestResponse<TRes>
        begin
          Result := TRestResponseImpl<TRes>.Create(Base.StatusCode, Base.StatusText, Base.ContentStream, 
            TDextJson.Deserialize<TRes>(Base.ContentString));
        end
      )
  );
end;

function TRestClient.Put(const AEndpoint: string): TAsyncBuilder<IRestResponse>;
begin
  Result := ExecuteAsync(hmPUT, AEndpoint);
end;

function TRestClient.Put(const AEndpoint: string; const ABody: TStream): TAsyncBuilder<IRestResponse>;
begin
  Result := ExecuteAsync(hmPUT, AEndpoint, ABody);
end;

function TRestClient.Put<TRes>(const AEndpoint: string; const ABody: TRes): TAsyncBuilder<IRestResponse<TRes>>;
var
  LStream: TStringStream;
  LBuilder: TAsyncBuilder<IRestResponse>;
begin
  LStream := TStringStream.Create(TDextJson.Serialize(ABody), TEncoding.UTF8);
  LBuilder := ExecuteAsync(hmPUT, AEndpoint, LStream, True);
  Result := LBuilder.ThenBy<IRestResponse<TRes>>(
      TFunc<IRestResponse, IRestResponse<TRes>>(
        function(Base: IRestResponse): IRestResponse<TRes>
        begin
          Result := TRestResponseImpl<TRes>.Create(Base.StatusCode, Base.StatusText, Base.ContentStream, 
            TDextJson.Deserialize<TRes>(Base.ContentString));
        end
      )
  );
end;

function TRestClient.Put<T>(const AEndpoint: string): TAsyncBuilder<T>;
var
  LBuilder: TAsyncBuilder<IRestResponse>;
begin
  LBuilder := Put(AEndpoint);
  Result := LBuilder.ThenBy<T>(
    function(LRes: IRestResponse): T
    begin
      Result := TDextJson.Deserialize<T>(LRes.ContentString);
    end);
end;

function TRestClient.Delete(const AEndpoint: string): TAsyncBuilder<IRestResponse>;
begin
  Result := ExecuteAsync(hmDELETE, AEndpoint);
end;

function TRestClient.Delete<T>(const AEndpoint: string): TAsyncBuilder<T>;
var
  LBuilder: TAsyncBuilder<IRestResponse>;
begin
  LBuilder := Delete(AEndpoint);
  Result := LBuilder.ThenBy<T>(
    function(LRes: IRestResponse): T
    begin
      Result := TDextJson.Deserialize<T>(LRes.ContentString);
    end);
end;

function TRestClient.ExecuteAsync(AMethod: TDextHttpMethod; const AEndpoint: string; 
  const ABody: TStream; AOwnsBody: Boolean; AHeaders: TDictionary<string, string>): TAsyncBuilder<IRestResponse>;
begin
  Result := FImpl.ExecuteAsync(AMethod, AEndpoint, ABody, AOwnsBody, AHeaders);
end;


end.
