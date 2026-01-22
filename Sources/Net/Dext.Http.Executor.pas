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
{  Created: 2026-01-22                                                      }
{                                                                           }
{  HTTP Request Executor - Executes parsed .http requests using TRestClient }
{                                                                           }
{***************************************************************************}
unit Dext.Http.Executor;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Dext.Threading.Async,
  Dext.Http.Request,
  Dext.Http.Parser,
  Dext.Net.RestClient;

type
  /// <summary>
  ///   Extended response including request metadata for display purposes.
  /// </summary>
  THttpExecutionResult = record
    RequestName: string;
    RequestMethod: string;
    RequestUrl: string;
    StatusCode: Integer;
    StatusText: string;
    ResponseBody: string;
    ResponseHeaders: TDictionary<string, string>;
    DurationMs: Int64;
    Success: Boolean;
    ErrorMessage: string;
  end;

  /// <summary>
  ///   Executor for HTTP requests using Dext.Net.RestClient.
  /// </summary>
  THttpExecutor = class
  private
    class function MethodToEnum(const AMethod: string): TDextHttpMethod; static;
  public
    /// <summary>
    ///   Executes a single HTTP request asynchronously.
    /// </summary>
    class function ExecuteAsync(const ARequest: THttpRequestInfo): TAsyncBuilder<IRestResponse>; static;
    
    /// <summary>
    ///   Executes a request with variables resolved from the collection.
    /// </summary>
    class function ExecuteWithVariablesAsync(ARequest: THttpRequestInfo; 
      AVariables: TList<THttpVariable>): TAsyncBuilder<IRestResponse>; static;
    
    /// <summary>
    ///   Executes a request and returns a detailed result record.
    ///   This is a synchronous wrapper for use in Dashboard UI.
    /// </summary>
    class function ExecuteSync(ARequest: THttpRequestInfo; 
      AVariables: TList<THttpVariable>): THttpExecutionResult; static;
  end;

implementation

uses
  System.Diagnostics;

{ THttpExecutor }

class function THttpExecutor.MethodToEnum(const AMethod: string): TDextHttpMethod;
var
  Method: string;
begin
  Method := AMethod.ToUpper;
  if Method = 'GET' then Result := hmGET
  else if Method = 'POST' then Result := hmPOST
  else if Method = 'PUT' then Result := hmPUT
  else if Method = 'DELETE' then Result := hmDELETE
  else if Method = 'PATCH' then Result := hmPATCH
  else if Method = 'HEAD' then Result := hmHEAD
  else if Method = 'OPTIONS' then Result := hmOPTIONS
  else Result := hmGET; // Default
end;

class function THttpExecutor.ExecuteAsync(const ARequest: THttpRequestInfo): TAsyncBuilder<IRestResponse>;
var
  LClient: TRestClient;
  LBody: TStringStream;
begin
  LClient := TRestClient.Create(ARequest.Url);
  
  // Add headers
  for var LPair in ARequest.Headers do
    LClient := LClient.Header(LPair.Key, LPair.Value);
  
  // Create body stream if needed
  if ARequest.Body <> '' then
  begin
    LBody := TStringStream.Create(ARequest.Body, TEncoding.UTF8);
    Result := LClient.ExecuteAsync(MethodToEnum(ARequest.Method), '', LBody, True);
  end
  else
    Result := LClient.ExecuteAsync(MethodToEnum(ARequest.Method), '', nil, False);
end;

class function THttpExecutor.ExecuteWithVariablesAsync(ARequest: THttpRequestInfo; 
  AVariables: TList<THttpVariable>): TAsyncBuilder<IRestResponse>;
begin
  // Resolve variables first
  THttpRequestParser.ResolveRequest(ARequest, AVariables);
  Result := ExecuteAsync(ARequest);
end;

class function THttpExecutor.ExecuteSync(ARequest: THttpRequestInfo; 
  AVariables: TList<THttpVariable>): THttpExecutionResult;
var
  Stopwatch: TStopwatch;
  Response: IRestResponse;
  Client: TRestClient;
  Body: TStringStream;
begin
  // Initialize result
  Result.RequestName := ARequest.Name;
  Result.RequestMethod := ARequest.Method;
  Result.Success := False;
  Result.ErrorMessage := '';
  Result.ResponseHeaders := nil;
  
  // Resolve variables first
  THttpRequestParser.ResolveRequest(ARequest, AVariables);
  Result.RequestUrl := ARequest.Url;
  
  Stopwatch := TStopwatch.StartNew;
  
  try
    Client := TRestClient.Create(ARequest.Url);
    
    // Add headers
    for var LPair in ARequest.Headers do
      Client := Client.Header(LPair.Key, LPair.Value);
    
    // Execute request
    if ARequest.Body <> '' then
    begin
      Body := TStringStream.Create(ARequest.Body, TEncoding.UTF8);
      try
        Response := Client.ExecuteAsync(MethodToEnum(ARequest.Method), '', Body, False).Await;
      finally
        Body.Free;
      end;
    end
    else
      Response := Client.ExecuteAsync(MethodToEnum(ARequest.Method), '', nil, False).Await;
    
    Stopwatch.Stop;
    
    // Populate result
    Result.StatusCode := Response.StatusCode;
    Result.StatusText := Response.StatusText;
    Result.ResponseBody := Response.ContentString;
    Result.DurationMs := Stopwatch.ElapsedMilliseconds;
    Result.Success := (Response.StatusCode >= 200) and (Response.StatusCode < 400);
    
  except
    on E: Exception do
    begin
      Stopwatch.Stop;
      Result.StatusCode := 0;
      Result.StatusText := 'Error';
      Result.ResponseBody := '';
      Result.DurationMs := Stopwatch.ElapsedMilliseconds;
      Result.ErrorMessage := E.Message;
    end;
  end;
end;

end.
