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
{  HTTP Request Models for .http file parsing                               }
{                                                                           }
{***************************************************************************}
unit Dext.Http.Request;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections;

type
  /// <summary>
  ///   Represents a variable defined in an .http file.
  ///   Format: @name = value
  /// </summary>
  THttpVariable = record
    Name: string;
    Value: string;
    /// <summary>
    ///   True if the value uses {{env:NAME}} syntax to reference an environment variable.
    /// </summary>
    IsEnvVar: Boolean;
    /// <summary>
    ///   The name of the environment variable if IsEnvVar is True.
    /// </summary>
    EnvVarName: string;
    
    class function Create(const AName, AValue: string): THttpVariable; static;
    class function CreateEnvVar(const AName, AEnvVarName: string): THttpVariable; static;
  end;

  /// <summary>
  ///   Represents a parsed HTTP request from an .http file.
  /// </summary>
  THttpRequestInfo = class
  private
    FName: string;
    FMethod: string;
    FUrl: string;
    FHeaders: TDictionary<string, string>;
    FBody: string;
    FLineNumber: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    /// <summary>
    ///   The name of the request (from ### Comment).
    /// </summary>
    property Name: string read FName write FName;
    
    /// <summary>
    ///   HTTP method (GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS).
    /// </summary>
    property Method: string read FMethod write FMethod;
    
    /// <summary>
    ///   The URL (may contain {{variables}}).
    /// </summary>
    property Url: string read FUrl write FUrl;
    
    /// <summary>
    ///   Request headers.
    /// </summary>
    property Headers: TDictionary<string, string> read FHeaders;
    
    /// <summary>
    ///   Request body (for POST/PUT/PATCH).
    /// </summary>
    property Body: string read FBody write FBody;
    
    /// <summary>
    ///   Line number in the original file where this request starts.
    /// </summary>
    property LineNumber: Integer read FLineNumber write FLineNumber;
  end;

  /// <summary>
  ///   Collection of HTTP requests and variables parsed from an .http file.
  /// </summary>
  THttpRequestCollection = class
  private
    FVariables: TList<THttpVariable>;
    FRequests: TObjectList<THttpRequestInfo>;
  public
    constructor Create;
    destructor Destroy; override;
    
    /// <summary>
    ///   Variables defined in the file (@name = value).
    /// </summary>
    property Variables: TList<THttpVariable> read FVariables;
    
    /// <summary>
    ///   HTTP requests parsed from the file.
    /// </summary>
    property Requests: TObjectList<THttpRequestInfo> read FRequests;
    
    /// <summary>
    ///   Finds a request by name.
    /// </summary>
    function FindByName(const AName: string): THttpRequestInfo;
    
    /// <summary>
    ///   Gets the value of a variable by name. Returns empty string if not found.
    /// </summary>
    function GetVariable(const AName: string): string;
    
    /// <summary>
    ///   Adds a variable to the collection.
    /// </summary>
    procedure AddVariable(const AVariable: THttpVariable);
    
    /// <summary>
    ///   Adds a request to the collection.
    /// </summary>
    procedure AddRequest(ARequest: THttpRequestInfo);
  end;

implementation

{ THttpVariable }

class function THttpVariable.Create(const AName, AValue: string): THttpVariable;
begin
  Result.Name := AName;
  Result.Value := AValue;
  Result.IsEnvVar := False;
  Result.EnvVarName := '';
end;

class function THttpVariable.CreateEnvVar(const AName, AEnvVarName: string): THttpVariable;
begin
  Result.Name := AName;
  Result.Value := '';
  Result.IsEnvVar := True;
  Result.EnvVarName := AEnvVarName;
end;

{ THttpRequestInfo }

constructor THttpRequestInfo.Create;
begin
  inherited;
  FHeaders := TDictionary<string, string>.Create;
end;

destructor THttpRequestInfo.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

{ THttpRequestCollection }

constructor THttpRequestCollection.Create;
begin
  inherited;
  FVariables := TList<THttpVariable>.Create;
  FRequests := TObjectList<THttpRequestInfo>.Create;
end;

destructor THttpRequestCollection.Destroy;
begin
  FRequests.Free;
  FVariables.Free;
  inherited;
end;

function THttpRequestCollection.FindByName(const AName: string): THttpRequestInfo;
begin
  for var Request in FRequests do
    if SameText(Request.Name, AName) then
      Exit(Request);
  Result := nil;
end;

function THttpRequestCollection.GetVariable(const AName: string): string;
begin
  for var Variable in FVariables do
  begin
    if SameText(Variable.Name, AName) then
    begin
      if Variable.IsEnvVar then
        Exit(GetEnvironmentVariable(Variable.EnvVarName))
      else
        Exit(Variable.Value);
    end;
  end;
  Result := '';
end;

procedure THttpRequestCollection.AddVariable(const AVariable: THttpVariable);
begin
  FVariables.Add(AVariable);
end;

procedure THttpRequestCollection.AddRequest(ARequest: THttpRequestInfo);
begin
  FRequests.Add(ARequest);
end;

end.
