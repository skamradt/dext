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
{  HTTP Request Parser for .http files (VS Code / IntelliJ format)          }
{                                                                           }
{  Supported syntax:                                                        }
{    @variable = value                    - Variable definition             }
{    @variable = [[env:ENV_NAME]]         - Environment variable reference  }
{    ### Request Name                     - Request separator with name     }
{    GET http://example.com/api           - HTTP method and URL             }
{    Header-Name: Header-Value            - Request header                  }
{    <blank line>                         - Separator before body           }
{    JSON body content                    - Request body                    }
{    [[variable]]                         - Variable interpolation          }
{                                                                           }
{  Note: In actual .http files, use double curly braces, not [[ ]]         }
{                                                                           }
{***************************************************************************}
unit Dext.Http.Parser;

interface

uses
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.RegularExpressions,
  System.SysUtils,
  Dext.Http.Request;

type
  /// <summary>
  ///   Parser for .http files (VS Code / IntelliJ REST Client format).
  /// </summary>
  THttpRequestParser = class
  private
    class function TrimLine(const ALine: string): string; static;
    class function IsVariableLine(const ALine: string): Boolean; static;
    class function IsSeparatorLine(const ALine: string): Boolean; static;
    class function IsRequestLine(const ALine: string): Boolean; static;
    class function IsHeaderLine(const ALine: string): Boolean; static;
    class function IsBlankLine(const ALine: string): Boolean; static;
    class function IsCommentLine(const ALine: string): Boolean; static;
    
    class function ParseVariableLine(const ALine: string): THttpVariable; static;
    class function ParseRequestLine(const ALine: string; out AMethod, AUrl: string): Boolean; static;
    class function ParseHeaderLine(const ALine: string; out AName, AValue: string): Boolean; static;
    class function ExtractRequestName(const ALine: string): string; static;
  public
    /// <summary>
    ///   Parses .http file content into a collection of requests and variables.
    /// </summary>
    class function Parse(const AContent: string): THttpRequestCollection; static;
    
    /// <summary>
    ///   Parses an .http file from disk.
    /// </summary>
    class function ParseFile(const AFilePath: string): THttpRequestCollection; static;
    
    /// <summary>
    ///   Interpolates variables in a text string.
    ///   Replaces {{varName}} with the corresponding variable value.
    ///   Replaces {{env:VAR_NAME}} with the environment variable value.
    /// </summary>
    class function InterpolateVariables(const AText: string; 
      AVariables: TList<THttpVariable>): string; static;
    
    /// <summary>
    ///   Resolves all variables in a request (URL, headers, body).
    /// </summary>
    class procedure ResolveRequest(ARequest: THttpRequestInfo; 
      AVariables: TList<THttpVariable>); static;
  end;

implementation

const
  HTTP_METHODS: array[0..6] of string = ('GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'HEAD', 'OPTIONS');

{ THttpRequestParser }

class function THttpRequestParser.TrimLine(const ALine: string): string;
begin
  Result := ALine.Trim;
end;

class function THttpRequestParser.IsBlankLine(const ALine: string): Boolean;
begin
  Result := TrimLine(ALine) = '';
end;

class function THttpRequestParser.IsCommentLine(const ALine: string): Boolean;
var
  LTrimmed: string;
begin
  LTrimmed := TrimLine(ALine);
  // Comments start with # but not ###
  Result := LTrimmed.StartsWith('#') and not LTrimmed.StartsWith('###');
end;

class function THttpRequestParser.IsVariableLine(const ALine: string): Boolean;
begin
  // Variables start with @
  Result := TrimLine(ALine).StartsWith('@');
end;

class function THttpRequestParser.IsSeparatorLine(const ALine: string): Boolean;
begin
  // Request separators start with ###
  Result := TrimLine(ALine).StartsWith('###');
end;

class function THttpRequestParser.IsRequestLine(const ALine: string): Boolean;
var
  Trimmed: string;
  Method: string;
begin
  Trimmed := TrimLine(ALine).ToUpper;
  for Method in HTTP_METHODS do
    if Trimmed.StartsWith(Method + ' ') then
      Exit(True);
  Result := False;
end;

class function THttpRequestParser.IsHeaderLine(const ALine: string): Boolean;
var
  Trimmed: string;
  ColonPos: Integer;
begin
  Trimmed := TrimLine(ALine);
  if Trimmed = '' then
    Exit(False);
  
  // Header lines contain a colon but don't start with special chars
  ColonPos := Trimmed.IndexOf(':');
  Result := (ColonPos > 0) and not Trimmed.StartsWith('@') and not Trimmed.StartsWith('#');
end;

class function THttpRequestParser.ParseVariableLine(const ALine: string): THttpVariable;
var
  Trimmed: string;
  EqualPos: Integer;
  Name, Value: string;
begin
  Trimmed := TrimLine(ALine);
  
  // Remove leading @
  Trimmed := Trimmed.Substring(1);
  
  EqualPos := Trimmed.IndexOf('=');
  if EqualPos > 0 then
  begin
    Name := Trimmed.Substring(0, EqualPos).Trim;
    Value := Trimmed.Substring(EqualPos + 1).Trim;
    
    // Check if value is an environment variable reference {{env:NAME}}
    if Value.StartsWith('{{env:') and Value.EndsWith('}}') then
    begin
      var LEnvName := Value.Substring(6, Value.Length - 8);
      Result := THttpVariable.CreateEnvVar(Name, LEnvName);
    end
    else
      Result := THttpVariable.Create(Name, Value);
  end
  else
  begin
    Result := THttpVariable.Create(Trimmed, '');
  end;
end;

class function THttpRequestParser.ParseRequestLine(const ALine: string; out AMethod, AUrl: string): Boolean;
var
  Trimmed: string;
  SpacePos: Integer;
  Method: string;
begin
  Trimmed := TrimLine(ALine);
  SpacePos := Trimmed.IndexOf(' ');
  
  if SpacePos > 0 then
  begin
    Method := Trimmed.Substring(0, SpacePos).ToUpper;
    for var M in HTTP_METHODS do
    begin
      if M = Method then
      begin
        AMethod := Method;
        AUrl := Trimmed.Substring(SpacePos + 1).Trim;
        Exit(True);
      end;
    end;
  end;
  
  Result := False;
end;

class function THttpRequestParser.ParseHeaderLine(const ALine: string; out AName, AValue: string): Boolean;
var
  Trimmed: string;
  ColonPos: Integer;
begin
  Trimmed := TrimLine(ALine);
  ColonPos := Trimmed.IndexOf(':');
  
  if ColonPos > 0 then
  begin
    AName := Trimmed.Substring(0, ColonPos).Trim;
    AValue := Trimmed.Substring(ColonPos + 1).Trim;
    Result := True;
  end
  else
    Result := False;
end;

class function THttpRequestParser.ExtractRequestName(const ALine: string): string;
begin
  // Remove leading ### and trim
  Result := TrimLine(ALine);
  if Result.StartsWith('###') then
    Result := Result.Substring(3).Trim;
end;

class function THttpRequestParser.Parse(const AContent: string): THttpRequestCollection;

  type
    TParserState = (psStart, psRequest, psHeaders, psBody);
    
var
  Lines: TArray<string>;
  State: TParserState;
  CurrentRequest: THttpRequestInfo;
  CurrentName: string;
  BodyBuilder: TStringBuilder;
  Method, Url, HeaderName, HeaderValue: string;
  I: Integer;
begin
  Result := THttpRequestCollection.Create;
  State := psStart;
  CurrentRequest := nil;
  CurrentName := '';
  BodyBuilder := TStringBuilder.Create;
  
  try
    Lines := AContent.Replace(#13#10, #10).Split([#10]);
    
    for I := 0 to High(Lines) do
    begin
      var LLine := Lines[I];
      
      // Skip comment lines (but not separators)
      if IsCommentLine(LLine) then
        Continue;
      
      // Variable definition (can appear anywhere)
      if IsVariableLine(LLine) then
      begin
        Result.AddVariable(ParseVariableLine(LLine));
        Continue;
      end;
      
      // Request separator
      if IsSeparatorLine(LLine) then
      begin
        // Save pending request
        if Assigned(CurrentRequest) then
        begin
          if State = psBody then
            CurrentRequest.Body := BodyBuilder.ToString.Trim;
          Result.AddRequest(CurrentRequest);
        end;
        
        // Start new request context
        CurrentRequest := nil;
        CurrentName := ExtractRequestName(LLine);
        State := psStart;
        BodyBuilder.Clear;
        Continue;
      end;
      
      // Request line (method + URL)
      if IsRequestLine(LLine) and (State in [psStart]) then
      begin
        // Save any pending request
        if Assigned(CurrentRequest) then
        begin
          if State = psBody then
            CurrentRequest.Body := BodyBuilder.ToString.Trim;
          Result.AddRequest(CurrentRequest);
        end;
        
        if ParseRequestLine(LLine, Method, Url) then
        begin
          CurrentRequest := THttpRequestInfo.Create;
          CurrentRequest.Name := CurrentName;
          CurrentRequest.Method := Method;
          CurrentRequest.Url := Url;
          CurrentRequest.LineNumber := I + 1;
          State := psHeaders;
          BodyBuilder.Clear;
          CurrentName := ''; // Reset name for next request
        end;
        Continue;
      end;
      
      // Handle based on state
      case State of
        psHeaders:
        begin
          if IsBlankLine(LLine) then
          begin
            State := psBody;
          end
          else if IsHeaderLine(LLine) then
          begin
            if ParseHeaderLine(LLine, HeaderName, HeaderValue) then
              CurrentRequest.Headers.AddOrSetValue(HeaderName, HeaderValue);
          end
          else if IsRequestLine(LLine) then
          begin
            // New request without separator - save current and start new
            if Assigned(CurrentRequest) then
              Result.AddRequest(CurrentRequest);
            
            if ParseRequestLine(LLine, Method, Url) then
            begin
              CurrentRequest := THttpRequestInfo.Create;
              CurrentRequest.Name := CurrentName;
              CurrentRequest.Method := Method;
              CurrentRequest.Url := Url;
              CurrentRequest.LineNumber := I + 1;
              BodyBuilder.Clear;
            end;
          end;
        end;
        
        psBody:
        begin
          // Check if this is a new request line (without separator)
          if IsRequestLine(LLine) then
          begin
            CurrentRequest.Body := BodyBuilder.ToString.Trim;
            Result.AddRequest(CurrentRequest);
            
            if ParseRequestLine(LLine, Method, Url) then
            begin
              CurrentRequest := THttpRequestInfo.Create;
              CurrentRequest.Name := '';
              CurrentRequest.Method := Method;
              CurrentRequest.Url := Url;
              CurrentRequest.LineNumber := I + 1;
              State := psHeaders;
              BodyBuilder.Clear;
            end;
          end
          else
          begin
            // Add to body
            if BodyBuilder.Length > 0 then
              BodyBuilder.AppendLine;
            BodyBuilder.Append(LLine);
          end;
        end;
      end;
    end;
    
    // Save last request
    if Assigned(CurrentRequest) then
    begin
      if State = psBody then
        CurrentRequest.Body := BodyBuilder.ToString.Trim;
      Result.AddRequest(CurrentRequest);
    end;
    
  finally
    BodyBuilder.Free;
  end;
end;

class function THttpRequestParser.ParseFile(const AFilePath: string): THttpRequestCollection;
var
  Content: string;
begin
  Content := TFile.ReadAllText(AFilePath, TEncoding.UTF8);
  Result := Parse(Content);
end;

class function THttpRequestParser.InterpolateVariables(const AText: string; 
  AVariables: TList<THttpVariable>): string;
var
  Match: TMatch;
  VarName: string;
  Value: string;
begin
  Result := AText;
  
  // Match {{varName}} or {{env:VAR_NAME}}
  var Regex := TRegEx.Create('\{\{([^}]+)\}\}');
  
  while True do
  begin
    Match := Regex.Match(Result);
    if not Match.Success then
      Break;
    
    VarName := Match.Groups[1].Value;
    
    // Check for env: prefix
    if VarName.StartsWith('env:', True) then
    begin
      var LEnvName := VarName.Substring(4);
      Value := GetEnvironmentVariable(LEnvName);
    end
    else
    begin
      // Look up in variables
      Value := '';
      for var V in AVariables do
      begin
        if SameText(V.Name, VarName) then
        begin
          if V.IsEnvVar then
            Value := GetEnvironmentVariable(V.EnvVarName)
          else
            Value := V.Value;
          Break;
        end;
      end;
    end;
    
    Result := Result.Substring(0, Match.Index - 1) + Value + 
              Result.Substring(Match.Index + Match.Length - 1);
  end;
end;

class procedure THttpRequestParser.ResolveRequest(ARequest: THttpRequestInfo; 
  AVariables: TList<THttpVariable>);
var
  NewHeaders: TDictionary<string, string>;
begin
  // Resolve URL
  ARequest.Url := InterpolateVariables(ARequest.Url, AVariables);
  
  // Resolve headers
  NewHeaders := TDictionary<string, string>.Create;
  try
    for var LPair in ARequest.Headers do
      NewHeaders.Add(LPair.Key, InterpolateVariables(LPair.Value, AVariables));
    
    ARequest.Headers.Clear;
    for var LPair in NewHeaders do
      ARequest.Headers.Add(LPair.Key, LPair.Value);
  finally
    NewHeaders.Free;
  end;
  
  // Resolve body
  if ARequest.Body <> '' then
    ARequest.Body := InterpolateVariables(ARequest.Body, AVariables);
end;

end.
