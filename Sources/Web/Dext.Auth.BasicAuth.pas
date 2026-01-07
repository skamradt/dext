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
{  Created: 2026-01-07                                                      }
{                                                                           }
{  Basic Authentication Middleware                                          }
{                                                                           }
{***************************************************************************}
unit Dext.Auth.BasicAuth;

interface

uses
  System.SysUtils,
  System.NetEncoding,
  Dext.Web.Interfaces,
  Dext.Auth.Identity,
  Dext.Auth.JWT;

type
  /// <summary>
  ///   Configuration options for Basic Authentication.
  /// </summary>
  TBasicAuthOptions = record
    /// <summary>The realm name shown in the browser's authentication dialog</summary>
    Realm: string;
    /// <summary>If True, allows anonymous access to endpoints marked with [AllowAnonymous]</summary>
    AllowAnonymous: Boolean;
    
    class function Default: TBasicAuthOptions; static;
    
    /// <summary>
    ///   Creates options with a custom realm name.
    /// </summary>
    function WithRealm(const ARealm: string): TBasicAuthOptions;
  end;
  
  /// <summary>
  ///   Function type for validating user credentials.
  ///   Returns True if the credentials are valid.
  /// </summary>
  TBasicAuthValidateFunc = reference to function(const AUsername, APassword: string): Boolean;
  
  /// <summary>
  ///   Extended validation function that also returns roles for the user.
  /// </summary>
  TBasicAuthValidateWithRolesFunc = reference to function(const AUsername, APassword: string; 
    out ARoles: TArray<string>): Boolean;
  
  /// <summary>
  ///   Middleware that implements HTTP Basic Authentication (RFC 7617).
  /// </summary>
  TBasicAuthMiddleware = class(TInterfacedObject, IMiddleware)
  private
    FOptions: TBasicAuthOptions;
    FValidateCredentials: TBasicAuthValidateFunc;
    FValidateWithRoles: TBasicAuthValidateWithRolesFunc;
    
    function ExtractCredentials(const AAuthHeader: string; out AUsername, APassword: string): Boolean;
    function CreatePrincipal(const AUsername: string; const ARoles: TArray<string>): IClaimsPrincipal;
  protected
    procedure SendUnauthorizedResponse(AContext: IHttpContext);
  public
    constructor Create(const AOptions: TBasicAuthOptions; 
      AValidateFunc: TBasicAuthValidateFunc); overload;
    constructor Create(const AOptions: TBasicAuthOptions;
      AValidateFunc: TBasicAuthValidateWithRolesFunc); overload;
    
    procedure Invoke(AContext: IHttpContext; ANext: TRequestDelegate);
  end;
  
  /// <summary>
  ///   Extension methods for adding Basic Authentication to the application pipeline.
  /// </summary>
  /// <summary>
  ///   Extension methods for adding Basic Authentication to the application pipeline.
  /// </summary>
  /// <summary>
  ///   Extension methods for adding Basic Authentication to the application pipeline.
  /// </summary>
  /// <summary>
  ///   Extension methods for adding Basic Authentication to the application pipeline.
  /// </summary>
  TApplicationBuilderBasicAuthExtensions = class
  public
    /// <summary>
    ///   Adds Basic Authentication middleware with a simple validation function.
    /// </summary>
    /// <example>
    ///   App.Builder.UseBasicAuthentication('My API',
    ///     function(User, Pass: string): Boolean
    ///     begin
    ///       Result := (User = 'admin') and (Pass = 'secret');
    ///     end);
    /// </example>
    class function UseBasicAuthentication(
      const ABuilder: IApplicationBuilder;
      const ARealm: string;
      AValidateFunc: TBasicAuthValidateFunc): IApplicationBuilder; overload;
    
    /// <summary>
    ///   Adds Basic Authentication middleware with role support.
    /// </summary>
    class function UseBasicAuthentication(
      const ABuilder: IApplicationBuilder;
      const ARealm: string;
      AValidateFunc: TBasicAuthValidateWithRolesFunc): IApplicationBuilder; overload;
    
    /// <summary>
    ///   Adds Basic Authentication middleware with custom options.
    /// </summary>
    class function UseBasicAuthentication(
      const ABuilder: IApplicationBuilder;
      const AOptions: TBasicAuthOptions;
      AValidateFunc: TBasicAuthValidateFunc): IApplicationBuilder; overload;
  end;



implementation

uses
  System.StrUtils;

{ TBasicAuthOptions }

class function TBasicAuthOptions.Default: TBasicAuthOptions;
begin
  Result.Realm := 'Dext API';
  Result.AllowAnonymous := True;
end;

function TBasicAuthOptions.WithRealm(const ARealm: string): TBasicAuthOptions;
begin
  Result := Self;
  Result.Realm := ARealm;
end;

{ TBasicAuthMiddleware }

constructor TBasicAuthMiddleware.Create(const AOptions: TBasicAuthOptions;
  AValidateFunc: TBasicAuthValidateFunc);
begin
  inherited Create;
  FOptions := AOptions;
  FValidateCredentials := AValidateFunc;
  FValidateWithRoles := nil;
end;

constructor TBasicAuthMiddleware.Create(const AOptions: TBasicAuthOptions;
  AValidateFunc: TBasicAuthValidateWithRolesFunc);
begin
  inherited Create;
  FOptions := AOptions;
  FValidateCredentials := nil;
  FValidateWithRoles := AValidateFunc;
end;

function TBasicAuthMiddleware.ExtractCredentials(const AAuthHeader: string;
  out AUsername, APassword: string): Boolean;
var
  EncodedCredentials: string;
  DecodedCredentials: string;
  ColonPos: Integer;
begin
  Result := False;
  AUsername := '';
  APassword := '';
  
  // Check for "Basic " prefix (case-insensitive)
  if not StartsText('Basic ', AAuthHeader) then
    Exit;
  
  // Extract Base64-encoded credentials
  EncodedCredentials := Trim(Copy(AAuthHeader, 7, MaxInt));
  if EncodedCredentials = '' then
    Exit;
  
  try
    // Decode Base64
    DecodedCredentials := TNetEncoding.Base64.Decode(EncodedCredentials);
    
    // Split by colon - format is "username:password"
    ColonPos := Pos(':', DecodedCredentials);
    if ColonPos = 0 then
      Exit;
    
    AUsername := Copy(DecodedCredentials, 1, ColonPos - 1);
    APassword := Copy(DecodedCredentials, ColonPos + 1, MaxInt);
    
    Result := True;
  except
    // Invalid Base64 encoding
    Result := False;
  end;
end;

function TBasicAuthMiddleware.CreatePrincipal(const AUsername: string;
  const ARoles: TArray<string>): IClaimsPrincipal;
var
  Identity: IIdentity;
  Claims: TArray<TClaim>;
  I: Integer;
begin
  Identity := TClaimsIdentity.Create(AUsername, 'Basic');
  
  // Build claims array with username and roles
  SetLength(Claims, 1 + Length(ARoles));
  Claims[0] := TClaim.Create(TClaimTypes.Name, AUsername);
  
  for I := 0 to High(ARoles) do
    Claims[I + 1] := TClaim.Create(TClaimTypes.Role, ARoles[I]);
  
  Result := TClaimsPrincipal.Create(Identity, Claims);
end;

procedure TBasicAuthMiddleware.SendUnauthorizedResponse(AContext: IHttpContext);
begin
  AContext.Response.StatusCode := 401;
  AContext.Response.AddHeader('WWW-Authenticate', 
    Format('Basic realm="%s", charset="UTF-8"', [FOptions.Realm]));
  AContext.Response.SetContentType('application/json; charset=utf-8');
  AContext.Response.Write('{"error": "Unauthorized", "message": "Valid credentials required"}');
end;

procedure TBasicAuthMiddleware.Invoke(AContext: IHttpContext; ANext: TRequestDelegate);
var
  AuthHeader: string;
  Username, Password: string;
  Roles: TArray<string>;
  IsValid: Boolean;
  Principal: IClaimsPrincipal;
begin
  // Try to get Authorization header
  if AContext.Request.Headers.ContainsKey('Authorization') then
  begin
    AuthHeader := AContext.Request.Headers['Authorization'];
    
    if ExtractCredentials(AuthHeader, Username, Password) then
    begin
      // Validate credentials
      SetLength(Roles, 0);
      
      if Assigned(FValidateWithRoles) then
        IsValid := FValidateWithRoles(Username, Password, Roles)
      else if Assigned(FValidateCredentials) then
        IsValid := FValidateCredentials(Username, Password)
      else
        IsValid := False;
      
      if IsValid then
      begin
        // Create principal and set user
        Principal := CreatePrincipal(Username, Roles);
        AContext.User := Principal;
      end;
    end;
  end;
  
  // Continue pipeline - authorization will be handled by [Authorize] attribute
  ANext(AContext);
end;

{ TApplicationBuilderBasicAuthExtensions }

class function TApplicationBuilderBasicAuthExtensions.UseBasicAuthentication(
  const ABuilder: IApplicationBuilder;
  const ARealm: string;
  AValidateFunc: TBasicAuthValidateFunc): IApplicationBuilder;
var
  Options: TBasicAuthOptions;
  Middleware: IMiddleware;
begin
  Options := TBasicAuthOptions.Default;
  Options.Realm := ARealm;
  Middleware := TBasicAuthMiddleware.Create(Options, AValidateFunc);
  Result := ABuilder.UseMiddleware(Middleware);
end;

class function TApplicationBuilderBasicAuthExtensions.UseBasicAuthentication(
  const ABuilder: IApplicationBuilder;
  const ARealm: string;
  AValidateFunc: TBasicAuthValidateWithRolesFunc): IApplicationBuilder;
var
  Options: TBasicAuthOptions;
  Middleware: IMiddleware;
begin
  Options := TBasicAuthOptions.Default;
  Options.Realm := ARealm;
  Middleware := TBasicAuthMiddleware.Create(Options, AValidateFunc);
  Result := ABuilder.UseMiddleware(Middleware);
end;

class function TApplicationBuilderBasicAuthExtensions.UseBasicAuthentication(
  const ABuilder: IApplicationBuilder;
  const AOptions: TBasicAuthOptions;
  AValidateFunc: TBasicAuthValidateFunc): IApplicationBuilder;
var
  Middleware: IMiddleware;
begin
  Middleware := TBasicAuthMiddleware.Create(AOptions, AValidateFunc);
  Result := ABuilder.UseMiddleware(Middleware);
end;

end.
