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
unit Dext.Auth.JWT;
{$I ..\Dext.inc}

interface

uses
  System.Classes,
  System.DateUtils,
  System.Generics.Collections,
  System.JSON,
  System.NetEncoding,
  System.Rtti,
  System.SysUtils,
  System.Hash,
  IdGlobal,
  IdHashSHA,
  IdHMAC,
  IdHMACSHA1,
  IdSSLOpenSSL;

type
  /// <summary>
  ///   Represents a claim (key-value pair) in a JWT token.
  /// </summary>
  TClaim = record
    ClaimType: string;
    Value: string;
    constructor Create(const AType, AValue: string);
  end;

  /// <summary>
  ///   JWT token validation result.
  /// </summary>
  TJwtValidationResult = record
    IsValid: Boolean;
    ErrorMessage: string;
    Claims: TArray<TClaim>;
  end;

  /// <summary>
  ///   JWT token generator and validator interface.
  /// </summary>
  IJwtTokenHandler = interface
    ['{A1B2C3D4-E5F6-7A8B-9C0D-1E2F3A4B5C6D}']
    function GenerateToken(const AClaims: TArray<TClaim>): string;
    function ValidateToken(const AToken: string): TJwtValidationResult;
    function GetClaims(const AToken: string): TArray<TClaim>;
    
    function GetSecretKey: string;
    procedure SetSecretKey(const Value: string);
    function GetIssuer: string;
    procedure SetIssuer(const Value: string);
    function GetAudience: string;
    procedure SetAudience(const Value: string);
    function GetExpirationMinutes: Integer;
    procedure SetExpirationMinutes(const Value: Integer);
    
    property SecretKey: string read GetSecretKey write SetSecretKey;
    property Issuer: string read GetIssuer write SetIssuer;
    property Audience: string read GetAudience write SetAudience;
    property ExpirationMinutes: Integer read GetExpirationMinutes write SetExpirationMinutes;
  end;

  /// <summary>
  ///   JWT authentication configuration options.
  /// </summary>
  TJwtOptions = record
  public
    /// <summary>
    ///   Secret key used for signing and validating tokens.
    /// </summary>
    SecretKey: string;
    
    /// <summary>
    ///   Token issuer (iss claim).
    /// </summary>
    Issuer: string;
    
    /// <summary>
    ///   Token audience (aud claim).
    /// </summary>
    Audience: string;
    
    /// <summary>
    ///   Token expiration time in minutes.
    /// </summary>
    ExpirationMinutes: Integer;

    /// <summary>
    ///   Creates default JWT options.
    /// </summary>
    class function Create(const ASecretKey: string): TJwtOptions; static;
  end;

  /// <summary>
  ///   Fluent builder for creating JWT options.
  /// </summary>
  /// <summary>
  ///   Fluent builder for creating JWT options.
  /// </summary>
  TJwtOptionsBuilder = record
  private
    FOptions: TJwtOptions;
    FInitialized: Boolean;
    procedure EnsureInitialized(const ASecretKey: string = '');
  public
    /// <summary>
    ///   Creates a new JWT options builder.
    /// </summary>
    class function Create(const ASecretKey: string): TJwtOptionsBuilder; static;
    
    // =====================================================================
    // New API (without 'With' prefix)
    // =====================================================================
    
    function Issuer(const AIssuer: string): TJwtOptionsBuilder;
    function Audience(const AAudience: string): TJwtOptionsBuilder;
    function ExpirationMinutes(AMinutes: Integer): TJwtOptionsBuilder;

    // =====================================================================
    // Deprecated API (with 'With' prefix)
    // =====================================================================
    
    function WithIssuer(const AIssuer: string): TJwtOptionsBuilder; deprecated 'Use Issuer instead';
    function WithAudience(const AAudience: string): TJwtOptionsBuilder; deprecated 'Use Audience instead';
    function WithExpirationMinutes(AMinutes: Integer): TJwtOptionsBuilder; deprecated 'Use ExpirationMinutes instead';

    /// <summary>
    ///   Builds and returns the JWT options.
    /// </summary>
    function Build: TJwtOptions;
    
    /// <summary>
    ///   Implicit conversion to TJwtOptions.
    /// </summary>
    class operator Implicit(const ABuilder: TJwtOptionsBuilder): TJwtOptions;
  end;

  /// <summary>
  ///   Delegate for configuring JWT options via builder (passed by reference).
  /// </summary>
  TJwtBuilderProc = reference to procedure(var Builder: TJwtOptionsBuilder);

  /// <summary>
  ///   Helper for implicit conversion of TJwtOptions to TValue.
  /// </summary>
  TJwtOptionsHelper = record helper for TJwtOptions
  public
    class operator Implicit(const AValue: TJwtOptions): TValue;
  end;

  /// <summary>
  ///   JWT token generator and validator.
  /// </summary>
  TJwtTokenHandler = class(TInterfacedObject, IJwtTokenHandler)
  private
    FSecretKey: string;
    FIssuer: string;
    FAudience: string;
    FExpirationMinutes: Integer;
    FBase64: TBase64Encoding;

    function Base64UrlEncode(const AInput: string): string;
    function Base64UrlDecode(const AInput: string): string;
    function CreateSignature(const AHeader, APayload: string): string;
    function VerifySignature(const AToken: string): Boolean;
    
    function GetSecretKey: string;
    procedure SetSecretKey(const Value: string);
    function GetIssuer: string;
    procedure SetIssuer(const Value: string);
    function GetAudience: string;
    procedure SetAudience(const Value: string);
    function GetExpirationMinutes: Integer;
    procedure SetExpirationMinutes(const Value: Integer);
  public
    constructor Create(const ASecretKey: string; const AIssuer: string = '';
      const AAudience: string = ''; AExpirationMinutes: Integer = 60);
    destructor Destroy; override;

    /// <summary>
    ///   Generates a JWT token with the specified claims.
    /// </summary>
    function GenerateToken(const AClaims: TArray<TClaim>): string;

    /// <summary>
    ///   Validates a JWT token and returns the claims if valid.
    /// </summary>
    function ValidateToken(const AToken: string): TJwtValidationResult;

    /// <summary>
    ///   Extracts claims from a token without full validation (use with caution).
    /// </summary>
    function GetClaims(const AToken: string): TArray<TClaim>;

    property SecretKey: string read GetSecretKey write SetSecretKey;
    property Issuer: string read GetIssuer write SetIssuer;
    property Audience: string read GetAudience write SetAudience;
    property ExpirationMinutes: Integer read GetExpirationMinutes write SetExpirationMinutes;
  end;

implementation

{ TClaim }

constructor TClaim.Create(const AType, AValue: string);
begin
  ClaimType := AType;
  Value := AValue;
end;

{ TJwtTokenHandler }

constructor TJwtTokenHandler.Create(const ASecretKey, AIssuer, AAudience: string;
  AExpirationMinutes: Integer);
begin
  inherited Create;
  FSecretKey := ASecretKey;
  FIssuer := AIssuer;
  FAudience := AAudience;
  FExpirationMinutes := AExpirationMinutes;
  FBase64 := TBase64Encoding.Create(0);
end;

function TJwtTokenHandler.Base64UrlEncode(const AInput: string): string;
var
  Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(AInput);
  Result := FBase64.EncodeBytesToString(Bytes);
  
  // Convert to Base64URL format
  Result := Result.Replace('+', '-', [rfReplaceAll]);
  Result := Result.Replace('/', '_', [rfReplaceAll]);
  Result := Result.Replace('=', '', [rfReplaceAll]);
end;

function TJwtTokenHandler.Base64UrlDecode(const AInput: string): string;
var
  Base64: string;
  Bytes: TBytes;
  Padding: Integer;
begin
  Base64 := AInput;
  
  // Convert from Base64URL to standard Base64
  Base64 := Base64.Replace('-', '+', [rfReplaceAll]);
  Base64 := Base64.Replace('_', '/', [rfReplaceAll]);
  
  // Add padding
  Padding := 4 - (Length(Base64) mod 4);
  if Padding < 4 then
    Base64 := Base64 + StringOfChar('=', Padding);
  
  Bytes := TNetEncoding.Base64.DecodeStringToBytes(Base64);
  Result := TEncoding.UTF8.GetString(Bytes);
end;

function TJwtTokenHandler.CreateSignature(const AHeader, APayload: string): string;
var
  Data: string;
  Base64Hash: string;
  {$IFDEF DEXT_HAS_SYSTEM_HASH}
  Hash: TBytes;
  {$ELSE}
  HMAC: TIdHMACSHA256;
  Hash: TIdBytes;
  {$ENDIF}
begin
  Data := AHeader + '.' + APayload;
  
  {$IFDEF DEXT_HAS_SYSTEM_HASH}
  // Use native Delphi System.Hash (Delphi XE8+)
  Hash := THashSHA2.GetHMACAsBytes(
    TEncoding.UTF8.GetBytes(Data),
    TEncoding.UTF8.GetBytes(FSecretKey)
  );
  Base64Hash := TNetEncoding.Base64.EncodeBytesToString(Hash);
  {$ELSE}
  // Fallback to Indy/OpenSSL for older Delphi versions
  if not TIdHashSHA256.IsAvailable then
    LoadOpenSSLLibrary;
  
  HMAC := TIdHMACSHA256.Create;
  try
    HMAC.Key := IndyTextEncoding_UTF8.GetBytes(FSecretKey);
    Hash := HMAC.HashValue(IndyTextEncoding_UTF8.GetBytes(Data));
    Base64Hash := TNetEncoding.Base64.EncodeBytesToString(Hash);
  finally
    HMAC.Free;
  end;
  {$ENDIF}
    
  // Convert to Base64URL format
  Result := Base64Hash.Replace('+', '-', [rfReplaceAll]);
  Result := Result.Replace('/', '_', [rfReplaceAll]);
  Result := Result.Replace('=', '', [rfReplaceAll]);
end;

destructor TJwtTokenHandler.Destroy;
begin
  FBase64.Free;
  inherited;
end;

function TJwtTokenHandler.GetSecretKey: string;
begin
  Result := FSecretKey;
end;

procedure TJwtTokenHandler.SetSecretKey(const Value: string);
begin
  FSecretKey := Value;
end;

function TJwtTokenHandler.GetIssuer: string;
begin
  Result := FIssuer;
end;

procedure TJwtTokenHandler.SetIssuer(const Value: string);
begin
  FIssuer := Value;
end;

function TJwtTokenHandler.GetAudience: string;
begin
  Result := FAudience;
end;

procedure TJwtTokenHandler.SetAudience(const Value: string);
begin
  FAudience := Value;
end;

function TJwtTokenHandler.GetExpirationMinutes: Integer;
begin
  Result := FExpirationMinutes;
end;

procedure TJwtTokenHandler.SetExpirationMinutes(const Value: Integer);
begin
  FExpirationMinutes := Value;
end;

{ TJwtOptions }

class function TJwtOptions.Create(const ASecretKey: string): TJwtOptions;
begin
  Result.SecretKey := ASecretKey;
  Result.Issuer := '';
  Result.Audience := '';
  Result.ExpirationMinutes := 60; // Default: 1 hour
end;

{ TJwtOptionsBuilder }
 
procedure TJwtOptionsBuilder.EnsureInitialized(const ASecretKey: string);
begin
  if not FInitialized then
  begin
    FOptions := TJwtOptions.Create(ASecretKey);
    FInitialized := True;
  end;
end;

class function TJwtOptionsBuilder.Create(const ASecretKey: string): TJwtOptionsBuilder;
begin
  Result.FOptions := TJwtOptions.Create(ASecretKey);
  Result.FInitialized := True;
end;

function TJwtOptionsBuilder.Issuer(const AIssuer: string): TJwtOptionsBuilder;
begin
  EnsureInitialized;
  FOptions.Issuer := AIssuer;
  Result := Self;
end;

function TJwtOptionsBuilder.Audience(const AAudience: string): TJwtOptionsBuilder;
begin
  EnsureInitialized;
  FOptions.Audience := AAudience;
  Result := Self;
end;

function TJwtOptionsBuilder.ExpirationMinutes(AMinutes: Integer): TJwtOptionsBuilder;
begin
  EnsureInitialized;
  FOptions.ExpirationMinutes := AMinutes;
  Result := Self;
end;

// Deprecated Implementation
function TJwtOptionsBuilder.WithIssuer(const AIssuer: string): TJwtOptionsBuilder;
begin
  Result := Issuer(AIssuer);
end;

function TJwtOptionsBuilder.WithAudience(const AAudience: string): TJwtOptionsBuilder;
begin
  Result := Audience(AAudience);
end;

function TJwtOptionsBuilder.WithExpirationMinutes(AMinutes: Integer): TJwtOptionsBuilder;
begin
  Result := ExpirationMinutes(AMinutes);
end;

function TJwtOptionsBuilder.Build: TJwtOptions;
begin
  EnsureInitialized;
  Result := FOptions;
end;

class operator TJwtOptionsBuilder.Implicit(const ABuilder: TJwtOptionsBuilder): TJwtOptions;
begin
  Result := ABuilder.FOptions;
end;

{ TJwtOptionsHelper }

class operator TJwtOptionsHelper.Implicit(const AValue: TJwtOptions): TValue;
begin
  Result := TValue.From<TJwtOptions>(AValue);
end;

function TJwtTokenHandler.GenerateToken(const AClaims: TArray<TClaim>): string;
var
  HeaderStr, PayloadStr, Signature: string;
  Claim: TClaim;
  ExpirationTime: TDateTime;
begin
  // Create header - build JSON manually to avoid formatting
  HeaderStr := Base64UrlEncode('{"alg":"HS256","typ":"JWT"}');

  // Create payload - build JSON manually to avoid formatting
  var PayloadJson := TStringBuilder.Create;
  try
    PayloadJson.Append('{');
    
    // Add standard claims
    if FIssuer <> '' then
      PayloadJson.AppendFormat('"iss":"%s",', [FIssuer]);
    
    if FAudience <> '' then
      PayloadJson.AppendFormat('"aud":"%s",', [FAudience]);
    
    PayloadJson.AppendFormat('"iat":%d,', [DateTimeToUnix(Now)]);
    
    ExpirationTime := IncMinute(Now, FExpirationMinutes);
    PayloadJson.AppendFormat('"exp":%d', [DateTimeToUnix(ExpirationTime)]);
    
    // Add custom claims
    for Claim in AClaims do
      PayloadJson.AppendFormat(',"%s":"%s"', [Claim.ClaimType, Claim.Value]);
    
    PayloadJson.Append('}');
    PayloadStr := PayloadJson.ToString;
    PayloadStr := Base64UrlEncode(PayloadStr);
  finally
    PayloadJson.Free;
  end;

  // Create signature
  Signature := CreateSignature(HeaderStr, PayloadStr);
  
  Result := HeaderStr + '.' + PayloadStr + '.' + Signature;
end;

function TJwtTokenHandler.VerifySignature(const AToken: string): Boolean;
var
  Parts: TArray<string>;
  ExpectedSignature: string;
begin
  Parts := AToken.Split(['.']);
  if Length(Parts) <> 3 then
    Exit(False);
  
  ExpectedSignature := CreateSignature(Parts[0], Parts[1]);
  Result := Parts[2] = ExpectedSignature;
end;

function TJwtTokenHandler.GetClaims(const AToken: string): TArray<TClaim>;
var
  Parts: TArray<string>;
  PayloadJson: string;
  Payload: TJSONObject;
  Pair: TJSONPair;
  Claims: TList<TClaim>;
  Claim: TClaim;
begin
  SetLength(Result, 0);
  
  Parts := AToken.Split(['.']);
  if Length(Parts) <> 3 then
    Exit;
  
  try
    PayloadJson := Base64UrlDecode(Parts[1]);
    Payload := TJSONObject.ParseJSONValue(PayloadJson) as TJSONObject;
    if Payload = nil then
      Exit;
    
    try
      Claims := TList<TClaim>.Create;
      try
        for Pair in Payload do
        begin
          Claim.ClaimType := Pair.JsonString.Value;
          if Pair.JsonValue is TJSONString then
            Claim.Value := TJSONString(Pair.JsonValue).Value
          else if Pair.JsonValue is TJSONNumber then
            Claim.Value := TJSONNumber(Pair.JsonValue).ToString
          else if Pair.JsonValue is TJSONBool then
            Claim.Value := BoolToStr(TJSONBool(Pair.JsonValue).AsBoolean, True)
          else
            Claim.Value := Pair.JsonValue.ToString;
          
          Claims.Add(Claim);
        end;
        
        Result := Claims.ToArray;
      finally
        Claims.Free;
      end;
    finally
      Payload.Free;
    end;
  except
    SetLength(Result, 0);
  end;
end;

function TJwtTokenHandler.ValidateToken(const AToken: string): TJwtValidationResult;
var
  Claims: TArray<TClaim>;
  Claim: TClaim;
  ExpClaim: string;
  ExpTime: Int64;
begin
  Result.IsValid := False;
  Result.ErrorMessage := '';
  SetLength(Result.Claims, 0);
  
  // Verify signature
  if not VerifySignature(AToken) then
  begin
    Result.ErrorMessage := 'Invalid signature';
    Exit;
  end;
  
  // Get claims
  Claims := GetClaims(AToken);
  if Length(Claims) = 0 then
  begin
    Result.ErrorMessage := 'Invalid token format';
    Exit;
  end;
  
  // Check expiration
  ExpClaim := '';
  for Claim in Claims do
  begin
    if Claim.ClaimType = 'exp' then
    begin
      ExpClaim := Claim.Value;
      Break;
    end;
  end;
  
  if ExpClaim <> '' then
  begin
    ExpTime := StrToInt64Def(ExpClaim, 0);
    if ExpTime > 0 then
    begin
      if DateTimeToUnix(Now) > ExpTime then
      begin
        Result.ErrorMessage := 'Token expired';
        Exit;
      end;
    end;
  end;
  
  // Validate issuer if configured
  if FIssuer <> '' then
  begin
    var IssuerFound := False;
    for Claim in Claims do
    begin
      if (Claim.ClaimType = 'iss') and (Claim.Value = FIssuer) then
      begin
        IssuerFound := True;
        Break;
      end;
    end;
    
    if not IssuerFound then
    begin
      Result.ErrorMessage := 'Invalid issuer';
      Exit;
    end;
  end;
  
  // Validate audience if configured
  if FAudience <> '' then
  begin
    var AudienceFound := False;
    for Claim in Claims do
    begin
      if (Claim.ClaimType = 'aud') and (Claim.Value = FAudience) then
      begin
        AudienceFound := True;
        Break;
      end;
    end;
    
    if not AudienceFound then
    begin
      Result.ErrorMessage := 'Invalid audience';
      Exit;
    end;
  end;
  
  Result.IsValid := True;
  Result.Claims := Claims;
end;

end.




