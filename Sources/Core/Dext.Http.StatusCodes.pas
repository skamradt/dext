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
{  Moved:   2026-01-22 (from Dext.Web to Dext.Core)                         }
{                                                                           }
{  HTTP Status Codes - Clean naming convention (HttpStatus.OK)              }
{                                                                           }
{***************************************************************************}
unit Dext.Http.StatusCodes;

interface

type
  /// <summary>
  ///   Contains constants for well-known HTTP status codes.
  ///   Usage: HttpStatus.OK, HttpStatus.NotFound, etc.
  /// </summary>
  HttpStatus = class
  public const
    // =========================================================================
    // 1xx Informational
    // =========================================================================
    
    /// <summary>100 Continue</summary>
    Continue = 100;
    
    /// <summary>101 Switching Protocols</summary>
    SwitchingProtocols = 101;
    
    /// <summary>102 Processing (WebDAV)</summary>
    Processing = 102;
    
    /// <summary>103 Early Hints</summary>
    EarlyHints = 103;
    
    // =========================================================================
    // 2xx Success
    // =========================================================================
    
    /// <summary>200 OK</summary>
    OK = 200;
    
    /// <summary>201 Created</summary>
    Created = 201;
    
    /// <summary>202 Accepted</summary>
    Accepted = 202;
    
    /// <summary>203 Non-Authoritative Information</summary>
    NonAuthoritative = 203;
    
    /// <summary>204 No Content</summary>
    NoContent = 204;
    
    /// <summary>205 Reset Content</summary>
    ResetContent = 205;
    
    /// <summary>206 Partial Content</summary>
    PartialContent = 206;
    
    /// <summary>207 Multi-Status (WebDAV)</summary>
    MultiStatus = 207;
    
    /// <summary>208 Already Reported (WebDAV)</summary>
    AlreadyReported = 208;
    
    /// <summary>226 IM Used</summary>
    IMUsed = 226;
    
    // =========================================================================
    // 3xx Redirection
    // =========================================================================
    
    /// <summary>300 Multiple Choices</summary>
    MultipleChoices = 300;
    
    /// <summary>301 Moved Permanently</summary>
    MovedPermanently = 301;
    
    /// <summary>302 Found</summary>
    Found = 302;
    
    /// <summary>303 See Other</summary>
    SeeOther = 303;
    
    /// <summary>304 Not Modified</summary>
    NotModified = 304;
    
    /// <summary>305 Use Proxy (Deprecated)</summary>
    UseProxy = 305;
    
    /// <summary>307 Temporary Redirect</summary>
    TemporaryRedirect = 307;
    
    /// <summary>308 Permanent Redirect</summary>
    PermanentRedirect = 308;
    
    // =========================================================================
    // 4xx Client Error
    // =========================================================================
    
    /// <summary>400 Bad Request</summary>
    BadRequest = 400;
    
    /// <summary>401 Unauthorized</summary>
    Unauthorized = 401;
    
    /// <summary>402 Payment Required</summary>
    PaymentRequired = 402;
    
    /// <summary>403 Forbidden</summary>
    Forbidden = 403;
    
    /// <summary>404 Not Found</summary>
    NotFound = 404;
    
    /// <summary>405 Method Not Allowed</summary>
    MethodNotAllowed = 405;
    
    /// <summary>406 Not Acceptable</summary>
    NotAcceptable = 406;
    
    /// <summary>407 Proxy Authentication Required</summary>
    ProxyAuthenticationRequired = 407;
    
    /// <summary>408 Request Timeout</summary>
    RequestTimeout = 408;
    
    /// <summary>409 Conflict</summary>
    Conflict = 409;
    
    /// <summary>410 Gone</summary>
    Gone = 410;
    
    /// <summary>411 Length Required</summary>
    LengthRequired = 411;
    
    /// <summary>412 Precondition Failed</summary>
    PreconditionFailed = 412;
    
    /// <summary>413 Request Entity Too Large</summary>
    PayloadTooLarge = 413;
    
    /// <summary>414 Request-URI Too Long</summary>
    UriTooLong = 414;
    
    /// <summary>415 Unsupported Media Type</summary>
    UnsupportedMediaType = 415;
    
    /// <summary>416 Requested Range Not Satisfiable</summary>
    RangeNotSatisfiable = 416;
    
    /// <summary>417 Expectation Failed</summary>
    ExpectationFailed = 417;
    
    /// <summary>418 I'm a teapot (RFC 2324)</summary>
    ImATeapot = 418;
    
    /// <summary>421 Misdirected Request</summary>
    MisdirectedRequest = 421;
    
    /// <summary>422 Unprocessable Entity (WebDAV)</summary>
    UnprocessableEntity = 422;
    
    /// <summary>423 Locked (WebDAV)</summary>
    Locked = 423;
    
    /// <summary>424 Failed Dependency (WebDAV)</summary>
    FailedDependency = 424;
    
    /// <summary>425 Too Early</summary>
    TooEarly = 425;
    
    /// <summary>426 Upgrade Required</summary>
    UpgradeRequired = 426;
    
    /// <summary>428 Precondition Required</summary>
    PreconditionRequired = 428;
    
    /// <summary>429 Too Many Requests</summary>
    TooManyRequests = 429;
    
    /// <summary>431 Request Header Fields Too Large</summary>
    RequestHeaderFieldsTooLarge = 431;
    
    /// <summary>451 Unavailable For Legal Reasons</summary>
    UnavailableForLegalReasons = 451;
    
    // =========================================================================
    // 5xx Server Error
    // =========================================================================
    
    /// <summary>500 Internal Server Error</summary>
    InternalServerError = 500;
    
    /// <summary>501 Not Implemented</summary>
    NotImplemented = 501;
    
    /// <summary>502 Bad Gateway</summary>
    BadGateway = 502;
    
    /// <summary>503 Service Unavailable</summary>
    ServiceUnavailable = 503;
    
    /// <summary>504 Gateway Timeout</summary>
    GatewayTimeout = 504;
    
    /// <summary>505 HTTP Version Not Supported</summary>
    HttpVersionNotSupported = 505;
    
    /// <summary>506 Variant Also Negotiates</summary>
    VariantAlsoNegotiates = 506;
    
    /// <summary>507 Insufficient Storage (WebDAV)</summary>
    InsufficientStorage = 507;
    
    /// <summary>508 Loop Detected (WebDAV)</summary>
    LoopDetected = 508;
    
    /// <summary>510 Not Extended</summary>
    NotExtended = 510;
    
    /// <summary>511 Network Authentication Required</summary>
    NetworkAuthenticationRequired = 511;
    
  public
    /// <summary>
    ///   Gets the reason phrase for a given HTTP status code.
    /// </summary>
    class function GetReasonPhrase(AStatusCode: Integer): string; static;
    
    /// <summary>
    ///   Returns True if the status code indicates success (2xx).
    /// </summary>
    class function IsSuccess(AStatusCode: Integer): Boolean; static; inline;
    
    /// <summary>
    ///   Returns True if the status code indicates redirection (3xx).
    /// </summary>
    class function IsRedirect(AStatusCode: Integer): Boolean; static; inline;
    
    /// <summary>
    ///   Returns True if the status code indicates client error (4xx).
    /// </summary>
    class function IsClientError(AStatusCode: Integer): Boolean; static; inline;
    
    /// <summary>
    ///   Returns True if the status code indicates server error (5xx).
    /// </summary>
    class function IsServerError(AStatusCode: Integer): Boolean; static; inline;
  end;

implementation

{ HttpStatus }

class function HttpStatus.GetReasonPhrase(AStatusCode: Integer): string;
begin
  case AStatusCode of
    // 1xx
    100: Result := 'Continue';
    101: Result := 'Switching Protocols';
    102: Result := 'Processing';
    103: Result := 'Early Hints';
    
    // 2xx
    200: Result := 'OK';
    201: Result := 'Created';
    202: Result := 'Accepted';
    203: Result := 'Non-Authoritative Information';
    204: Result := 'No Content';
    205: Result := 'Reset Content';
    206: Result := 'Partial Content';
    207: Result := 'Multi-Status';
    208: Result := 'Already Reported';
    226: Result := 'IM Used';
    
    // 3xx
    300: Result := 'Multiple Choices';
    301: Result := 'Moved Permanently';
    302: Result := 'Found';
    303: Result := 'See Other';
    304: Result := 'Not Modified';
    305: Result := 'Use Proxy';
    307: Result := 'Temporary Redirect';
    308: Result := 'Permanent Redirect';
    
    // 4xx
    400: Result := 'Bad Request';
    401: Result := 'Unauthorized';
    402: Result := 'Payment Required';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    406: Result := 'Not Acceptable';
    407: Result := 'Proxy Authentication Required';
    408: Result := 'Request Timeout';
    409: Result := 'Conflict';
    410: Result := 'Gone';
    411: Result := 'Length Required';
    412: Result := 'Precondition Failed';
    413: Result := 'Payload Too Large';
    414: Result := 'URI Too Long';
    415: Result := 'Unsupported Media Type';
    416: Result := 'Range Not Satisfiable';
    417: Result := 'Expectation Failed';
    418: Result := 'I''m a teapot';
    421: Result := 'Misdirected Request';
    422: Result := 'Unprocessable Entity';
    423: Result := 'Locked';
    424: Result := 'Failed Dependency';
    425: Result := 'Too Early';
    426: Result := 'Upgrade Required';
    428: Result := 'Precondition Required';
    429: Result := 'Too Many Requests';
    431: Result := 'Request Header Fields Too Large';
    451: Result := 'Unavailable For Legal Reasons';
    
    // 5xx
    500: Result := 'Internal Server Error';
    501: Result := 'Not Implemented';
    502: Result := 'Bad Gateway';
    503: Result := 'Service Unavailable';
    504: Result := 'Gateway Timeout';
    505: Result := 'HTTP Version Not Supported';
    506: Result := 'Variant Also Negotiates';
    507: Result := 'Insufficient Storage';
    508: Result := 'Loop Detected';
    510: Result := 'Not Extended';
    511: Result := 'Network Authentication Required';
  else
    Result := 'Unknown Status Code';
  end;
end;

class function HttpStatus.IsSuccess(AStatusCode: Integer): Boolean;
begin
  Result := (AStatusCode >= 200) and (AStatusCode < 300);
end;

class function HttpStatus.IsRedirect(AStatusCode: Integer): Boolean;
begin
  Result := (AStatusCode >= 300) and (AStatusCode < 400);
end;

class function HttpStatus.IsClientError(AStatusCode: Integer): Boolean;
begin
  Result := (AStatusCode >= 400) and (AStatusCode < 500);
end;

class function HttpStatus.IsServerError(AStatusCode: Integer): Boolean;
begin
  Result := (AStatusCode >= 500) and (AStatusCode < 600);
end;

end.
