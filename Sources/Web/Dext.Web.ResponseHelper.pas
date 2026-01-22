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
{  HTTP Response Helper Utilities                                           }
{  Provides convenient functions for common response patterns               }
{                                                                           }
{***************************************************************************}
unit Dext.Web.ResponseHelper;

interface

uses
  System.SysUtils,
  Dext.Web.Interfaces;

/// <summary>
///   Writes JSON response with a specific HTTP status code.
///   Usage: RespondJson(Ctx, HttpStatus.OK, '{"id": 1}');
/// </summary>
procedure RespondJson(const AContext: IHttpContext; AStatusCode: Integer; const AJson: string); overload;

/// <summary>
///   Writes JSON response with status code and formatted string.
///   Usage: RespondJson(Ctx, HttpStatus.BadRequest, '{"error": "%s"}', [E.Message]);
/// </summary>
procedure RespondJson(const AContext: IHttpContext; AStatusCode: Integer; const AFormat: string; const AArgs: array of const); overload;

/// <summary>
///   Writes JSON error response with a specific HTTP status code.
///   Usage: RespondError(Ctx, HttpStatus.NotFound, 'Resource not found');
/// </summary>
procedure RespondError(const AContext: IHttpContext; AStatusCode: Integer; const AMessage: string);

/// <summary>
///   Writes JSON success response (HTTP 200 OK).
/// </summary>
procedure RespondOk(const AContext: IHttpContext; const AJson: string);

/// <summary>
///   Writes JSON created response (HTTP 201 Created).
/// </summary>
procedure RespondCreated(const AContext: IHttpContext; const AJson: string);

/// <summary>
///   Writes no content response (HTTP 204 No Content).
/// </summary>
procedure RespondNoContent(const AContext: IHttpContext);

implementation

uses
  Dext.Http.StatusCodes;

procedure RespondJson(const AContext: IHttpContext; AStatusCode: Integer; const AJson: string);
begin
  AContext.Response.StatusCode := AStatusCode;
  AContext.Response.Json(AJson);
end;

procedure RespondJson(const AContext: IHttpContext; AStatusCode: Integer; const AFormat: string; const AArgs: array of const);
begin
  AContext.Response.StatusCode := AStatusCode;
  AContext.Response.Json(Format(AFormat, AArgs));
end;

procedure RespondError(const AContext: IHttpContext; AStatusCode: Integer; const AMessage: string);
begin
  AContext.Response.StatusCode := AStatusCode;
  AContext.Response.Json(Format('{"error": "%s"}', [AMessage]));
end;

procedure RespondOk(const AContext: IHttpContext; const AJson: string);
begin
  AContext.Response.StatusCode := HttpStatus.OK;
  AContext.Response.Json(AJson);
end;

procedure RespondCreated(const AContext: IHttpContext; const AJson: string);
begin
  AContext.Response.StatusCode := HttpStatus.Created;
  AContext.Response.Json(AJson);
end;

procedure RespondNoContent(const AContext: IHttpContext);
begin
  AContext.Response.StatusCode := HttpStatus.NoContent;
end;

end.
