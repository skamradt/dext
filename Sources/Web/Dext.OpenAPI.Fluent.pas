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
{  Created: 2025-12-30                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.OpenAPI.Fluent;

interface

uses
  System.SysUtils,
  System.TypInfo,
  Dext.Web.Interfaces;

type
  /// <summary>
  ///   Fluent builder record for configuring endpoint metadata.
  ///   Follows the Dext pattern established by TAsyncBuilder and TSpecificationBuilder.
  /// </summary>
  /// <remarks>
  ///   Usage: SwaggerEndpoint.From(App.MapGet('/path', Handler)).Summary('...').Response(200, TypeInfo(T))
  /// </remarks>
  TEndpointBuilder = record
  private
    FApp: IApplicationBuilder;
  public
    /// <summary>
    ///   Creates a new endpoint builder wrapping the application builder.
    /// </summary>
    constructor Create(const AApp: IApplicationBuilder);
    
    /// <summary>
    ///   Implicit conversion to IApplicationBuilder for seamless chaining.
    /// </summary>
    class operator Implicit(const Value: TEndpointBuilder): IApplicationBuilder;
    
    // ========================================
    // Documentation
    // ========================================
    
    /// <summary>
    ///   Adds a summary to the endpoint (shown in Swagger UI header).
    /// </summary>
    function Summary(const ASummary: string): TEndpointBuilder;
    
    /// <summary>
    ///   Adds a detailed description to the endpoint.
    /// </summary>
    function Description(const ADescription: string): TEndpointBuilder;
    
    /// <summary>
    ///   Adds a single tag to the endpoint for grouping in Swagger UI.
    /// </summary>
    function Tag(const ATag: string): TEndpointBuilder;
    
    /// <summary>
    ///   Adds multiple tags to the endpoint.
    /// </summary>
    function Tags(const ATags: array of string): TEndpointBuilder;
    
    /// <summary>
    ///   Convenience method to set summary, description, and tags in one call.
    /// </summary>
    function Metadata(const ASummary, ADescription: string; 
      const ATags: array of string): TEndpointBuilder;
    
    // ========================================
    // Request/Response Types
    // ========================================
    
    /// <summary>
    ///   Explicitly sets the request body type for OpenAPI documentation.
    /// </summary>
    function RequestType(AType: PTypeInfo): TEndpointBuilder;
    
    /// <summary>
    ///   Adds a documented response to the endpoint.
    /// </summary>
    /// <param name="ACode">HTTP status code (e.g., 200, 404)</param>
    /// <param name="AType">Optional type info for the response body schema</param>
    /// <param name="ADescription">Optional description (defaults based on code)</param>
    function Response(ACode: Integer; AType: PTypeInfo = nil; 
      const ADescription: string = ''): TEndpointBuilder;
    
    // ========================================
    // Security
    // ========================================
    
    /// <summary>
    ///   Requires authorization for this endpoint using the specified scheme.
    /// </summary>
    function RequireAuthorization(const AScheme: string = 'Bearer'): TEndpointBuilder;
    
    // ========================================
    // Properties
    // ========================================
    
    /// <summary>
    ///   Direct access to the underlying application builder.
    /// </summary>
    property App: IApplicationBuilder read FApp;
  end;

  /// <summary>
  ///   Static factory for creating endpoint builders.
  ///   Provides an alternative entry point for fluent Swagger configuration.
  /// </summary>
  SwaggerEndpoint = record
  public
    /// <summary>
    ///   Creates an endpoint builder from an existing application builder.
    /// </summary>
    class function From(const AApp: IApplicationBuilder): TEndpointBuilder; static;
  end;

implementation

{ TEndpointBuilder }

constructor TEndpointBuilder.Create(const AApp: IApplicationBuilder);
begin
  FApp := AApp;
end;

class operator TEndpointBuilder.Implicit(const Value: TEndpointBuilder): IApplicationBuilder;
begin
  Result := Value.FApp;
end;

function TEndpointBuilder.Summary(const ASummary: string): TEndpointBuilder;
var
  Routes: TArray<TEndpointMetadata>;
  Metadata: TEndpointMetadata;
begin
  Routes := FApp.GetRoutes;
  if Length(Routes) > 0 then
  begin
    Metadata := Routes[High(Routes)];
    Metadata.Summary := ASummary;
    FApp.UpdateLastRouteMetadata(Metadata);
  end;
  Result := Self;
end;

function TEndpointBuilder.Description(const ADescription: string): TEndpointBuilder;
var
  Routes: TArray<TEndpointMetadata>;
  Metadata: TEndpointMetadata;
begin
  Routes := FApp.GetRoutes;
  if Length(Routes) > 0 then
  begin
    Metadata := Routes[High(Routes)];
    Metadata.Description := ADescription;
    FApp.UpdateLastRouteMetadata(Metadata);
  end;
  Result := Self;
end;

function TEndpointBuilder.Tag(const ATag: string): TEndpointBuilder;
var
  Routes: TArray<TEndpointMetadata>;
  Metadata: TEndpointMetadata;
  Tags: TArray<string>;
begin
  Routes := FApp.GetRoutes;
  if Length(Routes) > 0 then
  begin
    Metadata := Routes[High(Routes)];
    Tags := Metadata.Tags;
    SetLength(Tags, Length(Tags) + 1);
    Tags[High(Tags)] := ATag;
    Metadata.Tags := Tags;
    FApp.UpdateLastRouteMetadata(Metadata);
  end;
  Result := Self;
end;

function TEndpointBuilder.Tags(const ATags: array of string): TEndpointBuilder;
var
  Routes: TArray<TEndpointMetadata>;
  Metadata: TEndpointMetadata;
  NewTags: TArray<string>;
  I: Integer;
begin
  Routes := FApp.GetRoutes;
  if Length(Routes) > 0 then
  begin
    Metadata := Routes[High(Routes)];
    SetLength(NewTags, Length(ATags));
    for I := 0 to High(ATags) do
      NewTags[I] := ATags[I];
    Metadata.Tags := NewTags;
    FApp.UpdateLastRouteMetadata(Metadata);
  end;
  Result := Self;
end;

function TEndpointBuilder.Metadata(const ASummary, ADescription: string; 
  const ATags: array of string): TEndpointBuilder;
var
  Routes: TArray<TEndpointMetadata>;
  Metadata: TEndpointMetadata;
  NewTags: TArray<string>;
  I: Integer;
begin
  Routes := FApp.GetRoutes;
  if Length(Routes) > 0 then
  begin
    Metadata := Routes[High(Routes)];
    Metadata.Summary := ASummary;
    Metadata.Description := ADescription;
    
    SetLength(NewTags, Length(ATags));
    for I := 0 to High(ATags) do
      NewTags[I] := ATags[I];
    Metadata.Tags := NewTags;
    
    FApp.UpdateLastRouteMetadata(Metadata);
  end;
  Result := Self;
end;

function TEndpointBuilder.RequestType(AType: PTypeInfo): TEndpointBuilder;
var
  Routes: TArray<TEndpointMetadata>;
  Metadata: TEndpointMetadata;
begin
  Routes := FApp.GetRoutes;
  if Length(Routes) > 0 then
  begin
    Metadata := Routes[High(Routes)];
    Metadata.RequestType := AType;
    FApp.UpdateLastRouteMetadata(Metadata);
  end;
  Result := Self;
end;

function TEndpointBuilder.Response(ACode: Integer; AType: PTypeInfo; 
  const ADescription: string): TEndpointBuilder;
var
  Routes: TArray<TEndpointMetadata>;
  Metadata: TEndpointMetadata;
  Responses: TArray<TOpenAPIResponseMetadata>;
  Desc: string;
begin
  Routes := FApp.GetRoutes;
  if Length(Routes) > 0 then
  begin
    Metadata := Routes[High(Routes)];
    Responses := Metadata.Responses;
    SetLength(Responses, Length(Responses) + 1);
    
    // Default description based on code
    Desc := ADescription;
    if Desc = '' then
    begin
      case ACode of
        200: Desc := 'OK';
        201: Desc := 'Created';
        204: Desc := 'No Content';
        400: Desc := 'Bad Request';
        401: Desc := 'Unauthorized';
        403: Desc := 'Forbidden';
        404: Desc := 'Not Found';
        500: Desc := 'Internal Server Error';
      else
        Desc := 'Response ' + IntToStr(ACode);
      end;
    end;
    
    with Responses[High(Responses)] do
    begin
      StatusCode := ACode;
      Description := Desc;
      SchemaType := AType;
      MediaType := 'application/json';
    end;
    
    Metadata.Responses := Responses;
    FApp.UpdateLastRouteMetadata(Metadata);
  end;
  Result := Self;
end;

function TEndpointBuilder.RequireAuthorization(const AScheme: string): TEndpointBuilder;
var
  Routes: TArray<TEndpointMetadata>;
  Metadata: TEndpointMetadata;
  Schemes: TArray<string>;
begin
  Routes := FApp.GetRoutes;
  if Length(Routes) > 0 then
  begin
    Metadata := Routes[High(Routes)];
    Schemes := Metadata.Security;
    SetLength(Schemes, Length(Schemes) + 1);
    Schemes[High(Schemes)] := AScheme;
    Metadata.Security := Schemes;
    FApp.UpdateLastRouteMetadata(Metadata);
  end;
  Result := Self;
end;

{ SwaggerEndpoint }

class function SwaggerEndpoint.From(const AApp: IApplicationBuilder): TEndpointBuilder;
begin
  Result := TEndpointBuilder.Create(AApp);
end;

end.
