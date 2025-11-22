unit Dext.OpenAPI.Extensions;

interface

uses
  System.SysUtils,
  Dext.Http.Interfaces;

type
  /// <summary>
  ///   Fluent extensions for adding OpenAPI metadata to endpoints.
  /// </summary>
  TEndpointMetadataExtensions = class
  public
    /// <summary>
    ///   Adds a summary to the endpoint.
    /// </summary>
    class function WithSummary(App: IApplicationBuilder; const ASummary: string): IApplicationBuilder;
    
    /// <summary>
    ///   Adds a description to the endpoint.
    /// </summary>
    class function WithDescription(App: IApplicationBuilder; const ADescription: string): IApplicationBuilder;
    
    /// <summary>
    ///   Adds a tag to the endpoint.
    /// </summary>
    class function WithTag(App: IApplicationBuilder; const ATag: string): IApplicationBuilder;
    
    /// <summary>
    ///   Adds multiple tags to the endpoint.
    /// </summary>
    class function WithTags(App: IApplicationBuilder; const ATags: array of string): IApplicationBuilder;
    
    /// <summary>
    ///   Adds metadata to the endpoint (summary, description, and tags).
    /// </summary>
    class function WithMetadata(App: IApplicationBuilder; const ASummary, ADescription: string; const ATags: array of string): IApplicationBuilder;
  end;

implementation

uses
  Dext.Http.Core,
  Dext.Http.Routing;

{ TEndpointMetadataExtensions }

class function TEndpointMetadataExtensions.WithSummary(App: IApplicationBuilder; const ASummary: string): IApplicationBuilder;
var
  Routes: TArray<TEndpointMetadata>;
  Metadata: TEndpointMetadata;
begin
  Result := App;
  
  Routes := App.GetRoutes;
  if Length(Routes) > 0 then
  begin
    Metadata := Routes[High(Routes)];
    Metadata.Summary := ASummary;
    App.UpdateLastRouteMetadata(Metadata);
  end;
end;

class function TEndpointMetadataExtensions.WithDescription(App: IApplicationBuilder; const ADescription: string): IApplicationBuilder;
var
  Routes: TArray<TEndpointMetadata>;
  Metadata: TEndpointMetadata;
begin
  Result := App;
  
  Routes := App.GetRoutes;
  if Length(Routes) > 0 then
  begin
    Metadata := Routes[High(Routes)];
    Metadata.Description := ADescription;
    App.UpdateLastRouteMetadata(Metadata);
  end;
end;

class function TEndpointMetadataExtensions.WithTag(App: IApplicationBuilder; const ATag: string): IApplicationBuilder;
var
  Routes: TArray<TEndpointMetadata>;
  Metadata: TEndpointMetadata;
  CurrentTags: TArray<string>;
begin
  Result := App;
  
  Routes := App.GetRoutes;
  if Length(Routes) > 0 then
  begin
    Metadata := Routes[High(Routes)];
    CurrentTags := Metadata.Tags;
    SetLength(CurrentTags, Length(CurrentTags) + 1);
    CurrentTags[High(CurrentTags)] := ATag;
    Metadata.Tags := CurrentTags;
    App.UpdateLastRouteMetadata(Metadata);
  end;
end;

class function TEndpointMetadataExtensions.WithTags(App: IApplicationBuilder; const ATags: array of string): IApplicationBuilder;
var
  Routes: TArray<TEndpointMetadata>;
  Metadata: TEndpointMetadata;
  I: Integer;
  NewTags: TArray<string>;
begin
  Result := App;
  
  Routes := App.GetRoutes;
  if Length(Routes) > 0 then
  begin
    Metadata := Routes[High(Routes)];
    SetLength(NewTags, Length(ATags));
    for I := 0 to High(ATags) do
      NewTags[I] := ATags[I];
    Metadata.Tags := NewTags;
    App.UpdateLastRouteMetadata(Metadata);
  end;
end;

class function TEndpointMetadataExtensions.WithMetadata(App: IApplicationBuilder; 
  const ASummary, ADescription: string; const ATags: array of string): IApplicationBuilder;
var
  Routes: TArray<TEndpointMetadata>;
  Metadata: TEndpointMetadata;
  I: Integer;
  NewTags: TArray<string>;
begin
  Result := App;
  
  Routes := App.GetRoutes;
  if Length(Routes) > 0 then
  begin
    Metadata := Routes[High(Routes)];
    Metadata.Summary := ASummary;
    Metadata.Description := ADescription;
    
    SetLength(NewTags, Length(ATags));
    for I := 0 to High(ATags) do
      NewTags[I] := ATags[I];
    Metadata.Tags := NewTags;
    
    App.UpdateLastRouteMetadata(Metadata);
  end;
end;

end.
