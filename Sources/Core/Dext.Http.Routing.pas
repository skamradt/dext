unit Dext.Http.Routing;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  System.RegularExpressions,
  Dext.Http.Interfaces;

type
  TRoutePattern = class
  private
    FPattern: string;
    FRegex: TRegEx;
    FParameterNames: TArray<string>;
    function BuildRegexPattern(const APattern: string): string;
    function ExtractParameterNames(const APattern: string): TArray<string>;
  public
    constructor Create(const APattern: string);
    function Match(const APath: string; out AParams: TDictionary<string, string>): Boolean;
    property Pattern: string read FPattern;
    property ParameterNames: TArray<string> read FParameterNames;
  end;

  TRouteDefinition = class
  private
    FMethod: string;
    FPath: string;
    FHandler: TRequestDelegate;
    FPattern: TRoutePattern;
    FMetadata: TEndpointMetadata;
  public
    constructor Create(const AMethod, APath: string; AHandler: TRequestDelegate);
    destructor Destroy; override;
    property Method: string read FMethod;
    property Path: string read FPath;
    property Handler: TRequestDelegate read FHandler;
    property Pattern: TRoutePattern read FPattern;
    property Metadata: TEndpointMetadata read FMetadata write FMetadata;
  end;

  IRouteMatcher = interface
    ['{A1B2C3D4-E5F6-4A7B-8C9D-0E1F2A3B4C5D}']
    function FindMatchingRoute(const AMethod, APath: string;
      out AHandler: TRequestDelegate;
      out ARouteParams: TDictionary<string, string>;
      out AMetadata: TEndpointMetadata): Boolean;
  end;

  TRouteMatcher = class(TInterfacedObject, IRouteMatcher)
  private
    FRoutes: TObjectList<TRouteDefinition>;
  public
    constructor Create(const ARoutes: TList<TRouteDefinition>);
    destructor Destroy; override;
    function FindMatchingRoute(const AMethod, APath: string;
      out AHandler: TRequestDelegate;
      out ARouteParams: TDictionary<string, string>;
      out AMetadata: TEndpointMetadata): Boolean;
  end;

  ERouteException = class(Exception);

implementation

{ TRoutePattern }

constructor TRoutePattern.Create(const APattern: string);
begin
  inherited Create;
  FPattern := APattern;

  if APattern = '' then
    raise ERouteException.Create('Route pattern cannot be empty');

  FParameterNames := ExtractParameterNames(APattern);
  FRegex := TRegEx.Create(BuildRegexPattern(APattern), [roIgnoreCase]);
end;

function TRoutePattern.ExtractParameterNames(const APattern: string): TArray<string>;
var
  Matches: TMatchCollection;
  I: Integer;
begin
  var PatternRegex := TRegEx.Create('\{(.+?)\}');
  Matches := PatternRegex.Matches(APattern);

  SetLength(Result, Matches.Count);
  for I := 0 to Matches.Count - 1 do
  begin
    Result[I] := Matches[I].Groups[1].Value;

    if Result[I].IsEmpty then
      raise ERouteException.CreateFmt('Invalid parameter name in pattern: %s', [APattern]);
  end;
end;

function TRoutePattern.BuildRegexPattern(const APattern: string): string;
begin
  Result := APattern;
  Result := TRegEx.Replace(Result, '([.+?^=!:${}()|\[\]\\])', '\\$1');
  Result := TRegEx.Replace(Result, '\\\{([^}]+)\\\}', '([^/]+)');
  Result := '^' + Result + '$';
end;

function TRoutePattern.Match(const APath: string;
  out AParams: TDictionary<string, string>): Boolean;
var
  Match: TMatch;
  I: Integer;
begin
  AParams := nil;
  Result := False;

  Match := FRegex.Match(APath);

  if Match.Success then
  begin
    AParams := TDictionary<string, string>.Create;

    try
      for I := 0 to High(FParameterNames) do
      begin
        if I + 1 < Match.Groups.Count then
          AParams.Add(FParameterNames[I], Match.Groups[I + 1].Value);
      end;

      Result := True;

    except
      AParams.Free;
      raise;
    end;
  end;
end;

{ TRouteDefinition }

constructor TRouteDefinition.Create(const AMethod, APath: string; AHandler: TRequestDelegate);
begin
  inherited Create;
  FMethod := AMethod;
  FPath := APath;
  FHandler := AHandler;
  
  if APath.Contains('{') then
    FPattern := TRoutePattern.Create(APath)
  else
    FPattern := nil;

  FMetadata.Method := AMethod;
  FMetadata.Path := APath;
end;

destructor TRouteDefinition.Destroy;
begin
  if Assigned(FPattern) then
    FPattern.Free;
  inherited;
end;

{ TRouteMatcher }

constructor TRouteMatcher.Create(const ARoutes: TList<TRouteDefinition>);
var
  Route: TRouteDefinition;
begin
  inherited Create;
  FRoutes := TObjectList<TRouteDefinition>.Create(True); // Owns objects
  
  // Clone routes to ensure thread safety and independence
  for Route in ARoutes do
  begin
    var NewRoute := TRouteDefinition.Create(Route.Method, Route.Path, Route.Handler);
    NewRoute.Metadata := Route.Metadata;
    FRoutes.Add(NewRoute);
  end;
end;

destructor TRouteMatcher.Destroy;
begin
  FRoutes.Free;
  inherited;
end;

function TRouteMatcher.FindMatchingRoute(const AMethod, APath: string;
  out AHandler: TRequestDelegate;
  out ARouteParams: TDictionary<string, string>;
  out AMetadata: TEndpointMetadata): Boolean;
var
  Route: TRouteDefinition;
begin
  ARouteParams := nil;
  Result := False;

  // 1. Exact Match First (Performance optimization)
  for Route in FRoutes do
  begin
    if (Route.Pattern = nil) and 
       (Route.Method = AMethod) and 
       (Route.Path = APath) then
    begin
      AHandler := Route.Handler;
      AMetadata := Route.Metadata;
      Exit(True);
    end;
  end;

  // 2. Pattern Match
  for Route in FRoutes do
  begin
    if (Route.Pattern <> nil) and 
       (Route.Method = AMethod) then
    begin
      if Route.Pattern.Match(APath, ARouteParams) then
      begin
        AHandler := Route.Handler;
        AMetadata := Route.Metadata;
        Exit(True);
      end;
    end;
  end;
end;

end.
