// Dext.Http.RoutingMiddleware.pas
unit Dext.Http.RoutingMiddleware;

interface

uses
  Dext.Http.Core,
  Dext.Http.Interfaces,
  Dext.Http.Routing;  // ✅ Para IRouteMatcher

type
  TRoutingMiddleware = class(TMiddleware)
  private
    FRouteMatcher: IRouteMatcher;  // ✅ Interface - sem reference circular!
  public
    constructor Create(const ARouteMatcher: IRouteMatcher);
    procedure Invoke(AContext: IHttpContext; ANext: TRequestDelegate); override;
  end;

implementation

uses
  System.Generics.Collections,
  Dext.Http.Indy;

{ TRoutingMiddleware }

constructor TRoutingMiddleware.Create(const ARouteMatcher: IRouteMatcher);
begin
  inherited Create;
  FRouteMatcher := ARouteMatcher;  // ✅ Interface gerencia ciclo de vida
end;

procedure TRoutingMiddleware.Invoke(AContext: IHttpContext; ANext: TRequestDelegate);
var
  Handler: TRequestDelegate;
  RouteParams: TDictionary<string, string>;
  Metadata: TEndpointMetadata;
  IndyContext: TIndyHttpContext;
begin
  var Path := AContext.Request.Path;
  var Method := AContext.Request.Method;

  // ✅ USAR RouteMatcher via interface com suporte a Método
  if FRouteMatcher.FindMatchingRoute(Method, Path, Handler, RouteParams, Metadata) then
  begin
    try
      // ✅ INJETAR parâmetros de rota se encontrados
      if Assigned(RouteParams) and (AContext is TIndyHttpContext) then
      begin
        IndyContext := TIndyHttpContext(AContext);
        IndyContext.SetRouteParams(RouteParams);
      end;

      // TODO: Store Metadata in Context.Items if available for other middlewares (e.g. Auth)

      Handler(AContext);
    finally
      RouteParams.Free;
    end;
  end
  else
  begin
    // Nenhuma rota encontrada - chamar next (404 handler)
    ANext(AContext);
  end;
end;

end.
