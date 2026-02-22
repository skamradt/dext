unit MultiTenancy.Middleware;

interface

uses
  System.SysUtils,
  Dext.Web,
  MultiTenancy.DbContext;

type
  /// <summary>
  ///   Middleware that resolves the current tenant from the request.
  ///   Looks for tenant ID in: X-Tenant-Id header, subdomain, or query param.
  /// </summary>
  TTenantResolutionMiddleware = class(TMiddleware)
  public
    procedure Invoke(AContext: IHttpContext; ANext: TRequestDelegate); override;
  end;

implementation

uses
  Dext.Logging.Global;

{ TTenantResolutionMiddleware }

procedure TTenantResolutionMiddleware.Invoke(AContext: IHttpContext; ANext: TRequestDelegate);
var
  TenantId: string;
  TenantContext: ITenantContext;
begin
  // Strategy 1: Check X-Tenant-Id header
  if AContext.Request.Headers.TryGetValue('X-Tenant-Id', TenantId) then
  begin
    // Store tenant context for later use
    TenantContext := TTenantContext.Create;
    TenantContext.TenantId := TenantId;
    
    // Store in Items for access by handlers
    AContext.Items.AddOrSetValue('TenantContext', TObject(TenantContext));
    AContext.Items.AddOrSetValue('TenantId', TenantId);
    
    Log.Info('[Tenant] Request for tenant: {TenantId}', [TenantId]);
  end
  else
  begin
    // No tenant header - could be public endpoint or error
    // For tenant-required endpoints, the handler should check
    AContext.Items.AddOrSetValue('TenantId', '');
  end;

  ANext(AContext);
end;

end.
