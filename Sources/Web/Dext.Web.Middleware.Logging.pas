unit Dext.Web.Middleware.Logging;

interface

uses
  System.SysUtils,
  Dext.Web.Core,
  Dext.Web.Interfaces;

type
  TRequestLoggingMiddleware = class(TMiddleware)
  public
    procedure Invoke(AContext: IHttpContext; ANext: TRequestDelegate); override;
  end;

implementation

uses
  Dext.Utils,
  Dext.Logging,
  Dext.Logging.Global,
  Dext.Types.UUID;

{ TRequestLoggingMiddleware }

procedure TRequestLoggingMiddleware.Invoke(AContext: IHttpContext; ANext: TRequestDelegate);
var
  Method, Path: string;
  ReqId: string;
  Scope: IDisposable;
begin
  Method := AContext.Request.Method;
  Path := AContext.Request.Path;
  
  // Try to extract existing TraceId/RequestID
  ReqId := '';
  if AContext.Request.Headers.ContainsKey('X-Request-ID') then
    ReqId := AContext.Request.Headers['X-Request-ID'];
    
  if ReqId = '' then
  begin
    // Generate new if missing
    // Ideally we should use TUUID.NewV7 for sorting, or just random
    ReqId := TUUID.NewV7.ToString; 
    // And maybe inject it back? AContext.Items?
  end;
  
  // Start Scope with this Request ID
  // This pushes TraceId to TraceContext.Current
  // Format: Name {Args}
  Scope := Log.Logger.BeginScope('HTTP {Method} {Path} [{RequestId}]', [Method, Path, ReqId]);
  try
    Log.Info('REQ: %s %s', [Method, Path]);
    
    try
      ANext(AContext);
    except
      on E: Exception do
      begin
        Log.Error('Unhandled Exception processing %s %s: %s', [Method, Path, E.Message]);
        raise;
      end;
    end;
    
    Log.Info('RES: %s %s -> %d', [Method, Path, AContext.Response.StatusCode]);
    
  finally
    Scope.Dispose;
  end;
end;

end.
