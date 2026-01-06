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
  Dext.Utils;


{ TRequestLoggingMiddleware }

procedure TRequestLoggingMiddleware.Invoke(AContext: IHttpContext; ANext: TRequestDelegate);
var
  Method, Path: string;
begin
  Method := AContext.Request.Method;
  Path := AContext.Request.Path;
  
  SafeWriteLn('?? [REQ] ' + Format('%s %s', [Method, Path]));
  
  try
    ANext(AContext);
  finally
    SafeWriteLn('? [RES] ' + Format('%s %s -> %d',
      [Method, Path, AContext.Response.StatusCode]));
  end;
end;

end.
