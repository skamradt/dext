unit Dext.Core.Routing;

interface

uses
  System.SysUtils;

type
  // ===========================================================================
  // ATRIBUTO BASE PARA ROTAS
  // ===========================================================================
  DextRouteAttribute = class(TCustomAttribute)
  private
    FPath: string;
    FMethod: string;
  public
    constructor Create(const APath: string; const AMethod: string);
    property Path: string read FPath;
    property Method: string read FMethod;
  end;

  // ===========================================================================
  // ATRIBUTOS ESPECÍFICOS POR VERBO HTTP
  // ===========================================================================
  DextGetAttribute = class(DextRouteAttribute)
  public
    constructor Create(const APath: string);
  end;

  DextPostAttribute = class(DextRouteAttribute)
  public
    constructor Create(const APath: string);
  end;

  DextPutAttribute = class(DextRouteAttribute)
  public
    constructor Create(const APath: string);
  end;

  DextDeleteAttribute = class(DextRouteAttribute)
  public
    constructor Create(const APath: string);
  end;

  DextPatchAttribute = class(DextRouteAttribute)
  public
    constructor Create(const APath: string);
  end;

  DextHeadAttribute = class(DextRouteAttribute)
  public
    constructor Create(const APath: string);
  end;

  DextOptionsAttribute = class(DextRouteAttribute)
  public
    constructor Create(const APath: string);
  end;

  // ===========================================================================
  // ATRIBUTO PARA CONTROLLERS (opcional)
  // ===========================================================================
  DextControllerAttribute = class(TCustomAttribute)
  private
    FPrefix: string;
  public
    constructor Create(const APrefix: string = '');
    property Prefix: string read FPrefix;
  end;

  // ===========================================================================
  // EXCEÇÃO HTTP PERSONALIZADA
  // ===========================================================================
  EDextHttpException = class(Exception)
  private
    FStatusCode: Integer;
  public
    constructor Create(AStatusCode: Integer; const AMessage: string);
    property StatusCode: Integer read FStatusCode;
  end;

implementation

{ DextRouteAttribute }

constructor DextRouteAttribute.Create(const APath: string; const AMethod: string);
begin
  inherited Create;
  FPath := APath;
  FMethod := AMethod;
end;

{ DextGetAttribute }

constructor DextGetAttribute.Create(const APath: string);
begin
  inherited Create(APath, 'GET');
end;

{ DextPostAttribute }

constructor DextPostAttribute.Create(const APath: string);
begin
  inherited Create(APath, 'POST');
end;

{ DextPutAttribute }

constructor DextPutAttribute.Create(const APath: string);
begin
  inherited Create(APath, 'PUT');
end;

{ DextDeleteAttribute }

constructor DextDeleteAttribute.Create(const APath: string);
begin
  inherited Create(APath, 'DELETE');
end;

{ DextPatchAttribute }

constructor DextPatchAttribute.Create(const APath: string);
begin
  inherited Create(APath, 'PATCH');
end;

{ DextHeadAttribute }

constructor DextHeadAttribute.Create(const APath: string);
begin
  inherited Create(APath, 'HEAD');
end;

{ DextOptionsAttribute }

constructor DextOptionsAttribute.Create(const APath: string);
begin
  inherited Create(APath, 'OPTIONS');
end;

{ DextControllerAttribute }

constructor DextControllerAttribute.Create(const APrefix: string);
begin
  inherited Create;
  FPrefix := APrefix;
end;

{ EDextHttpException }

constructor EDextHttpException.Create(AStatusCode: Integer; const AMessage: string);
begin
  inherited Create(AMessage);
  FStatusCode := AStatusCode;
end;

end.
