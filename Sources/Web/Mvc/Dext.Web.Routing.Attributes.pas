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
{  Created: 2025-12-08                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.Web.Routing.Attributes;

interface

uses
  System.SysUtils;

type
  // ===========================================================================
  // BASE ROUTE ATTRIBUTE
  // ===========================================================================
  
  /// <summary>
  ///   Base attribute for HTTP route definitions.
  /// </summary>
  RouteAttribute = class(TCustomAttribute)
  private
    FPath: string;
    FMethod: string;
  public
    constructor Create(const APath: string; const AMethod: string = '');
    property Path: string read FPath;
    property Method: string read FMethod;
  end;

  // ===========================================================================
  // HTTP VERB ATTRIBUTES
  // ===========================================================================
  
  /// <summary>Marks a method as handling HTTP GET requests.</summary>
  GetAttribute = class(RouteAttribute)
  public
    constructor Create(const APath: string = '');
  end;

  /// <summary>Marks a method as handling HTTP POST requests.</summary>
  PostAttribute = class(RouteAttribute)
  public
    constructor Create(const APath: string = '');
  end;

  /// <summary>Marks a method as handling HTTP PUT requests.</summary>
  PutAttribute = class(RouteAttribute)
  public
    constructor Create(const APath: string = '');
  end;

  /// <summary>Marks a method as handling HTTP DELETE requests.</summary>
  DeleteAttribute = class(RouteAttribute)
  public
    constructor Create(const APath: string = '');
  end;

  /// <summary>Marks a method as handling HTTP PATCH requests.</summary>
  PatchAttribute = class(RouteAttribute)
  public
    constructor Create(const APath: string = '');
  end;

  /// <summary>Marks a method as handling HTTP HEAD requests.</summary>
  HeadAttribute = class(RouteAttribute)
  public
    constructor Create(const APath: string = '');
  end;

  /// <summary>Marks a method as handling HTTP OPTIONS requests.</summary>
  OptionsAttribute = class(RouteAttribute)
  public
    constructor Create(const APath: string = '');
  end;

  // ===========================================================================
  // CONTROLLER ATTRIBUTE
  // ===========================================================================
  
  /// <summary>
  ///   Marks a class as an API controller with an optional route prefix.
  /// </summary>
  ControllerAttribute = class(TCustomAttribute)
  private
    FPrefix: string;
  public
    constructor Create(const APrefix: string = '');
    property Prefix: string read FPrefix;
  end;

  // ===========================================================================
  // HTTP EXCEPTION
  // ===========================================================================
  
  /// <summary>
  ///   Exception that carries an HTTP status code.
  /// </summary>
  HttpException = class(Exception)
  private
    FStatusCode: Integer;
  public
    constructor Create(AStatusCode: Integer; const AMessage: string);
    property StatusCode: Integer read FStatusCode;
  end;

  // ===========================================================================
  // DEPRECATED ALIASES (for backward compatibility)
  // ===========================================================================
  
  /// <summary>Deprecated. Use RouteAttribute instead.</summary>
  DextRouteAttribute = RouteAttribute deprecated 'Use RouteAttribute instead';
  
  /// <summary>Deprecated. Use GetAttribute instead.</summary>
  DextGetAttribute = GetAttribute deprecated 'Use GetAttribute instead';
  
  /// <summary>Deprecated. Use PostAttribute instead.</summary>
  DextPostAttribute = PostAttribute deprecated 'Use PostAttribute instead';
  
  /// <summary>Deprecated. Use PutAttribute instead.</summary>
  DextPutAttribute = PutAttribute deprecated 'Use PutAttribute instead';
  
  /// <summary>Deprecated. Use DeleteAttribute instead.</summary>
  DextDeleteAttribute = DeleteAttribute deprecated 'Use DeleteAttribute instead';
  
  /// <summary>Deprecated. Use PatchAttribute instead.</summary>
  DextPatchAttribute = PatchAttribute deprecated 'Use PatchAttribute instead';
  
  /// <summary>Deprecated. Use HeadAttribute instead.</summary>
  DextHeadAttribute = HeadAttribute deprecated 'Use HeadAttribute instead';
  
  /// <summary>Deprecated. Use OptionsAttribute instead.</summary>
  DextOptionsAttribute = OptionsAttribute deprecated 'Use OptionsAttribute instead';
  
  /// <summary>Deprecated. Use ControllerAttribute instead.</summary>
  DextControllerAttribute = ControllerAttribute deprecated 'Use ControllerAttribute instead';
  
  /// <summary>Deprecated. Use HttpException instead.</summary>
  EDextHttpException = HttpException deprecated 'Use HttpException instead';

implementation

{ RouteAttribute }

constructor RouteAttribute.Create(const APath: string; const AMethod: string);
begin
  inherited Create;
  FPath := APath;
  FMethod := AMethod;
end;

{ GetAttribute }

constructor GetAttribute.Create(const APath: string);
begin
  inherited Create(APath, 'GET');
end;

{ PostAttribute }

constructor PostAttribute.Create(const APath: string);
begin
  inherited Create(APath, 'POST');
end;

{ PutAttribute }

constructor PutAttribute.Create(const APath: string);
begin
  inherited Create(APath, 'PUT');
end;

{ DeleteAttribute }

constructor DeleteAttribute.Create(const APath: string);
begin
  inherited Create(APath, 'DELETE');
end;

{ PatchAttribute }

constructor PatchAttribute.Create(const APath: string);
begin
  inherited Create(APath, 'PATCH');
end;

{ HeadAttribute }

constructor HeadAttribute.Create(const APath: string);
begin
  inherited Create(APath, 'HEAD');
end;

{ OptionsAttribute }

constructor OptionsAttribute.Create(const APath: string);
begin
  inherited Create(APath, 'OPTIONS');
end;

{ ControllerAttribute }

constructor ControllerAttribute.Create(const APrefix: string);
begin
  inherited Create;
  FPrefix := APrefix;
end;

{ HttpException }

constructor HttpException.Create(AStatusCode: Integer; const AMessage: string);
begin
  inherited Create(AMessage);
  FStatusCode := AStatusCode;
end;

end.
