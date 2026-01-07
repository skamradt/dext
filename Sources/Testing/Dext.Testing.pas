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
{  Dext.Testing - Wildcard unit for all testing features.                    }
{***************************************************************************}
unit Dext.Testing;

interface

uses
  System.SysUtils,
  System.Rtti,
  {$I Dext.Testing.Uses.inc};

type
  {$I Dext.Testing.Aliases.inc}

// Global helper functions for cleaner syntax
function Should(const Value: string): ShouldString; overload;
function Should(Value: Integer): ShouldInteger; overload;
function Should(Value: Int64): ShouldInt64; overload;
function Should(Value: Boolean): ShouldBoolean; overload;
function Should(Value: Double): ShouldDouble; overload;
function Should(const Action: TProc): ShouldAction; overload;
function Should(const Value: TObject): ShouldObject; overload;
function Should(const Value: IInterface): ShouldInterface; overload;
function Should(const Value: TGUID): ShouldGuid; overload;
function Should(const Value: TUUID): ShouldUUID; overload;
function Should(const Value: Variant): ShouldVariant; overload;
function ShouldDate(Value: TDateTime): ShouldDateTime; overload;

implementation

function Should(const Value: string): ShouldString; begin Result := Dext.Assertions.Should(Value); end;
function Should(Value: Integer): ShouldInteger; begin Result := Dext.Assertions.Should(Value); end;
function Should(Value: Int64): ShouldInt64; begin Result := Dext.Assertions.Should(Value); end;
function Should(Value: Boolean): ShouldBoolean; begin Result := Dext.Assertions.Should(Value); end;
function Should(Value: Double): ShouldDouble; begin Result := Dext.Assertions.Should(Value); end;
function Should(const Action: TProc): ShouldAction; begin Result := Dext.Assertions.Should(Action); end;
function Should(const Value: TObject): ShouldObject; begin Result := Dext.Assertions.Should(Value); end;
function Should(const Value: IInterface): ShouldInterface; begin Result := Dext.Assertions.Should(Value); end;
function Should(const Value: TGUID): ShouldGuid; begin Result := Dext.Assertions.Should(Value); end;
function Should(const Value: TUUID): ShouldUUID; begin Result := Dext.Assertions.Should(Value); end;
function Should(const Value: Variant): ShouldVariant; begin Result := Dext.Assertions.Should(Value); end;
function ShouldDate(Value: TDateTime): ShouldDateTime; begin Result := Dext.Assertions.ShouldDate(Value); end;

end.
