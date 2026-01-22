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
{  Author:  Cesar Romero & Antigravity                                      }
{  Created: 2026-01-21                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.Net.ConnectionPool;

interface

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Generics.Collections,
  System.Net.HttpClient;

type
  /// <summary>
  ///   Connection Pool for THttpClient instances to improve performance through reuse.
  /// </summary>
  TConnectionPool = class
  private
    FPool: TStack<THttpClient>;
    FLock: TCriticalSection;
    FMaxPoolSize: Integer;
    FCount: Integer;
  public
    constructor Create(AMaxPoolSize: Integer = 32);
    destructor Destroy; override;
    
    function Acquire: THttpClient;
    procedure Release(AClient: THttpClient);
    procedure Clear;
    
    property MaxPoolSize: Integer read FMaxPoolSize write FMaxPoolSize;
    property CurrentCount: Integer read FCount;
  end;

implementation

{ TConnectionPool }

constructor TConnectionPool.Create(AMaxPoolSize: Integer);
begin
  inherited Create;
  FMaxPoolSize := AMaxPoolSize;
  FPool := TStack<THttpClient>.Create;
  FLock := TCriticalSection.Create;
  FCount := 0;
end;

destructor TConnectionPool.Destroy;
begin
  Clear;
  FPool.Free;
  FLock.Free;
  inherited;
end;

function TConnectionPool.Acquire: THttpClient;
begin
  FLock.Enter;
  try
    if FPool.Count > 0 then
    begin
      Result := FPool.Pop;
    end
    else
    begin
      Result := THttpClient.Create;
      Inc(FCount);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TConnectionPool.Release(AClient: THttpClient);
begin
  if not Assigned(AClient) then Exit;
  
  FLock.Enter;
  try
    if FPool.Count < FMaxPoolSize then
    begin
      FPool.Push(AClient);
    end
    else
    begin
      AClient.Free;
      Dec(FCount);
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TConnectionPool.Clear;
begin
  FLock.Enter;
  try
    while FPool.Count > 0 do
      FPool.Pop.Free;
    FCount := 0;
  finally
    FLock.Leave;
  end;
end;

end.
