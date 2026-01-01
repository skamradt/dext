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
unit Dext.Configuration.EnvironmentVariables;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.IOUtils,
  Dext.Configuration.Interfaces,
  Dext.Configuration.Core;

type
  TEnvironmentVariablesConfigurationProvider = class(TConfigurationProvider)
  private
    FPrefix: string;
  public
    constructor Create(const Prefix: string = '');
    procedure Load; override;
  end;

  TEnvironmentVariablesConfigurationSource = class(TInterfacedObject, IConfigurationSource)
  private
    FPrefix: string;
  public
    constructor Create(const Prefix: string = '');
    function Build(Builder: IConfigurationBuilder): IConfigurationProvider;
  end;

implementation

{ TEnvironmentVariablesConfigurationSource }

constructor TEnvironmentVariablesConfigurationSource.Create(const Prefix: string);
begin
  inherited Create;
  FPrefix := Prefix;
end;

function TEnvironmentVariablesConfigurationSource.Build(Builder: IConfigurationBuilder): IConfigurationProvider;
begin
  Result := TEnvironmentVariablesConfigurationProvider.Create(FPrefix);
end;

{ TEnvironmentVariablesConfigurationProvider }

constructor TEnvironmentVariablesConfigurationProvider.Create(const Prefix: string);
begin
  inherited Create;
  FPrefix := Prefix;
end;

procedure TEnvironmentVariablesConfigurationProvider.Load;
var
  Vars: TStringList;
  I: Integer;
  Key, Value: string;
  EqIndex: Integer;
  EnvKey: string;
begin
  // Assuming FData is accessible. If TConfigurationProvider keeps it private, we must rely on Set or SetValue.
  // Since User said "FData não está sendo atribuido", it implies FData is visible and wasn't set.
  // We use direct access or standard dictionary methods.
  FData.Clear;
  Vars := TStringList.Create;
  try
    // Capture environment variables - Platform specific
    {$IFDEF MSWINDOWS}
    var P: PChar := GetEnvironmentStrings;
    try
      var PVar := P;
      while PVar^ <> #0 do
      begin
        Vars.Add(string(PVar));
        Inc(PVar, StrLen(PVar) + 1);
      end;
    finally
      FreeEnvironmentStrings(P);
    end;
    {$ENDIF}

    {$IFDEF LINUX}
    // Read from /proc/self/environ using TFileStream because TFile.ReadAllBytes fails on 0-size files (procfs)
    if TFile.Exists('/proc/self/environ') then
    begin
      var Stream := TFileStream.Create('/proc/self/environ', fmOpenRead or fmShareDenyNone);
      try
        var Bytes: TBytes;
        SetLength(Bytes, 4096);
        var TotalCount: Integer := 0;
        var ReadCount: Integer;

        // Read chunks until EOF
        while True do
        begin
          if TotalCount = Length(Bytes) then
            SetLength(Bytes, Length(Bytes) * 2);

          ReadCount := Stream.Read(Bytes[TotalCount], Length(Bytes) - TotalCount);
          if ReadCount = 0 then Break;
          Inc(TotalCount, ReadCount);
        end;
        SetLength(Bytes, TotalCount);

        // Parse null-terminated strings
        var StartIdx: Integer := 0;
        for var J := 0 to High(Bytes) do
        begin
          if Bytes[J] = 0 then
          begin
            if J > StartIdx then
              Vars.Add(TEncoding.UTF8.GetString(Bytes, StartIdx, J - StartIdx));
            StartIdx := J + 1;
          end;
        end;
      finally
        Stream.Free;
      end;
    end;
    {$ENDIF}

    // Process variables
    for I := 0 to Vars.Count - 1 do
    begin
      var Line := Vars[I];
      EqIndex := Pos('=', Line);
      if EqIndex > 1 then
      begin
        EnvKey := Copy(Line, 1, EqIndex - 1);
        Value := Copy(Line, EqIndex + 1, MaxInt);

        // Filter by prefix
        if (FPrefix <> '') and (not EnvKey.StartsWith(FPrefix, True)) then
          Continue;

        // Remove prefix
        if FPrefix <> '' then
          Key := EnvKey.Substring(Length(FPrefix))
        else
          Key := EnvKey;

        // Replace double underscore with colon
        Key := StringReplace(Key, '__', TConfigurationPath.KeyDelimiter, [rfReplaceAll]);

        // Set value directly in FData dictionary
        FData.AddOrSetValue(Key, Value);
      end;
    end;

  finally
    Vars.Free;
  end;
end;

end.

