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
unit Dext.Utils;

interface

{$IFDEF MSWINDOWS}
uses
  WinApi.Windows;
{$ENDIF}

function ConsolePause: Boolean;
procedure DebugLog(const AMessage: string);
procedure SetConsoleCharSet(CharSet: Cardinal = 65001);

/// <summary>
///   Checks if console output is available. Returns False for GUI applications
///   (VCL/FMX) that don't have a console attached.
/// </summary>
function IsConsoleAvailable: Boolean;

/// <summary>
///   Writes a message to console only if console is available.
///   Silently does nothing in GUI applications to prevent I/O error 105.
/// </summary>
procedure SafeWriteLn(const AMessage: string); overload;
procedure SafeWriteLn; overload;
procedure SafeWrite(const AMessage: string);

implementation

uses
  System.SysUtils;

var
  ConsoleAvailable: Boolean = False;
  ConsoleChecked: Boolean = False;

function IsConsoleAvailable: Boolean;
begin
  if not ConsoleChecked then
  begin
    ConsoleChecked := True;
    {$IFDEF CONSOLE}
    ConsoleAvailable := True;
    {$ELSE}
      {$IFDEF MSWINDOWS}
      var Handle := GetStdHandle(STD_OUTPUT_HANDLE);
      ConsoleAvailable := (Handle <> 0) and (Handle <> INVALID_HANDLE_VALUE);
      {$ELSE}
      ConsoleAvailable := System.IsConsole;
      {$ENDIF}
    {$ENDIF}
  end;
  Result := ConsoleAvailable;
end;

procedure SafeWriteLn(const AMessage: string);
begin
  if IsConsoleAvailable then
  try
    System.Writeln(AMessage);
  except
    // Silently ignore I/O errors
  end;
end;

procedure SafeWriteLn;
begin
  if IsConsoleAvailable then
  try
    System.Writeln;
  except
    // Silently ignore I/O errors
  end;
end;

procedure SafeWrite(const AMessage: string);
begin
  if IsConsoleAvailable then
  try
    System.Write(AMessage);
  except
    // Silently ignore I/O errors
  end;
end;

function ConsolePause: Boolean;
begin
  Result := FindCmdLineSwitch('no-wait', ['-', '\'], True);
  if not Result then
  begin
    if IsConsoleAvailable then
    begin
      System.Write('Press <ENTER> to continue...');
      System.ReadLn;
    end;
  end;
end;

procedure DebugLog(const AMessage: string);
begin
{$IFDEF MSWINDOWS}
  OutputDebugString(PChar(AMessage + sLineBreak));
{$ENDIF}
  SafeWriteLn(AMessage);
end;

procedure SetConsoleCharSet(CharSet: Cardinal);
begin
{$IFDEF MSWINDOWS}
 SetConsoleOutputCP(65001);
{$ENDIF}
end;


end.

