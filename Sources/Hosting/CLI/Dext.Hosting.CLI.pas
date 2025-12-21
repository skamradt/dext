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
unit Dext.Hosting.CLI;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Dext.Entity,
  Dext.Entity.Core,
  Dext.Entity.Migrations,
  Dext.Entity.Migrations.Runner,
  Dext.Entity.Migrations.Json,
  Dext.Entity.Drivers.Interfaces,
  Dext.Hosting.CLI.Args;

type
  IConsoleCommand = interface
    ['{A1B2C3D4-E5F6-4789-0123-456789ABCDEF}']
    function GetName: string;
    function GetDescription: string;
    procedure Execute(const Args: TCommandLineArgs);
  end;

  TDextCLI = class
  private
    FCommands: TDictionary<string, IConsoleCommand>;
    FContextFactory: TFunc<IDbContext>;
    procedure RegisterCommands;
    procedure ShowHelp;
  public
    constructor Create(AContextFactory: TFunc<IDbContext>);
    destructor Destroy; override;
    // Returns True if a command was executed, False if normal startup should proceed
    function Run: Boolean; 
  end;

  // --- Commands are now in Dext.Hosting.CLI.Commands.* units ---


implementation

uses
  Dext.Hosting.CLI.Commands.MigrateUp,
  Dext.Hosting.CLI.Commands.MigrateList;

{ TDextCLI }

constructor TDextCLI.Create(AContextFactory: TFunc<IDbContext>);
begin
  FContextFactory := AContextFactory;
  FCommands := TDictionary<string, IConsoleCommand>.Create;
  RegisterCommands;
end;

destructor TDextCLI.Destroy;
begin
  FCommands.Free;
  inherited;
end;

procedure TDextCLI.RegisterCommands;
begin
  var CmdUp := TMigrateUpCommand.Create(FContextFactory);
  FCommands.Add(CmdUp.GetName, CmdUp);
  
  var CmdList := TMigrateListCommand.Create(FContextFactory);
  FCommands.Add(CmdList.GetName, CmdList);
end;

procedure TDextCLI.ShowHelp;
begin
  WriteLn('Dext CLI Tool');
  WriteLn('-------------');
  WriteLn('Usage: MyApp.exe <command> [args]');
  WriteLn('');
  WriteLn('Available Commands:');
  for var Cmd in FCommands.Values do
  begin
    WriteLn('  ' + Cmd.GetName.PadRight(20) + Cmd.GetDescription);
  end;
  WriteLn('');
end;

function TDextCLI.Run: Boolean;
var
  CmdName: string;
  Cmd: IConsoleCommand;
  Args: TCommandLineArgs;
  RawArgs: TArray<string>;
  i: Integer;
begin
  // Check if any arguments passed
  if ParamCount = 0 then
    Exit(False); // No command, proceed to normal app startup

  Args := TCommandLineArgs.Create;
  try
    SetLength(RawArgs, ParamCount);
    for i := 1 to ParamCount do
      RawArgs[i-1] := ParamStr(i);
      
    Args.Parse(RawArgs);
    
    CmdName := Args.Command.ToLower;
    
    // Handle Help
    if (CmdName = 'help') or (CmdName = '') or Args.HasOption('help') or Args.HasOption('h') then
    begin
      ShowHelp;
      Exit(True);
    end;

    if FCommands.TryGetValue(CmdName, Cmd) then
    begin
      try
        Cmd.Execute(Args);
      except
        on E: Exception do
          WriteLn('Error executing command: ' + E.Message);
      end;
      Result := True; // Command executed, app should terminate
    end
    else
    begin
      // If arg starts with -, it might be a flag for the main app, so ignore
      if CmdName.StartsWith('-') then
        Exit(False);
        
      WriteLn('Unknown command: ' + CmdName);
      ShowHelp;
      Result := True; // Prevent normal startup on bad command
    end;
  finally
    Args.Free;
  end;
end;

end.

