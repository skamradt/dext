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
  System.IOUtils,
  Dext.Hosting.CLI.Registry,
  Dext.Hosting.CLI.Commands.MigrateUp,
  Dext.Hosting.CLI.Commands.MigrateList,
  Dext.Hosting.CLI.Commands.MigrateDown,
  Dext.Hosting.CLI.Commands.MigrateGenerate,
  Dext.Hosting.CLI.Commands.Test,
  Dext.Hosting.CLI.Commands.Configuration,
  Dext.Hosting.CLI.Commands.UI,
  Dext.Utils;

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

  var CmdDown := TMigrateDownCommand.Create(FContextFactory);
  FCommands.Add(CmdDown.GetName, CmdDown);

  var CmdGen := TMigrateGenerateCommand.Create;
  FCommands.Add(CmdGen.GetName, CmdGen);

  var CmdTest := TTestCommand.Create;
  FCommands.Add(CmdTest.GetName, CmdTest);
  
  var CmdConfig := TConfigInitCommand.Create;
  FCommands.Add(CmdConfig.GetName, CmdConfig);
  
  var CmdEnv := TEnvScanCommand.Create;
  FCommands.Add(CmdEnv.GetName, CmdEnv);

  var CmdUI := TUICommand.Create;
  FCommands.Add(CmdUI.GetName, CmdUI);
end;

procedure TDextCLI.ShowHelp;
begin
  SafeWriteLn('Dext CLI Tool');
  SafeWriteLn('-------------');
  SafeWriteLn('Usage: MyApp.exe <command> [args]');
  SafeWriteLn('');
  SafeWriteLn('Available Commands:');
  for var Cmd in FCommands.Values do
  begin
    SafeWriteLn('  ' + Cmd.GetName.PadRight(20) + Cmd.GetDescription);
  end;
  SafeWriteLn('');
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
          SafeWriteLn('Error executing command: ' + E.Message);
      end;
      Result := True; // Command executed, app should terminate
    end
    else
    begin
      // If arg starts with -, it might be a flag for the main app, so ignore
      if CmdName.StartsWith('-') then
        Exit(False);
        
      SafeWriteLn('Unknown command: ' + CmdName);
      ShowHelp;
      Result := True; // Prevent normal startup on bad command
    end;
  finally
    Args.Free;
  end;

  // Auto-register project if we are in a valid folder (has .dproj or .dext.config)
  try
    var CurrentDir := GetCurrentDir;
    // Simple heuristic: if .dproj exists or .dext.config exists
    if Length(TDirectory.GetFiles(CurrentDir, '*.dproj')) > 0 then
    begin
       // Use lazy loaded registry to avoid performance hit on every command?
       // It's fast enough for CLI.
       var Registry := TProjectRegistry.Create;
       try
         Registry.RegisterProject(CurrentDir);
       finally
         Registry.Free;
       end;
    end;
  except
    // Ignore errors here, logging shouldn't break CLI flow
  end;
end;

end.

