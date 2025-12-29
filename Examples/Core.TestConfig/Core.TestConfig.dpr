program Core.TestConfig;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  System.IOUtils,
  Dext.Configuration.Interfaces,
  Dext.Configuration.Core,
  Dext.Configuration.Json,
  Dext.Configuration.EnvironmentVariables;

const
  // ============================================================================
  // DEFAULT CONFIGURATION - FOR DEMO PURPOSES ONLY
  // ============================================================================
  // In a real application, the appsettings.json file should be:
  //   1. Created manually in the project folder
  //   2. Deployed via the IDE's Deployment Manager (for FMX/mobile projects)
  //   3. Copied to the output folder by a build script (for console/VCL projects)
  //   4. Part of your CI/CD pipeline
  //
  // This auto-generation is ONLY for demonstration purposes so the example
  // works out-of-the-box without manual file copying.
  // ============================================================================
  DEFAULT_CONFIG_JSON = 
    '{' + sLineBreak +
    '  "AppSettings": {' + sLineBreak +
    '    "Message": "Hello from appsettings.json!"' + sLineBreak +
    '  },' + sLineBreak +
    '  "Logging": {' + sLineBreak +
    '    "LogLevel": {' + sLineBreak +
    '      "Default": "Information",' + sLineBreak +
    '      "System": "Warning",' + sLineBreak +
    '      "Microsoft": "Warning"' + sLineBreak +
    '    }' + sLineBreak +
    '  },' + sLineBreak +
    '  "ConnectionStrings": {' + sLineBreak +
    '    "DefaultConnection": "Server=localhost;Database=DextDemo;User=admin;Password=secret"' + sLineBreak +
    '  }' + sLineBreak +
    '}';

procedure EnsureConfigFileExists(const APath: string);
begin
  if not TFile.Exists(APath) then
  begin
    Writeln('');
    Writeln('>>> CONFIG FILE NOT FOUND - Creating default config for demo...');
    Writeln('>>> In production, this file should be deployed with your application!');
    Writeln('');
    TFile.WriteAllText(APath, DEFAULT_CONFIG_JSON, TEncoding.UTF8);
  end;
end;

procedure RunTest;
var
  Builder: IConfigurationBuilder;
  Config: IConfigurationRoot;
  Val: string;
  ExeDir: string;
  ConfigPath: string;
begin
  Writeln('Starting Configuration Test...');

  // Determine config file path (relative to executable directory)
  ExeDir := ExtractFilePath(ParamStr(0));
  ConfigPath := ExeDir + 'appsettings.json';
  
  // Ensure config file exists (DEMO ONLY - see comment above)
  EnsureConfigFileExists(ConfigPath);

  Builder := TConfigurationBuilder.Create;
  
  // Add JSON source
  Writeln('Adding JSON source: ', ConfigPath);
  Builder.Add(TJsonConfigurationSource.Create(ConfigPath, True));
  
  // Add Env Vars source
  Writeln('Adding Environment Variables source...');
  Builder.Add(TEnvironmentVariablesConfigurationSource.Create);
  
  Writeln('Building configuration...');
  Config := Builder.Build;
  
  Writeln('Reading values...');
  
  Val := Config['AppSettings:Message'];
  Writeln('AppSettings:Message = ', Val);
  
  Val := Config['Logging:LogLevel:Default'];
  Writeln('Logging:LogLevel:Default = ', Val);
  
  // Test Environment Variable (PATH is usually present)
  Val := Config['PATH']; 
  if Val = '' then Val := Config['Path'];
  Writeln('PATH length = ', Length(Val));
  
  Writeln('Test Finished.');
end;

begin
  SetConsoleCharSet;
  try
    RunTest;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  
  // Only pause if not running in automated mode
  if not FindCmdLineSwitch('no-wait', True) then
    ReadLn;
end.

