program DextTool;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  Dext.Hosting.CLI,
  Dext.Hosting.CLI.Registry,
  Dext.Entity.Core,
  Dext.Entity.Context,
  Dext.Entity.Setup,
  Dext.Configuration.Core,
  Dext.Configuration.Interfaces,
  Dext.Configuration.Json,
  Dext.Hosting.CLI.Commands.MigrateUp,
  Dext.Hosting.CLI.Commands.MigrateDown,
  Dext.Hosting.CLI.Commands.MigrateList,
  Dext.Hosting.CLI.Commands.MigrateGenerate,
  Dext.Hosting.CLI.Commands.Test,
  Dext.Hosting.CLI.Commands.Configuration,
  Dext.Hosting.CLI.Commands.UI,
  Dext.Hosting.CLI.Commands.Scaffold,
  Dext.Hosting.CLI.Commands.Doc,
  Dext.Hosting.CLI.Commands.Facade;

function CreateDbContext: IDbContext;
var
  Builder: IConfigurationBuilder;
  Config: IConfigurationRoot;
  Options: TDbContextOptions;
  ConnString: string;
  Driver: string;
begin
  // Build Configuration
  Builder := TConfigurationBuilder.Create;
  Builder.Add(TJsonConfigurationSource.Create('appsettings.json', True));
  Config := Builder.Build;

  // Configure Options
  Options := TDbContextOptions.Create;
  try
    ConnString := Config['ConnectionStrings:DefaultConnection'];
    if ConnString = '' then
      ConnString := 'Data Source=dext_cli.db;Mode=ReadWriteCreate'; // Default fallback
      
    Driver := Config['Database:Driver'];
    if Driver = '' then 
      Driver := 'SQLite';
      
    // Set up options
    Options.UseDriver(Driver);
    // Explicitly set connection params if needed, or rely on ConnectionString parsing 
    // (FireDAC TFDConnection does not autoparse "Data Source=..." string into Params unless using ADO style, 
    //  but Dext.Entity.Setup uses Params dictionary manually. 
    //  Let's simplify: if SQLite, use helper. If generic, use Params).
    
    if Driver.ToLower = 'sqlite' then
    begin
       // Extract filename from "Data Source=X;..." simple parsing
       var DbFile := 'dext_cli.db';
       // Very basic parser for demo
       if ConnString.Contains('Data Source=') then
       begin
         var Parts := ConnString.Split([';']);
         for var P in Parts do
           if P.Trim.StartsWith('Data Source=') then
             DbFile := P.Trim.Substring(12);
       end;
       Options.UseSQLite(DbFile);
    end
    else
    begin
      // For other drivers, we might need more robust parsing or config binding
      // For now, assume SQLite default for CLI tool or simple params
      Options.Params.AddOrSetValue('Database', ConnString);
    end;

    // Create Context
    Result := TDbContext.Create(Options);
  except
    Options.Free;
    raise;
  end;
end;

var
  CLI: TDextCLI;
begin
  try
    CLI := TDextCLI.Create(CreateDbContext);
    try
      // Migration Commands
      CLI.AddCommand(TMigrateUpCommand.Create(CreateDbContext));
      CLI.AddCommand(TMigrateDownCommand.Create(CreateDbContext));
      CLI.AddCommand(TMigrateListCommand.Create(CreateDbContext));
      CLI.AddCommand(TMigrateGenerateCommand.Create);
      
      // Tool Commands
      CLI.AddCommand(TTestCommand.Create);
      CLI.AddCommand(TConfigInitCommand.Create);
      CLI.AddCommand(TEnvScanCommand.Create);
      CLI.AddCommand(TUICommand.Create);
      CLI.AddCommand(TScaffoldCommand.Create);
      CLI.AddCommand(TDocCommand.Create);
      CLI.AddCommand(TFacadeCommand.Create);

      CLI.Run;
    finally
      CLI.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
