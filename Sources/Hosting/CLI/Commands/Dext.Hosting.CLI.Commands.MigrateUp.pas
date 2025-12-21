unit Dext.Hosting.CLI.Commands.MigrateUp;

interface

uses
  System.SysUtils,
  Dext.Entity,
  Dext.Entity.Core,
  Dext.Entity.Migrations.Runner,
  Dext.Entity.Migrations.Json,
  Dext.Hosting.CLI,
  Dext.Hosting.CLI.Args;

type
  TMigrateUpCommand = class(TInterfacedObject, IConsoleCommand)
  private
    FContextFactory: TFunc<IDbContext>;
  public
    constructor Create(AContextFactory: TFunc<IDbContext>);
    function GetName: string;
    function GetDescription: string;
    procedure Execute(const Args: TCommandLineArgs);
  end;

implementation

{ TMigrateUpCommand }

constructor TMigrateUpCommand.Create(AContextFactory: TFunc<IDbContext>);
begin
  FContextFactory := AContextFactory;
end;

function TMigrateUpCommand.GetName: string;
begin
  Result := 'migrate:up';
end;

function TMigrateUpCommand.GetDescription: string;
begin
  Result := 'Applies all pending migrations to the database. Usage: migrate:up [--source <path>]';
end;

procedure TMigrateUpCommand.Execute(const Args: TCommandLineArgs);
var
  Context: IDbContext;
  Migrator: TMigrator;
  SourcePath: string;
begin
  SourcePath := Args.GetOption('source');
  if SourcePath = '' then
    SourcePath := Args.GetOption('s'); // Alias

  if SourcePath <> '' then
  begin
    WriteLn('   📂 Loading migrations from: ' + SourcePath);
    TJsonMigrationLoader.LoadFromDirectory(SourcePath);
  end;

  WriteLn('Starting migration update...');
  Context := FContextFactory();
  try
    Migrator := TMigrator.Create(Context);
    try
      Migrator.Migrate;
      WriteLn('Database is up to date.');
    finally
      Migrator.Free;
    end;
  finally
    if Context is TDbContext then
    begin
      var CtxObj := Context as TDbContext;
      Context := nil; 
      CtxObj.Free;
    end;
  end;
end;

end.
