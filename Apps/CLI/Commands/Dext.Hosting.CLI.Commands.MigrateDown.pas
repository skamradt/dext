unit Dext.Hosting.CLI.Commands.MigrateDown;

interface

uses
  System.SysUtils,
  Dext.Entity,
  Dext.Entity.Core,
  Dext.Entity.Migrations.Runner,
  Dext.Entity.Migrations.Json,
  Dext.Hosting.CLI,
  Dext.Hosting.CLI.Args,
  Dext.Utils;

type
  TMigrateDownCommand = class(TInterfacedObject, IConsoleCommand)
  private
    FContextFactory: TFunc<IDbContext>;
  public
    constructor Create(AContextFactory: TFunc<IDbContext>);
    function GetName: string;
    function GetDescription: string;
    procedure Execute(const Args: TCommandLineArgs);
  end;

implementation

{ TMigrateDownCommand }

constructor TMigrateDownCommand.Create(AContextFactory: TFunc<IDbContext>);
begin
  FContextFactory := AContextFactory;
end;

function TMigrateDownCommand.GetName: string;
begin
  Result := 'migrate:down';
end;

function TMigrateDownCommand.GetDescription: string;
begin
  Result := 'Reverts the last migration or down to a specific target. Usage: migrate:down [--target <id>]';
end;

procedure TMigrateDownCommand.Execute(const Args: TCommandLineArgs);
var
  Context: IDbContext;
  Migrator: TMigrator;
  TargetId: string;
begin
  TargetId := Args.GetOption('target');
  if TargetId = '' then
    TargetId := Args.GetOption('t');

  SafeWriteLn('Starting migration rollback...');
  Context := FContextFactory();
  try
    Migrator := TMigrator.Create(Context);
    try
      Migrator.Rollback(TargetId);
      SafeWriteLn('Rollback completed.');
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
