unit Dext.Hosting.CLI.Commands.MigrateList;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Dext.Entity,
  Dext.Entity.Core,
  Dext.Entity.Migrations,
  Dext.Entity.Migrations.Runner,
  Dext.Hosting.CLI,
  Dext.Hosting.CLI.Args,
  Dext.Entity.Drivers.Interfaces;

type
  TMigrateListCommand = class(TInterfacedObject, IConsoleCommand)
  private
    FContextFactory: TFunc<IDbContext>;
  public
    constructor Create(AContextFactory: TFunc<IDbContext>);
    function GetName: string;
    function GetDescription: string;
    procedure Execute(const Args: TCommandLineArgs);
  end;

implementation

{ TMigrateListCommand }

constructor TMigrateListCommand.Create(AContextFactory: TFunc<IDbContext>);
begin
  FContextFactory := AContextFactory;
end;

function TMigrateListCommand.GetName: string;
begin
  Result := 'migrate:list';
end;

function TMigrateListCommand.GetDescription: string;
begin
  Result := 'Lists applied and pending migrations.';
end;

procedure TMigrateListCommand.Execute(const Args: TCommandLineArgs);
var
  Context: IDbContext;
  Migrator: TMigrator;
  Applied: TList<string>;
  Available: TArray<IMigration>;
  Status: string;
begin
  Context := FContextFactory();
  try
    Migrator := TMigrator.Create(Context);
    try
      WriteLn('Migration Status:');
      WriteLn('-----------------');
      
      Available := TMigrationRegistry.Instance.GetMigrations;
      
      if not Context.Connection.TableExists('__DextMigrations') then
      begin
        WriteLn('History table not found. All ' + Length(Available).ToString + ' migrations are PENDING.');
        Exit;
      end;
      
      Applied := TList<string>.Create;
      try
        var Cmd := Context.Connection.CreateCommand('SELECT Id FROM __DextMigrations');
        var Reader := IDbCommand(Cmd).ExecuteQuery;
        while Reader.Next do
          Applied.Add(Reader.GetValue(0).AsString);
          
        for var Mig in Available do
        begin
          if Applied.Contains(Mig.GetId) then
            Status := '[Applied]'
          else
            Status := '[Pending]';
            
          WriteLn(Status.PadRight(12) + Mig.GetId);
        end;
        
      finally
        Applied.Free;
      end;

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
