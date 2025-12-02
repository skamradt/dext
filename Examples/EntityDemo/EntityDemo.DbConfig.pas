unit EntityDemo.DbConfig;

interface

uses
  System.SysUtils,
  FireDAC.Comp.Client,
  FireDAC.Stan.Def,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.PG,
  FireDAC.Phys.FB,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.Dialects;

type
  /// <summary>
  ///   Database provider enumeration
  /// </summary>
  TDatabaseProvider = (
    dpSQLite,
    dpPostgreSQL,
    dpFirebird,
    dpMySQL,
    dpSQLServer,
    dpOracle
  );

  /// <summary>
  ///   Database configuration helper
  ///   Provides easy switching between database providers for testing
  /// </summary>
  TDbConfig = class
  private
    class var FCurrentProvider: TDatabaseProvider;
    class var FSQLiteFile: string;
    class var FPostgreSQLHost: string;
    class var FPostgreSQLPort: Integer;
    class var FPostgreSQLDatabase: string;
    class var FPostgreSQLUsername: string;
    class var FPostgreSQLPassword: string;
    class var FFirebirdFile: string;
    class var FFirebirdUsername: string;
    class var FFirebirdPassword: string;
  public
    /// <summary>
    ///   Initialize default configuration
    /// </summary>
    class constructor Create;
    
    /// <summary>
    ///   Get current database provider
    /// </summary>
    class function GetProvider: TDatabaseProvider; static;
    
    /// <summary>
    ///   Set current database provider
    /// </summary>
    class procedure SetProvider(AProvider: TDatabaseProvider); static;
    
    /// <summary>
    ///   Create a connection for the current provider
    /// </summary>
    class function CreateConnection: IDbConnection; static;
    
    /// <summary>
    ///   Create a dialect for the current provider
    /// </summary>
    class function CreateDialect: ISQLDialect; static;
    
    /// <summary>
    ///   Get provider name as string
    /// </summary>
    class function GetProviderName: string; static;
    
    /// <summary>
    ///   Configure SQLite connection
    /// </summary>
    class procedure ConfigureSQLite(const AFileName: string = 'test.db'); static;
    
    /// <summary>
    ///   Configure PostgreSQL connection
    /// </summary>
    class procedure ConfigurePostgreSQL(
      const AHost: string = 'localhost';
      APort: Integer = 5432;
      const ADatabase: string = 'dext_test';
      const AUsername: string = 'postgres';
      const APassword: string = 'postgres'
    ); static;
    
    /// <summary>
    ///   Configure Firebird connection
    /// </summary>
    class procedure ConfigureFirebird(
      const AFileName: string = 'test.fdb';
      const AUsername: string = 'SYSDBA';
      const APassword: string = 'masterkey'
    ); static;
    
    /// <summary>
    ///   Drop and recreate database (for testing)
    /// </summary>
    class procedure ResetDatabase; static;
  end;

implementation

uses
  System.IOUtils;

{ TDbConfig }

class constructor TDbConfig.Create;
begin
  // Default to SQLite for compatibility
  FCurrentProvider := dpSQLite;
  
  // Default SQLite configuration
  FSQLiteFile := 'test.db';
  
  // Default PostgreSQL configuration
  FPostgreSQLHost := 'localhost';
  FPostgreSQLPort := 5432;
  FPostgreSQLDatabase := 'dext_test';
  FPostgreSQLUsername := 'postgres';
  FPostgreSQLPassword := 'postgres';
  
  // Default Firebird configuration
  FFirebirdFile := 'test.fdb';
  FFirebirdUsername := 'SYSDBA';
  FFirebirdPassword := 'masterkey';
end;

class function TDbConfig.GetProvider: TDatabaseProvider;
begin
  Result := FCurrentProvider;
end;

class procedure TDbConfig.SetProvider(AProvider: TDatabaseProvider);
begin
  FCurrentProvider := AProvider;
  WriteLn('🔄 Database Provider changed to: ' + GetProviderName);
end;

class function TDbConfig.CreateConnection: IDbConnection;
var
  FDConn: TFDConnection;
begin
  FDConn := TFDConnection.Create(nil);
  
  case FCurrentProvider of
    dpSQLite:
    begin
      FDConn.DriverName := 'SQLite';
      FDConn.Params.Values['Database'] := FSQLiteFile;
      FDConn.Params.Values['LockingMode'] := 'Normal';
    end;
    
    dpPostgreSQL:
    begin
      FDConn.DriverName := 'PG';
      FDConn.Params.Values['Server'] := FPostgreSQLHost;
      FDConn.Params.Values['Port'] := FPostgreSQLPort.ToString;
      FDConn.Params.Values['Database'] := FPostgreSQLDatabase;
      FDConn.Params.Values['User_Name'] := FPostgreSQLUsername;
      FDConn.Params.Values['Password'] := FPostgreSQLPassword;
    end;
    
    dpFirebird:
    begin
      FDConn.DriverName := 'FB';
      FDConn.Params.Values['Database'] := FFirebirdFile;
      FDConn.Params.Values['User_Name'] := FFirebirdUsername;
      FDConn.Params.Values['Password'] := FFirebirdPassword;
      FDConn.Params.Values['CharacterSet'] := 'UTF8';
    end;
    
    else
      raise Exception.CreateFmt('Database provider %s not yet implemented', [GetProviderName]);
  end;
  
  Result := TFireDACConnection.Create(FDConn);
end;

class function TDbConfig.CreateDialect: ISQLDialect;
begin
  case FCurrentProvider of
    dpSQLite:     Result := TSQLiteDialect.Create;
    dpPostgreSQL: Result := TPostgreSQLDialect.Create;
    dpFirebird:   Result := TFirebirdDialect.Create;
    else
      raise Exception.CreateFmt('Dialect for %s not yet implemented', [GetProviderName]);
  end;
end;

class function TDbConfig.GetProviderName: string;
begin
  case FCurrentProvider of
    dpSQLite:     Result := 'SQLite';
    dpPostgreSQL: Result := 'PostgreSQL';
    dpFirebird:   Result := 'Firebird';
    dpMySQL:      Result := 'MySQL';
    dpSQLServer:  Result := 'SQL Server';
    dpOracle:     Result := 'Oracle';
    else          Result := 'Unknown';
  end;
end;

class procedure TDbConfig.ConfigureSQLite(const AFileName: string);
begin
  FSQLiteFile := AFileName;
  WriteLn('✅ SQLite configured: ' + AFileName);
end;

class procedure TDbConfig.ConfigurePostgreSQL(
  const AHost: string;
  APort: Integer;
  const ADatabase: string;
  const AUsername: string;
  const APassword: string
);
begin
  FPostgreSQLHost := AHost;
  FPostgreSQLPort := APort;
  FPostgreSQLDatabase := ADatabase;
  FPostgreSQLUsername := AUsername;
  FPostgreSQLPassword := APassword;
  WriteLn(Format('✅ PostgreSQL configured: %s:%d/%s', [AHost, APort, ADatabase]));
end;

class procedure TDbConfig.ConfigureFirebird(
  const AFileName: string;
  const AUsername: string;
  const APassword: string
);
begin
  FFirebirdFile := AFileName;
  FFirebirdUsername := AUsername;
  FFirebirdPassword := APassword;
  WriteLn('✅ Firebird configured: ' + AFileName);
end;

class procedure TDbConfig.ResetDatabase;
begin
  case FCurrentProvider of
    dpSQLite:
    begin
      // Delete SQLite file if exists
      if TFile.Exists(FSQLiteFile) then
      begin
        TFile.Delete(FSQLiteFile);
        WriteLn('🗑️  Deleted SQLite database: ' + FSQLiteFile);
      end;
    end;
    
    dpPostgreSQL:
    begin
      // PostgreSQL: Drop all tables (handled by EnsureCreated)
      WriteLn('⚠️  PostgreSQL: Tables will be recreated by EnsureCreated');
    end;
    
    dpFirebird:
    begin
      // Delete Firebird file if exists
      if TFile.Exists(FFirebirdFile) then
      begin
        TFile.Delete(FFirebirdFile);
        WriteLn('🗑️  Deleted Firebird database: ' + FFirebirdFile);
      end;
    end;
  end;
end;

end.
