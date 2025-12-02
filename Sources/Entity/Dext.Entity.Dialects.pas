unit Dext.Entity.Dialects;

interface

uses
  System.SysUtils,
  System.TypInfo,
  Dext.Entity.Attributes;

type
  /// <summary>
  ///   Abstracts database-specific SQL syntax differences.
  /// </summary>
  ISQLDialect = interface
    ['{20000000-0000-0000-0000-000000000001}']
    function QuoteIdentifier(const AName: string): string;
    function GetParamPrefix: string;
    function GeneratePaging(ASkip, ATake: Integer): string;
    function BooleanToSQL(AValue: Boolean): string;
    function GetColumnType(ATypeInfo: PTypeInfo; AIsAutoInc: Boolean = False): string;
    function GetCascadeActionSQL(AAction: TCascadeAction): string;
    function GetLastInsertIdSQL: string;
    function GetCreateTableSQL(const ATableName, ABody: string): string;
  end;

  /// <summary>
  ///   Base class for dialects.
  /// </summary>
  TBaseDialect = class(TInterfacedObject, ISQLDialect)
  public
    function QuoteIdentifier(const AName: string): string; virtual;
    function GetParamPrefix: string; virtual;
    function GeneratePaging(ASkip, ATake: Integer): string; virtual; abstract;
    function BooleanToSQL(AValue: Boolean): string; virtual;
    function GetColumnType(ATypeInfo: PTypeInfo; AIsAutoInc: Boolean = False): string; virtual; abstract;
    function GetCascadeActionSQL(AAction: TCascadeAction): string; virtual;
    function GetLastInsertIdSQL: string; virtual; abstract;
    function GetCreateTableSQL(const ATableName, ABody: string): string; virtual;
  end;

  /// <summary>
  ///   SQLite Dialect implementation.
  /// </summary>
  TSQLiteDialect = class(TBaseDialect)
  public
    function QuoteIdentifier(const AName: string): string; override;
    function GeneratePaging(ASkip, ATake: Integer): string; override;
    function BooleanToSQL(AValue: Boolean): string; override;
    function GetColumnType(ATypeInfo: PTypeInfo; AIsAutoInc: Boolean = False): string; override;
    function GetLastInsertIdSQL: string; override;
    function GetCreateTableSQL(const ATableName, ABody: string): string; override;
  end;

  /// <summary>
  ///   PostgreSQL Dialect implementation.
  /// </summary>
  TPostgreSQLDialect = class(TBaseDialect)
  public
    function QuoteIdentifier(const AName: string): string; override;
    function GeneratePaging(ASkip, ATake: Integer): string; override;
    function BooleanToSQL(AValue: Boolean): string; override;
    function GetColumnType(ATypeInfo: PTypeInfo; AIsAutoInc: Boolean = False): string; override;
    function GetLastInsertIdSQL: string; override;
    function GetCreateTableSQL(const ATableName, ABody: string): string; override;
  end;

  /// <summary>
  ///   Firebird 3.0+ Dialect implementation.
  /// </summary>
  TFirebirdDialect = class(TBaseDialect)
  public
    function QuoteIdentifier(const AName: string): string; override;
    function GeneratePaging(ASkip, ATake: Integer): string; override;
    function BooleanToSQL(AValue: Boolean): string; override;
    function GetColumnType(ATypeInfo: PTypeInfo; AIsAutoInc: Boolean = False): string; override;
    function GetLastInsertIdSQL: string; override;
    function GetCreateTableSQL(const ATableName, ABody: string): string; override;
  end;

  /// <summary>
  ///   SQL Server (2012+) Dialect implementation.
  /// </summary>
  TSQLServerDialect = class(TBaseDialect)
  public
    function QuoteIdentifier(const AName: string): string; override;
    function GeneratePaging(ASkip, ATake: Integer): string; override;
    function BooleanToSQL(AValue: Boolean): string; override;
    function GetColumnType(ATypeInfo: PTypeInfo; AIsAutoInc: Boolean = False): string; override;
    function GetLastInsertIdSQL: string; override;
    function GetCreateTableSQL(const ATableName, ABody: string): string; override;
  end;

  /// <summary>
  ///   MySQL / MariaDB Dialect implementation.
  /// </summary>
  TMySQLDialect = class(TBaseDialect)
  public
    function QuoteIdentifier(const AName: string): string; override;
    function GeneratePaging(ASkip, ATake: Integer): string; override;
    function BooleanToSQL(AValue: Boolean): string; override;
    function GetColumnType(ATypeInfo: PTypeInfo; AIsAutoInc: Boolean = False): string; override;
    function GetLastInsertIdSQL: string; override;
    function GetCreateTableSQL(const ATableName, ABody: string): string; override;
  end;

  /// <summary>
  ///   Oracle (12c+) Dialect implementation.
  /// </summary>
  TOracleDialect = class(TBaseDialect)
  public
    function QuoteIdentifier(const AName: string): string; override;
    function GeneratePaging(ASkip, ATake: Integer): string; override;
    function BooleanToSQL(AValue: Boolean): string; override;
    function GetColumnType(ATypeInfo: PTypeInfo; AIsAutoInc: Boolean = False): string; override;
    function GetLastInsertIdSQL: string; override;
    function GetCreateTableSQL(const ATableName, ABody: string): string; override;
  end;

implementation

{ TBaseDialect }

function TBaseDialect.BooleanToSQL(AValue: Boolean): string;
begin
  if AValue then Result := '1' else Result := '0';
end;

function TBaseDialect.GetParamPrefix: string;
begin
  Result := ':'; // Standard for FireDAC
end;

function TBaseDialect.QuoteIdentifier(const AName: string): string;
begin
  Result := '"' + AName + '"';
end;

function TBaseDialect.GetCascadeActionSQL(AAction: TCascadeAction): string;
begin
  case AAction of
    caNoAction: Result := 'NO ACTION';
    caCascade:  Result := 'CASCADE';
    caSetNull:  Result := 'SET NULL';
    caRestrict: Result := 'RESTRICT';
  else
    Result := 'NO ACTION';
  end;
end;

function TBaseDialect.GetCreateTableSQL(const ATableName, ABody: string): string;
begin
  // Default implementation (Standard SQL)
  Result := Format('CREATE TABLE %s (%s);', [ATableName, ABody]);
end;

{ TSQLiteDialect }

function TSQLiteDialect.BooleanToSQL(AValue: Boolean): string;
begin
  // SQLite uses 1/0 for boolean
  if AValue then Result := '1' else Result := '0';
end;

function TSQLiteDialect.GeneratePaging(ASkip, ATake: Integer): string;
begin
  // LIMIT <count> OFFSET <skip>
  Result := Format('LIMIT %d OFFSET %d', [ATake, ASkip]);
end;

function TSQLiteDialect.QuoteIdentifier(const AName: string): string;
begin
  // SQLite supports double quotes for identifiers
  Result := '"' + AName + '"';
end;

function TSQLiteDialect.GetColumnType(ATypeInfo: PTypeInfo; AIsAutoInc: Boolean): string;
begin
  if AIsAutoInc then
    Exit('INTEGER'); // SQLite AutoInc must be INTEGER PRIMARY KEY

  case ATypeInfo.Kind of
    tkInteger, tkInt64: Result := 'INTEGER';
    tkFloat: 
      begin
        if ATypeInfo = TypeInfo(TDateTime) then Result := 'REAL' // Or TEXT/INTEGER depending on storage pref
        else if ATypeInfo = TypeInfo(TDate) then Result := 'REAL'
        else if ATypeInfo = TypeInfo(TTime) then Result := 'REAL'
        else Result := 'REAL';
      end;
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString: Result := 'TEXT';
    tkEnumeration:
      begin
        if ATypeInfo = TypeInfo(Boolean) then Result := 'INTEGER' // 0 or 1
        else Result := 'INTEGER'; // Store Enums as Ints by default
      end;
    tkVariant: Result := 'BLOB'; // Fallback
  else
    Result := 'TEXT'; // Default fallback
  end;
end;

function TSQLiteDialect.GetLastInsertIdSQL: string;
begin
  Result := 'SELECT last_insert_rowid()';
end;

function TSQLiteDialect.GetCreateTableSQL(const ATableName, ABody: string): string;
begin
  // SQLite supports IF NOT EXISTS
  Result := Format('CREATE TABLE IF NOT EXISTS %s (%s);', [ATableName, ABody]);
end;

{ TPostgreSQLDialect }

function TPostgreSQLDialect.BooleanToSQL(AValue: Boolean): string;
begin
  // Postgres uses TRUE/FALSE
  if AValue then Result := 'TRUE' else Result := 'FALSE';
end;

function TPostgreSQLDialect.GeneratePaging(ASkip, ATake: Integer): string;
begin
  // LIMIT <count> OFFSET <skip>
  Result := Format('LIMIT %d OFFSET %d', [ATake, ASkip]);
end;

function TPostgreSQLDialect.QuoteIdentifier(const AName: string): string;
begin
  // Postgres uses double quotes, but forces lowercase unless quoted.
  // We quote to preserve case sensitivity if needed.
  Result := '"' + AName + '"';
end;

function TPostgreSQLDialect.GetColumnType(ATypeInfo: PTypeInfo; AIsAutoInc: Boolean): string;
begin
  if AIsAutoInc then
    Exit('SERIAL');

  case ATypeInfo.Kind of
    tkInteger: Result := 'INTEGER';
    tkInt64: Result := 'BIGINT';
    tkFloat: 
      begin
        if ATypeInfo = TypeInfo(Double) then Result := 'DOUBLE PRECISION'
        else if ATypeInfo = TypeInfo(Single) then Result := 'REAL'
        else if ATypeInfo = TypeInfo(Currency) then Result := 'MONEY'
        else if ATypeInfo = TypeInfo(TDateTime) then Result := 'TIMESTAMP'
        else Result := 'DOUBLE PRECISION';
      end;
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString: Result := 'VARCHAR(255)'; // Default length
    tkEnumeration:
      begin
        if ATypeInfo = TypeInfo(Boolean) then Result := 'BOOLEAN'
        else Result := 'INTEGER';
      end;
    tkRecord:
      begin
        if ATypeInfo = TypeInfo(TGUID) then Result := 'UUID'
        else Result := 'TEXT';
      end;
  else
    Result := 'TEXT';
  end;
end;

function TPostgreSQLDialect.GetLastInsertIdSQL: string;
begin
  Result := 'SELECT lastval()';
end;

function TPostgreSQLDialect.GetCreateTableSQL(const ATableName, ABody: string): string;
begin
  // PostgreSQL supports IF NOT EXISTS
  Result := Format('CREATE TABLE IF NOT EXISTS %s (%s);', [ATableName, ABody]);
end;

{ TFirebirdDialect }

function TFirebirdDialect.BooleanToSQL(AValue: Boolean): string;
begin
  // Firebird 3.0+ supports BOOLEAN type (TRUE/FALSE)
  if AValue then Result := 'TRUE' else Result := 'FALSE';
end;

function TFirebirdDialect.GeneratePaging(ASkip, ATake: Integer): string;
begin
  // Firebird 3.0+: OFFSET x ROWS FETCH NEXT y ROWS ONLY
  Result := Format('OFFSET %d ROWS FETCH NEXT %d ROWS ONLY', [ASkip, ATake]);
end;

function TFirebirdDialect.QuoteIdentifier(const AName: string): string;
begin
  Result := '"' + AName + '"';
end;

function TFirebirdDialect.GetColumnType(ATypeInfo: PTypeInfo; AIsAutoInc: Boolean): string;
begin
  if AIsAutoInc then
    Exit('INTEGER GENERATED BY DEFAULT AS IDENTITY'); // Firebird 3.0+

  case ATypeInfo.Kind of
    tkInteger: Result := 'INTEGER';
    tkInt64: Result := 'BIGINT';
    tkFloat: 
      begin
        if ATypeInfo = TypeInfo(Double) then Result := 'DOUBLE PRECISION'
        else if ATypeInfo = TypeInfo(Single) then Result := 'FLOAT'
        else if ATypeInfo = TypeInfo(Currency) then Result := 'DECIMAL(18,4)'
        else if ATypeInfo = TypeInfo(TDateTime) then Result := 'TIMESTAMP'
        else Result := 'DOUBLE PRECISION';
      end;
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString: Result := 'VARCHAR(255)';
    tkEnumeration:
      begin
        if ATypeInfo = TypeInfo(Boolean) then Result := 'BOOLEAN'
        else Result := 'INTEGER';
      end;
    tkRecord:
      begin
        if ATypeInfo = TypeInfo(TGUID) then Result := 'CHAR(36)' // No native UUID in FB3, FB4 has BINARY(16)
        else Result := 'VARCHAR(255)';
      end;
  else
    Result := 'VARCHAR(255)';
  end;
end;

function TFirebirdDialect.GetLastInsertIdSQL: string;
begin
  // Firebird usually requires RETURNING clause in INSERT
  // There is no safe global "last insert id" function without RETURNING
  Result := ''; 
end;

function TFirebirdDialect.GetCreateTableSQL(const ATableName, ABody: string): string;
begin
  // Firebird doesn't support IF NOT EXISTS in CREATE TABLE directly in all versions,
  // but we can use RECREATE TABLE or EXECUTE BLOCK.
  // For simplicity in ORM generation, we stick to standard CREATE TABLE.
  // User should handle existence check or use EnsureCreated carefully.
  Result := Format('CREATE TABLE %s (%s);', [ATableName, ABody]);
end;

{ TSQLServerDialect }

function TSQLServerDialect.BooleanToSQL(AValue: Boolean): string;
begin
  if AValue then Result := '1' else Result := '0';
end;

function TSQLServerDialect.GeneratePaging(ASkip, ATake: Integer): string;
begin
  // SQL Server 2012+: OFFSET x ROWS FETCH NEXT y ROWS ONLY
  // Requires ORDER BY clause in the query!
  Result := Format('OFFSET %d ROWS FETCH NEXT %d ROWS ONLY', [ASkip, ATake]);
end;

function TSQLServerDialect.QuoteIdentifier(const AName: string): string;
begin
  Result := '[' + AName + ']';
end;

function TSQLServerDialect.GetColumnType(ATypeInfo: PTypeInfo; AIsAutoInc: Boolean): string;
begin
  if AIsAutoInc then
    Exit('INT IDENTITY(1,1)');

  case ATypeInfo.Kind of
    tkInteger: Result := 'INT';
    tkInt64: Result := 'BIGINT';
    tkFloat: 
      begin
        if ATypeInfo = TypeInfo(Double) then Result := 'FLOAT'
        else if ATypeInfo = TypeInfo(Single) then Result := 'REAL'
        else if ATypeInfo = TypeInfo(Currency) then Result := 'MONEY'
        else if ATypeInfo = TypeInfo(TDateTime) then Result := 'DATETIME2'
        else Result := 'FLOAT';
      end;
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString: Result := 'NVARCHAR(255)';
    tkEnumeration:
      begin
        if ATypeInfo = TypeInfo(Boolean) then Result := 'BIT'
        else Result := 'INT';
      end;
    tkRecord:
      begin
        if ATypeInfo = TypeInfo(TGUID) then Result := 'UNIQUEIDENTIFIER'
        else Result := 'NVARCHAR(MAX)';
      end;
  else
    Result := 'NVARCHAR(MAX)';
  end;
end;

function TSQLServerDialect.GetLastInsertIdSQL: string;
begin
  Result := 'SELECT SCOPE_IDENTITY()';
end;

function TSQLServerDialect.GetCreateTableSQL(const ATableName, ABody: string): string;
begin
  // SQL Server doesn't support IF NOT EXISTS in CREATE TABLE syntax directly (needs IF OBJECT_ID...)
  // We keep it simple for now.
  Result := Format('CREATE TABLE %s (%s);', [ATableName, ABody]);
end;

{ TMySQLDialect }

function TMySQLDialect.BooleanToSQL(AValue: Boolean): string;
begin
  if AValue then Result := '1' else Result := '0';
end;

function TMySQLDialect.GeneratePaging(ASkip, ATake: Integer): string;
begin
  Result := Format('LIMIT %d OFFSET %d', [ATake, ASkip]);
end;

function TMySQLDialect.QuoteIdentifier(const AName: string): string;
begin
  Result := '`' + AName + '`';
end;

function TMySQLDialect.GetColumnType(ATypeInfo: PTypeInfo; AIsAutoInc: Boolean): string;
begin
  if AIsAutoInc then
    Exit('INT AUTO_INCREMENT');

  case ATypeInfo.Kind of
    tkInteger: Result := 'INT';
    tkInt64: Result := 'BIGINT';
    tkFloat: 
      begin
        if ATypeInfo = TypeInfo(Double) then Result := 'DOUBLE'
        else if ATypeInfo = TypeInfo(Single) then Result := 'FLOAT'
        else if ATypeInfo = TypeInfo(Currency) then Result := 'DECIMAL(15,2)'
        else if ATypeInfo = TypeInfo(TDateTime) then Result := 'DATETIME'
        else Result := 'DOUBLE';
      end;
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString: Result := 'VARCHAR(255)';
    tkEnumeration:
      begin
        if ATypeInfo = TypeInfo(Boolean) then Result := 'TINYINT(1)'
        else Result := 'INT';
      end;
    tkRecord:
      begin
        if ATypeInfo = TypeInfo(TGUID) then Result := 'CHAR(36)'
        else Result := 'TEXT';
      end;
  else
    Result := 'TEXT';
  end;
end;

function TMySQLDialect.GetLastInsertIdSQL: string;
begin
  Result := 'SELECT LAST_INSERT_ID()';
end;

function TMySQLDialect.GetCreateTableSQL(const ATableName, ABody: string): string;
begin
  Result := Format('CREATE TABLE IF NOT EXISTS %s (%s);', [ATableName, ABody]);
end;


{ TOracleDialect }

function TOracleDialect.BooleanToSQL(AValue: Boolean): string;
begin
  // Oracle doesn't have BOOLEAN type in SQL (only PL/SQL).
  // Standard practice is NUMBER(1) check (0/1) or CHAR(1) ('Y'/'N').
  // We use 1/0.
  if AValue then Result := '1' else Result := '0';
end;

function TOracleDialect.GeneratePaging(ASkip, ATake: Integer): string;
begin
  // Oracle 12c+: OFFSET x ROWS FETCH NEXT y ROWS ONLY
  Result := Format('OFFSET %d ROWS FETCH NEXT %d ROWS ONLY', [ASkip, ATake]);
end;

function TOracleDialect.QuoteIdentifier(const AName: string): string;
begin
  // Oracle uses double quotes for case sensitivity.
  // Unquoted identifiers are UPPERCASE.
  Result := '"' + AName + '"';
end;

function TOracleDialect.GetColumnType(ATypeInfo: PTypeInfo; AIsAutoInc: Boolean): string;
begin
  if AIsAutoInc then
    Exit('NUMBER(10) GENERATED BY DEFAULT AS IDENTITY'); // Oracle 12c+

  case ATypeInfo.Kind of
    tkInteger: Result := 'NUMBER(10)';
    tkInt64: Result := 'NUMBER(19)';
    tkFloat: 
      begin
        if ATypeInfo = TypeInfo(Double) then Result := 'BINARY_DOUBLE'
        else if ATypeInfo = TypeInfo(Single) then Result := 'BINARY_FLOAT'
        else if ATypeInfo = TypeInfo(Currency) then Result := 'NUMBER(19,4)'
        else if ATypeInfo = TypeInfo(TDateTime) then Result := 'TIMESTAMP'
        else Result := 'BINARY_DOUBLE';
      end;
    tkChar, tkString, tkWChar, tkLString, tkWString, tkUString: Result := 'VARCHAR2(255)';
    tkEnumeration:
      begin
        if ATypeInfo = TypeInfo(Boolean) then Result := 'NUMBER(1)'
        else Result := 'NUMBER(10)';
      end;
    tkRecord:
      begin
        if ATypeInfo = TypeInfo(TGUID) then Result := 'VARCHAR2(36)' // Or RAW(16)
        else Result := 'VARCHAR2(4000)';
      end;
  else
    Result := 'VARCHAR2(255)';
  end;
end;

function TOracleDialect.GetLastInsertIdSQL: string;
begin
  // Oracle requires RETURNING INTO clause.
  // No global scalar function like SCOPE_IDENTITY available easily without PL/SQL context.
  Result := ''; 
end;

function TOracleDialect.GetCreateTableSQL(const ATableName, ABody: string): string;
begin
  // Oracle doesn't support IF NOT EXISTS.
  Result := Format('CREATE TABLE %s (%s);', [ATableName, ABody]);
end;

end.
