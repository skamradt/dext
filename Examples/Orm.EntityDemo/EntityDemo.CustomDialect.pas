unit EntityDemo.CustomDialect;

interface

uses
  Data.DB,
  Dext.Entity.Dialects,
  System.SysUtils;

type
  /// <summary>
  ///   A custom SQLite dialect that forces upper case on all identifiers.
  ///   This demonstrates how to override part of the SQL generation logic.
  /// </summary>
  TCustomSQLiteDialect = class(TSQLiteDialect)
  public
    function QuoteIdentifier(const AName: string): string; override;
  end;

implementation

{ TCustomSQLiteDialect }

function TCustomSQLiteDialect.QuoteIdentifier(const AName: string): string;
begin
  // Custom logic: Force upper case and use brackets instead of double quotes
  Result := '[' + AName.ToUpper + ']';
end;

end.
