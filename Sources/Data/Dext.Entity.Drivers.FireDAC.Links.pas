unit Dext.Entity.Drivers.FireDAC.Links;

interface

{$I Dext.inc}

uses
  // Standard Drivers (Commonly available)
  {$IFDEF DEXT_ENABLE_DB_SQLITE} FireDAC.Phys.SQLite, {$ENDIF}
  {$IFDEF DEXT_ENABLE_DB_POSTGRES} FireDAC.Phys.PG, {$ENDIF}
  {$IFDEF DEXT_ENABLE_DB_MYSQL} FireDAC.Phys.MySQL, {$ENDIF}
  {$IFDEF DEXT_ENABLE_DB_MSSQL} FireDAC.Phys.MSSQL, {$ENDIF}
  {$IFDEF DEXT_ENABLE_DB_ORACLE} FireDAC.Phys.Oracle, {$ENDIF}
  {$IFDEF DEXT_ENABLE_DB_FIREBIRD} FireDAC.Phys.FB, {$ENDIF}
  {$IFDEF DEXT_ENABLE_DB_ODBC} FireDAC.Phys.ODBC, {$ENDIF}
  
  // Additional Enterprise Drivers
  {$IFDEF DEXT_ENABLE_DB_ADS} FireDAC.Phys.ADS, {$ENDIF}
  {$IFDEF DEXT_ENABLE_DB_ASA} FireDAC.Phys.ASA, {$ENDIF}
  {$IFDEF DEXT_ENABLE_DB_DB2} FireDAC.Phys.DB2, {$ENDIF}
  {$IFDEF DEXT_ENABLE_DB_DS} FireDAC.Phys.DS, {$ENDIF} // DataSnap
  {$IFDEF DEXT_ENABLE_DB_IB} FireDAC.Phys.IB, {$ENDIF} // InterBase
  {$IFDEF DEXT_ENABLE_DB_INFX} FireDAC.Phys.Infx, {$ENDIF} // Informix
  {$IFDEF DEXT_ENABLE_DB_MONGODB} FireDAC.Phys.MongoDB, {$ENDIF}
  {$IFDEF DEXT_ENABLE_DB_MSACC} FireDAC.Phys.MSAcc, {$ENDIF}
  {$IFDEF DEXT_ENABLE_DB_TERADATA} FireDAC.Phys.TData, {$ENDIF} // Teradata (TData)
  
  // Catch-all or other specific drivers can be added here
  System.SysUtils;

implementation

end.
