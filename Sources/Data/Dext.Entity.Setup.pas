unit Dext.Entity.Setup;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.Dialects,
  Dext.Entity.Drivers.FireDAC,
  Dext.Entity.Drivers.FireDAC.Manager,
  FireDAC.Comp.Client;

type
  /// <summary>
  ///   Configuration options for a DbContext.
  /// </summary>
  TDbContextOptions = class
  private
    FDriverName: string;
    FConnectionString: string;
    FConnectionDefName: string;
    FConnectionDefString: string;
    FParams: TDictionary<string, string>;
    FPooling: Boolean;
    FPoolMax: Integer;
    FOptimizations: TFireDACOptimizations; // Connect Optimizations
    FDialect: ISQLDialect;
    FCustomConnection: IDbConnection;
  public
    constructor Create;
    destructor Destroy; override;

    property DriverName: string read FDriverName write FDriverName;
    property ConnectionString: string read FConnectionString write FConnectionString;
    property ConnectionDefName: string read FConnectionDefName write FConnectionDefName;
    property ConnectionDefString: string read FConnectionDefString write FConnectionDefString;
    property Params: TDictionary<string, string> read FParams;
    property Pooling: Boolean read FPooling write FPooling;
    property PoolMax: Integer read FPoolMax write FPoolMax;
    property Optimizations: TFireDACOptimizations read FOptimizations write FOptimizations;
    property Dialect: ISQLDialect read FDialect write FDialect;
    property CustomConnection: IDbConnection read FCustomConnection write FCustomConnection;

    function BuildConnection: IDbConnection;
    function BuildDialect: ISQLDialect;

    // Fluent Helpers
    function UseSQLite(const DatabaseFile: string): TDbContextOptions;
    function UseDriver(const ADriverName: string): TDbContextOptions;
    function UseConnectionDef(const ADefName: string): TDbContextOptions;
    function WithPooling(Enable: Boolean = True; MaxSize: Integer = 50): TDbContextOptions;
    function ConfigureOptimizations(AOpts: TFireDACOptimizations): TDbContextOptions;
    function UseCustomDialect(const ADialect: ISQLDialect): TDbContextOptions;
  end;

  /// <summary>
  ///   Builder for configuring DbContext options.
  /// </summary>
  TDbContextOptionsBuilder = class
  private
    FOptions: TDbContextOptions;
  public
    constructor Create(Options: TDbContextOptions);
    function UseSQLite(const DatabaseFile: string): TDbContextOptionsBuilder;
    function UseDriver(const ADriverName: string): TDbContextOptionsBuilder;
  end;

implementation

{ TDbContextOptions }

constructor TDbContextOptions.Create;
begin
  FParams := TDictionary<string, string>.Create;
  FPooling := False;
  FPoolMax := 50;
  // Default legacy optimization behavior (Matches original hardcoded logic)
  FOptimizations := [optDisableMacros, optDisableEscapes, optDirectExecute];
end;

destructor TDbContextOptions.Destroy;
begin
  FParams.Free;
  inherited;
end;

function TDbContextOptions.UseDriver(const ADriverName: string): TDbContextOptions;
begin
  FDriverName := ADriverName;
  FConnectionDefName := '';
  Result := Self;
end;

function TDbContextOptions.UseConnectionDef(const ADefName: string): TDbContextOptions;
begin
  FConnectionDefName := ADefName;
  FDriverName := '';
  FConnectionString := '';
  Result := Self;
end;

function TDbContextOptions.UseSQLite(const DatabaseFile: string): TDbContextOptions;
begin
  FDriverName := 'SQLite';
  FConnectionDefName := '';
  FParams.AddOrSetValue('Database', DatabaseFile);
  FParams.AddOrSetValue('LockingMode', 'Normal');
  // Dialect is auto-detected by TDbContext from the connection driver
  Result := Self;
end;

function TDbContextOptions.WithPooling(Enable: Boolean; MaxSize: Integer): TDbContextOptions;
begin
  FPooling := Enable;
  FPoolMax := MaxSize;
  Result := Self;
end;

function TDbContextOptions.ConfigureOptimizations(AOpts: TFireDACOptimizations): TDbContextOptions;
begin
  FOptimizations := AOpts;
  Result := Self;
end;

function TDbContextOptions.UseCustomDialect(const ADialect: ISQLDialect): TDbContextOptions;
begin
  FDialect := ADialect;
  Result := Self;
end;

function TDbContextOptions.BuildConnection: IDbConnection;
var
  FDConn: TFDConnection;
  DefName: string;
begin
  if FCustomConnection <> nil then
    Exit(FCustomConnection);

  FDConn := TFDConnection.Create(nil);
  try
    if FConnectionDefName <> '' then
    begin
      FDConn.ConnectionDefName := FConnectionDefName;
    end
    else if FDriverName <> '' then
    begin
      if FPooling then
      begin
        var SL := TStringList.Create;
        try
          for var Pair in FParams do
            SL.Values[Pair.Key] := Pair.Value;
          
          DefName := TDextFireDACManager.Instance.RegisterConnectionDef(FDriverName, TStrings(SL), FPoolMax);
          FDConn.ConnectionDefName := DefName;
        finally
          SL.Free;
        end;
      end
      else
      begin
        FDConn.DriverName := FDriverName;
        for var Pair in FParams do
          FDConn.Params.Values[Pair.Key] := Pair.Value;
      end;
    end;
    
    // Resource options (Applying configured optimizations)
    TDextFireDACManager.Instance.ApplyResourceOptions(FDConn, FOptimizations);

    Result := TFireDACConnection.Create(FDConn, True);
  except
    FDConn.Free;
    raise;
  end;
end;

function TDbContextOptions.BuildDialect: ISQLDialect;
begin
  Result := FDialect;
end;

{ TDbContextOptionsBuilder }

constructor TDbContextOptionsBuilder.Create(Options: TDbContextOptions);
begin
  FOptions := Options;
end;

function TDbContextOptionsBuilder.UseDriver(const ADriverName: string): TDbContextOptionsBuilder;
begin
  FOptions.UseDriver(ADriverName);
  Result := Self;
end;

function TDbContextOptionsBuilder.UseSQLite(const DatabaseFile: string): TDbContextOptionsBuilder;
begin
  FOptions.UseSQLite(DatabaseFile);
  Result := Self;
end;

end.
