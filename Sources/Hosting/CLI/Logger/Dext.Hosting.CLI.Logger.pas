unit Dext.Hosting.CLI.Logger;

interface

uses
  System.SysUtils,
  System.Rtti,
  Dext.Logging,
  Dext.Web.Hubs.Interfaces,
  Dext.Web.Hubs.Extensions, // Static access to HubContext
  Dext.Utils;

type
  TConsoleHubLogger = class(TAbstractLogger)
  private
    FCategory: string;
  protected
    procedure Log(ALevel: TLogLevel; const AMessage: string; const AArgs: array of const); overload; override;
    procedure Log(ALevel: TLogLevel; const AException: Exception; const AMessage: string; const AArgs: array of const); overload; override;
    function IsEnabled(ALevel: TLogLevel): Boolean; override;
    
    function BeginScope(const AMessage: string; const AArgs: array of const): IDisposable; overload; override;
    function BeginScope(const AState: TObject): IDisposable; overload; override;
  public
    constructor Create(const Category: string);
  end;

  TConsoleHubLoggerProvider = class(TInterfacedObject, ILoggerProvider)
  public
    function CreateLogger(const ACategoryName: string): ILogger;
    procedure Dispose;
  end;

implementation

{ TConsoleHubLogger }

constructor TConsoleHubLogger.Create(const Category: string);
begin
  inherited Create;
  FCategory := Category;
end;

function TConsoleHubLogger.IsEnabled(ALevel: TLogLevel): Boolean;
begin
  Result := True;
end;

function TConsoleHubLogger.BeginScope(const AMessage: string; const AArgs: array of const): IDisposable;
begin
  // Scopes not supported in CLI/Hub logger yet
  Result := TNullDisposable.Create;
end;

function TConsoleHubLogger.BeginScope(const AState: TObject): IDisposable;
begin
  Result := TNullDisposable.Create;
end;

procedure TConsoleHubLogger.Log(ALevel: TLogLevel; const AMessage: string; const AArgs: array of const);
var
  Msg, LevelStr: string;
  HubArgs: TArray<TValue>;
  HubContext: IHubContext;
begin
  Msg := Format(AMessage, AArgs);
  
  // 1. Console Output
  SafeWriteLn(Msg);
  
  // 2. Hub Output (Try to get context, if fails, it means Hubs not yet initialized or failed)
  try
    // Lazy access via global extension helper
    HubContext := THubExtensions.GetHubContext;
  except
    HubContext := nil;
  end;
  
  if HubContext <> nil then
  begin
    case ALevel of
      TLogLevel.Trace: LevelStr := 'debug';
      TLogLevel.Debug: LevelStr := 'debug';
      TLogLevel.Information: LevelStr := 'info';
      TLogLevel.Warning: LevelStr := 'warn';
      TLogLevel.Error: LevelStr := 'error';
      TLogLevel.Critical: LevelStr := 'error';
    else
      LevelStr := 'info';
    end;
    
    SetLength(HubArgs, 2);
    HubArgs[0] := TValue.From<string>(LevelStr);
    HubArgs[1] := TValue.From<string>(Msg);

    try
      HubContext.Clients.All.SendAsync('LogMessage', HubArgs);
    except
      // Suppress logging errors
    end;
  end;
end;

procedure TConsoleHubLogger.Log(ALevel: TLogLevel; const AException: Exception; const AMessage: string; const AArgs: array of const);
var
  Msg: string;
begin
  Msg := Format(AMessage, AArgs);
  if AException <> nil then
    Msg := Msg + ' Exception: ' + AException.Message;
    
  Log(ALevel, Msg, []);
end;

{ TConsoleHubLoggerProvider }

function TConsoleHubLoggerProvider.CreateLogger(const ACategoryName: string): ILogger;
begin
  Result := TConsoleHubLogger.Create(ACategoryName);
end;

procedure TConsoleHubLoggerProvider.Dispose;
begin
  // Nothing to dispose
end;

end.
