{***************************************************************************}
{           Dext Framework                                                  }
{           Copyright (C) 2025 Cesar Romero & Dext Contributors             }
{***************************************************************************}
unit Dext.Logging.Global;

interface

uses
  System.SysUtils,
  System.Classes,
  Dext.Logging,
  Dext.Logging.Async,
  Dext.Logging.Sinks,
  Dext.Logging.Sinks.Sidecar;

type
  /// <summary>
  ///   Provides a static entry point for application-wide logging.
  /// </summary>
  Log = class
  private
    class var FLogger: ILogger;
    class var FFactory: TAsyncLoggerFactory;
    class function GetLogger: ILogger; static;
  public
    class constructor Create;
    class destructor Destroy;
    
    /// <summary>
    ///   Initializes the global logger with default sinks (Console, File).
    ///   Calling this is optional; if not called, default config is used on first access.
    /// </summary>
    class procedure Initialize;
    
    /// <summary>
    ///   Access the global logger instance.
    /// </summary>
    class property Logger: ILogger read GetLogger;
    
    // Convenience proxies (optional, but requested "Static Registry")
    
    class procedure Trace(const AMessage: string); overload;
    class procedure Trace(const AMessage: string; const AArgs: array of const); overload;
    
    class procedure Debug(const AMessage: string); overload;
    class procedure Debug(const AMessage: string; const AArgs: array of const); overload;
    
    class procedure Info(const AMessage: string); overload;
    class procedure Info(const AMessage: string; const AArgs: array of const); overload;
    
    class procedure Warn(const AMessage: string); overload;
    class procedure Warn(const AMessage: string; const AArgs: array of const); overload;
    
    class procedure Error(const AMessage: string); overload;
    class procedure Error(const AMessage: string; const AArgs: array of const); overload;
    
    class procedure Critical(const AMessage: string); overload;
    class procedure Critical(const AMessage: string; const AArgs: array of const); overload;
    // ... add more as needed
  end;

implementation

{ Log }

class constructor Log.Create;
begin
  FLogger := nil;
  FFactory := nil;
end;

class destructor Log.Destroy;
begin
  FLogger := nil;
  if FFactory <> nil then
  begin
    FFactory.Dispose;
    FFactory.Free;
  end;
end;

class procedure Log.Initialize;
begin
  if FFactory <> nil then Exit; // Already initialized

  FFactory := TAsyncLoggerFactory.Create;
  
  // Default Sinks
  FFactory.AddSink(TConsoleSink.Create);
  // Optional: Add File Sink by default? Or let the user configure it?
  // User request: "registro padrão de Logger multhtread para facilitar a adoção"
  // Let's add a file sink in the current directory or temp.
  // Ideally, based on config. But for "Zero Config", a reasonable default is needed.

  
  // Sidecar Sink (Fire and Forget)
  // Auto-discovery via Environment Variable (set by Sidecar or execution context)
  var Port := 3030;
  var PortStr := GetEnvironmentVariable('DEXT_SIDECAR_PORT');
  if PortStr <> '' then
    Port := StrToIntDef(PortStr, 3030);
    
  FFactory.AddSink(TSidecarSink.Create('http://localhost:' + Port.ToString));
  
  // Let's Create the Logger instance
  FLogger := FFactory.CreateLogger('App');
end;

class function Log.GetLogger: ILogger;
begin
  if FLogger = nil then
    Initialize;
  Result := FLogger;
end;

class procedure Log.Trace(const AMessage: string);
begin
  Logger.Trace(AMessage, []);
end;

class procedure Log.Trace(const AMessage: string; const AArgs: array of const);
begin
  Logger.Trace(AMessage, AArgs);
end;

class procedure Log.Debug(const AMessage: string);
begin
  Logger.Debug(AMessage, []);
end;

class procedure Log.Debug(const AMessage: string; const AArgs: array of const);
begin
  Logger.Debug(AMessage, AArgs);
end;

class procedure Log.Info(const AMessage: string);
begin
  Logger.Info(AMessage, []);
end;

class procedure Log.Info(const AMessage: string; const AArgs: array of const);
begin
  Logger.Info(AMessage, AArgs);
end;

class procedure Log.Warn(const AMessage: string);
begin
  Logger.Warn(AMessage, []);
end;

class procedure Log.Warn(const AMessage: string; const AArgs: array of const);
begin
  Logger.Warn(AMessage, AArgs);
end;

class procedure Log.Error(const AMessage: string);
begin
  Logger.Error(AMessage, []);
end;

class procedure Log.Error(const AMessage: string; const AArgs: array of const);
begin
  Logger.Error(AMessage, AArgs);
end;

class procedure Log.Critical(const AMessage: string);
begin
  Logger.Critical(AMessage, []);
end;

class procedure Log.Critical(const AMessage: string; const AArgs: array of const);
begin
  Logger.Critical(AMessage, AArgs);
end;

end.
