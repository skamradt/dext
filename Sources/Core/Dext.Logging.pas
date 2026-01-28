{***************************************************************************}
{                                                                           }
{           Dext Framework                                                  }
{                                                                           }
{           Copyright (C) 2025 Cesar Romero & Dext Contributors             }
{                                                                           }
{           Licensed under the Apache License, Version 2.0 (the "License"); }
{           you may not use this file except in compliance with the License.}
{           You may obtain a copy of the License at                         }
{                                                                           }
{               http://www.apache.org/licenses/LICENSE-2.0                  }
{                                                                           }
{           Unless required by applicable law or agreed to in writing,      }
{           software distributed under the License is distributed on an     }
{           "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    }
{           either express or implied. See the License for the specific     }
{           language governing permissions and limitations under the        }
{           License.                                                        }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Author:  Cesar Romero                                                    }
{  Created: 2025-12-08                                                      }
{                                                                           }
{***************************************************************************}
unit Dext.Logging;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Dext.Types.UUID;

type
  /// <summary>
  ///   Defines logging severity levels.
  /// </summary>
  TLogLevel = (
    Trace = 0,
    Debug = 1,
    Information = 2,
    Warning = 3,
    Error = 4,
    Critical = 5,
    None = 6
  );
  {$M+}
  /// <summary>
  ///   Defines a mechanism for releasing resources.
  /// </summary>
  IDisposable = interface
    ['{00000000-0000-0000-C000-000000000046}']
    procedure Dispose;
  end;

  /// <summary>
  ///   Represents a type used to perform logging.
  /// </summary>
  ILogger = interface
    ['{A1B2C3D4-E5F6-7890-1234-567890ABCDEF}']
    procedure Log(ALevel: TLogLevel; const AMessage: string; const AArgs: array of const); overload;
    procedure Log(ALevel: TLogLevel; const AException: Exception; const AMessage: string; const AArgs: array of const); overload;
    
    function IsEnabled(ALevel: TLogLevel): Boolean;
    
    function BeginScope(const AMessage: string; const AArgs: array of const): IDisposable; overload;
    function BeginScope(const AState: TObject): IDisposable; overload;

    // Short method names (preferred)
    procedure Trace(const AMessage: string); overload;
    procedure Trace(const AMessage: string; const AArgs: array of const); overload;
    
    procedure Debug(const AMessage: string); overload;
    procedure Debug(const AMessage: string; const AArgs: array of const); overload;
    
    procedure Info(const AMessage: string); overload;
    procedure Info(const AMessage: string; const AArgs: array of const); overload;
    
    procedure Warn(const AMessage: string); overload;
    procedure Warn(const AMessage: string; const AArgs: array of const); overload;
    
    procedure Error(const AMessage: string); overload;
    procedure Error(const AMessage: string; const AArgs: array of const); overload;
    procedure Error(const AException: Exception; const AMessage: string; const AArgs: array of const); overload;
    
    procedure Critical(const AMessage: string); overload;
    procedure Critical(const AMessage: string; const AArgs: array of const); overload;
    procedure Critical(const AException: Exception; const AMessage: string; const AArgs: array of const); overload;
    
    // Legacy method names (for backwards compatibility)
    procedure LogTrace(const AMessage: string); overload;
    procedure LogTrace(const AMessage: string; const AArgs: array of const); overload;
    
    procedure LogDebug(const AMessage: string); overload;
    procedure LogDebug(const AMessage: string; const AArgs: array of const); overload;
    
    procedure LogInformation(const AMessage: string); overload;
    procedure LogInformation(const AMessage: string; const AArgs: array of const); overload;
    
    procedure LogWarning(const AMessage: string); overload;
    procedure LogWarning(const AMessage: string; const AArgs: array of const); overload;
    
    procedure LogError(const AMessage: string); overload;
    procedure LogError(const AMessage: string; const AArgs: array of const); overload;
    procedure LogError(const AException: Exception; const AMessage: string; const AArgs: array of const); overload;
    
    procedure LogCritical(const AMessage: string); overload;
    procedure LogCritical(const AMessage: string; const AArgs: array of const); overload;
    procedure LogCritical(const AException: Exception; const AMessage: string; const AArgs: array of const); overload;
  end;

  /// <summary>
  ///   Represents a type that can create instances of ILogger.
  /// </summary>
  ILoggerProvider = interface
    ['{B2C3D4E5-F678-9012-3456-7890ABCDEF12}']
    function CreateLogger(const ACategoryName: string): ILogger;
    procedure Dispose;
  end;

  /// <summary>
  ///   Represents a type used to configure the logging system and create instances of ILogger.
  /// </summary>
  ILoggerFactory = interface
    ['{C3D4E5F6-7890-1234-5678-90ABCDEF1234}']
    function CreateLogger(const ACategoryName: string): ILogger;
    procedure AddProvider(const AProvider: ILoggerProvider);
    procedure Dispose;
  end;
{$M-}

  /// <summary>
  ///   Base class for ILogger implementations.
  ///   Implements convenience methods by delegating to the abstract Log method.
  /// </summary>
  TAbstractLogger = class(TInterfacedObject, ILogger)
  protected
    procedure Log(ALevel: TLogLevel; const AMessage: string; const AArgs: array of const); overload; virtual; abstract;
    procedure Log(ALevel: TLogLevel; const AException: Exception; const AMessage: string; const AArgs: array of const); overload; virtual; abstract;

    function IsEnabled(ALevel: TLogLevel): Boolean; virtual; abstract;
    function BeginScope(const AMessage: string; const AArgs: array of const): IDisposable; overload; virtual; abstract;
    function BeginScope(const AState: TObject): IDisposable; overload; virtual; abstract;
  public
    // Short method names (preferred)
    procedure Trace(const AMessage: string); overload;
    procedure Trace(const AMessage: string; const AArgs: array of const); overload;
    
    procedure Debug(const AMessage: string); overload;
    procedure Debug(const AMessage: string; const AArgs: array of const); overload;
    
    procedure Info(const AMessage: string); overload;
    procedure Info(const AMessage: string; const AArgs: array of const); overload;
    
    procedure Warn(const AMessage: string); overload;
    procedure Warn(const AMessage: string; const AArgs: array of const); overload;
    
    procedure Error(const AMessage: string); overload;
    procedure Error(const AMessage: string; const AArgs: array of const); overload;
    procedure Error(const AException: Exception; const AMessage: string; const AArgs: array of const); overload;
    
    procedure Critical(const AMessage: string); overload;
    procedure Critical(const AMessage: string; const AArgs: array of const); overload;
    procedure Critical(const AException: Exception; const AMessage: string; const AArgs: array of const); overload;
    
    // Legacy method names (for backwards compatibility)
    procedure LogTrace(const AMessage: string); overload;
    procedure LogTrace(const AMessage: string; const AArgs: array of const); overload;
    
    procedure LogDebug(const AMessage: string); overload;
    procedure LogDebug(const AMessage: string; const AArgs: array of const); overload;
    
    procedure LogInformation(const AMessage: string); overload;
    procedure LogInformation(const AMessage: string; const AArgs: array of const); overload;
    
    procedure LogWarning(const AMessage: string); overload;
    procedure LogWarning(const AMessage: string; const AArgs: array of const); overload;
    
    procedure LogError(const AMessage: string); overload;
    procedure LogError(const AMessage: string; const AArgs: array of const); overload;
    procedure LogError(const AException: Exception; const AMessage: string; const AArgs: array of const); overload;
    
    procedure LogCritical(const AMessage: string); overload;
    procedure LogCritical(const AMessage: string; const AArgs: array of const); overload;
    procedure LogCritical(const AException: Exception; const AMessage: string; const AArgs: array of const); overload;
  end;

  /// <summary>
  ///   Aggregates multiple loggers into one.
  /// </summary>
  TAggregateLogger = class(TAbstractLogger)
  private
    FLoggers: TArray<ILogger>;
    FMinimumLevel: TLogLevel;
  public
    constructor Create(const ALoggers: TArray<ILogger>; AMinimumLevel: TLogLevel);
    
    procedure Log(ALevel: TLogLevel; const AMessage: string; const AArgs: array of const); override;
    procedure Log(ALevel: TLogLevel; const AException: Exception; const AMessage: string; const AArgs: array of const); override;
    
    function IsEnabled(ALevel: TLogLevel): Boolean; override;
    
    function BeginScope(const AMessage: string; const AArgs: array of const): IDisposable; override;
    function BeginScope(const AState: TObject): IDisposable; override;
  end;

  /// <summary>
  ///   Helper for aggregating multiple IDisposable instances.
  /// </summary>
  TCompositeDisposable = class(TInterfacedObject, IDisposable)
  private
    FDisposables: TArray<IDisposable>;
  public
    constructor Create(const ADisposables: TArray<IDisposable>);
    procedure Dispose;
  end;
  
  /// <summary>
  ///   No-op disposable for when no logger is enabled.
  /// </summary>
  TNullDisposable = class(TInterfacedObject, IDisposable)
  public
    procedure Dispose;
  end;

  /// <summary>
  ///   Default implementation of ILoggerFactory.
  ///   Uses TInterfacedObject for ARC-based lifecycle management.
  /// </summary>
  TLoggerFactory = class(TInterfacedObject, ILoggerFactory)
  private
    FProviders: TList<ILoggerProvider>;
    FLock: TObject;
    FMinimumLevel: TLogLevel;
  public
    constructor Create;
    destructor Destroy; override;

    function CreateLogger(const ACategoryName: string): ILogger;
    function CreateLoggerInstance(const ACategoryName: string): TAggregateLogger;
    procedure SetMinimumLevel(ALevel: TLogLevel);
    procedure AddProvider(const AProvider: ILoggerProvider);
    procedure Dispose;
  end;

implementation

{ TLoggerFactory }

constructor TLoggerFactory.Create;
begin
  inherited;
  FProviders := TList<ILoggerProvider>.Create;
  FLock := TObject.Create;
  FMinimumLevel := TLogLevel.Information; // Default
end;

destructor TLoggerFactory.Destroy;
var
  LProvider: ILoggerProvider;
begin
  TMonitor.Enter(FLock);
  try
    for LProvider in FProviders do
    begin
      LProvider.Dispose;
    end;
    FProviders.Free;
  finally
    TMonitor.Exit(FLock);
    FLock.Free;
  end;
  inherited;
end;

procedure TLoggerFactory.AddProvider(const AProvider: ILoggerProvider);
begin
  TMonitor.Enter(FLock);
  try
    FProviders.Add(AProvider);
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TLoggerFactory.SetMinimumLevel(ALevel: TLogLevel);
begin
  FMinimumLevel := ALevel;
end;

procedure TLoggerFactory.Dispose;
begin
  // Clean up providers?
  // For now, allow Destroy to handle it, or explicit cleanup if needed.
end;

function TLoggerFactory.CreateLogger(const ACategoryName: string): ILogger;
var
  LLoggers: TArray<ILogger>;
  i: Integer;
begin
  TMonitor.Enter(FLock);
  try
    SetLength(LLoggers, FProviders.Count);
    for i := 0 to FProviders.Count - 1 do
    begin
      LLoggers[i] := FProviders[i].CreateLogger(ACategoryName);
    end;
  finally
    TMonitor.Exit(FLock);
  end;
  
  Result := TAggregateLogger.Create(LLoggers, FMinimumLevel);
end;

function TLoggerFactory.CreateLoggerInstance(const ACategoryName: string): TAggregateLogger;
var
  LLoggers: TArray<ILogger>;
  i: Integer;
begin
  TMonitor.Enter(FLock);
  try
    SetLength(LLoggers, FProviders.Count);
    for i := 0 to FProviders.Count - 1 do
    begin
      LLoggers[i] := FProviders[i].CreateLogger(ACategoryName);
    end;
  finally
    TMonitor.Exit(FLock);
  end;
  
  Result := TAggregateLogger.Create(LLoggers, FMinimumLevel);
end;

{ TAbstractLogger }

// Short method implementations (preferred)

procedure TAbstractLogger.Trace(const AMessage: string);
begin
  Trace(AMessage, []);
end;

procedure TAbstractLogger.Trace(const AMessage: string; const AArgs: array of const);
begin
  Log(TLogLevel.Trace, AMessage, AArgs);
end;

procedure TAbstractLogger.Debug(const AMessage: string);
begin
  Debug(AMessage, []);
end;

procedure TAbstractLogger.Debug(const AMessage: string; const AArgs: array of const);
begin
  Log(TLogLevel.Debug, AMessage, AArgs);
end;

procedure TAbstractLogger.Info(const AMessage: string);
begin
  Info(AMessage, []);
end;

procedure TAbstractLogger.Info(const AMessage: string; const AArgs: array of const);
begin
  Log(TLogLevel.Information, AMessage, AArgs);
end;

procedure TAbstractLogger.Warn(const AMessage: string);
begin
  Warn(AMessage, []);
end;

procedure TAbstractLogger.Warn(const AMessage: string; const AArgs: array of const);
begin
  Log(TLogLevel.Warning, AMessage, AArgs);
end;

procedure TAbstractLogger.Error(const AMessage: string);
begin
  Error(AMessage, []);
end;

procedure TAbstractLogger.Error(const AMessage: string; const AArgs: array of const);
begin
  Log(TLogLevel.Error, AMessage, AArgs);
end;

procedure TAbstractLogger.Error(const AException: Exception; const AMessage: string; const AArgs: array of const);
begin
  Log(TLogLevel.Error, AException, AMessage, AArgs);
end;

procedure TAbstractLogger.Critical(const AMessage: string);
begin
  Critical(AMessage, []);
end;

procedure TAbstractLogger.Critical(const AMessage: string; const AArgs: array of const);
begin
  Log(TLogLevel.Critical, AMessage, AArgs);
end;

procedure TAbstractLogger.Critical(const AException: Exception; const AMessage: string; const AArgs: array of const);
begin
  Log(TLogLevel.Critical, AException, AMessage, AArgs);
end;

// Legacy method implementations (for backwards compatibility)

procedure TAbstractLogger.LogTrace(const AMessage: string);
begin
  Trace(AMessage);
end;

procedure TAbstractLogger.LogTrace(const AMessage: string; const AArgs: array of const);
begin
  Trace(AMessage, AArgs);
end;

procedure TAbstractLogger.LogDebug(const AMessage: string);
begin
  Debug(AMessage);
end;

procedure TAbstractLogger.LogDebug(const AMessage: string; const AArgs: array of const);
begin
  Debug(AMessage, AArgs);
end;

procedure TAbstractLogger.LogInformation(const AMessage: string);
begin
  Info(AMessage);
end;

procedure TAbstractLogger.LogInformation(const AMessage: string; const AArgs: array of const);
begin
  Info(AMessage, AArgs);
end;

procedure TAbstractLogger.LogWarning(const AMessage: string);
begin
  Warn(AMessage);
end;

procedure TAbstractLogger.LogWarning(const AMessage: string; const AArgs: array of const);
begin
  Warn(AMessage, AArgs);
end;

procedure TAbstractLogger.LogError(const AMessage: string);
begin
  Error(AMessage);
end;

procedure TAbstractLogger.LogError(const AMessage: string; const AArgs: array of const);
begin
  Error(AMessage, AArgs);
end;

procedure TAbstractLogger.LogError(const AException: Exception; const AMessage: string; const AArgs: array of const);
begin
  Error(AException, AMessage, AArgs);
end;

procedure TAbstractLogger.LogCritical(const AMessage: string);
begin
  Critical(AMessage);
end;

procedure TAbstractLogger.LogCritical(const AMessage: string; const AArgs: array of const);
begin
  Critical(AMessage, AArgs);
end;

procedure TAbstractLogger.LogCritical(const AException: Exception; const AMessage: string; const AArgs: array of const);
begin
  Critical(AException, AMessage, AArgs);
end;

{ TAggregateLogger }

constructor TAggregateLogger.Create(const ALoggers: TArray<ILogger>; AMinimumLevel: TLogLevel);
begin
  inherited Create;
  FLoggers := ALoggers;
  FMinimumLevel := AMinimumLevel;
end;

function TAggregateLogger.IsEnabled(ALevel: TLogLevel): Boolean;
var
  LLogger: ILogger;
begin
  if ALevel < FMinimumLevel then Exit(False);

  Result := False;
  for LLogger in FLoggers do
  begin
    if LLogger.IsEnabled(ALevel) then
      Exit(True);
  end;
end;

procedure TAggregateLogger.Log(ALevel: TLogLevel; const AMessage: string; const AArgs: array of const);
var
  LLogger: ILogger;
begin
  if ALevel < FMinimumLevel then Exit;

  for LLogger in FLoggers do
  begin
    LLogger.Log(ALevel, AMessage, AArgs);
  end;
end;

procedure TAggregateLogger.Log(ALevel: TLogLevel; const AException: Exception; const AMessage: string; const AArgs: array of const);
var
  LLogger: ILogger;
begin
  if ALevel < FMinimumLevel then Exit;

  for LLogger in FLoggers do
  begin
    LLogger.Log(ALevel, AException, AMessage, AArgs);
  end;
end;

function TAggregateLogger.BeginScope(const AMessage: string; const AArgs: array of const): IDisposable;
var
  LDisposables: TArray<IDisposable>;
  i: Integer;
begin
  SetLength(LDisposables, Length(FLoggers));
  for i := 0 to High(FLoggers) do
  begin
    LDisposables[i] := FLoggers[i].BeginScope(AMessage, AArgs);
  end;
  Result := TCompositeDisposable.Create(LDisposables);
end;

function TAggregateLogger.BeginScope(const AState: TObject): IDisposable;
var
  LDisposables: TArray<IDisposable>;
  i: Integer;
begin
  SetLength(LDisposables, Length(FLoggers));
  for i := 0 to High(FLoggers) do
  begin
    LDisposables[i] := FLoggers[i].BeginScope(AState);
  end;
  Result := TCompositeDisposable.Create(LDisposables);
end;

{ TCompositeDisposable }

constructor TCompositeDisposable.Create(const ADisposables: TArray<IDisposable>);
begin
  inherited Create;
  FDisposables := ADisposables;
end;

procedure TCompositeDisposable.Dispose;
var
  LDisposable: IDisposable;
begin
  for LDisposable in FDisposables do
  begin
    if LDisposable <> nil then
      LDisposable.Dispose;
  end;
end;

{ TNullDisposable }

procedure TNullDisposable.Dispose;
begin
  // No-op
end;

end.

