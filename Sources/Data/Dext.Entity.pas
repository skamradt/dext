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
unit Dext.Entity;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  FireDAC.Comp.Client,
  System.SysUtils,
  System.Classes,
  Dext,
  Dext.Types.Lazy,
  {$I Dext.Entity.Uses.inc};

type
  // External Aliases
  TFDConnection = FireDAC.Comp.Client.TFDConnection;

  {$I Dext.Entity.Aliases.inc}

  // ===========================================================================
  // 🧩 Local Types & Helpers
  // ===========================================================================
type
  Lazy<T> = record
  private
    FInstance: ILazy;
    function GetIsValueCreated: Boolean;
    function GetValue: T;
  public
    class function Create: Lazy<T>; overload; static;
    constructor Create(const AValueFactory: TFunc<T>; AOwnsValue: Boolean = True); overload;
    constructor CreateFrom(const AValue: T; AOwnsValue: Boolean = False);

    class operator Implicit(const Value: Lazy<T>): T;
    class operator Implicit(const Value: T): Lazy<T>;
    class operator Implicit(const ValueFactory: TFunc<T>): Lazy<T>;

    property IsValueCreated: Boolean read GetIsValueCreated;
    property Value: T read GetValue;
  end;

  /// <summary>
  ///   Persistence Setup Helper
  /// </summary>
  TDbContextClass = class of TDbContext;
  
  /// <summary>
  ///   Helper for TDextServices to add Persistence features.
  /// </summary>
  TDextPersistenceServicesHelper = record helper for TDextServices
  public
    /// <summary>
    ///   Registers a DbContext with the dependency injection container.
    /// </summary>
    function AddDbContext<T: TDbContext>(Config: TProc<TDbContextOptions>): TDextServices;
  end;

  TPersistence = class
  public
    /// <summary>
    ///   Registers a DbContext with the dependency injection container.
    /// </summary>
    class procedure AddDbContext<T: TDbContext>(Services: IServiceCollection; Config: TProc<TDbContextOptions>);
  end;

const
  // Cascade Action Aliases (if not already in inc, but safe to keep or remove if duplicate)
  // Checking aliases inc content previously... they might be there as TCascadeAction.caNoAction
  // Let's keep them manually if they map enum values directly for convenience.
  caNoAction = Dext.Entity.Attributes.TCascadeAction.caNoAction;
  caCascade = Dext.Entity.Attributes.TCascadeAction.caCascade;
  caSetNull = Dext.Entity.Attributes.TCascadeAction.caSetNull;
  caRestrict = Dext.Entity.Attributes.TCascadeAction.caRestrict;

implementation

uses
  System.Rtti,
  Dext.Specifications.OrderBy,
  Dext.DI.Core; // Added for IServiceProvider, TServiceType

{ TDextPersistenceServicesHelper }

function TDextPersistenceServicesHelper.AddDbContext<T>(Config: TProc<TDbContextOptions>): TDextServices;
begin
  TPersistence.AddDbContext<T>(Self.Unwrap, Config);
  Result := Self;
end;

{ TPersistence }

class procedure TPersistence.AddDbContext<T>(Services: IServiceCollection; Config: TProc<TDbContextOptions>);
begin
  Services.AddScoped(
    TServiceType.FromClass(T),
    T,
    function(Provider: IServiceProvider): TObject
    begin
      var Options := TDbContextOptions.Create;
      try
        // Apply user configuration
        if Assigned(Config) then
          Config(Options);

        // 1. Connection Creation (Pooled)
        var Connection: IDbConnection;
        
        if Options.CustomConnection <> nil then
        begin
          Connection := Options.CustomConnection;
        end
        else
        begin
          // FireDAC Creation
          var FDConn := TFDConnection.Create(nil);
          
          if Options.ConnectionDefString <> '' then
          begin
            var DefName := Options.ConnectionDefName;
            if DefName = '' then DefName := 'DextMemoryDef_' + IntToHex(Options.ConnectionDefString.GetHashCode, 8);
            TDextFireDACManager.Instance.RegisterConnectionDefFromString(DefName, Options.ConnectionDefString);
            FDConn.ConnectionDefName := DefName;
          end
          else if Options.ConnectionDefName <> '' then
          begin
            FDConn.ConnectionDefName := Options.ConnectionDefName;
          end
          else if Options.Pooling then
          begin
             var Params := TStringList.Create;
             try
               for var Pair in Options.Params do
                 Params.Values[Pair.Key] := Pair.Value;
                 
               // Use Manager to register/get Def
               var DefName := TDextFireDACManager.Instance.RegisterConnectionDef(
                 Options.DriverName, 
                 Params, 
                 Options.PoolMax
               );
               
                FDConn.ConnectionDefName := DefName;
                
                // Apply performance and resource options
                TDextFireDACManager.Instance.ApplyResourceOptions(FDConn);
              finally
                Params.Free;
              end;
          end
          else
          begin
            if Options.DriverName <> '' then
              FDConn.DriverName := Options.DriverName;
              
            if Options.ConnectionString <> '' then
              FDConn.ConnectionString := Options.ConnectionString;
              
            for var Pair in Options.Params do
               FDConn.Params.Values[Pair.Key] := Pair.Value;

            // Apply performance and resource options
            TDextFireDACManager.Instance.ApplyResourceOptions(FDConn);
          end;
          
          try
            // Ensure unique name for components created in threads (request scoped)
            // to avoid global name conflicts in FireDAC manager
            FDConn.SetUniqueName;
            
            FDConn.Open; 
          except
             FDConn.Free;
             raise;
          end;

          Connection := TFireDACConnection.Create(FDConn, True); // Owns FDConn
        end;

        // 2. Dialect Resolution
        var Dialect := Options.Dialect;
        if Dialect = nil then
          Dialect := TSQLiteDialect.Create;

        // 3. Create Context
        var Ctx := TDbContextClass(T).Create(Connection, Dialect, nil);
        Result := Ctx;
        
      except
        Options.Free;
        raise;
      end;
      Options.Free;
    end
  );
end;

{ Lazy<T> }

class function Lazy<T>.Create: Lazy<T>;
begin
  // Default constructor returns empty/default
  Result.FInstance := TValueLazy<T>.Create(Default(T));
end;

constructor Lazy<T>.Create(const AValueFactory: TFunc<T>; AOwnsValue: Boolean);
begin
  FInstance := TLazy<T>.Create(AValueFactory, AOwnsValue);
end;

constructor Lazy<T>.CreateFrom(const AValue: T; AOwnsValue: Boolean = False);
begin
  FInstance := TValueLazy<T>.Create(AValue, AOwnsValue);
end;

function Lazy<T>.GetIsValueCreated: Boolean;
begin
  if FInstance <> nil then
    Result := FInstance.IsValueCreated
  else
    Result := False;
end;

function Lazy<T>.GetValue: T;
begin
  if FInstance <> nil then
    Result := FInstance.Value.AsType<T>
  else
    Result := Default(T);
end;

class operator Lazy<T>.Implicit(const Value: Lazy<T>): T;
begin
  Result := Value.Value;
end;

class operator Lazy<T>.Implicit(const Value: T): Lazy<T>;
begin
  Result.CreateFrom(Value);
end;

class operator Lazy<T>.Implicit(const ValueFactory: TFunc<T>): Lazy<T>;
begin
  Result.Create(ValueFactory);
end;


end.

