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
{  Created: 2026-01-03                                                      }
{                                                                           }
{  Dext.Interception.ClassProxy - Virtual Method Interception for Classes   }
{                                                                           }
{***************************************************************************}
unit Dext.Interception.ClassProxy;

interface

uses
  System.Rtti,
  System.SysUtils,
  System.TypInfo,
  Dext.Interception,
  Dext.Interception.Proxy;

type
  /// <summary>
  ///   Proxy for class types using TVirtualMethodInterceptor.
  /// </summary>
  TClassProxy = class
  private
    FVMI: TVirtualMethodInterceptor;
    FInstance: TObject;
    FInterceptors: TArray<IInterceptor>;
    FOwnsInstance: Boolean;
    FInstanceIsDead: Boolean;
    
    procedure DoBefore(Instance: TObject; Method: TRttiMethod;
      const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
  public
    /// <summary>
    ///   Creates a class proxy. 
    ///   Note: This instantiates the class using basic TObject.Create behavior.
    ///   Constructors of the target class are NOT executed unless invoked manually via RTTI.
    /// </summary>
    constructor Create(AClass: TClass; const AInterceptors: TArray<IInterceptor>; AOwnsInstance: Boolean = True);
    destructor Destroy; override;
    procedure Unproxify;
    
    property Instance: TObject read FInstance;
    property OwnsInstance: Boolean read FOwnsInstance write FOwnsInstance;
  end;

implementation

{ TClassProxy }

constructor TClassProxy.Create(AClass: TClass; const AInterceptors: TArray<IInterceptor>; AOwnsInstance: Boolean);
begin
  inherited Create;
  FInterceptors := AInterceptors;
  FOwnsInstance := AOwnsInstance;
  
  // Create instance (Allocates memory, VTable pointing to original class)
  FInstance := AClass.Create;
  
  // Create Interceptor for the class
  FVMI := TVirtualMethodInterceptor.Create(AClass);
  
  // Setup callbacks
  FVMI.OnBefore := DoBefore;
  
  // Install interceptor on specific instance
  // This overwrites the VTable pointer in FInstance to point to VMI's dynamic VTable
  FVMI.Proxify(FInstance);
end;

procedure TClassProxy.Unproxify;
begin
  if Assigned(FVMI) and not FInstanceIsDead then
  begin
    // TVirtualMethodInterceptor will revert the VTable in its destructor.
    // However, if the instance is already dead, this would crash.
    if Assigned(FInstance) and not FInstanceIsDead then
      FreeAndNil(FVMI)
    else
      FVMI := nil; // Just leak it or hope it doesn't crash? 
                   // Actually, we must free it as a TObject to avoid VMT revert if instance is dead.
  end;
end;

destructor TClassProxy.Destroy;
begin
  if not FInstanceIsDead then
  begin
    // Important: Free the interceptor FIRST while memory is valid to revert VTable
    FreeAndNil(FVMI);
    
    if FOwnsInstance and Assigned(FInstance) then
      FreeAndNil(FInstance);
  end
  else
    FreeAndNil(FVMI); // Memory is already dead/vmt reverted or we don't care

  inherited;
end;

procedure TClassProxy.DoBefore(Instance: TObject; Method: TRttiMethod;
  const Args: TArray<TValue>; out DoInvoke: Boolean; out Result: TValue);
var
  Invocation: IInvocation;
begin
  // Don't intercept TObject methods (lifecycle, etc.)
  if Method.Parent.AsInstance.MetaclassType = TObject then
  begin
    if SameText(Method.Name, 'BeforeDestruction') then
    begin
      FInstanceIsDead := True;
      FOwnsInstance := False;
    end;
    DoInvoke := True;
    Exit;
  end;

  // Create invocation wrapper
  Invocation := TInvocation.Create(Method, Args, FInterceptors, Instance);
  
  // Execute interception chain
  Invocation.Proceed;
  
  // Set result
  Result := Invocation.Result;
  
  // Suppress original execution (Loose mock behavior)
  DoInvoke := False;
end;

end.
