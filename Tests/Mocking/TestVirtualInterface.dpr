{***************************************************************************}
{           Test TVirtualInterface directly                                 }
{***************************************************************************}
program TestVirtualInterface;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  System.Rtti;

type
  {$M+}
  ICalculator = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function Add(A, B: Integer): Integer;
  end;
  {$M-}

  TMyHandler = class
    procedure HandleInvoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
  end;

procedure TMyHandler.HandleInvoke(Method: TRttiMethod; const Args: TArray<TValue>; out Result: TValue);
begin
  WriteLn('  HandleInvoke called for: ', Method.Name);
  if Method.Name = 'Add' then
    Result := TValue.From<Integer>(999);
end;

procedure TestDirectVirtualInterface;
var
  Handler: TMyHandler;
  VIRef: IInterface;  // MUST keep as interface reference!
  VI: TVirtualInterface;
  Calc: ICalculator;
begin
  WriteLn('Creating Handler...');
  Handler := TMyHandler.Create;
  try
    WriteLn('Creating TVirtualInterface directly...');
    
    VI := TVirtualInterface.Create(TypeInfo(ICalculator), Handler.HandleInvoke);
    
    // CRITICAL: Keep interface reference to prevent destruction
    VIRef := VI;
    
    WriteLn('TVirtualInterface created');
    
    WriteLn('Querying for ICalculator...');
    if Supports(VIRef, ICalculator, Calc) then
    begin
      WriteLn('Got ICalculator, calling Add...');
      var R := Calc.Add(10, 20);
      WriteLn('Add returned: ', R);
    end
    else
      WriteLn('Supports returned False!');
      
    WriteLn('Test complete');
  finally
    Handler.Free;
  end;
end;

begin
  try
    TestDirectVirtualInterface;
  except
    on E: Exception do
      WriteLn('ERROR: ', E.ClassName, ': ', E.Message);
  end;
  
  ConsolePause;
end.
