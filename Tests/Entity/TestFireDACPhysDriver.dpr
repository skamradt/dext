program TestFireDACPhysDriver;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  FireDAC.Stan.Def,
  FireDAC.Phys.Intf,
  Dext.Entity.Drivers.FireDAC.Phys in '..\..\Sources\Data\Dext.Entity.Drivers.FireDAC.Phys.pas';

begin
  try
    Writeln('Compiling Dext.Entity.Drivers.FireDAC.Phys...');
    // No runtime logic needed, just compilation check
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
