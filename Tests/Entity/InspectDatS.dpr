program InspectDatS;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Rtti,
  FireDAC.DatS;

procedure InspectRow;
var
  Ctx: TRttiContext;
  Typ: TRttiType;
  Method: TRttiMethod;
  Prop: TRttiProperty;
begin
  Ctx := TRttiContext.Create;
  try
    Typ := Ctx.GetType(TFDDatSRow);
    if Typ = nil then
    begin
      Writeln('TFDDatSRow type not found');
      Exit;
    end;
    
    Writeln('Methods of TFDDatSRow:');
    for Method in Typ.GetMethods do
      Writeln('  ' + Method.Name);
      
    Writeln('Properties of TFDDatSRow:');
    for Prop in Typ.GetProperties do
      Writeln('  ' + Prop.Name);
  finally
    Ctx.Free;
  end;
end;

begin
  try
    InspectRow;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
