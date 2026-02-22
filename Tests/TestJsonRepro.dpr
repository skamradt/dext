program TestJsonRepro;
{$APPTYPE CONSOLE}


uses
  System.SysUtils,
  System.Rtti,
  Dext.Json,
  Dext.Utils;

type
  {$M+}
  {$RTTI EXPLICIT FIELDS([vcPrivate, vcPublic]) PROPERTIES([vcPublic, vcPublished])}
  TJsonMetadata = class
  private
    FName: string;
    FValue: Integer;
  public
    property Name: string read FName write FName;
    property Value: Integer read FValue write FValue;
  end;

procedure Main;
var
  Meta: TJsonMetadata;
  Val: TValue;
  Json: string;
begin
  try
    WriteLn('Testing JSON Serialization Reproduction...');

    Meta := TJsonMetadata.Create;
    try
      Meta.Name := 'Dext';
      Meta.Value := 10;

      // Direct Serialize<T>
      Json := TDextJson.Serialize<TJsonMetadata>(Meta);
      WriteLn('Generic Serialize<TJsonMetadata>: ', Json);

      // TValue Serialize
      Val := TValue.From<TJsonMetadata>(Meta);
      Json := TDextJson.Serialize(Val);
      WriteLn('TValue Serialize (Specific TypeInfo): ', Json);

      // TValue as TObject
      Val := TValue.From<TObject>(Meta);
      Json := TDextJson.Serialize(Val);
      WriteLn('TValue Serialize (As TObject): ', Json);
      
    finally
      Meta.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end;

begin
  SetConsoleCharSet;
  Main;
  ConsolePause;
end.
