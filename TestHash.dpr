program TestHash;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Hash;

begin
  try
    WriteLn('Testing System.Hash.THashSHA2.GetHMACAsBytes');
    var Key := TEncoding.UTF8.GetBytes('secret');
    var Data := TEncoding.UTF8.GetBytes('data');
    var HashBytes := THashSHA2.GetHMACAsBytes(Data, Key);
    WriteLn('HMAC Bytes Length: ' + Length(HashBytes).ToString);
    WriteLn('SUCCESS: System.Hash.GetHMACAsBytes is available.');
  except
    on E: Exception do
      WriteLn('ERROR: ' + E.Message);
  end;
end.
