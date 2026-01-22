unit User.Entity;

interface

uses
  Dext.Json.Types;

type
  TUser = class
  private
    FId: Integer;
    FName: string;
    FEmail: string;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
  end;

implementation

end.
