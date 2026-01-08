unit Dext.Web.Indy.Types;

interface

uses
  System.Classes,
  Dext.Web.Interfaces;

type
  TIndyFormFile = class(TInterfacedObject, IFormFile)
  private
    FFileName: string;
    FName: string;
    FContentType: string;
    FStream: TStream;
  public
    constructor Create(const AName, AFileName, AContentType: string; AStream: TStream);
    destructor Destroy; override;
    function GetFileName: string;
    function GetName: string;
    function GetContentType: string;
    function GetLength: Int64;
    function GetStream: TStream;
  end;

implementation

{ TIndyFormFile }

constructor TIndyFormFile.Create(const AName, AFileName, AContentType: string; AStream: TStream);
begin
  inherited Create;
  FName := AName;
  FFileName := AFileName;
  FContentType := AContentType;
  FStream := AStream;
end;

destructor TIndyFormFile.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TIndyFormFile.GetFileName: string; begin Result := FFileName; end;
function TIndyFormFile.GetName: string; begin Result := FName; end;
function TIndyFormFile.GetContentType: string; begin Result := FContentType; end;
function TIndyFormFile.GetLength: Int64; begin Result := FStream.Size; end;
function TIndyFormFile.GetStream: TStream; begin Result := FStream; end;

end.
