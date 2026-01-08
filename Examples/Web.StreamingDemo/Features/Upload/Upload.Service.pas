unit Upload.Service;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,
  Dext.Web;

type
  IUploadService = interface
    ['{B9CEC60E-8E7B-4F7E-9F9B-B129FE8817D5}']
    function UploadFile(const AFile: IFormFile): string;
  end;

  TUploadService = class(TInterfacedObject, IUploadService)
  private
    FUploadDir: string;
  public
    constructor Create;
    function UploadFile(const AFile: IFormFile): string;
  end;

implementation

{ TUploadService }

constructor TUploadService.Create;
begin
  FUploadDir := TPath.Combine(ExtractFilePath(ParamStr(0)), 'uploads');
  if not TDirectory.Exists(FUploadDir) then
    TDirectory.CreateDirectory(FUploadDir);
end;

function TUploadService.UploadFile(const AFile: IFormFile): string;
var
  DestPath: string;
  FileStream: TFileStream;
begin
  if AFile = nil then
    raise Exception.Create('No file provided');

  DestPath := TPath.Combine(FUploadDir, AFile.FileName);
  
  // Save file
  FileStream := TFileStream.Create(DestPath, fmCreate);
  try
    FileStream.CopyFrom(AFile.Stream, 0);
  finally
    FileStream.Free;
  end;

  Result := AFile.FileName;
end;

end.
