unit Download.Service;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils, System.Generics.Collections;

type
  TFileInfo = record
    Name: string;
    Size: Int64;
    UploadDate: TDateTime;
  end;

  IDownloadService = interface
    ['{D7F2A6C4-B1E2-4D9A-8F7E-3C5A6B7D8E9F}']
    function GetFile(const FileName: string): TStream;
    function ListFiles: TList<TFileInfo>;
    function GetMimeType(const FileName: string): string;
  end;

  TDownloadService = class(TInterfacedObject, IDownloadService)
  private
    FUploadDir: string;
  public
    constructor Create;
    function GetFile(const FileName: string): TStream;
    function ListFiles: TList<TFileInfo>;
    function GetMimeType(const FileName: string): string;
  end;

implementation

{ TDownloadService }

constructor TDownloadService.Create;
begin
  FUploadDir := TPath.Combine(ExtractFilePath(ParamStr(0)), 'uploads');
  if not TDirectory.Exists(FUploadDir) then
    TDirectory.CreateDirectory(FUploadDir);
end;

function TDownloadService.GetFile(const FileName: string): TStream;
var
  FilePath: string;
begin
  FilePath := TPath.Combine(FUploadDir, FileName);
  if not TFile.Exists(FilePath) then
    Exit(nil);
    
  Result := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
end;

function TDownloadService.ListFiles: TList<TFileInfo>;
var
  Files: TArray<string>;
  F: string;
  Info: TFileInfo;
  Attr: TSearchRec;
begin
  Result := TList<TFileInfo>.Create;
  Files := TDirectory.GetFiles(FUploadDir);
  for F in Files do
  begin
    Info.Name := TPath.GetFileName(F);
    if FindFirst(F, faAnyFile, Attr) = 0 then
    begin
      Info.Size := Attr.Size;
      Info.UploadDate := Attr.TimeStamp;
      FindClose(Attr);
    end;
    Result.Add(Info);
  end;
end;

function TDownloadService.GetMimeType(const FileName: string): string;
var
  Ext: string;
begin
  Ext := LowerCase(TPath.GetExtension(FileName));
  if Ext = '.pdf' then Result := 'application/pdf'
  else if (Ext = '.jpg') or (Ext = '.jpeg') then Result := 'image/jpeg'
  else if Ext = '.png' then Result := 'image/png'
  else if Ext = '.txt' then Result := 'text/plain'
  else if Ext = '.zip' then Result := 'application/zip'
  else Result := 'application/octet-stream';
end;

end.
