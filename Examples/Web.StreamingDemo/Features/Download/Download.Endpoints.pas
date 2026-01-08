unit Download.Endpoints;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  Dext.Web, Dext.Json,
  Download.Service;

type
  TDownloadEndpoints = class
  public
    class procedure Map(const App: TDextAppBuilder);
  end;

implementation

{ TDownloadEndpoints }

class procedure TDownloadEndpoints.Map(const App: TDextAppBuilder);
begin
  // GET: List files
  App.MapGet<IDownloadService, IHttpContext>('/download/list',
    procedure(Service: IDownloadService; Ctx: IHttpContext)
    var
      List: TList<TFileInfo>;
    begin
      List := Service.ListFiles;
      try
        Ctx.Response.Json(TDextJson.Serialize<TList<TFileInfo>>(List));
      finally
        List.Free;
      end;
    end);

  // GET: Download file (as attachment)
  App.MapGet<IDownloadService, IHttpContext>('/download/{name}',
    procedure(Service: IDownloadService; Ctx: IHttpContext)
    var
      FileName: string;
      Stream: TStream;
    begin
      FileName := Ctx.Request.RouteParams['name'];
      Stream := Service.GetFile(FileName);
      
      if Stream = nil then
      begin
        Ctx.Response.StatusCode := 404;
        Ctx.Response.Json(TDextJson.Serialize<string>('{"error": "File not found"}'));
        Exit;
      end;

      Ctx.Response.ContentType := Service.GetMimeType(FileName);
      Ctx.Response.AddHeader('Content-Disposition', 'attachment; filename="' + FileName + '"');
      Ctx.Response.Write(Stream);
    end);

  // GET: Stream file (inline)
  App.MapGet<IDownloadService, IHttpContext>('/stream/{name}',
    procedure(Service: IDownloadService; Ctx: IHttpContext)
    var
      FileName: string;
      Stream: TStream;
    begin
      FileName := Ctx.Request.RouteParams['name'];
      Stream := Service.GetFile(FileName);
      
      if Stream = nil then
      begin
        Ctx.Response.StatusCode := 404;
        Exit;
      end;

      Ctx.Response.ContentType := Service.GetMimeType(FileName);
      Ctx.Response.Write(Stream);
    end);
end;

end.
