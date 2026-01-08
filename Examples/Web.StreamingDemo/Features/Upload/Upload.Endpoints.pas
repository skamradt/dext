unit Upload.Endpoints;

interface

uses
  System.SysUtils,
  Dext.Web, Dext.Json,
  Upload.Service;

type
  TUploadEndpoints = class
  public
    class procedure Map(const App: TDextAppBuilder);
  end;

implementation

{ TUploadEndpoints }

class procedure TUploadEndpoints.Map(const App: TDextAppBuilder);
begin
  // GET: Simple upload form
  App.MapGet('/upload/form', procedure(Ctx: IHttpContext)
    begin
      Ctx.Response.ContentType := 'text/html';
      Ctx.Response.Write(
        '<html><body>' +
        '<h2>Dext Streaming Demo - Upload</h2>' +
        '<form action="/upload" method="post" enctype="multipart/form-data">' +
        '  <input type="file" name="myfile"><br><br>' +
        '  <input type="submit" value="Upload File">' +
        '</form>' +
        '</body></html>'
      );
    end);

  // POST: Single file upload
  App.MapPost<IUploadService, IHttpContext>('/upload',
    procedure(Service: IUploadService; Ctx: IHttpContext)
    var
      AFile: IFormFile;
      SavedName: string;
    begin
      AFile := Ctx.Request.Files.GetFile('myfile');
      if AFile = nil then
      begin
        Ctx.Response.StatusCode := 400;
        Ctx.Response.Json('{"error": "File not found in request (use field name ''myfile'')"}');
        Exit;
      end;

      SavedName := Service.UploadFile(AFile);
      Ctx.Response.Json(TJsonBuilder.Create
        .Add('success', True)
        .Add('filename', SavedName)
        .Add('size', AFile.Length)
        .ToString);
    end);

  // POST: Multiple files upload
  App.MapPost<IUploadService, IHttpContext>('/upload/multiple',
    procedure(Service: IUploadService; Ctx: IHttpContext)
    var
      I: Integer;
      AFile: IFormFile;
      Summary: string;
    begin
      Summary := '';
      for I := 0 to Ctx.Request.Files.Count - 1 do
      begin
        AFile := Ctx.Request.Files[I];
        Service.UploadFile(AFile);
        if Summary <> '' then Summary := Summary + ', ';
        Summary := Summary + AFile.FileName;
      end;
      
      Ctx.Response.Json(TJsonBuilder.Create
        .Add('success', True)
        .Add('files', Summary)
        .Add('count', Ctx.Request.Files.Count)
        .ToString);
    end);
end;

end.
