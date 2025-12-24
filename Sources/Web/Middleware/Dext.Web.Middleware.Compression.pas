unit Dext.Web.Middleware.Compression;

interface

uses
  System.SysUtils, System.Classes, System.ZLib,
  Dext.Web.Interfaces, Dext.Web.Core;

type
  TCompressionMiddleware = class(TMiddleware)
  public
    procedure Invoke(AContext: IHttpContext; ANext: TRequestDelegate); override;
  end;

implementation

type
  TBufferedResponse = class(TInterfacedObject, IHttpResponse)
  private
    FInner: IHttpResponse;
    FBuffer: TMemoryStream;
  public
    constructor Create(const AInner: IHttpResponse);
    destructor Destroy; override;

    function GetStatusCode: Integer;
    function Status(AValue: Integer): IHttpResponse;
    procedure SetStatusCode(AValue: Integer);
    procedure SetContentType(const AValue: string);
    procedure SetContentLength(const AValue: Int64);
    procedure Write(const AContent: string); overload;
    procedure Write(const ABuffer: TBytes); overload;
    procedure Write(const AStream: TStream); overload;
    procedure Json(const AJson: string);
    procedure AddHeader(const AName, AValue: string);
    procedure AppendCookie(const AName, AValue: string; const AOptions: TCookieOptions); overload;
    procedure AppendCookie(const AName, AValue: string); overload;
    procedure DeleteCookie(const AName: string);
    property StatusCode: Integer read GetStatusCode write SetStatusCode;
    property Buffer: TMemoryStream read FBuffer;
  end;

{ TBufferedResponse }

constructor TBufferedResponse.Create(const AInner: IHttpResponse);
begin
  inherited Create;
  FInner := AInner;
  FBuffer := TMemoryStream.Create;
end;

destructor TBufferedResponse.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

procedure TBufferedResponse.AddHeader(const AName, AValue: string); begin FInner.AddHeader(AName, AValue); end;
procedure TBufferedResponse.AppendCookie(const AName, AValue: string; const AOptions: TCookieOptions); begin FInner.AppendCookie(AName, AValue, AOptions); end;
procedure TBufferedResponse.AppendCookie(const AName, AValue: string); begin FInner.AppendCookie(AName, AValue); end;
procedure TBufferedResponse.DeleteCookie(const AName: string); begin FInner.DeleteCookie(AName); end;
function TBufferedResponse.GetStatusCode: Integer; begin Result := FInner.StatusCode; end;
procedure TBufferedResponse.Json(const AJson: string);
begin
  SetContentType('application/json; charset=utf-8');
  Write(AJson);
end;
procedure TBufferedResponse.SetContentLength(const AValue: Int64); begin FInner.SetContentLength(AValue); end;
procedure TBufferedResponse.SetContentType(const AValue: string); begin FInner.SetContentType(AValue); end;
procedure TBufferedResponse.SetStatusCode(AValue: Integer); begin FInner.StatusCode := AValue; end;
function TBufferedResponse.Status(AValue: Integer): IHttpResponse; begin FInner.Status(AValue); Result := Self; end;
procedure TBufferedResponse.Write(const AContent: string);
var
  Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(AContent);
  FBuffer.WriteBuffer(Bytes[0], Length(Bytes));
end;
procedure TBufferedResponse.Write(const ABuffer: TBytes);
begin
  if Length(ABuffer) > 0 then
    FBuffer.WriteBuffer(ABuffer[0], Length(ABuffer));
end;

procedure TBufferedResponse.Write(const AStream: TStream);
begin
  if (AStream <> nil) and (AStream.Size > 0) then
  begin
    AStream.Position := 0;
    FBuffer.CopyFrom(AStream, AStream.Size);
  end;
end;

{ TCompressionMiddleware }

procedure TCompressionMiddleware.Invoke(AContext: IHttpContext; ANext: TRequestDelegate);
var
  AcceptEncoding: string;
  BufferedResponse: TBufferedResponse;
  OriginalResponse: IHttpResponse;
  CompressedStream: TMemoryStream;
  ZStream: TZCompressionStream;
begin
  AcceptEncoding := AContext.Request.GetHeader('Accept-Encoding').ToLower;
  
  if (Pos('gzip', AcceptEncoding) = 0) then
  begin
    ANext(AContext);
    Exit;
  end;

  OriginalResponse := AContext.Response;
  BufferedResponse := TBufferedResponse.Create(OriginalResponse);
  try
    AContext.Response := BufferedResponse;
    
    ANext(AContext);
    
    // Perform compression
    if BufferedResponse.Buffer.Size > 0 then
    begin
      CompressedStream := TMemoryStream.Create;
      try
        ZStream := TZCompressionStream.Create(CompressedStream, TZCompressionLevel.zcDefault, 15 + 16); // 15+16 = GZIP mode
        try
          BufferedResponse.Buffer.Position := 0;
          ZStream.CopyFrom(BufferedResponse.Buffer, BufferedResponse.Buffer.Size);
        finally
          ZStream.Free;
        end;

        OriginalResponse.AddHeader('Content-Encoding', 'gzip');
        OriginalResponse.SetContentLength(CompressedStream.Size);
        
        CompressedStream.Position := 0;
        var OutBuffer: TBytes;
        SetLength(OutBuffer, CompressedStream.Size);
        CompressedStream.ReadBuffer(OutBuffer[0], CompressedStream.Size);
        OriginalResponse.Write(OutBuffer);
      finally
        CompressedStream.Free;
      end;
    end;
  finally
    AContext.Response := OriginalResponse;
    // BufferedResponse will be freed by interface refcount if we are careful, 
    // but here it's a local object. TBufferedResponse is TInterfacedObject.
  end;
end;

end.
