program Web.CachingDemo;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  System.SysUtils,
  Dext,
  Dext.Web;

var
  App: IWebApplication;
  RequestCount: Integer = 0;
begin
  try
    WriteLn('💾 Dext Response Caching Demo');
    WriteLn('==============================');
    
    // Create App
    App := TDextApplication.Create;
    var Builder := App.Builder;

    // 1. Configure Response Caching
    WriteLn('📦 Configuring Response Caching...');
    Builder
      .UseResponseCache(
        ResponseCacheOptions
          .DefaultDuration(30)
          .MaxSize(100)
          .VaryByQueryString);
      
    // 2. Map Endpoints
    
    // Endpoint to demonstrate caching (returns generic IResult / JSON string)
    Builder.MapGet('/api/time',
      procedure(Ctx: IHttpContext)
      begin
        Inc(RequestCount);
        WriteLn(Format('[%d] Generating fresh response at %s', [RequestCount, FormatDateTime('hh:nn:ss', Now)]));
        
        var Json := Format(
          '{"timestamp":"%s","request_count":%d,"message":"This response is cached for 30 seconds"}',
          [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), RequestCount]);
          
        Ctx.Response.Json(Json);
      end);

    // Endpoint with vary by query
    Builder.MapGet('/api/data',
      procedure(Ctx: IHttpContext)
      begin
        var Json := Format(
          '{"data":"Sample data","generated_at":"%s"}',
          [FormatDateTime('hh:nn:ss', Now)]);
        Ctx.Response.Json(Json);
      end);

    // Index
    Builder.MapGet('/',
      procedure(Ctx: IHttpContext)
      begin
        Ctx.Response.Json('{"message":"Caching Demo - Try /api/time or /api/data"}');
      end);

    WriteLn('✅ Endpoints configured');
    WriteLn('🌐 Server running on http://localhost:8080');
    
    App.Run(8080);
    
  except
    on E: Exception do
      WriteLn('❌ Error: ', E.Message);
  end;
end.
