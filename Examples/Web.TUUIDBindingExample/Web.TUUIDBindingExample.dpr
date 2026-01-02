program Web.TUUIDBindingExample;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  System.SysUtils,
  System.DateUtils,
  Dext,
  Dext.Web,
  Dext.Json,
  Dext.Types.UUID;

type
  /// <summary>
  ///   DTO with TUUID field for JSON binding demonstration.
  /// </summary>
  TProductRequest = record
    Id: TUUID;
    Name: string;
    Price: Double;
  end;

  /// <summary>
  ///   Response showing UUID format conversions.
  /// </summary>
  TFormatResponse = record
    Input: string;
    Canonical: string;
    WithBraces: string;
    AsTGUID: string;
  end;

var
  App: IWebApplication;
begin
  try
    WriteLn('Web TUUID Binding Example');
    WriteLn('=========================');
    WriteLn;
    WriteLn('This example demonstrates TUUID binding from various sources.');
    WriteLn;

    App := TDextApplication.Create;

    // ══════════════════════════════════════════════════════════════════════════
    // ENDPOINT 1: Manual TUUID parsing from URL (Didactic)
    // GET /api/products/{id}
    // Shows: TUUID.FromString for flexible format parsing
    // ══════════════════════════════════════════════════════════════════════════
    App.Builder.MapGet('/api/products/{id}',
      procedure(Context: IHttpContext)
      var
        IdStr: string;
        U: TUUID;
        Product: TProductRequest;
      begin
        IdStr := Context.Request.RouteParams['id'];
        
        WriteLn('GET /api/products/{id} [MANUAL]');
        WriteLn('  Input: ', IdStr);
        
        try
          U := TUUID.FromString(IdStr);  // Handles all formats
          WriteLn('  Parsed: ', U.ToString);
          
          Product.Id := U;
          Product.Name := 'Product ' + U.ToString.Substring(0, 8);
          Product.Price := 99.99;
          
          Context.Response.Status(200).Json(TDextJson.Serialize<TProductRequest>(Product));
        except
          on E: Exception do
            Context.Response.Status(400).Json('{"error": "Invalid UUID: ' + E.Message + '"}');
        end;
      end);

    // ══════════════════════════════════════════════════════════════════════════
    // ENDPOINT 2: Automatic TUUID binding from route (Modern)
    // GET /api/products/lookup/{id}
    // Shows: Framework automatically binds TUUID from route parameter
    // ══════════════════════════════════════════════════════════════════════════
    App.Builder.MapGet<TUUID, IResult>('/api/products/lookup/{id}',
      function(Id: TUUID): IResult
      var
        Product: TProductRequest;
      begin
        WriteLn('GET /api/products/lookup/{id} [AUTO-BOUND]');
        WriteLn('  Bound TUUID: ', Id.ToString);
        
        Product.Id := Id;
        Product.Name := 'Auto-Bound Product';
        Product.Price := 149.99;
        
        Result := Results.Ok<TProductRequest>(Product);
      end);

    // ══════════════════════════════════════════════════════════════════════════
    // ENDPOINT 3: TUUID in JSON body (Model Binding)
    // POST /api/products
    // Shows: TUUID field in DTO automatically deserialized from JSON
    // ══════════════════════════════════════════════════════════════════════════
    App.Builder.MapPost<TProductRequest, IResult>('/api/products',
      function(Product: TProductRequest): IResult
      begin
        WriteLn('POST /api/products [BODY BINDING]');
        WriteLn('  ID: ', Product.Id.ToString);
        WriteLn('  Name: ', Product.Name);
        WriteLn('  Price: ', Product.Price:0:2);
        WriteLn('  As TGUID: ', GUIDToString(Product.Id.ToGUID));
        
        // Return 201 Created with Location header
        Result := Results.Created<TProductRequest>('/api/products/' + Product.Id.ToString, Product);
      end);

    // ══════════════════════════════════════════════════════════════════════════
    // ENDPOINT 4: TUUID from URL + Body with validation
    // PUT /api/products/{id}
    // Shows: TUUID equality operator, mixed binding sources
    // ══════════════════════════════════════════════════════════════════════════
    App.Builder.MapPut<TUUID, TProductRequest, IResult>('/api/products/{id}',
      function(UrlId: TUUID; Body: TProductRequest): IResult
      begin
        WriteLn('PUT /api/products/{id} [MIXED BINDING]');
        WriteLn('  URL ID:  ', UrlId.ToString);
        WriteLn('  Body ID: ', Body.Id.ToString);
        
        // TUUID supports equality operator
        if UrlId <> Body.Id then
        begin
          WriteLn('  ❌ ID MISMATCH');
          Result := Results.BadRequest('URL ID does not match body ID');
          Exit;
        end;
        
        WriteLn('  ✅ IDs match - updating: ', Body.Name);
        Result := Results.Ok<TProductRequest>(Body);
      end);

    // ══════════════════════════════════════════════════════════════════════════
    // ENDPOINT 5: UUID v7 generation
    // POST /api/products/generate-v7
    // Shows: TUUID.NewV7 for time-ordered UUIDs
    // ══════════════════════════════════════════════════════════════════════════
    App.Builder.MapPost<IResult>('/api/products/generate-v7',
      function: IResult
      var
        NewId: TUUID;
        Product: TProductRequest;
      begin
        NewId := TUUID.NewV7;  // Time-ordered UUID
        
        WriteLn('POST /api/products/generate-v7');
        WriteLn('  Generated: ', NewId.ToString);
        
        Product.Id := NewId;
        Product.Name := 'Generated Product';
        Product.Price := 0.0;
        
        Result := Results.Ok<TProductRequest>(Product);
      end);

    // ══════════════════════════════════════════════════════════════════════════
    // ENDPOINT 6: UUID format parsing playground
    // GET /api/uuid/formats/{id}
    // Shows: TUUID.FromString accepts multiple formats
    // ══════════════════════════════════════════════════════════════════════════
    App.Builder.MapGet('/api/uuid/formats/{id}',
      procedure(Context: IHttpContext)
      var
        IdStr: string;
        U: TUUID;
        Resp: TFormatResponse;
      begin
        IdStr := Context.Request.RouteParams['id'];
        
        WriteLn('GET /api/uuid/formats/{id} [FORMAT TEST]');
        WriteLn('  Input: ', IdStr);
        
        try
          U := TUUID.FromString(IdStr);
          
          Resp.Input := IdStr;
          Resp.Canonical := U.ToString;
          Resp.WithBraces := U.ToStringWithBraces;
          Resp.AsTGUID := GUIDToString(U.ToGUID);
          
          WriteLn('  Canonical: ', Resp.Canonical);
          
          Context.Response.Status(200).Json(TDextJson.Serialize<TFormatResponse>(Resp));
        except
          on E: Exception do
            Context.Response.Status(400).Json('{"error": "Invalid UUID: ' + E.Message + '"}');
        end;
      end);

    // ═══════════════════════════════════════════════════════════════════════════
    WriteLn('Available endpoints:');
    WriteLn('───────────────────────────────────────────────────────────────────');
    WriteLn('  GET    /api/products/{id}          - Manual TUUID parsing');
    WriteLn('  GET    /api/products/lookup/{id}   - Auto-bound TUUID');
    WriteLn('  POST   /api/products               - TUUID in JSON body');
    WriteLn('  PUT    /api/products/{id}          - URL + Body validation');
    WriteLn('  POST   /api/products/generate-v7   - Generate UUID v7');
    WriteLn('  GET    /api/uuid/formats/{id}      - Format parsing test');
    WriteLn('═══════════════════════════════════════════════════════════════════');
    WriteLn;
    WriteLn('Server running on http://localhost:8080');
    WriteLn('Press Ctrl+C to stop');
    WriteLn;

    App.Run(8080);
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end.
