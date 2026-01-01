program WebUUIDExample;

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
  TProductDto = record
    Id: TGUID;
    Name: string;
    Price: Double;
  end;

var
  App: IWebApplication;
begin
  try
    WriteLn('Web UUID Example Server');
    WriteLn('=======================');
    WriteLn;

    App := TDextApplication.Create;

    // =========================================================================
    // Example 1: GET with UUID in URL path (MANUAL - for didactic purposes)
    // Demonstrates: Manual TUUID.FromString parsing from route parameter
    // =========================================================================
    App.Builder.MapGet('/api/products/{id}',
      procedure(Context: IHttpContext)
      var
        IdStr: string;
        U: TUUID;
        Product: TProductDto;
      begin
        IdStr := Context.Request.RouteParams['id'];
        WriteLn('GET /api/products/{id} [MANUAL]');
        WriteLn('  Received: ', IdStr);

        try
          U := TUUID.FromString(IdStr);
          WriteLn('  Parsed:   ', U.ToString);

          Product.Id := U.ToGUID;
          Product.Name := 'Sample Product';
          Product.Price := 99.99;

          Context.Response.Status(200).Json(TDextJson.Serialize<TProductDto>(Product));
        except
          on E: Exception do
            Context.Response.Status(400).Json('{"error": "Invalid UUID: ' + E.Message + '"}');
        end;
      end);

    // =========================================================================
    // Example 2: POST with UUID in request body
    // Demonstrates: Model binding of TProductDto with TGUID field
    // =========================================================================
    App.Builder.MapPost<TProductDto, IResult>('/api/products',
      function(Dto: TProductDto): IResult
      var
        U: TUUID;
      begin
        WriteLn('POST /api/products');
        WriteLn('  ID:    ', GUIDToString(Dto.Id));
        WriteLn('  Name:  ', Dto.Name);
        WriteLn('  Price: ', Dto.Price:0:2);

        U := TUUID.FromGUID(Dto.Id);
        WriteLn('  UUID:  ', U.ToString);

        Result := Results.Created<TProductDto>('/api/products/' + U.ToString, Dto);
      end);

    // =========================================================================
    // Example 3: PUT with UUID in both URL and body
    // Demonstrates: Validating URL ID matches body ID
    // =========================================================================
    App.Builder.MapPut<TUUID, TProductDto, IResult>('/api/products/{id}',
      function(Id: TUUID; Dto: TProductDto): IResult
      var
        BodyId: TUUID;
      begin
        BodyId := TUUID.FromGUID(Dto.Id);

        if Id <> BodyId then
        begin
          WriteLn('PUT /api/products/{id} - ID MISMATCH');
          Result := Results.BadRequest('URL ID does not match body ID');
          Exit;
        end;

        WriteLn('PUT /api/products/{id}');
        WriteLn('  ID:    ', Id.ToString);
        WriteLn('  Name:  ', Dto.Name);
        WriteLn('  Price: ', Dto.Price:0:2);

        Result := Results.Ok<TProductDto>(Dto);
      end);

    // =========================================================================
    // Example 4: Generate new UUID v7
    // Demonstrates: TUUID.NewV7 for modern time-sortable UUIDs
    // =========================================================================
    App.Builder.MapPost<IResult>('/api/products/generate',
      function: IResult
      var
        U: TUUID;
        Product: TProductDto;
      begin
        U := TUUID.NewV7;

        WriteLn('POST /api/products/generate');
        WriteLn('  Generated: ', U.ToString);

        Product.Id := U.ToGUID;
        Product.Name := 'Auto-generated Product';
        Product.Price := 0.0;

        Result := Results.Ok<TProductDto>(Product);
      end);

    // =========================================================================
    // Example 5: Test endpoint showing all formats
    // Demonstrates: Format conversion between TUUID, TGUID, and strings
    // =========================================================================
    App.Builder.MapGet<IResult>('/api/uuid/test',
      function: IResult
      var
        U: TUUID;
        Response: string;
        TimestampMs: Int64;
      begin
        U := TUUID.NewV7;
        TimestampMs := DateTimeToUnix(Now, False) * 1000 + MillisecondOf(Now);

        WriteLn('GET /api/uuid/test');
        WriteLn('  UUID v7:     ', U.ToString);
        WriteLn('  With braces: ', U.ToStringWithBraces);
        WriteLn('  As TGUID:    ', GUIDToString(U.ToGUID));

        Response := Format(
          '{"uuid_v7": "%s", "with_braces": "%s", "tguid": "%s", "timestamp_ms": %d}',
          [U.ToString, U.ToStringWithBraces, GUIDToString(U.ToGUID), TimestampMs]);

        Result := Results.Ok(Response);
      end);

    // =========================================================================
    // Example 6: Automatic TUUID Model Binding
    // Demonstrates: Framework binding TUUID directly from route parameter
    // =========================================================================
    App.Builder.MapGet<TUUID, IResult>('/api/uuid/lookup/{id}',
      function(Id: TUUID): IResult
      var
        Product: TProductDto;
      begin
        WriteLn('GET /api/uuid/lookup/{id} [AUTO-BOUND]');
        WriteLn('  Bound ID: ', Id.ToString);

        Product.Id := Id.ToGUID;
        Product.Name := 'Auto-Bound Product';
        Product.Price := 42.00;

        Result := Results.Ok<TProductDto>(Product);
      end);

    WriteLn('Endpoints:');
    WriteLn('  GET    /api/products/{id}       - Get product by UUID (manual)');
    WriteLn('  POST   /api/products            - Create product (UUID in body)');
    WriteLn('  PUT    /api/products/{id}       - Update product (validate IDs)');
    WriteLn('  POST   /api/products/generate   - Generate new UUID v7');
    WriteLn('  GET    /api/uuid/test           - Test UUID formats');
    WriteLn('  GET    /api/uuid/lookup/{id}    - Get by UUID (auto-bound)');
    WriteLn;
    WriteLn('=========================================');
    WriteLn('Server running on http://localhost:8080');
    WriteLn('=========================================');
    WriteLn;
    WriteLn('Press Enter to stop the server...');
    WriteLn;

    App.Run(8080);

    if not FindCmdLineSwitch('no-wait', True) then
      ReadLn;

    WriteLn('[OK] Server stopped successfully');

  except
    on E: Exception do
    begin
      WriteLn('[ERROR] ', E.Message);
      if not FindCmdLineSwitch('no-wait', True) then
      begin
        WriteLn('Press Enter to exit...');
        ReadLn;
      end;
    end;
  end;
end.
