unit Customer.Endpoints;

interface

uses
  Dext,
  Dext.Web,
  Dext.Collections,
  Dext.Persistence,
  // DbContext, // Removed
  Customer,
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.NetEncoding;

type
  TCustomerEndpoints = class
  public
    class procedure Map(App: TDextAppBuilder);
  private
    class function GenerateCustomerRow(const C: TCustomer): string;
    class function GenerateCustomerForm(const C: TCustomer): string;
    class function CheckAuth(Context: IHttpContext): Boolean;
  end;

implementation

uses
  AppResponseConsts,
  Customer.Service; // Added

{ TCustomerEndpoints }

class function TCustomerEndpoints.CheckAuth(Context: IHttpContext): Boolean;
begin
  Result := (Context.User <> nil) and 
            (Context.User.Identity <> nil) and 
            (Context.User.Identity.IsAuthenticated);
            
  if not Result then
    Context.Response.StatusCode := 401;
end;

class function TCustomerEndpoints.GenerateCustomerRow(const C: TCustomer): string;
begin
  Result := Format(HTML_CUSTOMER_ROW,
    [C.Id, C.Id, C.Name, C.Email, C.TotalSpent, C.Id, C.Id]);
end;

class function TCustomerEndpoints.GenerateCustomerForm(const C: TCustomer): string;
var
  IsEdit: Boolean;
  Title, Action, Method, SwapTarget, SwapMode: string;
  NameValue, EmailValue, TotalSpentValue: string;
begin
  IsEdit := C <> nil;
  
  if IsEdit then
  begin
    Title := 'Edit Customer';
    Action := Format('/customers/%d', [C.Id]);
    Method := 'hx-put';
    SwapTarget := Format('#customer-row-%d', [C.Id]);
    SwapMode := 'outerHTML';
    NameValue := C.Name;
    EmailValue := C.Email;
    TotalSpentValue := FloatToStr(C.TotalSpent);
  end
  else
  begin
    Title := 'Add Customer';
    Action := '/customers/';
    Method := 'hx-post';
    SwapTarget := '#customers-table-body';
    SwapMode := 'beforeend';
    NameValue := '';
    EmailValue := '';
    TotalSpentValue := '0.00';
  end;
  
  Result := Format(HTML_CUSTOMER_FORM,
    [Title, Method, Action, SwapTarget, SwapMode, NameValue, EmailValue, TotalSpentValue]);
end;

class procedure TCustomerEndpoints.Map(App: TDextAppBuilder);
begin
  // GET /customers/ - List all customers
  App.MapGet<ICustomerService, IHttpContext>('/customers/',
    procedure(Service: ICustomerService; Context: IHttpContext)
    var
      Customers: IList<TCustomer>;
      Html: TStringBuilder;
      C: TCustomer;
    begin
      if not CheckAuth(Context) then Exit; // Auth Check
      
      Customers := Service.GetAll;
      
      Html := TStringBuilder.Create;
      try
        Html.Append(HTML_CUSTOMER_LIST_HEADER);
        for C in Customers do
          Html.Append(GenerateCustomerRow(C));
        Html.Append(HTML_CUSTOMER_LIST_FOOTER);
        
        var Res: IResult := TContentResult.Create(Html.ToString, 'text/html');
        Res.Execute(Context);
      finally
        Html.Free;
      end;
    end);

  // GET /customers/form
  App.MapGet('/customers/form',
    procedure(Context: IHttpContext)
    begin
      if not CheckAuth(Context) then Exit; // Auth Check
      var Html := GenerateCustomerForm(nil);
      var Res: IResult := TContentResult.Create(Html, 'text/html');
      Res.Execute(Context);
    end);

  // GET /customers/{id}/form
  App.MapGet<ICustomerService, IHttpContext>('/customers/{id}/form',
    procedure(Service: ICustomerService; Context: IHttpContext)
    var
      Id: Integer;
      C: TCustomer;
    begin
      if not CheckAuth(Context) then Exit; // Auth Check
      
      Id := StrToIntDef(Context.Request.RouteParams['id'], 0);
      
      if Id > 0 then
      begin
        C := Service.GetById(Id);
        if C <> nil then
        begin
          var Res: IResult := TContentResult.Create(GenerateCustomerForm(C), 'text/html');
          Res.Execute(Context);
          Exit;
        end;
      end;
      
      Context.Response.StatusCode := 404;
    end);

  // POST /customers/ - Add new customer
  App.MapPost<ICustomerService, IHttpContext>('/customers/',
    procedure(Service: ICustomerService; Context: IHttpContext)
    var
      C: TCustomer;
      Body, Name, Email, TotalSpentStr: string;
      Reader: TStreamReader;
    begin
      if not CheckAuth(Context) then Exit; // Auth Check
      
      // Manual Body Parsing (keeping logic for now)
      if Context.Request.Body <> nil then
      begin
        Context.Request.Body.Position := 0;
        Reader := TStreamReader.Create(Context.Request.Body, TEncoding.UTF8, False);
        try
          Body := Reader.ReadToEnd;
        finally
          Reader.Free;
        end;
      end;

      // Parse Form Data...
      Name := ''; Email := ''; TotalSpentStr := '0';
      var Params := Body.Split(['&']);
      for var Param in Params do
      begin
         var Parts := Param.Split(['=']);
         if Length(Parts) = 2 then
         begin
           var K := TNetEncoding.URL.Decode(Parts[0]);
           var V := TNetEncoding.URL.Decode(Parts[1]);
           if SameText(K, 'name') then Name := V
           else if SameText(K, 'email') then Email := V
           else if SameText(K, 'totalspent') then TotalSpentStr := V;
         end;
      end;

      C := TCustomer.Create;
      C.Name := Name;
      C.Email := Email;
      C.TotalSpent := StrToFloatDef(TotalSpentStr, 0);
      C.Status := TCustomerStatus.Active;
      
      Service.Add(C);
    
      var Html := GenerateCustomerRow(C);
      var Res: IResult := TContentResult.Create(Html, 'text/html');
      Res.Execute(Context);
      
      Context.Response.AddHeader('HX-Trigger', '{"closeModal": true, "showToast": {"message": "Customer added successfully", "type": "success"}}');
    end);

  // PUT /customers/{id}
  App.MapPut<ICustomerService, IHttpContext>('/customers/{id}',
    procedure(Service: ICustomerService; Context: IHttpContext)
    var
      Id: Integer;
      C: TCustomer;
      Body: string;
      Reader: TStreamReader;
    begin
      if not CheckAuth(Context) then Exit;
      
      Id := StrToIntDef(Context.Request.RouteParams['id'], 0);
      if Id > 0 then
      begin
        C := Service.GetById(Id);
        if C <> nil then
        begin
          if Context.Request.Body <> nil then
          begin
             Context.Request.Body.Position := 0;
             Reader := TStreamReader.Create(Context.Request.Body, TEncoding.UTF8, False);
             try Body := Reader.ReadToEnd; finally Reader.Free; end;
          end;

          var Params := Body.Split(['&']);
          for var Param in Params do
          begin
             var Parts := Param.Split(['=']);
             if Length(Parts)=2 then
             begin
               var K := TNetEncoding.URL.Decode(Parts[0]);
               var V := TNetEncoding.URL.Decode(Parts[1]);
               if SameText(K, 'name') then C.Name := V
               else if SameText(K, 'email') then C.Email := V
               else if SameText(K, 'totalspent') then C.TotalSpent := StrToFloatDef(V, 0);
             end;
          end;
          
          Service.Update(C);
          
          var Html := GenerateCustomerRow(C);
          var Res: IResult := TContentResult.Create(Html, 'text/html');
          Res.Execute(Context);
          Context.Response.AddHeader('HX-Trigger', '{"closeModal": true, "showToast": {"message": "Customer updated successfully", "type": "success"}}');
          Exit;
        end;
      end;
      Context.Response.StatusCode := 404;
    end);

  // DELETE /customers/{id}
  App.MapDelete<ICustomerService, IHttpContext>('/customers/{id}',
    procedure(Service: ICustomerService; Context: IHttpContext)
    var
      Id: Integer;
    begin
      if not CheckAuth(Context) then Exit; // Auth Check
      
      Id := StrToIntDef(Context.Request.RouteParams['id'], 0);
      
      if Id > 0 then
      begin
        // We don't check existence here as Service handles it, but typically endpoints check and return 404.
        // For brevity we assume success or 404 if service throws (not implemented).
        // Let's rely on basic "Try Delete" logic.
        // Assuming user wants 404 if not found? Service.Delete(Id) could handle it or return bool.
        // Service.Delete(Id) void -> we assume success.
        
        // Better: Check existence
        var C := Service.GetById(Id);
        if C <> nil then
        begin
             Service.Delete(Id);
             Results.Ok.Execute(Context);
             Exit;
        end;
      end;
      
      Results.NotFound.Execute(Context);
    end);
end;

end.
