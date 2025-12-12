unit Dashboard.Endpoints;

interface

uses
  Dext,
  Dext.Web,
  Dext.Persistence,
  DbContext,
  Customer, Order, // Feature units
  System.IOUtils, // Added for TFile
  System.SysUtils;

type
  TDashboardEndpoints = class
  public
    class procedure Map(App: TDextAppBuilder);
  private
    class function CheckAuth(Context: IHttpContext): Boolean;
  end;

implementation

uses
  AppResponseConsts;

function GetFilePath(const RelativePath: string): string;
begin
  // Executable runs from Output\ directory, files are in Dext.Starter.Admin\
  Result := IncludeTrailingPathDelimiter(GetCurrentDir) + '..\Dext.Starter.Admin\' + RelativePath;
end;

{ TDashboardEndpoints }

class function TDashboardEndpoints.CheckAuth(Context: IHttpContext): Boolean;
begin
  Result := (Context.User <> nil) and 
            (Context.User.Identity <> nil) and 
            (Context.User.Identity.IsAuthenticated);
            
  if not Result then
    Context.Response.StatusCode := 401;
end;

class procedure TDashboardEndpoints.Map(App: TDextAppBuilder);
begin
  // Root path
  App.MapGet('/',
    procedure(Context: IHttpContext)
    begin
      // TODO: Consider separating logic for index.html loading
      var Res: IResult := TContentResult.Create(TFile.ReadAllText(GetFilePath('wwwroot\index.html')), 'text/html');
      Res.Execute(Context);
    end);

  // Dashboard fragment
  App.MapGet('/dashboard',
    procedure(Context: IHttpContext)
    begin
      if not CheckAuth(Context) then Exit;
      var Res: IResult := TContentResult.Create(TFile.ReadAllText(GetFilePath('wwwroot\views\dashboard_fragment.html')), 'text/html');
      Res.Execute(Context);
    end);

  // Dashboard stats - Injected TAppDbContext
  App.MapGet<TAppDbContext, IHttpContext>('/dashboard/stats',
    procedure(Db: TAppDbContext; Context: IHttpContext)
    begin
      if not CheckAuth(Context) then Exit;
      
      var TotalCustomers := Db.Entities<TCustomer>.List.Count; 
      
      var Orders := Db.Entities<TOrder>.List;
      var TotalSales: Currency := 0;
      for var O in Orders do
        TotalSales := TotalSales + O.Total;
        
      var Html := Format(HTML_DASHBOARD_STATS, [TotalCustomers, TotalSales]);

      var Res: IResult := TContentResult.Create(Html, 'text/html');
      Res.Execute(Context);
    end);

  // Dashboard chart data
  App.MapGet('/dashboard/chart',
    procedure(Context: IHttpContext)
    begin
      if not CheckAuth(Context) then Exit;
      var Res := Results.Json(JSON_DASHBOARD_CHART);
      Res.Execute(Context);
    end);
end;

end.
