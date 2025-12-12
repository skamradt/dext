unit Dashboard.Service;

interface

uses
  DbContext,
  Customer,
  Order,
  System.SysUtils;

type
  TDashboardStats = record
    TotalCustomers: Integer;
    TotalSales: Currency;
  end;

  IDashboardService = interface
    ['{B1C2D3E4-F5A6-47B8-9C0D-1E2F3A4B5C6D}']
    function GetStats: TDashboardStats;
    function GetChartDataJson: string;
  end;

  TDashboardService = class(TInterfacedObject, IDashboardService)
  private
    FDb: TAppDbContext;
  public
    constructor Create(Db: TAppDbContext);
    function GetStats: TDashboardStats;
    function GetChartDataJson: string;
  end;

implementation

uses
  Dext.Collections,
  AppResponseConsts;

{ TDashboardService }

constructor TDashboardService.Create(Db: TAppDbContext);
begin
  FDb := Db;
end;

function TDashboardService.GetStats: TDashboardStats;
var
  Orders: IList<TOrder>;
  O: TOrder;
begin
  Result.TotalCustomers := FDb.Entities<TCustomer>.List.Count;
  
  Orders := FDb.Entities<TOrder>.List;
  Result.TotalSales := 0;
  for O in Orders do
    Result.TotalSales := Result.TotalSales + O.Total;
end;

function TDashboardService.GetChartDataJson: string;
begin
  Result := JSON_DASHBOARD_CHART;
end;

end.
