unit Sales.Endpoints;

{***************************************************************************}
{                                                                           }
{           Sales System - API Endpoints                                    }
{                                                                           }
{           All API route definitions are centralized here                  }
{                                                                           }
{***************************************************************************}

interface

uses
  Dext.Collections,
  Dext.Web.Results,
  Dext.Web.DataApi,
  Dext,
  Dext.Entity,
  Dext.Web;

type
  /// <summary>
  ///   Centralized endpoint registration for the Sales System API.
  ///   Separates route definitions from application configuration.
  /// </summary>
  TSalesEndpoints = class
  public
    /// <summary>
    ///   Maps all API endpoints to the application builder.
    /// </summary>
    class procedure MapEndpoints(const Builder: TAppBuilder); static;
  end;

type
  { Health Check DTO }
  THealthStatus = record
    status: string;
    timestamp: string;
  end;

  { Order DTOs }
  TOrderItemDto = record
    productId: Integer;
    quantity: Integer;
  end;

  {$DEFINE MODEL_BINDING_ARRAY}
  TOrderItemList = {$IFDEF MODEL_BINDING_ARRAY}TArray<TOrderItemDto>{$ELSE}IList<TOrderItemDto>{$ENDIF};

  { TCreateOrderDto como CLASSE para forçar o uso do BindBody do Dext.
    Agora usando IList<T> novamente pois o framework suporta o fallback para TSmartList<T>! }
  TCreateOrderDto = class
  private
    FItems: TOrderItemList;
  public
    property Items: TOrderItemList read FItems write FItems;
  end;

  {$IFDEF MODEL_BINDING_ARRAY}
  TArrayHelper = record helper for TArray<TOrderItemDto>
  public
    function Count: Integer;
  end;
  {$ENDIF}

implementation

uses
  System.SysUtils,
  Sales.Auth,
  Sales.Data.Context,
  Sales.Domain.Entities,
  Sales.Domain.Models,
  Sales.Domain.Enums;

{ TSalesEndpoints }

class procedure TSalesEndpoints.MapEndpoints(const Builder: TAppBuilder);
begin
  // ==========================================================================
  // PUBLIC ENDPOINTS
  // ==========================================================================
  
  // Health Check
  Builder.MapGet<IResult>('/health', 
    function: IResult
    begin
      var Status: THealthStatus;
      Status.status := 'healthy';
      Status.timestamp := DateTimeToStr(Now);
      Result := Results.Ok(Status);
    end); 

  // Authentication
  Builder.MapPost<TLoginRequest, IAuthService, IResult>('/auth/login',
    function(Req: TLoginRequest; Auth: IAuthService): IResult
    begin
      var Token := Auth.Login(Req.username, Req.password);
      if Token = '' then
        Exit(Results.StatusCode(401)); 
        
      var Resp: TLoginResponse;
      Resp.token := Token;
      Result := Results.Ok(Resp);
    end);

  // ==========================================================================
  // PROTECTED ENDPOINTS (Require Authentication)
  // ==========================================================================
  
  // DataApi - Customers (Auto-generated CRUD)
  TDataApiHandler<TCustomer>.Map(Builder, '/api/customers',
    TDataApiOptions<TCustomer>.Create
      .DbContext<TSalesDbContext>
      .UseSnakeCase
      .Tag('Customers (Query)'));

  // DataApi - Products (Auto-generated CRUD)
  TDataApiHandler<TProduct>.Map(Builder, '/api/products',
    TDataApiOptions<TProduct>.Create
      .DbContext<TSalesDbContext>
      .UseSnakeCase
      .Tag('Products (Query)'));

  // ==========================================================================
  // CQRS COMMAND - Order Creation
  // ==========================================================================
  Builder.MapPost<IHttpContext, TSalesDbContext, TCreateOrderDto, IResult>('/api/orders',
    function(Context: IHttpContext; Db: TSalesDbContext; Dto: TCreateOrderDto): IResult
    begin
      // Note: Dto is automatically freed by the framework after handler execution
      if (Dto = nil) then
        Exit(Results.BadRequest('Erro na leitura do corpo da requisicao.'));

      if (Dto.items.Count = 0) then
        Exit(Results.BadRequest('O pedido deve conter pelo menos um item.'));

      var OrderEntity := TOrder.Create;
      try
        OrderEntity.Status := TOrderStatus.Draft;
        OrderEntity.CustomerId := 1; 
        
        var Handler := TOrderModel.Create(OrderEntity);
        try
          try
            Writeln(Format('[DEBUG] Processing Order with %d items', [Dto.items.Count]));
            for var ItemDto in Dto.items do
            begin
              Writeln(Format('[DEBUG] Item: ProductId=%d, Quantity=%d', [ItemDto.productId, ItemDto.quantity]));
              var ValidProd := Db.Products.Find(ItemDto.productId);
              if ValidProd = nil then
              begin
                Writeln(Format('[DEBUG] Product %d NOT FOUND in database', [ItemDto.productId]));
                Exit(Results.BadRequest('Produto nao encontrado: ' + IntToStr(ItemDto.productId)));
              end;
              
              Handler.AddItem(ValidProd, ItemDto.quantity);
            end;

            Handler.Submit;

            Db.Orders.Add(Handler.Entity);
            Db.SaveChanges;

            Result := Results.Created('/api/orders/' + Handler.Entity.Id.Value.ToString, Handler.Entity);
          except
            on E: EDomainError do Result := Results.BadRequest(E.Message);
            on E: Exception do Result := Results.InternalServerError(E.Message);
          end;
        finally
          Handler.Free;
        end;
      except
        OrderEntity.Free; 
        raise;
      end;
    end);

  // List Orders
  Builder.MapGet<TSalesDbContext, IResult>('/api/orders',
    function(Db: TSalesDbContext): IResult
    begin
      var Orders := Db.Orders.QueryAll.OrderBy(TOrder.Props.CreatedAt.Desc).ToList;
      Result := Results.Ok(Orders);
    end); 
end;

{$IFDEF MODEL_BINDING_ARRAY}
function TArrayHelper.Count: Integer;
begin
  if Self = nil then
    Result := 0
  else
    Result := Length(Self);
end;
{$ENDIF}

end.
