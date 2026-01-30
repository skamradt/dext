unit DextFood.Hubs;

interface

uses
  Dext.Web.Hubs;

type
  /// <summary>
  /// Hub para notificações de pedidos em tempo real via SSE.
  /// </summary>
  TOrderHub = class(THub)
  end;

implementation

end.
