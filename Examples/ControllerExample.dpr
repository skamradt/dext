program ControllerExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Dext.Core.Routing,
  Dext.Core.WebApplication,
  Dext.DI.Interfaces,
  Dext.DI.Extensions,
  Dext.Http.Interfaces,
  Dext.Core.Controllers;

type
  // Service Interface
  IGreetingService = interface
    ['{A1B2C3D4-E5F6-7890-1234-567890ABCDEF}']
    function GetGreeting(const Name: string): string;
  end;

  // Service Implementation
  TGreetingService = class(TInterfacedObject, IGreetingService)
  public
    function GetGreeting(const Name: string): string;
  end;

  // Controller Class (Instance-based with DI)
  [DextController('/api/greet')]
  TGreetingController = class
  private
    FService: IGreetingService;
  public
    // Constructor Injection!
    constructor Create(AService: IGreetingService);

    [DextGet('/{name}')]
    procedure GetGreeting(Ctx: IHttpContext; const Name: string);
  end;

{ TGreetingService }

function TGreetingService.GetGreeting(const Name: string): string;
begin
  Result := Format('Hello, %s! Welcome to Dext Controllers.', [Name]);
end;

{ TGreetingController }

constructor TGreetingController.Create(AService: IGreetingService);
begin
  FService := AService;
end;

procedure TGreetingController.GetGreeting(Ctx: IHttpContext; const Name: string);
begin
  var Message := FService.GetGreeting(Name);
  Ctx.Response.Json(Format('{"message": "%s"}', [Message]));
end;

begin
  try
    WriteLn('🚀 Starting Dext Controller Example...');
    
    var App := TDextApplication.Create;
    
    // Register services
    TServiceCollectionExtensions.AddSingleton<IGreetingService, TGreetingService>(App.Services);
    TServiceCollectionExtensions.AddControllers(App.Services);
    
    // Map controllers
    App.MapControllers;
    
    // Run
    App.Run(8080);
  except
    on E: Exception do
      Writeln('❌ Error: ', E.ClassName, ': ', E.Message);
  end;
end.
