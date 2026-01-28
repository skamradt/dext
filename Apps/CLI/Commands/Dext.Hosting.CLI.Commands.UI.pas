unit Dext.Hosting.CLI.Commands.UI;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.DateUtils,
  System.JSON,
  System.Types, // RT_RCDATA
{$IFDEF MSWINDOWS}
  Winapi.Windows,
  Winapi.ShellAPI,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Stdlib,
{$ENDIF}
  Dext.Web.Hubs.Extensions,
  Dext.Hosting.CLI.Args,
  Dext.WebHost,
  Dext.Web.Interfaces,
  Dext.DI.Interfaces,
  Dext.DI.Core,
  Dext.Hosting.CLI.Registry,
  Dext.Yaml,
  Dext.Hosting.CLI.Logger,
  Dext.Logging,
  Dext.Utils,
  Dext.Dashboard.Routes;

type
  TUICommand = class(TInterfacedObject, IConsoleCommand)
  private
    procedure OpenBrowser(const Url: string);
  public
    function GetName: string;
    function GetDescription: string;
    procedure Execute(const Args: TCommandLineArgs);
  end;

implementation



{ TUICommand }

function TUICommand.GetName: string;
begin
  Result := 'ui';
end;

function TUICommand.GetDescription: string;
begin
  Result := 'Launches the Web Configuration Dashboard. Usage: dext ui';
end;

procedure TUICommand.OpenBrowser(const Url: string);
begin
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', PChar(Url), nil, nil, SW_SHOWNORMAL);
{$ENDIF}
{$IFDEF POSIX}
  _system(PAnsiChar('open ' + AnsiString(Url))); 
{$ENDIF}
end;

procedure TUICommand.Execute(const Args: TCommandLineArgs);
var
  Port: Integer;
  Host: IWebHost;
begin
  Port := 3000;
  if Args.HasOption('port') then
    Port := StrToIntDef(Args.GetOption('port'), 3000);

  SafeWriteLn(Format('Starting Dext Dashboard (Material 3) on port %d...', [Port]));

  Host := TWebHostBuilder.CreateDefault(nil)
    .UseUrls(Format('http://localhost:%d', [Port]))
    .ConfigureServices(procedure(Services: IServiceCollection)
      var
        RegistryType: TServiceType;
        RegistryInstance: TObject;
        LoggerType: TServiceType;
        FactoryFunc: TFunc<IServiceProvider, TObject>;
      begin
        RegistryType := TServiceType.FromClass(TProjectRegistry);
        RegistryInstance := TProjectRegistry.Create;
        Services.AddSingleton(RegistryType, RegistryInstance);
        
        LoggerType := TServiceType.FromInterface(TypeInfo(ILoggerFactory));
        
        FactoryFunc := function(Provider: IServiceProvider): TObject
           var
             Factory: TLoggerFactory;
           begin
              Factory := TLoggerFactory.Create;
              Factory.AddProvider(TConsoleHubLoggerProvider.Create);
              Result := Factory;
           end;
           
        Services.AddSingleton(LoggerType, TClass(nil), FactoryFunc);
      end)
    .Configure(procedure(App: IApplicationBuilder)
      begin
        TDashboardRoutes.Configure(App);
      end)
    .Build;
  OpenBrowser(Format('http://localhost:%d', [Port]));
  Host.Run;
end;

end.
