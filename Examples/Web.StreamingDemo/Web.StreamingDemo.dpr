program Web.StreamingDemo;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Dext,
  Dext.Web,
  App.Startup in 'App.Startup.pas',
  Upload.Endpoints in 'Features\Upload\Upload.Endpoints.pas',
  Upload.Service in 'Features\Upload\Upload.Service.pas',
  Download.Endpoints in 'Features\Download\Download.Endpoints.pas',
  Download.Service in 'Features\Download\Download.Service.pas';

begin
  try
    var App: IWebApplication := TDextApplication.Create;
    App.UseStartup(TStartup.Create);
    App.Run(8080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
