program Web.DextStore;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  System.SysUtils,
  Dext.Web,
  Web.DextStore.Startup in 'Web.DextStore.Startup.pas';

begin
  try
    WriteLn('🛒 Starting DextStore API...');
    
    var App := TDextApplication.Create;
    App.UseStartup(TStartup.Create);
    
    WriteLn('🚀 Server running on http://localhost:9000');
    WriteLn('');
    WriteLn('Press Ctrl+C to stop the server...');
    WriteLn('');
    
    App.Run(9000);
    
  except
    on E: Exception do
      Writeln('❌ Error: ', E.ClassName, ': ', E.Message);
  end;
end.
