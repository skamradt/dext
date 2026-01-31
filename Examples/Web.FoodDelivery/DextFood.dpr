program DextFood;

{$APPTYPE CONSOLE}

uses
  Dext.MM, // Gerenciador de memória avançado (FastMM5)
  System.SysUtils,
  Dext.Web,
  DextFood.Startup in 'DextFood.Startup.pas',
  DextFood.Domain in 'DextFood.Domain.pas',
  DextFood.Services in 'DextFood.Services.pas',
  DextFood.Hubs in 'DextFood.Hubs.pas',
  DextFood.DbSeeder in 'DextFood.DbSeeder.pas';

begin
  try
    Writeln('🚀 Iniciando DextFood Backend...');
    
    // Instancia a aplicação Dext
    var App: IWebApplication := TDextApplication.Create;
    
    // Configura a aplicação via classe Startup
    App.UseStartup(TStartup.Create);
    
    // OBRIGATÓRIO para SQLite :memory: ou Seeding manual:
    // Construir os serviços antes de rodar o seeder, para que ele use o Provider final.
    var Provider := App.BuildServices;
    TDbSeeder.Seed(Provider);

    Writeln('🌐 Servidor ouvindo em: http://localhost:9000');
    Writeln('Endpoints disponíveis:');
    Writeln('  GET  /health');
    Writeln('  POST /api/orders');
    Writeln('  GET  /api/orders/high-value');
    Writeln;
    Writeln('Pressione Enter para encerrar.');
    
    // Inicia o servidor na porta 9000
    App.Run(9000);
    
  except
    on E: Exception do
      Writeln('❌ Erro crítico: ', E.ClassName, ': ', E.Message);
  end;
end.

