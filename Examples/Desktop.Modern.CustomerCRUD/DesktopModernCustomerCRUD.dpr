program DesktopModernCustomerCRUD;

uses
  Dext.MM,
  Vcl.Forms,
  Main.Form in 'Views\Main.Form.pas' {MainForm},
  Customer.Entity in 'Entities\Customer.Entity.pas',
  Customer.Service in 'Services\Customer.Service.pas',
  Customer.ViewModel in 'ViewModels\Customer.ViewModel.pas',
  Customer.List in 'Views\Customer.List.pas' {CustomerListFrame: TFrame},
  Customer.Edit in 'Views\Customer.Edit.pas' {CustomerEditFrame: TFrame},
  App.Startup in 'App\App.Startup.pas',
  Vcl.Themes,
  Vcl.Styles,
  Customer.Context in 'Data\Customer.Context.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  
  // Configure DI Container
  TAppStartup.Configure;
  try
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.Title := 'Customer Management';
    TStyleManager.TrySetStyle('Windows11 Impressive Dark SE');
    Application.CreateForm(TMainForm, MainForm);
  // Inject dependencies into MainForm
    MainForm.InjectDependencies(
      TAppStartup.GetCustomerService,
      TAppStartup.GetLogger
    );
    
    Application.Run;
  finally
    // Free MainForm first to release interface references
    MainForm.Free;
    TAppStartup.Shutdown;
  end;
end.
