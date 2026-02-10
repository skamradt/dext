program DesktopMVVMCustomerCRUD;

uses
  Dext.MM,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  Customer.Entity in 'Features\Customers\Customer.Entity.pas',
  Customer.Service in 'Features\Customers\Customer.Service.pas',
  Customer.ViewModel in 'Features\Customers\Customer.ViewModel.pas',
  Customer.Controller in 'Features\Customers\Customer.Controller.pas',
  Customer.Rules in 'Features\Customers\Customer.Rules.pas',
  Customer.Context in 'Data\Customer.Context.pas',
  Main.Form in 'Features\Layout\Main.Form.pas' {MainForm},
  Customer.List in 'Features\Customers\Customer.List.pas' {CustomerListFrame: TFrame},
  Customer.Edit in 'Features\Customers\Customer.Edit.pas' {CustomerEditFrame: TFrame},
  App.Startup in 'App\App.Startup.pas';

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
  // Inject controller (Navigator is created internally by MainForm)
    MainForm.InjectDependencies(TAppStartup.GetCustomerController);
    
    Application.Run;
  finally
    // Free MainForm first to release interface references
    MainForm.Free;
    TAppStartup.Shutdown;
  end;
end.
