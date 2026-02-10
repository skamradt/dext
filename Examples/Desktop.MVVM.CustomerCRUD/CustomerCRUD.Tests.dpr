{***************************************************************************}
{                                                                           }
{           Dext Framework - Example                                        }
{                                                                           }
{           CustomerCRUD Unit Tests                                         }
{                                                                           }
{***************************************************************************}
program CustomerCRUD.Tests;

{$APPTYPE CONSOLE}

uses
  Dext.MM,
  System.SysUtils,
  System.Rtti,
  Dext.Utils,
  Dext.Collections,
  Customer.Entity in 'Features\Customers\Customer.Entity.pas',
  Customer.Service in 'Features\Customers\Customer.Service.pas',
  Customer.Controller in 'Features\Customers\Customer.Controller.pas',
  Customer.ViewModel in 'Features\Customers\Customer.ViewModel.pas',
  Customer.Rules in 'Features\Customers\Customer.Rules.pas',
  Dext,
  Dext.Assertions,
  Dext.Mocks,
  Dext.Mocks.Matching,
  Dext.Testing.Attributes,
  Dext.Testing.Runner,
  Dext.Testing.Fluent;

type
  /// <summary>
  /// Tests for TCustomerRules - Business validation
  /// </summary>
  [TestFixture('Customer Rules')]
  TCustomerRulesTests = class
  public
    // Name validation
    [Test]
    procedure TestValidateName_Empty_Fails;
    
    [Test]
    procedure TestValidateName_TooShort_Fails;
    
    [Test]
    procedure TestValidateName_Valid_Passes;
    
    // Email validation
    [Test]
    procedure TestValidateEmail_Empty_Fails;
    
    [Test]
    procedure TestValidateEmail_InvalidFormat_Fails;
    
    [Test]
    procedure TestValidateEmail_Valid_Passes;
    
    // Phone validation
    [Test]
    procedure TestValidatePhone_Empty_Passes;
    
    [Test]
    procedure TestValidatePhone_InvalidFormat_Fails;
    
    [Test]
    procedure TestValidatePhone_Valid_Passes;
    
    // Document validation
    [Test]
    procedure TestValidateDocument_Empty_Passes;
    
    [Test]
    procedure TestValidateDocument_TooShort_Fails;
    
    [Test]
    procedure TestValidateDocument_Valid_Passes;
  end;

  /// <summary>
  /// Tests for TCustomerViewModel
  /// </summary>
  [TestFixture('Customer ViewModel')]
  TCustomerViewModelTests = class
  public
    [Test]
    procedure TestCreate_IsNew;
    
    [Test]
    procedure TestClear_ResetsToNew;
    
    [Test]
    procedure TestLoad_SetsProperties;
    
    [Test]
    procedure TestValidate_EmptyName_Fails;
    
    [Test]
    procedure TestValidate_ValidData_Passes;
    
    [Test]
    procedure TestReleaseOwnership;
  end;

  {$M+}
  /// <summary>
  /// Tests for TCustomerController with mocked dependencies
  /// </summary>
  [TestFixture('Customer Controller')]
  TCustomerControllerTests = class
  private
    FServiceMock: Mock<ICustomerService>;
    FLoggerMock: Mock<ILogger>;
    FViewMock: Mock<ICustomerView>;
    FController: ICustomerController;
  public
    [Setup]
    procedure Setup;
    
    [TearDown]
    procedure TearDown;
    
    [Test]
    procedure TestLoadCustomers_CallsService;
    
    [Test]
    procedure TestCreateNewCustomer_ShowsEditView;
    
    [Test]
    procedure TestCancelEdit_ShowsListView;
  end;
  {$M-}

{ TCustomerRulesTests }

procedure TCustomerRulesTests.TestValidateName_Empty_Fails;
var
  ErrorMsg: string;
begin
  Should(TCustomerRules.ValidateName('', ErrorMsg)).BeFalse;
  Should(ErrorMsg).Contain('required');
end;

procedure TCustomerRulesTests.TestValidateName_TooShort_Fails;
var
  ErrorMsg: string;
begin
  Should(TCustomerRules.ValidateName('Ab', ErrorMsg)).BeFalse;
  Should(ErrorMsg).Contain('at least');
end;

procedure TCustomerRulesTests.TestValidateName_Valid_Passes;
var
  ErrorMsg: string;
begin
  Should(TCustomerRules.ValidateName('John Doe', ErrorMsg)).BeTrue;
  Should(ErrorMsg).BeEmpty;
end;

procedure TCustomerRulesTests.TestValidateEmail_Empty_Fails;
var
  ErrorMsg: string;
begin
  Should(TCustomerRules.ValidateEmail('', ErrorMsg)).BeFalse;
  Should(ErrorMsg).Contain('required');
end;

procedure TCustomerRulesTests.TestValidateEmail_InvalidFormat_Fails;
var
  ErrorMsg: string;
begin
  Should(TCustomerRules.ValidateEmail('invalid-email', ErrorMsg)).BeFalse;
  Should(ErrorMsg).Contain('Invalid');
end;

procedure TCustomerRulesTests.TestValidateEmail_Valid_Passes;
var
  ErrorMsg: string;
begin
  Should(TCustomerRules.ValidateEmail('test@example.com', ErrorMsg)).BeTrue;
  Should(ErrorMsg).BeEmpty;
end;

procedure TCustomerRulesTests.TestValidatePhone_Empty_Passes;
var
  ErrorMsg: string;
begin
  // Phone is optional
  Should(TCustomerRules.ValidatePhone('', ErrorMsg)).BeTrue;
end;

procedure TCustomerRulesTests.TestValidatePhone_InvalidFormat_Fails;
var
  ErrorMsg: string;
begin
  Should(TCustomerRules.ValidatePhone('abc', ErrorMsg)).BeFalse;
  Should(ErrorMsg).Contain('Invalid');
end;

procedure TCustomerRulesTests.TestValidatePhone_Valid_Passes;
var
  ErrorMsg: string;
begin
  Should(TCustomerRules.ValidatePhone('11999998888', ErrorMsg)).BeTrue;
  Should(ErrorMsg).BeEmpty;
end;

procedure TCustomerRulesTests.TestValidateDocument_Empty_Passes;
var
  ErrorMsg: string;
begin
  // Document is optional
  Should(TCustomerRules.ValidateDocument('', ErrorMsg)).BeTrue;
end;

procedure TCustomerRulesTests.TestValidateDocument_TooShort_Fails;
var
  ErrorMsg: string;
begin
  Should(TCustomerRules.ValidateDocument('123', ErrorMsg)).BeFalse;
  Should(ErrorMsg).Contain('at least');
end;

procedure TCustomerRulesTests.TestValidateDocument_Valid_Passes;
var
  ErrorMsg: string;
begin
  Should(TCustomerRules.ValidateDocument('12345678900', ErrorMsg)).BeTrue;
  Should(ErrorMsg).BeEmpty;
end;

{ TCustomerViewModelTests }

procedure TCustomerViewModelTests.TestCreate_IsNew;
var
  VM: TCustomerViewModel;
begin
  VM := TCustomerViewModel.Create;
  try
    Should(VM.IsNew).BeTrue;
    Should(VM.Id).Be(0);
  finally
    VM.Free;
  end;
end;

procedure TCustomerViewModelTests.TestClear_ResetsToNew;
var
  VM: TCustomerViewModel;
  Customer: TCustomer;
begin
  VM := TCustomerViewModel.Create;
  try
    // Load existing customer
    Customer := TCustomer.Create;
    Customer.Id := 10;
    Customer.Name := 'Test';
    VM.Load(Customer, True);
    Should(VM.IsNew).BeFalse;
    
    // Clear should reset to new
    VM.Clear;
    Should(VM.IsNew).BeTrue;
    Should(VM.Id).Be(0);
  finally
    VM.Free;
  end;
end;

procedure TCustomerViewModelTests.TestLoad_SetsProperties;
var
  VM: TCustomerViewModel;
  Customer: TCustomer;
begin
  VM := TCustomerViewModel.Create;
  try
    Customer := TCustomer.Create;
    Customer.Id := 5;
    Customer.Name := 'Alice';
    Customer.Email := 'alice@test.com';
    
    VM.Load(Customer, True);
    
    Should(VM.Id).Be(5);
    Should(VM.Name).Be('Alice');
    Should(VM.Email).Be('alice@test.com');
  finally
    VM.Free;
  end;
end;

procedure TCustomerViewModelTests.TestValidate_EmptyName_Fails;
var
  VM: TCustomerViewModel;
begin
  VM := TCustomerViewModel.Create;
  try
    VM.Clear;
    VM.Name := '';
    VM.Email := 'test@test.com';
    
    Should(VM.Validate).BeFalse;
    Should(VM.Errors.Count).BeGreaterThan(0);
  finally
    VM.Free;
  end;
end;

procedure TCustomerViewModelTests.TestValidate_ValidData_Passes;
var
  VM: TCustomerViewModel;
begin
  VM := TCustomerViewModel.Create;
  try
    VM.Clear;
    VM.Name := 'Valid Name';
    VM.Email := 'valid@email.com';
    
    Should(VM.Validate).BeTrue;
    Should(VM.Errors.Count).Be(0);
  finally
    VM.Free;
  end;
end;

procedure TCustomerViewModelTests.TestReleaseOwnership;
var
  VM: TCustomerViewModel;
  Entity: TCustomer;
begin
  VM := TCustomerViewModel.Create;
  try
    VM.Clear; // Creates owned entity
    Entity := VM.GetEntity;
    VM.ReleaseOwnership;
    // After release, VM won't free the entity on destroy
    // We need to free it manually
    Entity.Free;
  finally
    VM.Free;
  end;
  // If we get here without AV, ownership release worked
  Should(True).BeTrue;
end;

{ TCustomerControllerTests }

procedure TCustomerControllerTests.Setup;
begin
  FServiceMock := Mock<ICustomerService>.Create;
  FLoggerMock := Mock<ILogger>.Create;
  FViewMock := Mock<ICustomerView>.Create;
  
  // Setup logger to accept any calls - Returns(TValue.Empty) for void/nil returns
  FLoggerMock.Setup.Returns(TValue.Empty).When.Info(Arg.Any<string>, []);
  FLoggerMock.Setup.Returns(TValue.Empty).When.Debug(Arg.Any<string>, []);
  FLoggerMock.Setup.Returns(TValue.Empty).When.Warn(Arg.Any<string>, []);
  
  FController := TCustomerController.Create(FServiceMock.Instance, FLoggerMock.Instance);
  FController.View := FViewMock.Instance;

  // Setup ViewMock to free the ViewModel to prevent memory leaks in tests
  FViewMock.Setup.Callback(procedure(Args: TArray<TValue>)
    begin
      if Length(Args) > 0 then
        Args[0].AsType<TCustomerViewModel>.Free;
    end).When.ShowEditView(Arg.Any<TCustomerViewModel>);
end;

procedure TCustomerControllerTests.TearDown;
begin
  FController := nil;
end;

procedure TCustomerControllerTests.TestLoadCustomers_CallsService;
var
  Customers: IList<TCustomer>;
begin
  // Arrange
  Customers := TCollections.CreateList<TCustomer>;
  FServiceMock.Setup.Returns(TValue.From<IList<TCustomer>>(Customers)).When.GetAll;
  
  // Act
  FController.LoadCustomers;
  
  // Assert
  FServiceMock.Received.GetAll;
  FViewMock.Received.RefreshList(Arg.Any<IList<TCustomer>>);
end;

procedure TCustomerControllerTests.TestCreateNewCustomer_ShowsEditView;
begin
  // Act
  FController.CreateNewCustomer;
  
  // Assert
  FViewMock.Received.ShowEditView(Arg.Any<TCustomerViewModel>);
end;

procedure TCustomerControllerTests.TestCancelEdit_ShowsListView;
begin
  // Act
  FController.CancelEdit;
  
  // Assert
  FViewMock.Received.ShowListView;
end;

begin
  SetConsoleCharSet;
  try
    WriteLn;
    WriteLn('CustomerCRUD Unit Tests');
    WriteLn('=======================');
    WriteLn;
    
    if TTest.Configure
      .Verbose
      .RegisterFixtures([TCustomerRulesTests, TCustomerViewModelTests, TCustomerControllerTests])
      .ExportToJUnit('test-results.xml')
      .Run then
      ExitCode := 0
    else
      ExitCode := 1;
  except
    on E: Exception do
    begin
      WriteLn('FATAL ERROR: ', E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
  ConsolePause;
end.
