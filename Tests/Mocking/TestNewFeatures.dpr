program TestNewFeatures;

{$APPTYPE CONSOLE}
{$RTTI EXPLICIT METHODS([vcPublic, vcPublished, vcProtected])}

uses
  Dext.MM,
  Dext.Utils,
  System.SysUtils,
  System.IOUtils,
  Dext.Assertions,
  Dext.Mocks,
  Dext.Mocks.Auto,
  Dext.Testing.Console;

type
  {$M+}
  IDependency = interface
    ['{89107954-2D44-4286-9790-2B16B45A2576}']
    function GetValue: Integer;
  end;
  {$M-}

  TSystemUnderTest = class
  private
    FDep: IDependency;
  public
    constructor Create(Dep: IDependency);
    property Dep: IDependency read FDep;
    function DoWork: Integer;
  end;

  TCustomerRepository = class
  public
    function GetCount: Integer; virtual;
  end;

  TServiceWithClass = class
  private
    FRepo: TCustomerRepository;
  public
    constructor Create(Repo: TCustomerRepository);
    destructor Destroy; override;
    function GetCountX2: Integer;
    property Repo: TCustomerRepository read FRepo;
  end;

{ TSystemUnderTest }

constructor TSystemUnderTest.Create(Dep: IDependency);
begin
  FDep := Dep;
end;

function TSystemUnderTest.DoWork: Integer;
begin
  Result := FDep.GetValue * 2;
end;

{ TCustomerRepository }

function TCustomerRepository.GetCount: Integer;
begin
  Result := 0;
end;

{ TServiceWithClass }

constructor TServiceWithClass.Create(Repo: TCustomerRepository);
begin
  FRepo := Repo;
end;

destructor TServiceWithClass.Destroy;
begin
  FRepo.Free; // SUT owns dependency
  inherited;
end;

function TServiceWithClass.GetCountX2: Integer;
begin
  Result := FRepo.GetCount * 2;
end;

{ Tests }

procedure TestAutoMocker;
var
  Mocker: TAutoMocker;
  Sut: TSystemUnderTest;
begin
  Mocker := TAutoMocker.Create;
  try
    Sut := Mocker.CreateInstance<TSystemUnderTest>;
    try
      Should(Sut).NotBeNil;
      Should(Sut.Dep).NotBeNil;
      
      // Setup Mock
      Mocker.GetMock<IDependency>.Setup.Returns(21).When.GetValue;
      
      // Execute
      var Res := Sut.DoWork;
      
      Should(Res).Be(42);
    finally
      Sut.Free;
    end;
  finally
    Mocker.Free;
  end;
end;


procedure TestClassMocking;
var
  MockRepo: Mock<TCustomerRepository>;
begin
  MockRepo := Mock<TCustomerRepository>.Create;
  MockRepo.Setup.Returns(100).When.GetCount;
  
  var Res := MockRepo.Instance.GetCount;
  Should(Res).Be(100);
end;

procedure TestAutoMockerWithClass;
var
  Mocker: TAutoMocker;
  Svc: TServiceWithClass;
begin
  Mocker := TAutoMocker.Create;
  try
    Svc := Mocker.CreateInstance<TServiceWithClass>;
    Should(TObject(Svc)).NotBeNil;
    Should(TObject(Svc.Repo)).NotBeNil;
    
    // Setup Class Mock behavior
    Mocker.GetMock<TCustomerRepository>.Setup.Returns(50).When.GetCount;
    
    Should(Svc.GetCountX2).Be(100);
    
    Svc.Free; // Destroys Repo too
  finally
    Mocker.Free;
  end;
end;

procedure TestSnapshot;
var
  JsonStr: string;
begin
  // Test String Snapshot
  JsonStr := '{"name": "John", "age": 30}';
  
  // We expect this to create the file if missing
  Should(JsonStr).MatchSnapshot('UserSnapshot');
  
  // Check file exists
  var Path := TPath.Combine(TPath.Combine(ExtractFilePath(ParamStr(0)), 'Snapshots'), 'UserSnapshot.json');
  if not FileExists(Path) then
    raise Exception.Create('Snapshot file not created at ' + Path);
    
  // Test match again
  Should(JsonStr).MatchSnapshot('UserSnapshot');
end;

begin
  try
    TTestRunner.Run('AutoMocker (Interface)', TestAutoMocker);
    TTestRunner.Run('Class Mocking', TestClassMocking);
    TTestRunner.Run('AutoMocker (Class)', TestAutoMockerWithClass);
    TTestRunner.Run('Snapshot Testing', TestSnapshot);
    
    TTestRunner.PrintSummary;
  except
    on E: Exception do
      WriteLn('Unhandled Exception: ', E.ClassName, ': ', E.Message);
  end;
  ConsolePause;
end.
