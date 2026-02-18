program TestProceduresIntegration;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Rtti,
  System.Generics.Collections,
  Data.DB,
  Dext.Entity,
  Dext.Entity.Attributes,
  Dext.Entity.Context,
  Dext.Entity.Dialects,
  Dext.Entity.Drivers.Interfaces,
  Dext.Mocks,
  Dext.Mocks.Matching,
  Dext.Utils;

type
  [StoredProcedure('GetEmployeeSalary')]
  TEmployeeSalaryDTO = class
  private
    FEmpId: Integer;
    FSalary: Double;
    FBonus: Double;
  public
    [DbParam(ptInput)]
    property EmpId: Integer read FEmpId write FEmpId;
    
    [DbParam(ptOutput)]
    property Salary: Double read FSalary write FSalary;
    
    [DbParam(ptOutput, 'p_bonus')]
    property Bonus: Double read FBonus write FBonus;
  end;

procedure TestProcedureMapping;
var
  ConnMock: Mock<IDbConnection>;
  CmdMock: Mock<IDbCommand>;
  Ctx: TDbContext;
  DTO: TEmployeeSalaryDTO;
begin
  WriteLn('Testing Procedure Mapping RTTI...');
  
  ConnMock := Mock<IDbConnection>.Create;
  CmdMock := Mock<IDbCommand>.Create;

  // Setup connection
  ConnMock.Setup.Returns(TValue.From<TDatabaseDialect>(ddSQLServer)).When.GetDialect;
  ConnMock.Setup.Returns(True).When.IsConnected;
  ConnMock.Setup.Returns(TValue.From<IDbCommand>(CmdMock.Instance)).When.CreateCommand(Arg.Any<string>);

  // Setup command to return output values
  CmdMock.Setup.Returns(TValue.From<Double>(5000.0)).When.GetParamValue('Salary');
  CmdMock.Setup.Returns(TValue.From<Double>(1000.0)).When.GetParamValue('p_bonus');

  Ctx := TDbContext.Create(ConnMock.Instance, TSQLServerDialect.Create, nil);
  try
    DTO := TEmployeeSalaryDTO.Create;
    try
      DTO.EmpId := 123;
      
      Ctx.ExecuteProcedure(DTO);
      
      if (DTO.Salary = 5000.0) and (DTO.Bonus = 1000.0) then
        WriteLn('   ✅ DTO Mapping Success')
      else
        WriteLn('   ❌ DTO Mapping Failed. Salary: ' + FloatToStr(DTO.Salary) + ' Bonus: ' + FloatToStr(DTO.Bonus));
        
    finally
      DTO.Free;
    end;
  finally
    Ctx.Free;
  end;
end;

begin
  SetConsoleCharset;
  try
    TestProcedureMapping;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ConsolePause;
end.
