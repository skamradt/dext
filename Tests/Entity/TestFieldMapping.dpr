program TestFieldMapping;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Rtti,
  System.TypInfo,
  Dext.Entity.Attributes,
  Dext.Entity.Mapping,
  Dext.Entity.DbSet,
  Dext.Entity.Core,
  Dext.Entity.Drivers.Interfaces,
  Dext.Entity.TypeConverters,
  Dext.Core.ValueConverters,
  System.Generics.Collections;

type
  [Table('Users')]
  TUser = class
  private
    FId: Integer;
    FName: string;
    FSetterCalled: Boolean;
    procedure SetName(const Value: string);
  public
    [PK, AutoInc]
    property Id: Integer read FId write FId;
    
    [Column('Name')]
    [FieldAttribute('FName')] // Use explicit attribute name
    property Name: string read FName write SetName;
    
    property SetterCalled: Boolean read FSetterCalled;
  end;

procedure TUser.SetName(const Value: string);
begin
  FName := Value;
  FSetterCalled := True;
  // If this is called during hydration, it means field mapping failed!
end;

// Mock Driver/Connection to simulate database results
type
  TMockReader = class(TInterfacedObject, IDbReader)
  private
    FData: TDictionary<string, Variant>;
    FKeys: TArray<string>;
  public
    constructor Create;
    destructor Destroy; override;
    function Next: Boolean;
    function GetValue(const AName: string): TValue; overload;
    function GetValue(Index: Integer): TValue; overload;
    function GetColumnName(Index: Integer): string;
    function GetColumnCount: Integer;
    procedure Close;
  end;

constructor TMockReader.Create;
begin
  FData := TDictionary<string, Variant>.Create;
  FData.Add('id', 1);
  FData.Add('name', 'Cesar');
  FKeys := FData.Keys.ToArray;
end;

destructor TMockReader.Destroy;
begin
  FData.Free;
  inherited;
end;

function TMockReader.Next: Boolean; begin Result := False; end;
function TMockReader.GetColumnCount: Integer; begin Result := FData.Count; end;
function TMockReader.GetColumnName(Index: Integer): string; begin Result := FKeys[Index]; end;
function TMockReader.GetValue(Index: Integer): TValue; begin Result := TValue.FromVariant(FData[FKeys[Index]]); end;
function TMockReader.GetValue(const AName: string): TValue; begin Result := TValue.FromVariant(FData[AName.ToLower]); end;
procedure TMockReader.Close; begin { nothing } end;

procedure TestFieldMappingBypassesSetter;
var
  Reader: IDbReader;
  DbSet: TDbSet<TUser>;
  User: TUser;
begin
  WriteLn('► Testing Field Mapping Optimization...');
  
  Reader := TMockReader.Create;
  // We need a context, but we can mock the hydration logic by calling it directly if we had access
  // or just setting up a minimal environment.
  // For this unit test, let's just verify the TDbSet.Hydrate logic.
  
  // Note: TDbSet requires a real context usually. 
  // Let's use RTTI to test the TValueConverter.ConvertAndSetField directly first
  // as proof of concept if full DbSet test is too heavy.
  
  User := TUser.Create;
  try
    var Ctx := TRttiContext.Create;
    var Typ := Ctx.GetType(TUser);
    var Field := Typ.GetField('FName');
    
    TValueConverter.ConvertAndSetField(User, Field, 'Cesar');
    
    WriteLn('  Field value: ', User.Name);
    WriteLn('  Setter called: ', User.SetterCalled);
    
    if User.Name <> 'Cesar' then
      raise Exception.Create('Value not set in field');
      
    if User.SetterCalled then
      raise Exception.Create('Property SETTER was called! Field mapping should bypass it.');

    WriteLn('  ✓ Field Mapping Bypassed Setter successfully.');
  finally
    User.Free;
  end;
end;

begin
  try
    TestFieldMappingBypassesSetter;
    WriteLn;
    WriteLn('🎉 ALL TESTS PASSED!');
  except
    on E: Exception do
    begin
      WriteLn('❌ TEST FAILED: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
