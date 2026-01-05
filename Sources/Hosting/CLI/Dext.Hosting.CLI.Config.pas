unit Dext.Hosting.CLI.Config;

interface

uses
  System.SysUtils, System.JSON, System.IOUtils, System.Generics.Collections;

type
  TDextTestConfig = record
    Project: string;
    ReportDir: string;
    CoverageExclude: TArray<string>;
    CoverageThreshold: Double;
  end;

  TDextConfig = class
  private
    FTest: TDextTestConfig;
    function LoadTestConfig(Json: TJSONObject): TDextTestConfig;
  public
    constructor Create;
    procedure LoadFromFile(const FileName: string);
    
    property Test: TDextTestConfig read FTest;
  end;

implementation

{ TDextConfig }

constructor TDextConfig.Create;
begin
  // Defaults
  FTest.Project := '';
  FTest.ReportDir := 'TestOutput\report'; 
  FTest.CoverageExclude := [];
  FTest.CoverageThreshold := 0;
end;

procedure TDextConfig.LoadFromFile(const FileName: string);
var
  Content: string;
  VerifyJson, Section: TJSONValue;
  MainObj: TJSONObject;
begin
  if not FileExists(FileName) then Exit;
  
  Content := TFile.ReadAllText(FileName);
  VerifyJson := TJSONObject.ParseJSONValue(Content);
  if VerifyJson = nil then Exit; // Or parse error
  
  try
    if VerifyJson is TJSONObject then
    begin
      MainObj := VerifyJson as TJSONObject;
      if MainObj.TryGetValue('test', Section) and (Section is TJSONObject) then
        FTest := LoadTestConfig(Section as TJSONObject);
    end;
  finally
    VerifyJson.Free;
  end;
end;

function TDextConfig.LoadTestConfig(Json: TJSONObject): TDextTestConfig;
var
  Val: TJSONValue;
  Arr: TJSONArray;
  I: Integer;
begin
  Result := FTest; // Start with current/defaults
  
  if Json.TryGetValue('project', Val) then
    Result.Project := Val.Value;
    
  if Json.TryGetValue('report_dir', Val) then
    Result.ReportDir := Val.Value;

  if Json.TryGetValue('coverage', Val) and (Val is TJSONObject) then
  begin
    var CovObj := Val as TJSONObject;
    if CovObj.TryGetValue('exclude', Arr) then
    begin
      SetLength(Result.CoverageExclude, Arr.Count);
      for I := 0 to Arr.Count - 1 do
        Result.CoverageExclude[I] := Arr.Items[I].Value;
    end;
    
    if CovObj.TryGetValue('threshold', Val) and (Val is TJSONNumber) then
       Result.CoverageThreshold := (Val as TJSONNumber).AsDouble;
  end;
end;

end.
