unit Dext.Testing.History;

interface

uses
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  System.IOUtils,
  System.JSON, // Using System.JSON for simple array manipulation
  Dext.Testing.Runner; // For TTestSummary

type
  TTestHistoryManager = class
  public
    const DEFAULT_FILENAME = 'dext_test_history.json';
    
    /// <summary>
    ///   Appends the current test run summary to the history file.
    /// </summary>
    class procedure AppendRun(const Summary: TTestSummary; const FileName: string = DEFAULT_FILENAME);
    
    /// <summary>
    ///   Loads the history as a JSON string.
    /// </summary>
    class function LoadHistoryJson(const FileName: string = DEFAULT_FILENAME): string;
  end;

implementation

{ TTestHistoryManager }

class procedure TTestHistoryManager.AppendRun(const Summary: TTestSummary;
  const FileName: string);
var
  JsonArray: TJSONArray;
  NewEntry: TJSONObject;
  Content: string;
  FullFileName: string;
begin
  FullFileName := TPath.GetFullPath(FileName);
  
  if TFile.Exists(FullFileName) then
  begin
    Content := TFile.ReadAllText(FullFileName);
    try
      JsonArray := TJSONObject.ParseJSONValue(Content) as TJSONArray;
    except
      JsonArray := nil;
    end;
  end
  else
    JsonArray := nil;

  if JsonArray = nil then
    JsonArray := TJSONArray.Create;

  try
    NewEntry := TJSONObject.Create;
    NewEntry.AddPair('date', DateToISO8601(Now, False));
    NewEntry.AddPair('total', TJSONNumber.Create(Summary.TotalTests));
    NewEntry.AddPair('passed', TJSONNumber.Create(Summary.Passed));
    NewEntry.AddPair('failed', TJSONNumber.Create(Summary.Failed));
    NewEntry.AddPair('skipped', TJSONNumber.Create(Summary.Skipped));
    NewEntry.AddPair('duration', TJSONNumber.Create(Round(Summary.TotalDuration.TotalMilliseconds)));
    
    JsonArray.AddElement(NewEntry);
    
    // Limit history size? Let's keep last 50 runs for now
    while JsonArray.Count > 50 do
      JsonArray.Remove(0);
      
    TFile.WriteAllText(FullFileName, JsonArray.ToJSON);
  finally
    JsonArray.Free;
  end;
end;

class function TTestHistoryManager.LoadHistoryJson(const FileName: string): string;
var
  FullFileName: string;
begin
  FullFileName := TPath.GetFullPath(FileName);
  if TFile.Exists(FullFileName) then
    Result := TFile.ReadAllText(FullFileName)
  else
    Result := '[]';
end;

end.
