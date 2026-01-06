unit Dext.Hosting.CLI.Tools.Sonar;

interface

uses
  System.SysUtils, 
  System.Classes, 
  System.IOUtils, 
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Variants,
  Xml.XMLIntf, 
  Xml.XMLDoc,
  Winapi.ActiveX,
  Dext.Utils;

type
  TSonarConverter = class
  private
    class procedure ConvertInternal(const DccXmlFile, SonarXmlFile, SourceDir: string; Threshold: Double);
  public
    class procedure Convert(const DccXmlFile, SonarXmlFile, SourceDir: string; Threshold: Double = 0);
  end;

implementation

class procedure TSonarConverter.Convert(const DccXmlFile, SonarXmlFile, SourceDir: string; Threshold: Double);
begin
  CoInitialize(nil);
  try
    ConvertInternal(DccXmlFile, SonarXmlFile, SourceDir, Threshold);
  finally
    CoUninitialize;
  end;
end;

class procedure TSonarConverter.ConvertInternal(const DccXmlFile, SonarXmlFile, SourceDir: string; Threshold: Double);
var
  UnitMap: TDictionary<string, string>;
  Files: TArray<string>;
  FileName, UnitKey: string;
  
  Xml: IXMLDocument;
  Root: IXMLNode;
  I: Integer;
  
  SonarOutput: TStringList;
  CurrentFile: string;
  LineNumStr: string;
  Hits: Integer;
  Covered: string;
  LineEq: Integer;
  
  // Coverage Stats
  TotalLinesToCover, CoveredLinesCount: Integer;
  Percentage: Double;
  
  // XML Nodes
  DataNode, LineHitsNode, FileNode: IXMLNode;
  NameAttr, LineHitsStr: string;
  Pairs: TArray<string>;
  Pair: string;
  HitsStr: string;
begin
  if not FileExists(DccXmlFile) then 
  begin
    SafeWriteLn('Error: DCC XML file not found: ' + DccXmlFile);
    Exit;
  end;

  SafeWriteLn('Converting Coverage Report to Sonar Generic Format...');
  
  // 1. Build Unit Map (Case Insensitive via LowerCase keys)
  UnitMap := TDictionary<string, string>.Create;
  try
    Files := TDirectory.GetFiles(SourceDir, '*.pas', TSearchOption.SoAllDirectories);
    for FileName in Files do
    begin
      UnitKey := TPath.GetFileNameWithoutExtension(FileName).ToLower;
      if not UnitMap.ContainsKey(UnitKey) then
        UnitMap.Add(UnitKey, FileName);
    end;

    // 2. Parse DCC XML
    Xml := LoadXMLDocument(DccXmlFile);
    if Xml = nil then Exit;
    
    Root := Xml.DocumentElement; 
    if Root = nil then Exit;

    // Find "data" -> "linehits" or just "linehits"
    DataNode := Root.ChildNodes.FindNode('data');
    if DataNode = nil then DataNode := Root;
    
    LineHitsNode := DataNode.ChildNodes.FindNode('linehits');
    if LineHitsNode = nil then
    begin
       SafeWriteLn('Error: <linehits> node not found in XML report.');
       Exit;
    end;
    
    SafeWriteLn(Format('  Found <linehits> with %d files.', [LineHitsNode.ChildNodes.Count]));

    // 3. Prepare Sonar Output
    SonarOutput := TStringList.Create;
    TotalLinesToCover := 0;
    CoveredLinesCount := 0;
    try
      SonarOutput.Add('<coverage version="1">');
      
      for I := 0 to LineHitsNode.ChildNodes.Count - 1 do
      begin
        FileNode := LineHitsNode.ChildNodes[I];
        if not SameText(FileNode.NodeName, 'file') then Continue;
        
        if not FileNode.HasAttribute('name') then Continue;
        
        NameAttr := VarToStr(FileNode.Attributes['name']);
        UnitKey := TPath.GetFileNameWithoutExtension(NameAttr).ToLower;
        
        if not UnitMap.TryGetValue(UnitKey, CurrentFile) then 
        begin
           // Try implicit match if NameAttr looks like full path? Usually just filename.
           Continue;
        end;
           
        SonarOutput.Add(Format('  <file path="%s">', [CurrentFile]));
        
        // Parse "123=1;124=0;..."
        if not VarIsNull(FileNode.NodeValue) then
        begin
           LineHitsStr := VarToStr(FileNode.NodeValue);
           Pairs := LineHitsStr.Split([';']);
           
           for Pair in Pairs do
           begin
              if Pair.Trim = '' then Continue;
              
              LineEq := Pair.IndexOf('=');
              if LineEq < 0 then Continue;
              
              LineNumStr := Pair.Substring(0, LineEq);
              HitsStr := Pair.Substring(LineEq + 1);
              
              Hits := StrToIntDef(HitsStr, 0);
              Inc(TotalLinesToCover);
              
              if Hits > 0 then 
              begin
                 Covered := 'true';
                 Inc(CoveredLinesCount);
              end
              else 
                 Covered := 'false';
              
              SonarOutput.Add(Format('    <lineToCover lineNumber="%s" covered="%s"/>', [LineNumStr, Covered]));
           end;
        end;
        
        SonarOutput.Add('  </file>');
      end;
      
      SonarOutput.Add('</coverage>');
      SonarOutput.SaveToFile(SonarXmlFile);
      
      // 4. Calculate and Check Threshold
      if TotalLinesToCover > 0 then
         Percentage := (CoveredLinesCount / TotalLinesToCover) * 100
      else
         Percentage := 100.0;
         
      SafeWriteLn(Format('Coverage Report generated: %s', [ExtractFileName(SonarXmlFile)]));
      SafeWriteLn(Format('Total Lines: %d | Covered: %d | Coverage: %.2f%%', [TotalLinesToCover, CoveredLinesCount, Percentage]));
      
      if (Threshold > 0) and (Percentage < Threshold) then
      begin
         SafeWriteLn(Format('ERROR: Coverage (%.2f%%) is below the required threshold (%.2f%%).', [Percentage, Threshold]));
         raise Exception.Create('Code coverage threshold not met.');
      end;
    finally
      SonarOutput.Free;
    end;
    
  finally
    UnitMap.Free;
  end;
end;

end.
