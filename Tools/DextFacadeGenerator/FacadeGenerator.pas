unit FacadeGenerator;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  AST.Classes,
  AST.Delphi.Parser,
  AST.Delphi.Classes,
  AST.Delphi.DataTypes,
  AST.Delphi.Project,
  AST.Intf,
  AST.Parser.Options,
  AST.Pascal.Parser,
  AST.Delphi.Declarations,
  AST.Delphi.SysTypes,
  AST.Targets;

type
  TOrdinalIgnoreCaseComparer = class(TEqualityComparer<string>)
  public
    function Equals(const Left, Right: string): Boolean; override;
    function GetHashCode(const Value: string): Integer; override;
  end;

  TExtractedUnit = class
  public
    UnitName: string;
    Types: TList<string>;
    Consts: TList<string>;
    constructor Create(const AName: string);
    destructor Destroy; override;
  end;

  TFacadeGenerator = class
  protected
    FSourcePath: string;
    FSearchPattern: string;
    FExcludedUnits: THashSet<string>;
    FParsedUnits: TObjectList<TExtractedUnit>;
    FProject: IASTDelphiProject; 
    procedure ScanFolder(const Folder: string);
    procedure ProcessFile(const FileName: string); virtual;
    function IsExcluded(const UnitName: string): Boolean;
    function IsGeneric(Decl: TIDType): Boolean;
    function GetFullTypeName(UnitName, TypeName: string): string;
  public
    property ParsedUnits: TObjectList<TExtractedUnit> read FParsedUnits;
    constructor Create(const SourcePath, Wildcard: string; const Excluded: TArray<string>);
    destructor Destroy; override;
    procedure Execute; virtual;
    procedure GenerateArtifacts(const OutputPath: string; const BaseName: string); virtual;
  end;

implementation

{ TOrdinalIgnoreCaseComparer }

function TOrdinalIgnoreCaseComparer.Equals(const Left, Right: string): Boolean;
begin
  Result := SameText(Left, Right);
end;

function TOrdinalIgnoreCaseComparer.GetHashCode(const Value: string): Integer;
begin
  Result := TEqualityComparer<string>.Default.GetHashCode(UpperCase(Value));
end;

{ TExtractedUnit }

constructor TExtractedUnit.Create(const AName: string);
begin
  UnitName := AName;
  Types := TList<string>.Create;
  Consts := TList<string>.Create;
end;

destructor TExtractedUnit.Destroy;
begin
  Types.Free;
  Consts.Free;
  inherited;
end;

{ TFacadeGenerator }

constructor TFacadeGenerator.Create(const SourcePath, Wildcard: string; const Excluded: TArray<string>);
var
  S: string;
begin
  FSourcePath := SourcePath;
  FSearchPattern := Wildcard;
  FExcludedUnits := THashSet<string>.Create(TOrdinalIgnoreCaseComparer.Create);
  for S in Excluded do
    FExcludedUnits.Add(S);
    
  FParsedUnits := TObjectList<TExtractedUnit>.Create(True);
  FProject := TASTDelphiProject.Create('DextFacade');
  // Optional: Set Target to define pointer sizes etc. if needed for parsing directives
  // FProject.Target := TWINX64_Target; 
  // FProject.Defines.Add('MSWINDOWS'); 
  // FProject.Defines.Add('WIN64');
end;

destructor TFacadeGenerator.Destroy;
begin
  FParsedUnits.Free;
  FExcludedUnits.Free;
  // FProject is ref counted
  inherited;
end;

procedure TFacadeGenerator.Execute;
begin
  ScanFolder(FSourcePath);
end;

procedure TFacadeGenerator.ScanFolder(const Folder: string);
var
  FileName: string;
  SubFolder: string;
begin
  // Process files
  for FileName in TDirectory.GetFiles(Folder, FSearchPattern) do
  begin
    ProcessFile(FileName);
  end;

  // Recurse directories
  for SubFolder in TDirectory.GetDirectories(Folder) do
  begin
    ScanFolder(SubFolder);
  end;
end;

procedure TFacadeGenerator.ProcessFile(const FileName: string);
var
  LocalProject: IASTDelphiProject;
  Parser: TASTDelphiUnit;
  SourceCode: string;
  UnitDecl: TExtractedUnit;
  Decl: TIDDeclaration;
  I: Integer;
  UnitName: string;
begin
  // Simple check to skip obviously irrelevant files
  if TPath.GetFileName(FileName).StartsWith('.') then Exit;
  
  Writeln('Processing: ' + TPath.GetFileName(FileName));

  try
    SourceCode := TFile.ReadAllText(FileName);
  except
    Writeln('Error reading file: ' + FileName);
    Exit;
  end;

  // Use a local project to avoid state pollution and AVs with dangling pointers
  LocalProject := TASTDelphiProject.Create('TempProject');
  // Configure project
  LocalProject.StopCompileIfError := False; // Recover from missing system units
  
  // Cast to class to access fields if not in interface
  if LocalProject is TASTDelphiProject then
  begin
     TASTDelphiProject(LocalProject).Target := TWINX64_Target; 
     TASTDelphiProject(LocalProject).AddUnitSearchPath(TPath.GetDirectoryName(FSourcePath), True);

     // Define platform symbols to help parsing RTL/VCL units
     LocalProject.Defines.Add('MSWINDOWS');
     LocalProject.Defines.Add('WIN32'); // or WIN64, let's add both or specific
     LocalProject.Defines.Add('WIN64');
     LocalProject.Defines.Add('CPUX64');
     LocalProject.Defines.Add('UNICODE');


     // Try to add BDS sources if available
     var BDS := GetEnvironmentVariable('BDS');
     if BDS = '' then
       BDS := 'C:\Program Files (x86)\Embarcadero\Studio\23.0';
       
     if BDS <> '' then
     begin
       var BDSSource := TPath.Combine(BDS, 'source');
       if TDirectory.Exists(BDSSource) then
       begin
         TASTDelphiProject(LocalProject).AddUnitSearchPath(TPath.Combine(BDSSource, 'rtl'), True);
         TASTDelphiProject(LocalProject).AddUnitSearchPath(TPath.Combine(BDSSource, 'data'), True);
         // Writeln('Added BDS Sources from: ' + BDSSource); 
       end
       else
       begin
          // Try without 'source' subfolder? No, it should be there.
          // Writeln('BDS Source not found at: ' + BDSSource);
       end;
     end;
  end;
  
  // We parse the unit
  try
    Parser := TASTDelphiUnit.Create(LocalProject, FileName, SourceCode);
    // Do NOT free Parser manually - it is ref-counted by LocalProject!
    try
      if Parser.Compile(True) = CompileFail then
      begin
        Writeln('  Compile failed for ' + FileName + '. Messages: ' + Parser.MessagesText);
      end;

      UnitName := Parser.Name;
      if UnitName = '' then 
        UnitName := TPath.GetFileNameWithoutExtension(FileName);

      Writeln('  Unit: ' + UnitName);

      if IsExcluded(UnitName) then 
      begin
        Writeln('  Excluded.');
        Exit;
      end;

      UnitDecl := TExtractedUnit.Create(UnitName);
      
      if Parser.IntfScope <> nil then
      begin
        Writeln(Format('  Scope Items: %d', [Parser.IntfScope.Count]));
        for I := 0 to Parser.IntfScope.Count - 1 do
        begin
          Decl := Parser.IntfScope.Items[I];
          
          if Decl.Name = '' then Continue;
          if (Decl.ItemType = itUnit) then Continue; // Skip Unit metadata

          // Basic Info
          var DeclInfo := Format('    %-30s : %s', [Decl.Name, Decl.ClassName]);

          if Decl.Visibility in [vPrivate, vStrictPrivate, vProtected, vStrictProtected] then 
          begin
            // Writeln(DeclInfo + ' -> Skipped (Visibility)'); // Too noisy for non-public?
            Continue; 
          end;

          if Decl is TIDType then
          begin
            if IsGeneric(TIDType(Decl)) then 
            begin
               Writeln(DeclInfo + ' -> Skipped (Generic)');
               Continue;
            end;
            
            if (Decl is TIDClass) or 
               (Decl is TIDInterface) or 
               (Decl is TIDRecord) or 
               (Decl is TIDEnum) or
               (Decl is TIDClassOf) or
               (Decl is TIDPointer) or 
               (Decl is TIDProcType) then
            begin
               if (Decl.Name = UnitName) then Continue; 
               
               UnitDecl.Types.Add(Decl.Name);
               Writeln(DeclInfo + ' -> Added (Type)');
            end
            else if (Decl.ItemType = itType) then
            begin
               if (Decl.Name = UnitName) then Continue;
               
               // Filter out keywords mistakenly parsed as types
               if MatchStr(Decl.Name, ['public', 'protected', 'private', 'published', 'automated', 
                                     'static', 'virtual', 'override', 'overload', 'reintroduce', 'default']) then
                  Continue;
               
               UnitDecl.Types.Add(Decl.Name);
               Writeln(DeclInfo + ' -> Added (Alias)');
            end;
          end
          else if Decl is TIDConstant then
          begin
               UnitDecl.Consts.Add(Decl.Name);
               Writeln(DeclInfo + ' -> Added (Const)');
          end;
        end;
      end
      else
        Writeln('  IntfScope is NIL - Parser likely failed completely or unit has no interface section.');
      
      if (UnitDecl.Types.Count > 0) or (UnitDecl.Consts.Count > 0) then
        FParsedUnits.Add(UnitDecl)
      else
        UnitDecl.Free;

    finally
      // Parser is managed by LocalProject (interface)
    end;
  except
    on E: Exception do
    begin
      Writeln('Error parsing ' + FileName + ': ' + E.Message);
    end;
  end;
end;

function TFacadeGenerator.IsExcluded(const UnitName: string): Boolean;
begin
  Result := FExcludedUnits.Contains(UnitName);
end;

function TFacadeGenerator.IsGeneric(Decl: TIDType): Boolean;
begin
  Result := Decl.IsGeneric;
end;

function TFacadeGenerator.GetFullTypeName(UnitName, TypeName: string): string;
begin
  Result := UnitName + '.' + TypeName;
end;

procedure TFacadeGenerator.GenerateArtifacts(const OutputPath, BaseName: string);
var
  AliasesContent: TStringList;
  UsesContent: TStringList;
  UnitInfo: TExtractedUnit;
  TypeName: string;
  ConstName: string;
  FileNameAliases: string;
  FileNameUses: string;
begin
  AliasesContent := TStringList.Create;
  UsesContent := TStringList.Create;
  try
    AliasesContent.Add('// Auto-generated Aliases');
    
    // Sort units for stability
    FParsedUnits.Sort(TComparer<TExtractedUnit>.Construct(
      function(const L, R: TExtractedUnit): Integer
      begin
        Result := CompareText(L.UnitName, R.UnitName);
      end
    ));

    // Pass 1: Types and Uses
    for UnitInfo in FParsedUnits do
    begin
      if (UnitInfo.Types.Count > 0) or (UnitInfo.Consts.Count > 0) then
         UsesContent.Add('  ' + UnitInfo.UnitName + ',');

      if UnitInfo.Types.Count > 0 then
      begin
        AliasesContent.Add('');
        AliasesContent.Add('  // ' + UnitInfo.UnitName);
        for TypeName in UnitInfo.Types do
          AliasesContent.Add(Format('  %s = %s.%s;', [TypeName, UnitInfo.UnitName, TypeName]));
      end;
    end;

    // Pass 2: Consts
    var AnyConsts := False;
    for UnitInfo in FParsedUnits do 
      if UnitInfo.Consts.Count > 0 then 
      begin
        AnyConsts := True;
        Break;
      end;

    if AnyConsts then
    begin
      AliasesContent.Add('');
      AliasesContent.Add('const');
      for UnitInfo in FParsedUnits do
      begin
        if UnitInfo.Consts.Count > 0 then
        begin
           AliasesContent.Add('');
           AliasesContent.Add('  // ' + UnitInfo.UnitName);
           for ConstName in UnitInfo.Consts do
             AliasesContent.Add(Format('  %s = %s.%s;', [ConstName, UnitInfo.UnitName, ConstName]));
        end;
      end; 
    end;
    
    // Remove last comma from uses
    if UsesContent.Count > 0 then
    begin
      var LastIdx := UsesContent.Count - 1;
      var LastLine := UsesContent[LastIdx];
      if (LastLine.Length > 0) and (LastLine.EndsWith(',')) then
         UsesContent[LastIdx] := LastLine.Substring(0, LastLine.Length - 1);
    end;

    FileNameAliases := TPath.Combine(OutputPath, BaseName + '.Aliases.inc');
    FileNameUses := TPath.Combine(OutputPath, BaseName + '.Uses.inc');
    
    AliasesContent.SaveToFile(FileNameAliases);
    UsesContent.SaveToFile(FileNameUses);
    
    Writeln('Generated ' + FileNameAliases);
    Writeln('Generated ' + FileNameUses);
    
  finally
    AliasesContent.Free;
    UsesContent.Free;
  end;
end;

end.
