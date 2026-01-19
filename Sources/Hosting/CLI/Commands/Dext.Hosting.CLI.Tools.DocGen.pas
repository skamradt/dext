unit Dext.Hosting.CLI.Tools.DocGen;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  DelphiAST,
  DelphiAST.Classes,
  DelphiAST.Consts,
  SimpleParser.Lexer.Types,
  TypInfo, // Added for GetEnumName
  Dext.Utils;

type
  TMemberInfo = class
    Name: string;
    Visibility: string;
    XmlDoc: string;
    constructor Create(const AName, AVis, AXml: string);
  end;

  TMethodInfo = class(TMemberInfo)
    Args: string;
    ResultType: string;
    Kind: string; // procedure, function, constructor
  end;

  TPropertyInfo = class(TMemberInfo)
    PropType: string;
  end;

  // Generic item for Consts/Types
  TTypeInfo = class(TMemberInfo)
    Kind: string; // const, enum, alias
    Value: string; // For consts or type alias
    Details: string; // For enums (list of values)
  end;

  TClassInfo = class
    Name: string;
    UnitName: string;
    XmlDoc: string; // Added XmlDoc
    ParentName: string; // TObject, TInterfacedObject, etc
    Kind: string; // class, interface, record
    Interfaces: TList<string>;
    Methods: TObjectList<TMethodInfo>;
    Properties: TObjectList<TPropertyInfo>;
    Fields: TObjectList<TMemberInfo>;
    constructor Create;
    destructor Destroy; override;
  end;

  TUnitInfo = class
    Name: string;        // Simple unit name (e.g. MyUnit)
    DisplayName: string; // Name shown in UI (may include path for duplicates)
    FullPath: string;    // Full file path (unique key)
    FileName: string;    // Just the filename
    Classes: TObjectList<TClassInfo>;
    GlobalMethods: TObjectList<TMethodInfo>; 
    GlobalConstants: TObjectList<TTypeInfo>; // New: Consts
    GlobalTypes: TObjectList<TTypeInfo>; // New: Enums, Sets, etc.
    constructor Create;
    destructor Destroy; override;
  end;
  
  TDocRegistry = class
  private
    FUnits: TObjectDictionary<string, TUnitInfo>; // Keyed by FullPath
    FUnitsByName: TDictionary<string, TList<TUnitInfo>>; // For duplicate detection
  public
    constructor Create;
    destructor Destroy; override;
    function AddUnit(Info: TUnitInfo; const BaseDir: string): Boolean; // Returns false if duplicate path
    procedure ResolveDisplayNames; // Call after all units added
    function FindClass(const ClassName: string): TClassInfo;
    function GetAllUnits: TArray<TUnitInfo>;
    property Units: TObjectDictionary<string, TUnitInfo> read FUnits;
  end;

  TDextDocGenerator = class
  private
    FTemplate: string;
    FOutputDir: string;
    FTitle: string;
    FSourceLines: TDictionary<string, TArray<string>>;
    FRegistry: TDocRegistry;
    
    function ParseFile(const FileName: string): TSyntaxNode;
    function GetXmlDoc(const UnitName: string; Line: Integer): string;
    
    function BuildUnitModel(const UnitName, FileName: string; Node: TSyntaxNode): TUnitInfo;
    procedure ExtractMembers(CInfo: TClassInfo; ClassNode: TSyntaxNode);
    function GetMethodSignature(const MethodNode: TSyntaxNode; out Args, RetType: string): string;

    function GenerateSidebar: string; 
    function GenerateUnitHtml(Info: TUnitInfo; UnitNode: TSyntaxNode): string;
    function RenderHtmlDocumentationObj(CInfo: TClassInfo): string;
    
    function CleanId(const Name: string): string;
    function CleanType(const TypeName: string): string;
    function CleanMermaidText(const Text: string): string;
    function CleanMermaidId(const Name: string): string;
    function GetNodeText(Node: TSyntaxNode): string;
  public
    constructor Create(const ATemplate, AOutputDir: string; const ATitle: string = 'Dext Framework');
    destructor Destroy; override;
    procedure Generate(const InputDir: string);
  end;

implementation

{ TDextDocGenerator }

{ TClassInfo }

constructor TClassInfo.Create;
begin
  Interfaces := TList<string>.Create;
  Methods := TObjectList<TMethodInfo>.Create;
  Properties := TObjectList<TPropertyInfo>.Create;
  Fields := TObjectList<TMemberInfo>.Create;
end;

destructor TClassInfo.Destroy;
begin
  Interfaces.Free;
  Methods.Free;
  Properties.Free;
  Fields.Free;
  inherited;
end;

{ TMemberInfo }

constructor TMemberInfo.Create(const AName, AVis, AXml: string);
begin
  Name := AName;
  Visibility := AVis;
  XmlDoc := AXml;
end;

{ TUnitInfo }

constructor TUnitInfo.Create;
begin
  Classes := TObjectList<TClassInfo>.Create;
  GlobalMethods := TObjectList<TMethodInfo>.Create;
  GlobalConstants := TObjectList<TTypeInfo>.Create;
  GlobalTypes := TObjectList<TTypeInfo>.Create;
end;

destructor TUnitInfo.Destroy;
begin
  Classes.Free;
  GlobalMethods.Free;
  GlobalConstants.Free;
  GlobalTypes.Free;
  inherited;
end;

{ TDocRegistry }

constructor TDocRegistry.Create;
begin
  FUnits := TObjectDictionary<string, TUnitInfo>.Create([doOwnsValues]);
  FUnitsByName := TDictionary<string, TList<TUnitInfo>>.Create;
end;

destructor TDocRegistry.Destroy;
var
  List: TList<TUnitInfo>;
begin
  for List in FUnitsByName.Values do
    List.Free;
  FUnitsByName.Free;
  FUnits.Free;
  inherited;
end;

function TDocRegistry.AddUnit(Info: TUnitInfo; const BaseDir: string): Boolean;
var
  NameList: TList<TUnitInfo>;
  LowerPath: string;
begin
  LowerPath := Info.FullPath.ToLower;
  
  // Check for exact path duplicate (skip silently)
  if FUnits.ContainsKey(LowerPath) then
  begin
    SafeWriteLn('  [SKIP] Duplicate path: ' + Info.FullPath);
    Info.Free;
    Exit(False);
  end;
  
  // Add to main dictionary (keyed by full path)
  FUnits.Add(LowerPath, Info);
  
  // Track by simple name for duplicate detection
  if not FUnitsByName.TryGetValue(Info.Name.ToLower, NameList) then
  begin
    NameList := TList<TUnitInfo>.Create;
    FUnitsByName.Add(Info.Name.ToLower, NameList);
  end;
  NameList.Add(Info);
  
  Result := True;
end;

procedure TDocRegistry.ResolveDisplayNames;
var
  NameList: TList<TUnitInfo>;
  Info: TUnitInfo;
  RelPath: string;
begin
  for NameList in FUnitsByName.Values do
  begin
    if NameList.Count = 1 then
    begin
      // Unique name - use simple name
      NameList[0].DisplayName := NameList[0].Name;
    end
    else
    begin
      // Duplicates found - append relative path to distinguish
      SafeWriteLn(Format('  [WARN] %d units named "%s" - adding paths to distinguish', 
        [NameList.Count, NameList[0].Name]));
      for Info in NameList do
      begin
        // Extract meaningful path suffix (last 2 folders + filename)
        RelPath := ExtractRelativePath(ExtractFilePath(Info.FullPath), Info.FullPath);
        if RelPath = '' then RelPath := Info.FullPath;
        
        // Use parent folder + unit name
        var Folders := Info.FullPath.Split(['\', '/']);
        if Length(Folders) >= 2 then
          Info.DisplayName := Format('%s (%s)', [Info.Name, Folders[High(Folders)-1]])
        else
          Info.DisplayName := Info.Name;
          
        SafeWriteLn(Format('    -> %s', [Info.DisplayName]));
      end;
    end;
  end;
end;

function TDocRegistry.GetAllUnits: TArray<TUnitInfo>;
var
  List: TList<TUnitInfo>;
  Info: TUnitInfo;
begin
  List := TList<TUnitInfo>.Create;
  try
    for Info in FUnits.Values do
      List.Add(Info);
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

function TDocRegistry.FindClass(const ClassName: string): TClassInfo;
begin
  Result := nil;
  for var U in FUnits.Values do
  begin
    for var C in U.Classes do
      if SameText(C.Name, ClassName) then Exit(C);
  end;
end;

{ TDextDocGenerator }

constructor TDextDocGenerator.Create(const ATemplate, AOutputDir: string; const ATitle: string);
begin
  FTemplate := ATemplate;
  FOutputDir := AOutputDir;
  FTitle := ATitle;
  FSourceLines := TDictionary<string, TArray<string>>.Create;
  FRegistry := TDocRegistry.Create;
end;

destructor TDextDocGenerator.Destroy;
begin
  FRegistry.Free;
  FSourceLines.Free;
  inherited;
end;

function TDextDocGenerator.CleanId(const Name: string): string;
begin
  Result := Name.Replace('.', '_').Replace(' ', '_').Replace('<', '_lt_').Replace('>', '_gt_').Replace(',', '_');
end;

function TDextDocGenerator.CleanType(const TypeName: string): string;
begin
  Result := TypeName.Replace('<', '&lt;').Replace('>', '&gt;');
end;

function TDextDocGenerator.CleanMermaidText(const Text: string): string;
begin
  // Mermaid v11 supports tildes ~T~ properly even in method names
  Result := Text.Replace('<', '~').Replace('>', '~');
end;

function TDextDocGenerator.CleanMermaidId(const Name: string): string;
begin
  // Tildes WORK for Class Nodes (e.g. class List~T~)
  Result := Name.Replace('.', '_').Replace(' ', '_').Replace('<', '~').Replace('>', '~').Replace(',', '_');
end;

function TDextDocGenerator.GetNodeText(Node: TSyntaxNode): string;
begin
  Result := Node.GetAttribute(anName);
  if Result = '' then Result := Node.GetAttribute(anType); // Often types are here
  if Result = '' then Result := Node.GetAttribute(anKind);
  
  if Result = '' then
  begin
     if Node is TValuedSyntaxNode then
       Result := TValuedSyntaxNode(Node).Value;
  end;
  
  // Strip Pascal keyword escape char '&' (e.g. &And -> And, &Type -> Type)
  if Result.StartsWith('&') then
    Result := Result.Substring(1);
end;

function TDextDocGenerator.ParseFile(const FileName: string): TSyntaxNode;
var
  Builder: TPasSyntaxTreeBuilder;
  Content: string;
  Stream: TStringStream;
begin
  Result := nil;
  Builder := TPasSyntaxTreeBuilder.Create;
  try
    Builder.InitDefinesDefinedByCompiler;
    Builder.AddDefine('MSWINDOWS');
    Builder.UseDefines := True;
    
    Content := TFile.ReadAllText(FileName);
    FSourceLines.AddOrSetValue(TPath.GetFileNameWithoutExtension(FileName), TFile.ReadAllLines(FileName));
    
    Stream := TStringStream.Create(Content, TEncoding.UTF8);
    try
      try
        Result := Builder.Run(Stream);
      except
        // Ignore parse errors for now, just skip
      end;
    finally
      Stream.Free;
    end;
  finally
    Builder.Free;
  end;
end;

function TDextDocGenerator.GetMethodSignature(const MethodNode: TSyntaxNode; out Args, RetType: string): string;
var
  Params: TStringBuilder;
  Child, Param: TSyntaxNode;
  FirstParam: Boolean;
  ParamName, ParamType: string;
  Modifier: string;
begin
  Params := TStringBuilder.Create;
  try
    Params.Append('(');
    FirstParam := True;
    
    // Find Parameters node (Search all children)
    var ParamsNode: TSyntaxNode := nil;
    for Child in MethodNode.ChildNodes do
    begin
      if Child.Typ = ntParameters then
      begin
          ParamsNode := Child;
          Break;
      end;
    end;

    if ParamsNode <> nil then
    begin
          for Param in ParamsNode.ChildNodes do
          begin
             ParamName := GetNodeText(Param);
             if ParamName = '' then 
               for var PChild in Param.ChildNodes do 
                  if PChild.Typ = ntName then ParamName := GetNodeText(PChild);
             
             ParamType := Param.GetAttribute(anType);
             if ParamType = '' then 
               for var PChild in Param.ChildNodes do 
                  if PChild.Typ = ntType then ParamType := GetNodeText(PChild);

             // Modifiers: try anKind or Check attributes
             Modifier := Param.GetAttribute(anKind); // Try kind first
             if Modifier = '' then modifier := Param.GetAttribute(anType); // Sometimes type has it? Unlikely.
             // Manual check for 'const', 'var', 'out' strings in logic?
             // DelphiAST often puts modifier in anKind (e.g. 'const')
             // Clean up modifier
             if SameText(Modifier, '0') then Modifier := ''; // Default numeric?

             if not FirstParam then Params.Append('; ');
             
             if Modifier <> '' then Params.Append(Modifier + ' ');
             
             if ParamType <> '' then
                Params.AppendFormat('%s: %s', [ParamName, ParamType])
             else
                Params.Append(ParamName);
                
             FirstParam := False;
          end;
    end;
    
    Args := Params.ToString;
    if Args = '(' then Args := ''; 
    if Args <> '' then Args := Args + ')';
    
    // Return type
    RetType := '';
    for Child in MethodNode.ChildNodes do
    begin
      if Child.Typ = ntReturnType then
      begin
           RetType := ': ' + Child.GetAttribute(anType); 
           if RetType = ': ' then // Fallback to child check for return type too
           begin
                for var RChild in Child.ChildNodes do 
                    if RChild.Typ = ntType then RetType := ': ' + GetNodeText(RChild);
           end;
           Break;
      end;
    end;
  finally
    Params.Free;
  end;
end;

procedure TDextDocGenerator.ExtractMembers(CInfo: TClassInfo; ClassNode: TSyntaxNode);

    procedure Scan(ContextNode: TSyntaxNode; CurrentVis: string; Depth: Integer);
    var 
      CChild: TSyntaxNode;
      Vis: string;
    begin
       if Depth > 10 then Exit;
       Vis := CurrentVis;
       
       if ContextNode.Typ = ntPrivate then Vis := 'PRIVATE'
       else if ContextNode.Typ = ntStrictPrivate then Vis := 'PRIVATE'
       else if ContextNode.Typ = ntProtected then Vis := 'PROTECTED'
       else if ContextNode.Typ = ntPublic then Vis := 'PUBLIC'
       else if ContextNode.Typ = ntPublished then Vis := 'PUBLISHED';
       
       for CChild in ContextNode.ChildNodes do
       begin
           if CChild.Typ in [ntMethod, ntProperty, ntField] then
           begin
              var MName := GetNodeText(CChild);
              if MName = '' then Continue;
              
              var Xml := GetXmlDoc(CInfo.UnitName, CChild.Line);
              
              if CChild.Typ = ntMethod then
              begin
                 var M := TMethodInfo.Create(MName, Vis, Xml); 
                 M.Kind := CChild.GetAttribute(anKind); 
                 GetMethodSignature(CChild, M.Args, M.ResultType);
                 CInfo.Methods.Add(M);
              end
              else if CChild.Typ = ntProperty then
              begin
                 var P := TPropertyInfo.Create(MName, Vis, Xml);
                 P.PropType := CChild.GetAttribute(anType);
                 CInfo.Properties.Add(P);
              end
              else
              begin
                 var F := TMemberInfo.Create(MName, Vis, Xml);
                 CInfo.Fields.Add(F);
              end;
           end
           else
              Scan(CChild, Vis, Depth + 1);
       end;
    end;
    
begin
   Scan(ClassNode, 'PUBLIC', 0); 
end;

function TDextDocGenerator.BuildUnitModel(const UnitName, FileName: string; Node: TSyntaxNode): TUnitInfo;
var
  UInfo: TUnitInfo;
  InterfaceNode, Child, TypeNode: TSyntaxNode;
  CInfo: TClassInfo;
  TypeType, NodeName: string;
begin
  UInfo := TUnitInfo.Create;
  UInfo.Name := UnitName;
  UInfo.FullPath := FileName;
  UInfo.FileName := TPath.GetFileName(FileName);
  UInfo.DisplayName := UnitName; // Will be resolved later for duplicates
  
  InterfaceNode := Node.FindNode(ntInterface);
  if InterfaceNode = nil then InterfaceNode := Node;
  
  for Child in InterfaceNode.ChildNodes do
  begin
    // Check for Global Methods
    if Child.Typ = ntMethod then
    begin
         var MName := GetNodeText(Child);
         if MName <> '' then
         begin
             var M := TMethodInfo.Create(MName, 'PUBLIC', GetXmlDoc(UnitName, Child.Line));
             M.Kind := Child.GetAttribute(anKind);
             GetMethodSignature(Child, M.Args, M.ResultType);
             UInfo.GlobalMethods.Add(M);
         end;
    end
    else if Child.Typ = ntConstants then // CONSTS
    begin
         for var CNode in Child.ChildNodes do
         begin
             if CNode.Typ = ntConstant then
             begin
                  var CName := GetNodeText(CNode);
                  if CName <> '' then
                  begin
                      // Value is child or attribute??
                      var CVal := GetNodeText(CNode); // Often value is here
                      // But wait, name is attribute, value might be where?
                      // If GetNodeText returned NAME, then we check children for value?
                      // Actually TValuedSyntaxNode.Value is usually the name/value depending on node type.
                      // Let's assume Name is Name, and check children for Value node?
                      // Or maybe CNode itself has the value?
                      var TI := TTypeInfo.Create(CName, 'PUBLIC', GetXmlDoc(UnitName, CNode.Line));
                      TI.Kind := 'Constant';
                      TI.Value := ''; 
                      // Simple hack: if we can't easily find value, just list it.
                      // Try find value in child?
                      UInfo.GlobalConstants.Add(TI);
                  end;
             end;
         end;
    end
    else if Child.Typ = ntTypeSection then // TYPES & CLASSES
    begin
       for TypeNode in Child.ChildNodes do
       begin
          if TypeNode.Typ = ntTypeDecl then
          begin
             TypeType := TypeNode.GetAttribute(anType);
             NodeName := GetNodeText(TypeNode);
             
             // Check for Generic Parameters (ntTypeParams -> ntTypeParam -> ntType/ntName)
             var GenericParams: string := '';
             for var GChild in TypeNode.ChildNodes do
             begin
                if GChild.Typ = ntTypeParams then
                begin
                   var GSB := TStringBuilder.Create;
                   try
                      GSB.Append('<');
                      for var GP in GChild.ChildNodes do
                      begin
                         if GP.Typ = ntTypeParam then
                         begin
                            var GPName := GP.GetAttribute(anName);
                            if GPName = '' then GPName := GetNodeText(GP); // Try helper
                            // If still empty, check child type
                            if GPName = '' then
                               for var GPT in GP.ChildNodes do 
                                  if GPT.Typ in [ntName, ntType] then GPName := GetNodeText(GPT);
                            
                            if GSB.Length > 1 then GSB.Append(', ');
                            GSB.Append(GPName);
                         end;
                      end;
                      GSB.Append('>');
                      if GSB.Length > 2 then GenericParams := GSB.ToString; 
                   finally
                      GSB.Free;
                   end;
                   Break; // Only one params section
                end;
             end;
             
             NodeName := NodeName + GenericParams; // Append <T> to Name

             // Smart Fallback for Type Attribute
             var ClassNode: TSyntaxNode := TypeNode;
             
             // Fix for Generics: Check ALL children to find the 'class'/'interface' definition
             if (TypeType = '') and (Length(TypeNode.ChildNodes) > 0) then
             begin
                for var Candidate in TypeNode.ChildNodes do
                begin
                   var CandKind := Candidate.GetAttribute(anType);
                   if CandKind = '' then CandKind := Candidate.GetAttribute(anKind);
                   
                   if (SameText(CandKind, 'class')) or (SameText(CandKind, 'interface')) or (SameText(CandKind, 'record')) then
                   begin
                       TypeType := CandKind;
                       ClassNode := Candidate;
                       Break;
                   end;
                end;
                
                // Fallback (e.g. Type Alias or Simple Type)
                if (TypeType = '') then
                begin
                    var TypeChild: TSyntaxNode := TypeNode.ChildNodes[0];
                    TypeType := TypeChild.GetAttribute(anType);
                    if TypeType = '' then TypeType := TypeChild.GetAttribute(anKind);
                    ClassNode := TypeChild;                        
                end;
             end;
             
             // Accept class, interface OR record
             if SameText(TypeType, 'class') or SameText(TypeType, 'interface') or SameText(TypeType, 'record') then
             begin
                CInfo := TClassInfo.Create;
                CInfo.Name := NodeName;
                CInfo.UnitName := UnitName;
                CInfo.Kind := TypeType;
                CInfo.XmlDoc := GetXmlDoc(UnitName, TypeNode.Line); 
                
                // inheritance (Modified for robust detection)
                for var IChild in ClassNode.ChildNodes do
                begin
                   if IChild.Typ = ntInherited then 
                   begin
                        var NameFound := IChild.GetAttribute(anName);
                        if NameFound = '' then
                             for var Sub in IChild.ChildNodes do 
                               if Sub.Typ in [ntName, ntType] then 
                               begin
                                  NameFound := GetNodeText(Sub);
                                  Break;
                               end;
                        // Fix for Generics (e.g. TDictionary<K,V>)
                        if NameFound = '' then NameFound := GetNodeText(IChild);

                        if NameFound <> '' then
                        begin
                           if CInfo.ParentName = '' then CInfo.ParentName := NameFound
                           else CInfo.Interfaces.Add(NameFound);
                        end;
                   end
                   else if (IChild.Typ = ntType) then
                   begin
                      // A child ntType node often represents the parent class or interfaces
                      var NameFound := IChild.GetAttribute(anName);
                      // Ignore 'class', 'interface' type nodes themselves
                      if (NameFound <> '') and (NameFound <> 'class') and (NameFound <> 'interface') and (NameFound <> 'record') then
                      begin
                           if CInfo.ParentName = '' then CInfo.ParentName := NameFound
                           else CInfo.Interfaces.Add(NameFound);
                      end;
                   end
                   else if IChild.Typ = ntImplements then CInfo.Interfaces.Add(IChild.GetAttribute(anName));
                end;
                
                if (CInfo.ParentName = '') and SameText(TypeType, 'class') then CInfo.ParentName := 'TObject';
                
                ExtractMembers(CInfo, ClassNode);
                
                UInfo.Classes.Add(CInfo);
             end
             else
             begin
                 // ENUM, SET, ALIAS
                 
                 // IMPROVED ENUM DETECTION
                 // 1. Check if TypeType is empty, try to find "enum" in children types
                 if (TypeType = '') and (Length(ClassNode.ChildNodes) > 0) then
                 begin
                     for var Sub in ClassNode.ChildNodes do
                       if (Sub.Typ = ntType) and SameText(Sub.GetAttribute(anName), 'enum') then
                       begin
                          TypeType := 'enumeration';
                          Break;
                       end;
                 end;
                 
                 var TI := TTypeInfo.Create(NodeName, 'PUBLIC', GetXmlDoc(UnitName, TypeNode.Line));
                 TI.Kind := TypeType; 
                 
                 // Smart Detect Enum if still empty but has values
                 if (TI.Kind = '') and (Length(ClassNode.ChildNodes) > 0) and
                    (ClassNode.ChildNodes[0].Typ in [ntName, ntIdentifier]) then
                    TI.Kind := 'enumeration';
                 
                 if TI.Kind = '' then TI.Kind := 'Type';
                 
                 // If Enumeration, list values
                 if SameText(TI.Kind, 'enumeration') or SameText(TI.Kind, '(enumeration)') then
                 begin
                      var EnumSB := TStringBuilder.Create;
                      try
                         for var EVal in ClassNode.ChildNodes do 
                         begin
                            // Support ntName OR ntIdentifier
                            if (EVal.Typ = ntName) or (EVal.Typ = ntIdentifier) then 
                               EnumSB.Append(GetNodeText(EVal) + ', ');
                         end;
                         TI.Details := EnumSB.ToString.TrimRight([',', ' ']);
                      finally
                         EnumSB.Free;
                      end;
                 end;
                 
                 UInfo.GlobalTypes.Add(TI);
             end;
          end;
       end;
    end;
  end;
  
   // Debug AST removed

  
  Result := UInfo;
end;

procedure TDextDocGenerator.Generate(const InputDir: string);
var
  Files: TArray<string>;
  FileName: string;
  UnitName: string;
  UnitNodes: TDictionary<string, TSyntaxNode>; // Keyed by FullPath
  SidebarHtml: string;
  UnitHtml: string;
  FinalHtml: string;
  AllUnits: TArray<TUnitInfo>;
  Info: TUnitInfo;
begin
  Files := TDirectory.GetFiles(InputDir, '*.pas', TSearchOption.SoAllDirectories);
  UnitNodes := TDictionary<string, TSyntaxNode>.Create;
  try
     // 1. Pass: Build Registry
    SafeWriteLn('Phase 1: Indexing...');
    for FileName in Files do
    begin
       if FileName.ToLower.Contains('\tests\') then Continue; 

       try
         var Node := ParseFile(FileName);
         if Node <> nil then
         begin
           UnitName := TPath.GetFileNameWithoutExtension(FileName);
           
           // Build Model using Generator Logic (with access to XmlDoc)
           var UnitInfo := BuildUnitModel(UnitName, FileName, Node);
           
           // Try to add - may fail for duplicates (gracefully handled)
           if FRegistry.AddUnit(UnitInfo, InputDir) then
           begin
             UnitNodes.Add(FileName.ToLower, Node);
             SafeWriteLn('Indexed: ' + UnitName);
           end
           else
             Node.Free; // Free the node if unit was skipped
         end;
       except
         on E: Exception do
         begin
           SafeWriteLn(Format('  [ERROR] Failed to parse %s: %s', [FileName, E.Message]));
           // Continue processing other files
         end;
       end;
    end;
    
    // 1.5 Resolve display names for duplicates
    FRegistry.ResolveDisplayNames;
    
    // 2. Generate Sidebar
    SidebarHtml := GenerateSidebar;
    
    // 3. Generate HTML for each unit (Pass 2)
    SafeWriteLn('Phase 2: Generating HTML...');
    AllUnits := FRegistry.GetAllUnits;
    
    // Sort by DisplayName
    TArray.Sort<TUnitInfo>(AllUnits, TComparer<TUnitInfo>.Construct(
      function(const Left, Right: TUnitInfo): Integer
      begin
        Result := CompareText(Left.DisplayName, Right.DisplayName);
      end));
    
    for Info in AllUnits do
    begin
      if not UnitNodes.ContainsKey(Info.FullPath.ToLower) then Continue;
      
      try
        UnitHtml := GenerateUnitHtml(Info, UnitNodes[Info.FullPath.ToLower]);
       
        // Use safe filename (replace problematic chars for duplicates with paths)
        var SafeFileName := Info.DisplayName
          .Replace(' ', '_')
          .Replace('(', '_')
          .Replace(')', '_');
       
        FinalHtml := FTemplate
           .Replace('{{TITLE}}', Info.DisplayName)
           .Replace('{{PROJECT_TITLE}}', FTitle)
           .Replace('{{SIDEBAR_CONTENT}}', SidebarHtml)
           .Replace('{{MAIN_CONTENT}}', UnitHtml);
           
        TFile.WriteAllText(TPath.Combine(FOutputDir, SafeFileName + '.html'), FinalHtml);
      except
        on E: Exception do
          SafeWriteLn(Format('  [ERROR] Failed to generate HTML for %s: %s', [Info.Name, E.Message]));
      end;
    end;
    
    // 4. Generate Index
    FinalHtml := FTemplate
       .Replace('{{TITLE}}', 'Index')
       .Replace('{{PROJECT_TITLE}}', FTitle)
       .Replace('{{SIDEBAR_CONTENT}}', SidebarHtml)
       .Replace('{{MAIN_CONTENT}}', 
         '<h1>Dext Framework API</h1><p>Select a unit from the sidebar to view full documentation.</p>' +
         '<h2>Units</h2><div class="list-group">' + SidebarHtml + '</div>');
    TFile.WriteAllText(TPath.Combine(FOutputDir, 'index.html'), FinalHtml);
    
  finally
    var Node: TSyntaxNode;
    for Node in UnitNodes.Values do Node.Free;
    UnitNodes.Free;
  end;
end;

function TDextDocGenerator.GenerateSidebar: string;
var
  SB: TStringBuilder;
  AllUnits: TArray<TUnitInfo>;
  Info: TUnitInfo;
  SafeFileName: string;
begin
  SB := TStringBuilder.Create;
  try
    AllUnits := FRegistry.GetAllUnits;
    
    // Sort by DisplayName
    TArray.Sort<TUnitInfo>(AllUnits, TComparer<TUnitInfo>.Construct(
      function(const Left, Right: TUnitInfo): Integer
      begin
        Result := CompareText(Left.DisplayName, Right.DisplayName);
      end));
    
    for Info in AllUnits do
    begin
      // Use same safe filename logic as Generate
      SafeFileName := Info.DisplayName
        .Replace(' ', '_')
        .Replace('(', '_')
        .Replace(')', '_');
      SB.AppendFormat('<a href="%s.html" class="nav-item">%s</a>', [SafeFileName, Info.DisplayName]);
      SB.AppendLine;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

// Helper to log to file
procedure LogDebug(const Msg: string);
begin
  TFile.AppendAllText('debug_docgen.txt', Msg + sLineBreak);
end;

function TDextDocGenerator.GetXmlDoc(const UnitName: string; Line: Integer): string;
var
  Lines: TArray<string>;
  I: Integer;
  Comment: string;
  Doc: string;
begin
  Result := '';
  if not FSourceLines.TryGetValue(UnitName, Lines) then Exit;
  Doc := '';
  I := Line - 2;
  while (I >= 0) and (I < Length(Lines)) do
  begin
    Comment := Lines[I].TrimLeft;
    if Comment.StartsWith('///') then
    begin
       Comment := Comment.Substring(3).Trim;
       if Doc = '' then Doc := Comment
       else Doc := Comment + ' ' + Doc; 
       Dec(I);
    end
    else Break; 
  end;
  if Doc.Contains('<summary>') then
  begin
     var StartP := Doc.IndexOf('<summary>') + 9;
     var EndP := Doc.IndexOf('</summary>');
     if EndP > StartP then Result := Doc.Substring(StartP, EndP - StartP).Trim;
  end
  else if Doc <> '' then Result := Doc; 
end;

function TDextDocGenerator.RenderHtmlDocumentationObj(CInfo: TClassInfo): string;
var
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    var TargetVis: string;
    for TargetVis in ['PUBLISHED', 'PUBLIC', 'PROTECTED', 'PRIVATE'] do
    begin
       var List := TList<TMemberInfo>.Create;
       try
          for var M in CInfo.Methods do if M.Visibility = TargetVis then List.Add(M);
          for var P in CInfo.Properties do if P.Visibility = TargetVis then List.Add(P);
          for var F in CInfo.Fields do if F.Visibility = TargetVis then List.Add(F);
          
          if List.Count > 0 then
          begin
             SB.AppendFormat('<div class="visibility-group"><h5>%s</h5>', [TargetVis]);
             for var Item in List do
             begin
                 SB.AppendLine('<div class="api-item">');
                 if Item is TMethodInfo then
                 begin
                    var M := TMethodInfo(Item);
                    var Sig := M.Kind + ' ' + M.Name + M.Args + M.ResultType; 
                    if M.Kind = '' then Sig := 'procedure ' + M.Name + M.Args; 
                    SB.AppendFormat('<div class="api-signature">%s</div>', [CleanType(Sig)]);
                 end
                 else if Item is TPropertyInfo then
                 begin
                    SB.AppendFormat('<div class="api-signature">property %s: %s</div>', [Item.Name, CleanType(TPropertyInfo(Item).PropType)]);
                 end
                 else SB.AppendFormat('<div class="api-signature">%s</div>', [Item.Name]); 
                 
                 if Item.XmlDoc <> '' then SB.AppendFormat('<div class="description">%s</div>', [Item.XmlDoc]);
                 SB.AppendLine('</div>');
             end;
             SB.AppendLine('</div>');
          end;
       finally
          List.Free;
       end;
    end;
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

function TDextDocGenerator.GenerateUnitHtml(Info: TUnitInfo; UnitNode: TSyntaxNode): string;
var
  SB, MermaidSB: TStringBuilder;
  ValidClasses: Integer;
begin
  SB := TStringBuilder.Create;
  MermaidSB := TStringBuilder.Create;
  try
    SB.AppendFormat('<h1>Unit %s</h1>', [Info.Name]);
    
    // 1. Render Classes & Mermaid
    // Iterate CLASSES from REGISTRY (The Source of Truth)
    if Info.Classes.Count > 0 then
    begin
        MermaidSB.AppendLine('classDiagram');
        ValidClasses := 0;
        
        for var C in Info.Classes do
        begin
             ValidClasses := ValidClasses + 1;
             // Use CleanMermaidId (List~T~) directly as the Class ID
             MermaidSB.AppendFormat('class %s {', [CleanMermaidId(C.Name)]);
             
             // WE do not check TSyntaxNode here for members, we trust CInfo has members, 
             // but Mermaid renderer extracted explicitly from SyntaxNode inside Generate.
             // Let's stick to CInfo logic if possible? No, we have the UnitNodes from Phase 2 available.
             // Actually, we can just list methods from CInfo!
             
             // Methods and Properties
              var AddedMethods := TList<string>.Create;
              try
                 for var M in C.Methods do 
                 begin
                    var MName := CleanMermaidText(M.Name);
                    if not AddedMethods.Contains(MName) then
                    begin
                        MermaidSB.AppendFormat('    +%s()%s', [MName, sLineBreak]);
                        AddedMethods.Add(MName);
                    end;
                 end;
                 
                 for var P in C.Properties do MermaidSB.AppendFormat('    +%s%s', [CleanMermaidText(P.Name), sLineBreak]);
              finally
                 AddedMethods.Free;
              end;
             
             MermaidSB.AppendLine('}');
             
             // No Label Mapping needed if ID is readable (List~T~)
             
             if C.ParentName <> '' then MermaidSB.AppendFormat('%s <|-- %s%s', [CleanMermaidId(C.ParentName), CleanMermaidId(C.Name), sLineBreak]);
             for var Intf in C.Interfaces do MermaidSB.AppendFormat('%s <|.. %s%s', [CleanMermaidId(Intf), CleanMermaidId(C.Name), sLineBreak]);
        end;
        
        if ValidClasses > 0 then
        begin
           SB.AppendLine('<h2>Classes</h2>');
           SB.AppendLine('<details class="mermaid-details" open>');
           SB.AppendLine('<summary>Class Diagram <span class="collapse-text">(Click to collapse)</span><span class="expand-text">(Click to expand)</span></summary>');
           SB.AppendLine('<div class="mermaid-container">');
           SB.AppendLine('<div class="mermaid">');
           SB.Append(MermaidSB.ToString);
           SB.AppendLine('</div>');
           SB.AppendLine('</div>');
           SB.AppendLine('</details>');
        end;
        
        SB.AppendLine('<h2>API Details</h2>');
        
        // Render Class Details
        for var C in Info.Classes do
        begin
             SB.AppendLine('<div class="class-header">');
              SB.AppendFormat('<h3>%s %s</h3>', [C.Kind, CleanType(C.Name)]); // Show Kind (record/class) - Escape Name for HTML!
              SB.AppendFormat('<a name="%s"></a>', [CleanId(C.Name)]); // Anchor MUST be clean ID (no < >)
             if C.ParentName <> '' then SB.AppendFormat('<div class="ancestor">Inherits from: %s</div>', [CleanType(C.ParentName)]);
             if C.XmlDoc <> '' then SB.AppendFormat('<div class="description">%s</div>', [C.XmlDoc]); // Render XML
             SB.AppendLine('</div>');
             SB.Append(RenderHtmlDocumentationObj(C));
             SB.AppendLine('<hr>');
        end;
    end;
    
    // 2. Global Routines
    if Info.GlobalMethods.Count > 0 then
    begin
       SB.AppendLine('<h2>Global Routines</h2>');
        var List := TList<TMemberInfo>.Create;
       try
          // Cast TMethodInfo to TMemberInfo for re-use of simple loop? 
          // No, reusing logic is hard. Custom loop.
          SB.AppendFormat('<div class="visibility-group"><h5>Routines</h5>', []);
          for var M in Info.GlobalMethods do
          begin
                 SB.AppendLine('<div class="api-item">');
                 
                 var Sig := M.Kind + ' ' + M.Name + M.Args + M.ResultType; 
                 if M.Kind = '' then Sig := 'procedure ' + M.Name + M.Args; 
                 
                 SB.AppendFormat('<div class="api-signature">%s</div>', [CleanType(Sig)]);
                 
                 if M.XmlDoc <> '' then SB.AppendFormat('<div class="description">%s</div>', [M.XmlDoc]);
                 SB.AppendLine('</div>');
          end;
          SB.AppendLine('</div>');
       finally
          List.Free;
       end;
    end;

    // 3. Global Constants
    if Info.GlobalConstants.Count > 0 then
    begin
       SB.AppendLine('<h2>Constants</h2>');
       SB.AppendFormat('<div class="visibility-group">', []);
       for var C in Info.GlobalConstants do
       begin
              SB.AppendLine('<div class="api-item">');
              SB.AppendFormat('<div class="api-signature">const %s</div>', [C.Name]);
              if C.XmlDoc <> '' then SB.AppendFormat('<div class="description">%s</div>', [C.XmlDoc]);
              SB.AppendLine('</div>');
       end;
       SB.AppendLine('</div>');
    end;

    // 4. Global Types/Enums
    if Info.GlobalTypes.Count > 0 then
    begin
       SB.AppendLine('<h2>Types</h2>');
       SB.AppendFormat('<div class="visibility-group">', []);
       for var T in Info.GlobalTypes do
       begin
              SB.AppendLine('<div class="api-item">');
              SB.AppendFormat('<div class="api-signature">type %s = %s</div>', [T.Name, T.Kind]);
              if T.Details <> '' then SB.AppendFormat('<div class="enum-values">Values: %s</div>', [T.Details]);
              if T.XmlDoc <> '' then SB.AppendFormat('<div class="description">%s</div>', [T.XmlDoc]);
              SB.AppendLine('</div>');
       end;
       SB.AppendLine('</div>');
    end;
    
    Result := SB.ToString;
  finally
    MermaidSB.Free;
    SB.Free;
  end;
end;



end.
