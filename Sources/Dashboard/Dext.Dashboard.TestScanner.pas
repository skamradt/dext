unit Dext.Dashboard.TestScanner;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.IOUtils,
  // DelphiAST dependencies
  DelphiAST,
  DelphiAST.Classes,
  DelphiAST.Consts,
  DelphiAST.Writer;

type
  TTestMethodInfo = class
  public
    Name: string;
    LineNumber: Integer;
    Attributes: TArray<string>;
  end;

  TTestFixtureInfo = class
  public
    Name: string;
    UnitName: string;
    LineNumber: Integer;
    Methods: TObjectList<TTestMethodInfo>;
    constructor Create;
    destructor Destroy; override;
  end;

  TTestProjectInfo = class
  public
    ProjectFile: string;
    Fixtures: TObjectList<TTestFixtureInfo>;
    constructor Create;
    destructor Destroy; override;
  end;

  TTestScanner = class
  private
    class function ParseFile(const AFileName: string): TSyntaxNode;
    class procedure ExtractTests(const AUnitName: string; ARoot: TSyntaxNode; AFixtures: TObjectList<TTestFixtureInfo>);
    class function HasAttribute(AttrsNode: TSyntaxNode; const AAttributeName: string): Boolean;
    class function GetAttributeName(ANode: TSyntaxNode): string;
  public
    class function ScanProject(const AProjectFile: string): TTestProjectInfo;
  end;

implementation

{ TTestFixtureInfo }

constructor TTestFixtureInfo.Create;
begin
  Methods := TObjectList<TTestMethodInfo>.Create(True);
end;

destructor TTestFixtureInfo.Destroy;
begin
  Methods.Free;
  inherited;
end;

{ TTestProjectInfo }

constructor TTestProjectInfo.Create;
begin
  Fixtures := TObjectList<TTestFixtureInfo>.Create(True);
end;

destructor TTestProjectInfo.Destroy;
begin
  Fixtures.Free;
  inherited;
end;

{ TTestScanner }

class function TTestScanner.ScanProject(const AProjectFile: string): TTestProjectInfo;
var
  UnitFiles: TStringList;
  Source: string;
  BasePath: string;
  ParsingFile: string;
  Ast: TSyntaxNode;
  
  // Quick and dirty parser for 'in' clause in .dpr uses
  procedure FindUnits(const ADprSource: string);
  var
    Lines: TArray<string>;
    Line, UnitPath: string;
    P1, P2: Integer;
  begin
    Lines := ADprSource.Split([#13, #10], TStringSplitOptions.ExcludeEmpty);
    for Line in Lines do
    begin
      // Format: UnitName in 'Path\To\Unit.pas'
      if Line.Contains(' in ''') then
      begin
         P1 := Line.IndexOf('''');
         P2 := Line.LastIndexOf('''');
         if (P1 > 0) and (P2 > P1) then
         begin
           UnitPath := Line.Substring(P1 + 1, P2 - P1 - 1);
           if not TPath.IsPathRooted(UnitPath) then
             UnitPath := TPath.Combine(BasePath, UnitPath);
             
           if FileExists(UnitPath) then
             UnitFiles.Add(UnitPath);
         end;
      end;
    end;
  end;
  
begin
  Result := TTestProjectInfo.Create;
  Result.ProjectFile := AProjectFile;
  
  if not FileExists(AProjectFile) then Exit;
  
  BasePath := TPath.GetDirectoryName(AProjectFile);
  UnitFiles := TStringList.Create;
  try
    Source := TFile.ReadAllText(AProjectFile);
    FindUnits(Source);
    
    // Check if the project file itself contains tests (e.g. TestAttributeRunner.dpr defines classes inline)
    UnitFiles.Add(AProjectFile);
    
    for ParsingFile in UnitFiles do
    begin
       try
         Ast := ParseFile(ParsingFile);
         try
           ExtractTests(TPath.GetFileNameWithoutExtension(ParsingFile), Ast, Result.Fixtures);
         finally
           Ast.Free;
         end;
       except
         // Swallow parsing errors
       end;
    end;
    
  finally
    UnitFiles.Free;
  end;
end;

class function TTestScanner.ParseFile(const AFileName: string): TSyntaxNode;
begin
  Result := TPasSyntaxTreeBuilder.Run(AFileName);
end;

class function TTestScanner.HasAttribute(AttrsNode: TSyntaxNode; const AAttributeName: string): Boolean;

  function CheckAttribute(AttrNode: TSyntaxNode): Boolean;
  begin
    Result := SameText(GetAttributeName(AttrNode), AAttributeName);
  end;

var
  Child: TSyntaxNode;
begin
  Result := False;
  if (AttrsNode = nil) or (AttrsNode.Typ <> ntAttributes) then Exit;

  for Child in AttrsNode.ChildNodes do
  begin
    if (Child.Typ = ntAttribute) and CheckAttribute(Child) then Exit(True);
  end;
end;

class function TTestScanner.GetAttributeName(ANode: TSyntaxNode): string;
var
  C: TSyntaxNode;
begin
  Result := '';
  for C in ANode.ChildNodes do
  begin
    if (C.Typ = ntName) and (C is TValuedSyntaxNode) then
      Exit(TValuedSyntaxNode(C).Value);
  end;
end;

class procedure TTestScanner.ExtractTests(const AUnitName: string; ARoot: TSyntaxNode; AFixtures: TObjectList<TTestFixtureInfo>);
var
  InterfaceNode, TypeSection: TSyntaxNode;
  ChildNode, TypeDecl, ClassNode: TSyntaxNode;
  Fixture: TTestFixtureInfo;
  MethodInfo: TTestMethodInfo;
  LastAttributes: TSyntaxNode;
  
  procedure ProcessClass(AClassNode: TSyntaxNode; const AClassName: string; ClassAttributes: TSyntaxNode);
  var
    IsFixture: Boolean;
    MChild: TSyntaxNode;
    LastMethodAttrs: TSyntaxNode;
  begin
    IsFixture := HasAttribute(ClassAttributes, 'TestFixture');
    if not IsFixture then Exit;
    
    Fixture := TTestFixtureInfo.Create;
    Fixture.Name := AClassName;
    Fixture.UnitName := AUnitName;
    Fixture.LineNumber := AClassNode.Line;
    
    var ActualClassNode := AClassNode.FindNode(ntType);
    if ActualClassNode = nil then Exit;

    LastMethodAttrs := nil;
    
    var Members: TList<TSyntaxNode>;
    Members := TList<TSyntaxNode>.Create;
    try
      // Collect all members from all visibility sections
      for var Child in ActualClassNode.ChildNodes do
      begin
        case Child.Typ of
          ntPrivate, ntProtected, ntPublic, ntPublished, ntStrictPrivate, ntStrictProtected:
          begin
            for var VisChild in Child.ChildNodes do
              Members.Add(VisChild);
          end;
        else
          Members.Add(Child); // Direct members (if any)
        end;
      end;
      
      for MChild in Members do
      begin
         if MChild.Typ = ntAttributes then
         begin
           LastMethodAttrs := MChild;
         end
         else if MChild.Typ = ntMethod then
         begin
            if HasAttribute(LastMethodAttrs, 'Test') then
            begin
               MethodInfo := TTestMethodInfo.Create;
               MethodInfo.Name := MChild.GetAttribute(anName);
               MethodInfo.LineNumber := MChild.Line;
               MethodInfo.Attributes := ['Test'];
               Fixture.Methods.Add(MethodInfo);
            end;
            LastMethodAttrs := nil; // Reset after usage
         end
         else 
         begin
           if (MChild.Typ <> ntAnsiComment) and (MChild.Typ <> ntBorComment) and (MChild.Typ <> ntSlashesComment) then
             LastMethodAttrs := nil;
         end;
      end;
    finally
      Members.Free;
    end;
    
    if Fixture.Methods.Count > 0 then
      AFixtures.Add(Fixture)
    else
      Fixture.Free;
  end;

begin
  // Navigate to Interface -> Type Section, OR check Root directly (for Programs)
  InterfaceNode := ARoot.FindNode(ntInterface);
  
  // If interface found, use it as parent for search. If not, use Root (e.g. for .dpr)
  var SearchRoot: TSyntaxNode;
  if InterfaceNode <> nil then 
    SearchRoot := InterfaceNode
  else
    SearchRoot := ARoot;
  
  for TypeSection in SearchRoot.ChildNodes do
  begin
    if TypeSection.Typ = ntTypeSection then
    begin
      LastAttributes := nil;
      for ChildNode in TypeSection.ChildNodes do
      begin
        if ChildNode.Typ = ntAttributes then
        begin
          LastAttributes := ChildNode;
        end
        else if ChildNode.Typ = ntTypeDecl then
        begin
             TypeDecl := ChildNode;
             ClassNode := TypeDecl.FindNode(ntType);
             
             // Check if it is a class. 'class' keyword sets anType='class' in DelphiAST
             if (ClassNode <> nil) and SameText(ClassNode.GetAttribute(anType), 'class') then
             begin
                 ProcessClass(TypeDecl, TypeDecl.GetAttribute(anName), LastAttributes);
             end;
             LastAttributes := nil;
        end
        else
        begin
           if (ChildNode.Typ <> ntAnsiComment) and (ChildNode.Typ <> ntBorComment) and (ChildNode.Typ <> ntSlashesComment) then
             LastAttributes := nil;
        end;
      end;
    end;
  end;
end;

end.
