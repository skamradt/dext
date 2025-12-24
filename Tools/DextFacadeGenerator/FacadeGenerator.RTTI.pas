unit FacadeGenerator.RTTI;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  System.IOUtils,
  FacadeGenerator;

type
  TRTTIFacadeGenerator = class(TFacadeGenerator)
  private
    FCtx: TRttiContext;
    FKnownUnits: THashSet<string>;
    FOutputPath: string;
    FBaseName: string;
    procedure CollectRTTITypes;
    procedure MergeASTConsts;
  public
    constructor Create(const ASourcePath, AOutputPath, ABaseName: string; AExcludedUnits: TArray<string>); reintroduce;
    destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

{ TRTTIFacadeGenerator }

constructor TRTTIFacadeGenerator.Create(const ASourcePath, AOutputPath,
  ABaseName: string; AExcludedUnits: TArray<string>);
begin
  inherited Create(ASourcePath, '*.pas', AExcludedUnits); 
  FOutputPath := AOutputPath;
  FBaseName := ABaseName;
  
  FCtx := TRttiContext.Create;
  FKnownUnits := THashSet<string>.Create(TOrdinalIgnoreCaseComparer.Create);
end;

destructor TRTTIFacadeGenerator.Destroy;
begin
  FKnownUnits.Free;
  FCtx.Free;
  inherited;
end;

procedure TRTTIFacadeGenerator.Execute;
var
  Files: TArray<string>;
  FileName: string;
  UnitName: string;
begin
  Writeln('----------------------------------------');
  Writeln('Dext Facade Generator (RTTI Mode)');
  Writeln('----------------------------------------');
  Writeln('Source:   ' + FSourcePath);
  Writeln('Output:   ' + FOutputPath); 
  Writeln('BaseName: ' + FBaseName);

  if not TDirectory.Exists(FSourcePath) then
  begin
    Writeln('Error: Source directory not found: ' + FSourcePath);
    Exit;
  end;

  // 1. Scan files to build the list of "Known Units"
  Files := TDirectory.GetFiles(FSourcePath, '*.pas', TSearchOption.SoAllDirectories);
  
  for FileName in Files do
  begin
    UnitName := TPath.GetFileNameWithoutExtension(FileName);
    if IsExcluded(UnitName) then
    begin
       Writeln('Excluded: ' + UnitName);
       Continue;
    end;
    FKnownUnits.Add(UnitName);
  end;

  // 2. Use RTTI 
  CollectRTTITypes;

  // 3. Use AST for Consts
  MergeASTConsts;
  
  // 4. Generate
  GenerateArtifacts(FOutputPath, FBaseName);
  Writeln('RTTI Generation Done.');
end;

procedure TRTTIFacadeGenerator.CollectRTTITypes;
var
  Types: TArray<TRttiType>;
  T: TRttiType;
  UnitDecl: TExtractedUnit;
  UnitName: string;
  EnumT: TRttiEnumerationType;
  QName: string;
  DotPos: Integer;
  TypeName: string;
  Names: TArray<string>;
  EnumMember: string;
  I, J: Integer;
  Existing: TExtractedUnit;
begin
  Types := FCtx.GetTypes;

  for T in Types do
  begin
    if not T.IsPublicType then Continue;
    
    QName := T.QualifiedName;
    DotPos := LastDelimiter('.', QName);
    if DotPos <= 0 then Continue;
    
    UnitName := Copy(QName, 1, DotPos - 1);
    TypeName := Copy(QName, DotPos + 1, MaxInt);

    if not FKnownUnits.Contains(UnitName) then Continue;

    // Find or Create UnitDecl
    UnitDecl := nil;
    for I := 0 to FParsedUnits.Count - 1 do
    begin
      Existing := FParsedUnits[I];
      if SameText(Existing.UnitName, UnitName) then
      begin
        UnitDecl := Existing;
        Break;
      end;
    end;
      
    if UnitDecl = nil then
    begin
      UnitDecl := TExtractedUnit.Create(UnitName);
      FParsedUnits.Add(UnitDecl);
    end;

    if SameText(TypeName, UnitName) then Continue;
    
    if UnitDecl.Types.IndexOf(TypeName) = -1 then
    begin
       UnitDecl.Types.Add(TypeName);
       Writeln(Format('    + Type (RTTI): %s.%s', [UnitName, TypeName]));
    end;

    if T.TypeKind = tkEnumeration then
    begin
      if T is TRttiEnumerationType then
      begin
        EnumT := TRttiEnumerationType(T);
        Names := EnumT.GetNames;
        for EnumMember in Names do
        begin
           if UnitDecl.Consts.IndexOf(EnumMember) = -1 then
             UnitDecl.Consts.Add(EnumMember);
        end;
      end;
    end;
  end;
end;

procedure TRTTIFacadeGenerator.MergeASTConsts;
var
  ASTGen: TFacadeGenerator;
  ASTUnit: TExtractedUnit;
  TargetUnit: TExtractedUnit;
  I: Integer;
  C: string;
begin
  ASTGen := TFacadeGenerator.Create(FSourcePath, '*.pas', FExcludedUnits.ToArray);
  try
    ASTGen.Execute;

    for ASTUnit in ASTGen.ParsedUnits do
    begin
      TargetUnit := nil;
      for I := 0 to FParsedUnits.Count - 1 do
      begin
        if SameText(FParsedUnits[I].UnitName, ASTUnit.UnitName) then
        begin
          TargetUnit := FParsedUnits[I];
          Break;
        end;
      end;

      if TargetUnit = nil then
      begin
        TargetUnit := TExtractedUnit.Create(ASTUnit.UnitName);
        FParsedUnits.Add(TargetUnit);
      end;

      // Merge Types (Recover types missed by RTTI due to stripping or generics)
      for var T in ASTUnit.Types do
      begin
        if TargetUnit.Types.IndexOf(T) = -1 then
        begin
          TargetUnit.Types.Add(T);
          Writeln(Format('    + Type (AST): %s.%s', [ASTUnit.UnitName, T]));
        end;
      end;

      for C in ASTUnit.Consts do
      begin
        if TargetUnit.Consts.IndexOf(C) = -1 then
        begin
          TargetUnit.Consts.Add(C);
          Writeln(Format('    + Const (AST): %s.%s', [ASTUnit.UnitName, C]));
        end;
      end;
    end;
  finally
    ASTGen.Free;
  end;
end;

end.
