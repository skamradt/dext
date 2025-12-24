program DextFacadeGenerator;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  FacadeGenerator in 'FacadeGenerator.pas',
  FacadeGenerator.RTTI in 'FacadeGenerator.RTTI.pas',
  // Core
  Dext in '..\..\Sources\Core\Dext.pas',
  // Data
  Dext.Entity in '..\..\Sources\Data\Dext.Entity.pas',
  // Web
  Dext.Web in '..\..\Sources\Web\Dext.Web.pas';

var
  Generator: TFacadeGenerator;
  SourcePath: string;
  OutputFolder: string;
  BaseName: string;
  ExcludedStr: string;
  Excluded: TArray<string>;
  I: Integer;
  UseRTTI: Boolean;
begin
  try
    // Configure your Library Path to include '..\..\Libs\DelphiAST\Source'
    
    if ParamCount < 3 then
    begin
      Writeln('Usage: DextFacadeGenerator <SourceDirectory> <OutputDirectory> <BaseFileName> [ExcludedUnits] [-rtti]');
      Writeln('Example: DextFacadeGenerator Source/Data Source/Data Dext.Entity "Dext.Entity,Dext.Entity.Internal" -rtti');
      Exit;
    end;
    
    SourcePath := ParamStr(1);
    OutputFolder := ParamStr(2);
    BaseName := ParamStr(3);
    
    ExcludedStr := '';
    UseRTTI := False;

    for I := 4 to ParamCount do
    begin
      if SameText(ParamStr(I), '-rtti') then
        UseRTTI := True
      else
        ExcludedStr := ParamStr(I);
    end;
      
    Excluded := ExcludedStr.Split([',']);
    
    if UseRTTI then
    begin
      Generator := TRTTIFacadeGenerator.Create(SourcePath, OutputFolder, BaseName, Excluded);
    end
    else
    begin
      Generator := TFacadeGenerator.Create(SourcePath, '*.pas', Excluded);
    end;

    try
      Generator.Execute;
      // In RTTI mode, Execute calls GenerateArtifacts internally or we call it here?
      // TRTTIFacadeGenerator.Execute calls GenerateArtifacts.
      // TFacadeGenerator.Execute DOES NOT call GenerateArtifacts (it just scans).
      
      if not UseRTTI then
         Generator.GenerateArtifacts(OutputFolder, BaseName);
    finally
      Generator.Free;
    end;
    
    Writeln('Done.');

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

