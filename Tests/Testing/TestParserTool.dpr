program TestParserTool;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.IOUtils,
  Dext.Dashboard.TestScanner in '..\..\Sources\Dashboard\Dext.Dashboard.TestScanner.pas',
  DelphiAST in '..\..\External\DelphiAST\Source\DelphiAST.pas',
  DelphiAST.Consts in '..\..\External\DelphiAST\Source\DelphiAST.Consts.pas',
  DelphiAST.Classes in '..\..\External\DelphiAST\Source\DelphiAST.Classes.pas',
  DelphiAST.SimpleParserEx in '..\..\External\DelphiAST\Source\DelphiAST.SimpleParserEx.pas',
  SimpleParser in '..\..\External\DelphiAST\Source\SimpleParser\SimpleParser.pas',
  SimpleParser.Lexer in '..\..\External\DelphiAST\Source\SimpleParser\SimpleParser.Lexer.pas',
  SimpleParser.Lexer.Types in '..\..\External\DelphiAST\Source\SimpleParser\SimpleParser.Lexer.Types.pas';

procedure DumpAST(Node: TSyntaxNode; Indent: string = '');
var
  C: TSyntaxNode;
  S: string;
begin
  S := SyntaxNodeNames[Node.Typ];
  if Node is TValuedSyntaxNode then
    S := S + ' "' + TValuedSyntaxNode(Node).Value + '"';
    
  if Node.Typ = ntAttribute then
     S := S + ' [ATTR]';
     
  WriteLn(Indent + S);
  
  if Indent.Length > 20 then Exit; // Limit depth
  
  for C in Node.ChildNodes do
    DumpAST(C, Indent + '  ');
end;

procedure Run;
var
  ProjectFile: string;
  Info: TTestProjectInfo;
  F: TTestFixtureInfo;
  M: TTestMethodInfo;
  Root: TSyntaxNode;
begin
  ProjectFile := 'TestAttributeRunner.dpr';
  WriteLn('Scanning: ', ProjectFile);
  
  if not FileExists(ProjectFile) then
  begin
    WriteLn('Error: File not found.');
    Exit;
  end;
  
  // Debug AST Structure
  try
    Root := TPasSyntaxTreeBuilder.Run(ProjectFile);
    try
      WriteLn('--- AST DUMP START ---');
      DumpAST(Root);
      WriteLn('--- AST DUMP END ---');
    finally
      Root.Free;
    end;
  except
    on E: Exception do WriteLn('AST Error: ', E.Message);
  end;
  
  try
    Info := TTestScanner.ScanProject(ProjectFile);
    try
      WriteLn('Fixtures found: ', Info.Fixtures.Count);
      for F in Info.Fixtures do
      begin
        WriteLn('  Fixture: ', F.Name, ' (Line: ', F.LineNumber, ')');
        for M in F.Methods do
        begin
          WriteLn('    Test: ', M.Name, ' (Line: ', M.LineNumber, ')');
        end;
      end;
      
      if Info.Fixtures.Count = 0 then
        WriteLn('NO FIXTURES FOUND!');
        
    finally
      Info.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
