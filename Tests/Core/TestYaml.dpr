program TestYaml;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Generics.Collections,
  Dext.Utils,
  Dext.Yaml;

procedure Assert(Condition: Boolean; const Msg: string);
begin
  if not Condition then
    raise Exception.Create(Msg);
end;

procedure TestSimpleScalar;
var
  Parser: TYamlParser;
  Doc: TYamlDocument;
  Scalar: TYamlScalar;
begin
  WriteLn('Testing Simple Scalar...');
  Parser := TYamlParser.Create;
  try
    Doc := Parser.Parse('just a string');
    try
      Assert(Doc.Root <> nil, 'Root is nil');
      Assert(Doc.Root.GetNodeType = yntScalar, 'Root is not scalar');
      
      Scalar := Doc.Root as TYamlScalar;
      Assert(Scalar.Value = 'just a string', 'Scalar value mismatch: ' + Scalar.Value);
      WriteLn('  PASS');
    finally
      Doc.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TestSimpleMapping;
var
  Parser: TYamlParser;
  Doc: TYamlDocument;
  Mapping: TYamlMapping;
  Node: TYamlNode;
begin
  WriteLn('Testing Simple Mapping...');
  Parser := TYamlParser.Create;
  try
    Doc := Parser.Parse('key: value' + sLineBreak + 'another: 123');
    try
      Assert(Doc.Root <> nil, 'Root is nil');
      Assert(Doc.Root.GetNodeType = yntMapping, 'Root is not mapping');
      
      Mapping := Doc.Root as TYamlMapping;
      Assert(Mapping.Children.Count = 2, 'Mapping count mismatch');
      
      Assert(Mapping.TryGet('key', Node), 'Key not found');
      Assert((Node as TYamlScalar).Value = 'value', 'Value mismatch');
      
      Assert(Mapping.TryGet('another', Node), 'Another key not found');
      Assert((Node as TYamlScalar).Value = '123', 'Another value mismatch');
      
      WriteLn('  PASS');
    finally
      Doc.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TestSimpleSequence;
var
  Parser: TYamlParser;
  Doc: TYamlDocument;
  Seq: TYamlSequence;
begin
  WriteLn('Testing Simple Sequence...');
  Parser := TYamlParser.Create;
  try
    Doc := Parser.Parse('- item1' + sLineBreak + '- item2');
    try
      Assert(Doc.Root <> nil, 'Root is nil');
      Assert(Doc.Root.GetNodeType = yntSequence, 'Root is not sequence');
      
      Seq := Doc.Root as TYamlSequence;
      Assert(Seq.Items.Count = 2, 'Sequence count mismatch');
      
      Assert((Seq.Items[0] as TYamlScalar).Value = 'item1', 'Item 1 mismatch');
      Assert((Seq.Items[1] as TYamlScalar).Value = 'item2', 'Item 2 mismatch');
      
      WriteLn('  PASS');
    finally
      Doc.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TestNestedMapping;
var
  Parser: TYamlParser;
  Doc: TYamlDocument;
  Mapping, Nested: TYamlMapping;
  Node: TYamlNode;
begin
  WriteLn('Testing Nested Mapping...');
  var Yaml := 
    'parent:' + sLineBreak +
    '  child: value';
    
  Parser := TYamlParser.Create;
  try
    Doc := Parser.Parse(Yaml);
    try
      Assert(Doc.Root <> nil, 'Root is nil');
      Mapping := Doc.Root as TYamlMapping;
      
      Assert(Mapping.TryGet('parent', Node), 'Parent not found');
      Assert(Node.GetNodeType = yntMapping, 'Parent is not mapping');
      
      Nested := Node as TYamlMapping;
      Assert(Nested.TryGet('child', Node), 'Child not found');
      Assert((Node as TYamlScalar).Value = 'value', 'Child value mismatch');
      
      WriteLn('  PASS');
    finally
      Doc.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TestNestedSequenceInMapping;
var
  Parser: TYamlParser;
  Doc: TYamlDocument;
  Mapping: TYamlMapping;
  Seq: TYamlSequence;
  Node: TYamlNode;
begin
  WriteLn('Testing Nested Sequence in Mapping...');
  var Yaml := 
    'list:' + sLineBreak +
    '  - one' + sLineBreak +
    '  - two';
    
  Parser := TYamlParser.Create;
  try
    Doc := Parser.Parse(Yaml);
    try
      Assert(Doc.Root <> nil, 'Root is nil');
      Mapping := Doc.Root as TYamlMapping;
      
      Assert(Mapping.TryGet('list', Node), 'List not found');
      Assert(Node.GetNodeType = yntSequence, 'List is not sequence');
      
      Seq := Node as TYamlSequence;
      Assert(Seq.Items.Count = 2, 'List count mismatch');
      Assert((Seq.Items[0] as TYamlScalar).Value = 'one', 'Item 1 mismatch');
      
      WriteLn('  PASS');
    finally
      Doc.Free;
    end;
  finally
    Parser.Free;
  end;
end;

begin
  try
    WriteLn('Starting YAML Tests...');
    TestSimpleScalar;
    TestSimpleMapping;
    TestSimpleSequence;
    TestNestedMapping;
    TestNestedSequenceInMapping;
    WriteLn('All YAML tests passed!');
  except
    on E: Exception do
    begin
      WriteLn('Test Failed: ' + E.Message);
      ExitCode := 1;
    end;
  end;

  ConsolePause;
end.
